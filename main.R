library(tidyverse)
library(httr)
library(scales)
library(here)
library(glue)
library(svMisc)
library(patchwork)
library(jsonlite)
library(ggpubr)
library(jpeg)

key1 <- Sys.getenv('AZURE_KEY')
endpoint <- Sys.getenv('AZURE_ENDPOINT_DETECT')

# Check how curves vary witin second sampling to confirm consistency
# Check to make sure different races, ages, angles work too

legal_filetypes <- c(".mp4", ".MOV")

pattern <- str_c(legal_filetypes, collapse="|")

frame_splitter <- function(vid_name, fps) {
  filename <- vid_name %>%
    str_remove_all(pattern) %>%
    trimws
  
  path <- glue(here(), "/Video/", vid_name)
  output_dir <- glue(here(), "/Video/", filename, "_frames")
  dir.create(output_dir)
  system(glue("/Users/neerajsharma/Downloads/ffmpeg -i ", path, " -vf fps=", fps, " ",
      output_dir, "/image%05d.jpg"))
}

requester <- function(image_path, j) {
  # J is the frame number you are currently on
  req <- POST(url = endpoint,
              add_headers(.headers = c("Content-Type" = "application/octet-stream",
                                       "Ocp-Apim-Subscription-Key" = key1)),
              body = upload_file(image_path),
              query = list(returnFaceAttributes = "emotion", 
                           recognitionModel = "recognition_03", 
                           detectionModel = "detection_01"),
              accept_json())
  data <- tibble()
  
  if (length(content(req)) > 0) {
    for(i in seq(length(content(req)))) {
      id <- content(req)[[i]]$faceId
      rectangle_data <- content(req)[[i]]$faceRectangle %>% as_tibble()
      emotion_data <- content(req)[[i]]$faceAttributes$emotion %>%
        as_tibble() %>%
        pivot_longer(everything(),
                     names_to = "emotion",
                     values_to = c("value"))
      data <- bind_rows(data, tibble("i" = j, "id" = id, rectangle_data, "emotion" = emotion_data$emotion, "value" = emotion_data$value))
    }
  } else {
    emotion_data <- tibble(contempt = NA, disgust = NA, fear = NA, happiness = NA,
                   neutral = NA, sadness = NA, surprise = NA) %>%
      pivot_longer(everything(),
                   names_to = "emotion",
                   values_to = c("value"))
    data <- tibble("i" = j, "id" = NA, "top" = 0, "left" = 0, "width" = 0, "height" = 0, "emotion" = NA, "value" = NA)
  }
  return(data)
}

photo_batcher <- function(vid_name) {
  filename <- vid_name %>%
    str_remove_all(pattern) %>%
    trimws
  frames <- dir(glue(here(), "/Video/", filename, "_frames"), full.names = TRUE)
  
  # Preallocation
  data_store <- vector(mode = "list", length = length(frames))
  for (i in seq_along(frames)) {
    # svMisc::progress(i, progress.bar = TRUE)
    selection <- requester(frames[i], i)
    data_store[[i]] <- tibble(selection)
  }
  return(bind_rows(data_store))
}


grouper <- function(full_dataframe){
  ids <- full_dataframe %>% 
    distinct(id) %>% 
    drop_na(id) %>%
    pull(id)
  
  print(glue("Number of IDs: ", length(ids)))
  
  if(length(ids) <= 1000) {
    req <- POST(url = Sys.getenv('AZURE_ENDPOINT_GROUP'),
         add_headers(.headers = c("Content-Type" = "application/json",
                                  "Ocp-Apim-Subscription-Key" = key1)),
         body = list("faceIds" = ids),
         encode = "json")
    
    num_groups <- length(content(req))
    
    groupings <- content(req)[1] %>% 
      as_tibble() %>% 
      mutate(group_num = row_number()) %>% 
      unnest() %>% 
      unnest() %>%
      select(group_num, id = groups)
  # } else {
      
  }
  
  # Use groupings to left join to get the groups added onto full_data. 
  return(groupings)
}

grouper_editor <- function(full_data, change_from, change_to, filename) {
  # allows you to manually edit the groupings of a dataframe.
  # Make a new col called edited_group_num that is referenced in ggplots and stuff in app.R
  # Num_groups is the number of faces there should be. 
  
  # Two drop down menus, one on left side, one on right side, and a click button that makes the change
  # Updates the spreadsheet too
  full_data_edited <- full_data %>%
    mutate(group_num = ifelse(group_num == change_from, change_to, group_num))
  
  write_csv(full_data_edited, glue(here(), "/Video/", filename, "_emotion.csv"))
  return(full_data_edited)
}

image_dimensions <- function(vid_name){
  filename <- vid_name %>%
    str_remove_all(pattern) %>%
    trimws
  
  return(dim(readJPEG(glue(here(), "/Video/", filename, "_frames/image00001.jpg")))[-3])
  
}

main <- function(vid_name, fps) {
  filename <- vid_name %>%
    str_remove_all(pattern) %>%
    trimws
  
  # Check if emotion already exists
  if (file.exists(glue(here(), "/Video/", filename, "_emotion.csv"))) {
    print("DATA ALREADY EXISTS")
    return(read_csv(glue(here(), "/Video/", filename, "_emotion.csv")))
  # If the emotion data doesnt exist, make it. 
  } else {
    print("Frame Spliting")
    frame_splitter(vid_name, fps)
    print("Sending frames to API")
    full_data <- photo_batcher(vid_name)
    print("Grouping")
    answer <- grouper(full_data)
    print("Joining Groups with Data")
    full_data_wGroups <- left_join(full_data, answer) %>%
      drop_na(group_num)
    print("Writing to disk")
    write_csv(full_data_wGroups, glue(here(), "/Video/", filename, "_emotion.csv"))
    return(full_data_wGroups)
  }
}

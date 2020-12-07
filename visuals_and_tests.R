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

# verify syntax, use between two faces specifically
POST(url = Sys.getenv('AZURE_ENDPOINT_VERIFY'),
     add_headers(.headers = c("Content-Type" = "application/json",
                              "Ocp-Apim-Subscription-Key" = key1)),
     body = list("faceId1" = "90f12cd0-8da0-4d73-a432-f91a3634f0b9", "faceId2" = "97157f93-a2b6-4304-b9d6-c13d7ba8ad1f"),
     encode = "json")

# Group syntax, used to group up to 1000 face IDs
POST(url = Sys.getenv('AZURE_ENDPOINT_GROUP'),
     add_headers(.headers = c("Content-Type" = "application/json",
                              "Ocp-Apim-Subscription-Key" = key1)),
     body = list("faceIds" = c("bea231e6-e33a-477b-b300-30884d66f484", "ca98cdb0-c868-453a-81f5-e12cbcaf8109", "679dc750-57d5-4bf3-a585-2f3c37ee8e1d", "350d0491-546b-481f-bc96-a834df42bda1", "9b76a25b-266b-4f47-b65a-9d6b0b18be83", "5b773af8-faf0-4556-9eee-b8e99842afb6")),
     encode = "json")



# Query to get the output for my individual face. 
req <- POST(url = endpoint,
            add_headers(.headers = c("Content-Type" = "application/octet-stream",
                                     "Ocp-Apim-Subscription-Key" = key1)),
            body = upload_file(glue(here(), "/Video/ObamaShort_frames/image00029.jpg")),
            query = list(returnFaceLandmarks = TRUE, returnFaceAttributes = "emotion,gender,smile,facialHair,headPose,glasses,hair,makeup,accessories",
                         recognitionModel = "recognition_03",
                         detectionModel = "detection_01"),
            accept_json())

toJSON(content(req), pretty = TRUE)

obama <- tibble()

if (length(content(req)) > 0) {
  for(i in seq(length(content(req)))) {
    id <- content(req)[[i]]$faceId
    rectangle_data <- content(req)[[i]]$faceRectangle %>% as_tibble()
    emotion_data <- content(req)[[i]]$faceAttributes$emotion %>%
      as_tibble() %>%
      pivot_longer(everything(),
                   names_to = "emotion",
                   values_to = c("value"))
    obama <- bind_rows(obama, tibble("i" = 1, "id" = id, rectangle_data, "emotion" = emotion_data$emotion, "value" = emotion_data$value))
  }
}
obama_landmarks <- tibble()
if (length(content(req)) > 0) {
  for(i in seq(length(content(req)))) {
    id <- content(req)[[i]]$faceId
    obama_landmarks <- content(req)[[i]]$faceLandmarks %>%
      as_tibble() %>%
      unnest() %>%
      mutate(dims = c("x","y")) %>%
      pivot_longer(cols = -dims) %>%
      mutate("id" = id) %>%
      pivot_wider(-dims, names_from = dims) %>%
      bind_rows(obama_landmarks)

  }
}

obama_grouped <- obama %>%
  mutate(group_num = ifelse(row_number() <= 8, 1,2),
         xmin = left,
         xmax = left + width,
         ymax = -top,
         ymin = -top - height,
         group_num = factor(group_num))

p1 <- ggplot() +
  background_image(readJPEG(normalizePath(file.path(glue(here(), "/Video/ObamaShort_frames/image00029.jpg"))))) +
  geom_rect(obama_grouped, mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = group_num), fill = NA) +
  geom_point(obama_landmarks, mapping = aes(x = x, y = -y)) +
  # geom_text(obama_landmarks, mapping = aes(x = x, y = -y, label = name)) +
  scale_color_manual(values = c("#2E88BF", "#F37721")) +
  scale_x_continuous(limits = c(0, 1280)) +
  scale_y_continuous(limits = c(-720, 0)) +
  labs(x = NULL, y = NULL) +
  theme(plot.background = element_rect(fill = rgb(15, 42, 60, maxColorValue = 255)),
        panel.background = element_rect(fill = "#0F2A3C"),
        panel.grid = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(color = "white", size = 22),
        axis.text.x = element_text(color = "white", size = 16),
        axis.title.x = element_text(color = "white", size = 20),
        axis.ticks.y = element_blank())

p2 <- obama_grouped %>%
  ggplot(aes(x = emotion, y = value, fill = group_num)) +
  geom_col(position = "dodge") +
  labs(fill = "Group", y = NULL) +
  scale_fill_manual(values = c("#2E88BF", "#F37721")) +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = rgb(15, 42, 60, maxColorValue = 255)),
        panel.background = element_rect(fill = "#0F2A3C"),
        legend.background = element_rect(fill = "#0F2A3C"),
        legend.title = element_text(color = "white", size = 20),
        legend.text = element_text(color = "white", size = 16),
        legend.key = element_rect(color = "#0F2A3C"),
        panel.grid = element_blank(),
        axis.text.y = element_text(color = "white", size = 22),
        axis.text.x = element_text(color = "white", size = 16),
        axis.title.x = element_text(color = "white", size = 20),
        axis.ticks.y = element_blank())

p3 <- read_csv(glue(here(), "/Video/ObamaShort_emotion.csv")) %>%
  filter(emotion != "neutral") %>%
  ggplot(aes(x = i, y = value, color = emotion)) +
  geom_smooth(span = 0.3, se = FALSE) +
  labs(x = "Frame Number",
       y = "Emotional Probability",
       color = NULL) +
  facet_grid(~group_num) +
  scale_x_continuous(limits = c(0, 91)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("#2E88BF", "#F37721", "#469856", rgb(198,46,51,maxColorValue=255), "#FFFFFF", rgb(74,77,139, maxColorValue=255), rgb(28,59,240, maxColorValue = 255))) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        plot.background = element_rect(fill = rgb(15, 42, 60, maxColorValue = 255)),
        panel.background = element_rect(fill = "#0F2A3C"),
        legend.background = element_rect(fill = "#0F2A3C"),
        legend.title = element_text(color = "white", size = 20),
        legend.text = element_text(color = "white", size = 16),
        legend.key = element_rect(color = "#0F2A3C"),
        panel.grid = element_blank(),
        axis.text.y = element_text(color = "white", size = 22),
        axis.text.x = element_text(color = "white", size = 16),
        axis.title.y = element_text(color = "white", size = 20),
        axis.title.x = element_text(color = "white", size = 20),
        axis.ticks.y = element_blank(),
        # strip.background = element_rect(fill = "#0F2A3C"),
        strip.text = element_text(color = "white", size = 20)) +
        guides(color = guide_legend(nrow = 1,
                                    direction = "horizontal",
                                    label.position = "bottom",
                                    override.aes = list(size = 6)))


g <- ggplot_gtable(ggplot_build(p3))
stripr <- which(grepl('strip-', g$layout$name))
fills <- c("#2E88BF", "#F37721")
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid::grid.draw(g)


# Produce graph comparing different APIs
tibble(
  "Product" = c(
    "Google Cloud, DialogFlow",
    "Google Cloud, Vision",
    "Amazon Web Services, Rekognition",
    "Amazon Web Services, Transcribe",
    "Microsoft Azure, Face",
    "IBM Watson, Visual Recognition",
    "Google Cloud, Natural Language"
  ),
  "Cost per 1000 calls" = c(2, 1.5, 1, 4, 0.8, 1.5, 1),
  "Service" = c("NLP", "Vision", "Vision", "NLP", "Vision", "Vision", "NLP")
) %>% ggplot(aes(x = fct_reorder(Product, `Cost per 1000 calls`), y = `Cost per 1000 calls`, fill = Service)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("#2E88BF", "#F37721")) +
  scale_y_continuous(labels=scales::dollar_format()) +
  labs(x = NULL) +
theme(plot.background = element_rect(fill = rgb(15, 42, 60, maxColorValue = 255)),
      panel.background = element_rect(fill = "#0F2A3C"),
      legend.background = element_rect(fill = "#0F2A3C"),
      legend.title = element_text(color = "white", size = 20),
      legend.text = element_text(color = "white", size = 16),
      legend.key = element_rect(color = "#0F2A3C"),
      panel.grid = element_blank(),
      axis.text.y = element_text(color = "white", size = 22),
      axis.text.x = element_text(color = "white", size = 16),
      axis.title.x = element_text(color = "white", size = 20),
      axis.ticks.y = element_blank())
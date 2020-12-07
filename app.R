library(shiny)
library(shinyFiles)

source("main.R")

ui <- fluidPage(
  titlePanel("GUI to Azure API"),
  sidebarPanel(
    fluidRow(splitLayout(cellWidths = c("40%", "15%", "40%"),
    shinyFilesButton("dir", label = "File Select", multiple = FALSE, title = "Select a File", style = "width: 100%"),
    textInput("fps", label = NULL, value = 2),
    actionButton("button", "Initiate", width = "100%"))), 
    uiOutput("smoothing"),
    fluidRow(splitLayout(cellWidths = c("50%", "50%"), cellArgs = list(style = "padding-bottom: 75px"), uiOutput("changed_from"), uiOutput("changed_to"))),
    uiOutput("confirm_group_change"),
    width = 3),
  mainPanel(
    uiOutput("frame"),
    textOutput("displaypath"),
    fluidRow(splitLayout(cellWidths = c("60%", "40%"), plotOutput("plot1"), plotOutput("plot2"))),
    textOutput("imageURL"),
    plotOutput("frame_image_overlaid"))
)

server <- function(input, output, session) {
  shinyFileChoose(input, 'dir', roots = c(home = here()), filetypes = str_remove(legal_filetypes, "."))
  
  values <- reactiveValues()
  
  # Display filepath of the video navigated too
  output$displaypath <- renderText({parseFilePaths(c(home = here()), input$dir)}$datapath)

  
  observeEvent(input$button, {
    values$out <- parseFilePaths(c(home = here()), input$dir)$name
    
    values$data <- main(values$out, input$fps) %>% 
      filter(emotion != "neutral") %>%
      mutate(xmin = left,
             xmax = left + width,
             ymax = -top,
             ymin = -top - height,
             group_num = factor(group_num))
    
    dims <- image_dimensions(values$out)
    values$y <- dims[1]
    values$x <- dims[2]
    
    # Plot Emotion
    output$plot1 <- renderPlot({
      values$data %>%
        ggplot(aes(x = i, y = value, color = emotion)) + 
          geom_smooth(span = input$smoothing, se = FALSE) + 
          geom_vline(xintercept = input$frame, width = 4) + 
          labs(title = glue("Emotional movement of ", values$out),
               x = "Frame Number",
               y = "Emotional Probability",
               color = NULL) +
          facet_wrap(~group_num) +
          scale_x_continuous(limits = c(0, max(values$data$i))) +
          scale_y_continuous(limits = c(0, 1)) + 
          guides(color = guide_legend(nrow = 1, direction = "horizontal", label.position = "bottom", override.aes = list(size = 6))) +
          theme(legend.position = "bottom", plot.background = element_rect(fill = "lightblue"), legend.background = element_blank())
      })
    
    # Plot center of face location
    output$plot2 <- renderPlot({
      values$data %>% 
        ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = group_num)) + 
        geom_rect(fill = NA) + 
        scale_color_discrete() +
        labs(title = "Clustered Position of Faces", color = NULL) + 
        scale_x_continuous(limits = c(0, values$x)) +
        scale_y_continuous(limits = c(-values$y, 0)) 
    })
    
    # Create the frame slider 
    output$frame <- renderUI({sliderInput("frame", "Frame Number:", min = 1, 
                                          max = max(values$data["i"]), value = 1, step = 1, width = "100%")})
    
    # Create the smoothing slider
    output$smoothing <- renderUI({sliderInput("smoothing", "Smoothing: ", min = 0.01, max = 1, value = 0.5, width = "100%")})
    
    # Create the edit groupings
    output$changed_from <- renderUI({selectInput("changed_from", "Change Group", values$data$group_num %>% unique() %>% sort())})
    output$changed_to <- renderUI({selectInput("changed_to", "To", values$data$group_num %>% unique() %>% sort())})
    output$confirm_group_change <- renderUI({actionButton("confirm_group_change", "Confirm Group Change", width = "100%")})
    
    # Establish name of file to get into contents
    pattern <- str_c(c(".mp4", ".MOV"), collapse="|")
    values$name <- values$out %>%
      str_remove_all(pattern) %>%
      trimws
    
    # Create and update image URL
    output$imageURL <- renderText({glue(here(), "/Video/", values$name, 
                                        "_frames/image",
                                        str_pad(input$frame, 5, pad = "0"), ".jpg")})
    
    # Render the image of the grame overlaid with hitboxes
    output$frame_image_overlaid <- renderUI({plotOutput("frame_image_overlaid")})
    output$frame_image_overlaid <- renderPlot({
      values$data %>% 
        filter(i == input$frame) %>%
        ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = group_num)) + 
        background_image(readJPEG(normalizePath(file.path(glue(here(), "/Video/", values$name, "_frames/image",
                                                               str_pad(input$frame, 5, pad = "0"), ".jpg"))))) +
        geom_rect(fill = NA) + 
        scale_color_discrete() +
        labs(color = NULL) + 
        scale_x_continuous(limits = c(0, values$x)) +
        scale_y_continuous(limits = c(-values$y, 0)) +
        theme(axis.text = element_text(size = 25))
    }, width = values$x, height = values$y)
    
  })
  
  # Group Changer Handling
  observeEvent(input$confirm_group_change, {
    values$data <- grouper_editor(values$data, input$changed_from, input$changed_to, values$name)
  })
  
  

}

shinyApp(ui = ui, server = server)
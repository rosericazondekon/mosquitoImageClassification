#------------------------------------------------------------------------------------
# Mosquito image Classification Deployment App
# https://rosericazondekon.github.io/posts/mosquito-image-classification-with-torch/
#
# Atlanta, April 2023
# Author: G. Roseric Azondekon (roseric_2000@yahoo.fr)
#------------------------------------------------------------------------------------

options(shiny.maxRequestSize = 100 * 1024^2)

suppressPackageStartupMessages({
  libs <- c(
    "shiny", "torch", "torchvision", "luz", "shinycssloaders", "flexdashboard"
  )
  
  if (length(setdiff(libs, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(libs, rownames(installed.packages()))
                     , repos = "http://cran.us.r-project.org")
  }
  
  lapply(libs, library, character.only = TRUE, quietly = TRUE)
})

# Load torch model
model <- torch_load("mosquito_class_model.rt")

# Define class names
class_names <- c("Aedes aegypti", "Anopheles stephensi", "Culex quinquefasciatus")

# Define helper functions
predict_species <- function(path, dl_model = model, all_classes = class_names){
  img_tensor <- path %>% 
    jpeg::readJPEG() %>% 
    transform_to_tensor() %>%
    (function(x) x$to(device = device)) %>% 
    transform_resize(size = c(256, 256)) %>% 
    transform_normalize(mean = c(0.485, 0.456, 0.406), std = c(0.229, 0.224, 0.225))
  
  prediction <- img_tensor$unsqueeze(dim = 1) %>% 
    dl_model() %>% 
    softmax(dim=2) %>% 
    torch_max(dim = 2)
  
  list(
    "class" = all_classes[prediction[[2]] %>% as.numeric],
    "certainty" = round(as.numeric(prediction[[1]]) * 100, 2)
  )
}

softmax <- function(x, dim=1) {
  exp(x - max(x, dim=dim)) / sum(exp(x - max(x, dim=dim)), dim=dim)
}

# Front-end User Interface
ui <- navbarPage(
  title = "Mosquito Specie Prediction App",
  sidebarLayout(
    sidebarPanel(
      helpText(HTML("Only submit a picture in JPEG of one of <i>Anopheles stephensi</i>, 
      <i>Culex Quinquefasciatus</i> or <i>Aedes aegipti</i> to the app..<br><br>
      Uploading any other images would lead to unreliable prediction.<br><br>")),
      fileInput("file1", "Upload a JPEG file"),
      withSpinner(uiOutput("gauge_ui"))
    ),
    mainPanel(
      tags$div(
        style = "display: flex; justify-content: center;",
        fluidRow(column(12, uiOutput("image_ui")))
      )
    )
  )
)

#Back-end server 
server <- function(input, output) {
  output$image_ui <- renderUI({
    file <- input$file1
    if (is.null(file)) return(NULL)
    if (file$type != "image/jpeg") return(NULL)
    
    tagList(
      fluidRow(
        column(12, align="center", imageOutput("image"))
      )
    )
  })
  
  predict_from_image <- reactive({
    message(input$file1$datapath)
    input$file1$datapath %>% 
      predict_species(., dl_model = model, all_classes = class_names)
  })
  
  output$image <- renderImage({
    file <- input$file1
    list(src = file$datapath,
         contentType = file$type,
         height = 600)
  }, deleteFile = FALSE)
  
  output$prediction_ui <- renderText({
    predict_from_image()[["class"]]
  })
  
  output$gauge = renderGauge({
    gauge(predict_from_image()[["certainty"]],
          min = 0, 
          max = 100, 
          label = "Prediction certainty",
          symbol = "%",
          sectors = gaugeSectors(success = c(50, 100), 
                                 warning = c(30, 50),
                                 danger = c(0, 30)))
  })
  
  output$gauge_ui <- renderUI({
    file <- input$file1
    if (is.null(file)) return(NULL)
    if (file$type != "image/jpeg") return(NULL)
    tagList(br(),br(),br(),
      fluidRow(
        column(
          12, align="center", 
          div(
            style = "display: inline-block;vertical-align:top; width: 150px;",
            HTML("<b>Predicted Species: </b>"), 
            span(textOutput("prediction_ui"), style="color:blue;display:inline-block;font-size:20px;font-style: italic;")
          )
        )
      ),br(),br(),br(),
      fluidRow(column(12, align="center", gaugeOutput("gauge"))),
    )
  })
}

# Putting it altogether
shinyApp(ui, server)

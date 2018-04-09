library(shiny)

# add resource path from where data will be retrieved
addResourcePath("localfiles", "C:/Users/Hilser/Documents/R/Shiny/MetaShiny/www")

ui <- fluidPage(
  headerPanel("MetaShiny"),
  sidebarLayout( 
    sidebarPanel(
      width = 4,
      numericInput("CurrentItem", "Item number", value = 1, min = 1, max = 220, width = 80),
      splitLayout(
        textInput("HighlightWord1", "Highlight this word in the text...", value = "", width = 180),
        textInput("HighlightColor1", "... in this color", value = "", width = 125)
        ),
      submitButton("Highlight words")
      ), 
    mainPanel(
      
      tabsetPanel(
        tabPanel("Table",
                 tableOutput(outputId = "reviewFile")),
        tabPanel("PDF",
                 tags$iframe(
                   style = "height: 600px; width: 100%; scrolling = yes",
                   src = "localfiles/NewFile.pdf", sep = "" # show local file in www-folder
                   )
                 ),
        tabPanel("Categories",
                 splitLayout(
                   cellWidths = c(200, 700),
                   radioButtons("include", "Include or Exclude", choices = c("not vetted", "Yes", "No", "Maybe")),
                    htmlOutput("reviewFileCurrentItem")
                   # verbatimTextOutput(lapply("reviewFileCurrentItem", function(x) {gsub("learn", "<b>LEARN</b>", "reviewFileCurrentItem")}))
                   ),
                 radioButtons("studyType", "Type of Study", choices = c("not vetted", "empirical", "conceptual", "review"))
                 ), 
        tabPanel("Other", 
                 "Content")
        )
      ) # End of tabsetPanel 
    ) # End of mainPanel
)
 

server <- function(input, output){

# read in local scopus csv-file
  reviewFile <- read.csv("C:/Users/Hilser/Documents/R/Shiny/MetaShiny/data/reviewFile.csv")
  reviewFile <- reviewFile[,c("STUDY_ID", "Title", "Abstract", "INCLUDE","INCLUDE_COMMENT", "TYPE_OF_STUDY", "TYPE_OF_STUDY_COMMENT", "DOI")]
  
  
  reviewFile <- data.frame(lapply(reviewFile, {function(x) gsub("learn", paste("<b>", "learn", "</b>"), x)}), stringsAsFactors = FALSE)
  #reviewFile
  
  # reviewFile <- gsub("communities", "COMMUNITIES", reviewFile)
  # reviewFile <- lapply(reviewFile, 
  #                      function(x) {gsub("learn", "<b>LEARN</b>", reviewFile[, "Abstract"])}
  #                      )
  
  output$reviewFile <- renderTable({reviewFile})
  output$reviewFileCurrentItem <- renderText({
    paste(reviewFile[input$CurrentItem, "Abstract"])
  })
}
  
shinyApp(ui = ui, server = server)

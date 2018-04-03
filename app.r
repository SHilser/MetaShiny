### START: Load libraries #### ####
library(shiny)
library(stringr)
library(maps)
library(DT)
### END:   Load libraries ####

addResourcePath("localfiles", "C:/Users/Hilser/Documents/R/Shiny/Sample/www")

##############################################################################################################################
#                                           Start User Interface
##############################################################################################################################

ui <- fluidPage(
  headerPanel(h3("MetaShiny")),
  numericInput("CurrentItem", "Item number", value = 1, min = 1, max = 220, width = 120),
  tabsetPanel(
   ### START: Prepare File -> add review category creator #### ####
    tabPanel("Prepare File",
       sidebarLayout( 
         sidebarPanel(
           width = 4,
           fileInput("dataFile", "Choose CSV-file", accept = c("text/csv", "text/comma-separated-values", "text/plain"))
           ,
           actionButton("showColumns", "Show columns"),
           tags$div(id = "chooseColumns")
           # textInput("Bla", "Enter BlaBla", width = 250, placeholder = "This is a placeholder")
           ),
         mainPanel(DT::dataTableOutput(outputId = "myTable"))
         )
       )
   ### END:   Prepare File -> add review category creator ####
   
   ### START: Abstract Screening #### ####
    ,
    tabPanel("Abstract Screening",
      sidebarLayout( 
        sidebarPanel(
          width = 3,
          splitLayout(
            textInput("HighlightWord1", "Highlight this word...", value = "", placeholder = "Enter word to highlight", width = 180),
            textInput("HighlightColor1", "... in this color", value = "", width = 125)
            ),
          splitLayout(
            textInput("HighlightWord2", "Highlight this word...", value = "", placeholder = "Enter word to highlight", width = 180),
            textInput("HighlightColor2", "... in this color", value = "", width = 125)
            )
          # ,
          #  actionButton(inputId = "addHighlight", "Highlight words")
          # ,
          # dataTableOutput("dfHighlightedWords")
          ),
      
        mainPanel(h4("Title"),
                  tableOutput(outputId = "reviewFileCurrentTitle"),
                  h4("Abstract"),
                  tableOutput(outputId = "currentAbstract"),
                  h4("Keywords"),
                  tableOutput(outputId = "currentKeywords"),
                  h4("Type of Study")
                  ,
                  splitLayout(
                    cellWidths = c("14%", "43%", "43%"),
                    radioButtons("include", "Include or Exclude", choices = c("not vetted", "Yes", "No", "Maybe")),
                    textAreaInput("IncludeSnippet", "", width = "120%", height = 120, placeholder = "please paste text here upon which you base your decision"),
                    textAreaInput("IncludeSnippet", "", width = "120%", height = 120, placeholder = "please enter comments here")
                    )
                  )
        )
      )
   ### END:   Abstract Screening ####
    
   ### START: EXPERIMENTAL: Download PDFs #### ####
    # ,
    # tabPanel("EXPERIMENTAL: Download PDFs",
    #          sidebarLayout(
    #            sidebarPanel(
    #              width = 4,
    #              actionButton("DownloadPDF", label = "Download current PDF")
    #              ),
    #            mainPanel(
    #              
    #            )
    #            )
    #          )
   ### END:   EXPERIMENTAL: Download PDFs ####
    
   ### START: Fulltext review, showing local PDF #### ####
    ,
    tabPanel("Fulltext Review",
             sidebarLayout( 
               sidebarPanel(
                 width = 3,
                 # Detect selected Text: found on https://stackoverflow.com/questions/42274461/can-shiny-recognise-text-selection-with-mouse-highlighted-text
                 # a shiny element to display unformatted text
                 verbatimTextOutput("markedText"),
                 # javascript code to send data to shiny server
                 tags$script('
                             function getSelectionText() {
                             var text = "";
                             if (window.getSelection) {
                             text = window.getSelection().toString();
                             } else if (document.selection) {
                             text = document.selection.createRange().text;
                             }
                             return text;
                             }
                             
                             document.onmouseup = document.onkeyup = document.onselectionchange = function() {
                             var selection = getSelectionText();
                             Shiny.onInputChange("mydata", selection);
                             };
                             '),
                 tabsetPanel(
                   tabPanel("General Information",
                     textInput("Bla", "Enter BlaBla", width = 250, placeholder = "This is a placeholder"),
                     radioButtons("typeOfStudy", "Type of Study", choices = c("not vetted", "Empirical", "Conceptual", "Review"), inline = TRUE),
                     selectInput("caseStudyLocation", "Place of case study",
                                 choices =  map("world", namesonly = TRUE, plot = FALSE), multiple = TRUE),
                     selectInput("firstAuthorAffiliation", "Affiliation of first author (University, city, country)",
                                 choices = NULL)
                   ),
                   tabPanel("MITD",
                            h4("What is framed as MITD?"),
                            splitLayout(cellWidths = c(100, 20, 60, 60),
                                        "",
                                        "",
                                        "Object",
                                        "Subject"
                            ),
                            splitLayout(cellWidths = c(100, 20, 60, 60),
                                        "Multidisciplinary",
                                        "",
                                        checkboxInput("mdObject", label = ""),
                                        checkboxInput("mdSubject", label = "")
                                        ),
                            splitLayout(cellWidths = c(100, 20, 60, 60),
                                        "Interdisciplinary",
                                        "",
                                        checkboxInput("idObject", label = ""),
                                        checkboxInput("idSubject", label = "")
                                        )
                            )
                   )
               ),
               mainPanel(
                 htmlOutput("pdfviewer")
                 )
               )
             )
   ### END:   Fulltext review, showing local PDF ####
    
   ### START: SHOW PDF from Sci-Hub based on DOI from file (WORKING!!!), maybe adapt to University library #### ####
    ,
    tabPanel("SciHub",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 tableOutput("currentFileName"),
                 tableOutput("currentItemIncluded")
               ),
               mainPanel(
                 # h4("Title"),
                 # textOutput(outputId = "reviewFileCurrentTitle"), # when this line is activated the upload doesn't work anymore, no clue why? Apparently Shiny can't call the same output element twice!
                 htmlOutput("pdfviewer2")
               )
             ))

   ### END:   SHOW PDF from Sci-Hub based on DOI from file (WORKING!!!), maybe adapt to University library ####
    
   ### START: EXPERIMENTAL: Create Dynamic User Interface #### ####
   ,
   tabPanel("Dynamic UI",
            sidebarLayout(
              sidebarPanel(
                width = 4,
                numericInput(inputId = "numberOfTabs", label = "Number of Tabs", 1),
                actionButton("insertBtn", "Insert"),
                # uiOutput("tabElements"),
                tags$div(id = "placeholder"),
                tags$div(id = "changeTabNames")
              ),
              mainPanel(
                uiOutput("tabElements")
                )
              )
            )
   )
   
   ### END:   EXPERIMENTAL: Create Dynamic User Interface ####

   ### START: Highlight Experiment (IN PROGRESS) #### ###############################
    # ,
    # tabPanel("Highlight Experiment",
    #          sidebarLayout(
    #            sidebarPanel(
    #              width = 3,
    #              textInput("addNewHighlight", "Which text should be highlighted?"),
    #              actionButton("buttonAddNewHighlight", "Add highlight")
    #              
    #            ),
    #            mainPanel(
    #              htmlOutput("pdfviewer2")
    #            )
    #          ))
   ### END:   Highlight Experiment (IN PROGRESS) ####
)
#############################################################################################################################################
#                                                               Start SERVER
#############################################################################################################################################

server <- function(input, output){
  ### START: EXPERIMENTAL: GET marked text from embedded PDF #### ####
  output$markedText = renderPrint({
    input$mydata
  })
  ### END:   EXPERIMENTAL: GET marked text from embedded PDF #### ####
  
  ### START: create tabs #### ####
  listOfTabNames <- reactive ({
    # defaultTabNames <- 
      paste0("Tab", 1:input$numberOfTabs)
    # observeEvent(input$numberOfTabs, {
    #   defaultTabNames[1:input$numberOfTabs] <- textOutput(paste0("input$nameOfTab", 1:input$numberOfTabs))
      # })
    })
  
  output$tabElements <- renderUI({
    TabNames <- lapply(listOfTabNames(), tabPanel)
    do.call(tabsetPanel, TabNames)
  })
  ### END:   create tabs ####
  
  ### START: EXPERIMENTAL: Rename dynamic Tabs #### ####
  observeEvent(input$numberOfTabs, {
    insertUI(
      selector = "#changeTabNames",
      where = "beforeEnd",
      ui = textInput(inputId = paste0("nameOfTab", input$numberOfTabs),
                     label = paste0("Name of Tab ",input$numberOfTabs),
                     value = paste0("Tab", input$numberOfTabs)
                     )
    )
  })
  ### END:   EXPERIMENTAL: Rename dynamic Tabs ####
  
  ### START: Insert fields, used to define review widgets #### ####
  count <- 0
  observeEvent(input$insertBtn, {
  
    count <- count + 1
    insertUI(
      selector = "#placeholder",
      where = "afterEnd",
      ui = splitLayout(selectInput(inputId = paste("Element", count, sep = "_"),
                                   choices = c("Choice 1", "Choice 2", "Choice 3"),
                                   label = "Type of Element"),
                       textInput(inputId = paste("Values_Element_", count, sep = "_"), label = "Values")
                       )
      )
    })
  ### END:   Insert fields, used to define review widgets ####

  ### START: current filename #### ####
  
  output$currentFileName <- renderTable({
    reviewFile <- data.frame(reviewFile(),
                             stringsAsFactors = FALSE)
    fileName <- paste(word(reviewFile[input$CurrentItem, "Authors"],1), # word()-funciton requires stringr-library
                      reviewFile[input$CurrentItem, "Year"],
                      reviewFile[input$CurrentItem, "Title"],
                      sep = "_")
    # fileName <- gsub("\"", "'", fileName)
    # fileName <- gsub("‘‘", "'", fileName)
    # fileName <- gsub(":", " -", fileName)
    # fileName <- gsub("/", "-", fileName)
    # fileName <- gsub("  ", " ", fileName)
    return(fileName)
  })
  
  ### END:   current filename ####
  
  ### START: sample data #### ####
  # STUDY_ID <- c(1:4)
  # Title <- c("Title 1", "Title 2", "Title 3", "Title 4")
  # Abstract <- c("Despite the increasing number of virtual communities of practice (VCoPs), little is known about how organizations can help lead them to success. This paper aims to identify those management practices that are likely to increase their chance of success. Using an action research approach that brought together a multidisciplinary team of researchers, coaches and organizational participants, we closely followed the experiences of eight VCoPs over a six- to nine-month period and collected a large quantity of quantitative and qualitative data from many sources. Our results indicate that three types of management practices seem to have the most impact on a VCoP's success: taking ongoing actions to develop a knowledge-sharing culture, providing adequate resources to the VCoPs, and monitoring the leadership of the community in order to address any occurring problems. This study represents a first step towards building an empirically based understanding of how organizations can sustain their VCoPs",
  #               "A Health science courses aim to prepare students for the demands of their chosen profession by learning ways appropriate to that profession and the contexts they will work and live in. Expectations of what students should learn become re-contextualised and translated into entry-level curriculum, with students operating as a connection between what is intended and enacted in curriculum, and required in the real world. Drawing on phenomenology, this paper explores how students understand practice, the collective, purposeful knowing, doing and being of a community, in entry-level physiotherapy programs. Ways of thinking and practising (WTP). a framework attentive to the distinctive nature of a discipline, its values, philosophies and world-view (McCune and Hounsell in High Educ 49(3):255,289, 2005), provides the conceptual lens. Six themes describing how students see the WTP of physiotherapy practice emerged from the analysis: discovery of new knowledge; problem solving client related contexts; adopting a systems based approach to the body; contributing to a positive therapeutic alliance; developing a sense of self and the profession; and the organisation of the workforce. The study produces knowledge about practice by focusing on physiotherapy students' experiences of disciplinary ... ",
  #               "Abstract 3", 
  #               "Abstract 4")
  # 
  # reviewFile <- data.frame(STUDY_ID, Title, Abstract, stringsAsFactors = FALSE)
  # reviewFile
  ### END:   sample data ####
  
  ### START: Load file and subset #### ##############
  reviewFile <- reactive({
    infile <- input$dataFile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    fullTable <- read.csv(infile$datapath)

    # fullTable <- subset(fullTable, 
    #        select = c("STUDY_ID", "Authors", "Title", "Abstract", "DOI", "Year", "Author.Keywords", "INCLUDE")) # Make this dynamic so that users can choose the columns manually with checkboxes
           # select = input$chooseColumns) # Make this dynamic so that users can choose the columns manually with checkboxes
    # clean datatable from special characters (add special accents on top of letters as well, as in Poznań)
    # fullTable <- gsub("\"", "'", fullTable)
    # fullTable <- gsub("‘‘", "'", fullTable)
    # fullTable <- gsub(":", " -", fullTable)
    # fullTable <- gsub("/", "-", fullTable)
    # fullTable <- gsub("  ", " ", fullTable)
    return(fullTable)
    })
  
  output$loadedFile <- renderDataTable({
    reviewFile()
    })
  
  output$myTable <- DT::renderDataTable({
    subsetCategories <- data.frame(reviewFile(), stringsAsFactors = FALSE)
    # colnames(subsetCategories) = str_wrap(colnames(subsetCategories),width = 400) # didn't change anything either
    subsetCategories <- DT::datatable(subsetCategories[,input$relevantColumns, drop = FALSE]
                                      ,
                                      # filter = "top", 
                                      fillContainer = TRUE,
                                      options = list(autoWidth = TRUE, scrollX = TRUE, scrollY = 400,
                                                     columnDefs = list(list(width = '500px', targets = "Abstract")),
                                                     columns.width = '500px')
                                      )
    subsetCategories
    })
  
  
  observeEvent(input$showColumns,{
    infile <- input$dataFile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    fullTable <- read.csv(infile$datapath)
    insertUI(
      selector = "#chooseColumns",
      where = "afterEnd",
      ui =   checkboxGroupInput(inputId = "relevantColumns", label = "Choose which columns to include",
                                choices = names(fullTable),
                                selected = names(fullTable)
                                )
      )
  })
  
  # reactive({
  #   infile <- input$dataFile
  #   if (is.null(infile)) {
  #     # User has not uploaded a file yet
  #     return(NULL)
  #   }
  #   fullTable <- read.csv(infile$datapath)
  #   return(
  #     insertUI(
  #         selector = "#chooseColumns",
  #         where = "afterEnd",
  #         ui =   checkboxGroupInput(inputId = "relevantColumns", label = "Choose which columns to include",
  #                                   choices = names(fullTable),
  #                                   selected = names(fullTable)
  #                                   )
  #         )
  #     )
  #   })

  ### END:   Load file and subset ####
  
  ### START:  Mark keywords in current Abstract (ISSUE: only second Highlightword gets highlighted) #### ####  
        ###### Start: EXPERIMENT: Alternative 1 ###### ####
        # output$currentAbstract <- 
        #   renderText({
        #     reviewFile <- data.frame(reviewFile(), stringsAsFactors = FALSE)
        #     if (is.null(input$HighlightWord1)&is.null(input$HighlightWord2)){
        #       highlightedAbstract1 <- gsub(input$HighlightWord1,
        #                                    paste("<b><span style =\"color:",
        #                                          input$HighlightColor1,
        #                                          "\">",
        #                                          input$HighlightWord1,
        #                                          "</span></b>",
        #                                          sep = ""),
        #                                    reviewFile[input$CurrentItem,"Abstract"]
        #                                    )
        #       
        #       highlightedAbstract2 <- gsub(input$HighlightWord2,
        #                                    paste("<b><span style =\"color:",
        #                                          input$HighlightColor2,
        #                                          "\">",
        #                                          input$HighlightWord2,
        #                                          "</span></b>",
        #                                          sep = ""),
        #                                    highlightedAbstract1
        #                                    )
        #       return(highlightedAbstract2)
        #     }
        #     else reviewFile[input$CurrentItem, "Abstract"]
        #     })
        ###### END:   EXPERIMENT: Alternative 1 ######
        
        ###### START: EXPERIMENT: Alternative 2 #### #####
        # highlightWords <- reactive({
        #   list(pattern = c(input$HighlightWord1, input$HighlightWord2),
        #        replacement = c(paste("<b><span style =\"color:", input$HighlightColor1, "\">", input$HighlightWord1, "</span></b>", sep = ""),
        #                        paste(input$HighlightWord2, paste("<b><span style =\"color:", input$HighlightColor2, "\">", input$HighlightWord2, "</span></b>", sep = "")
        #                              )
        #                        ),
        #        x = c("x", "x")
        #        )
        # })
        ###### END:   EXPERIMENT: Alternative 2 ####
        ######
          
          
          output$dfHighlightedWords <- renderDataTable({
            listHighlightwords <- list(input$HighlightWord1, input$HighlightWord2)
            listHighlightcolors <- list(input$HighlightColor1, input$HighlightColor2)
            
            # dfHighlightedWords <- data.frame(matrix(, nrow = length(listHighlightWords), ncol = 0))
            # dfHighlightedWords$Words <- listHighlightwords
            # dfHighlightedWords$Colors <- listHighlightcolors
            dfHighlightedWords <- as.data.frame(as.matrix(listHighlightwords, listHighlightcolors, ncol = 2))
            # dfHighlightedWords <- as.data.frame(cbind(Words = listHighlightwords,
            #                                           Colors = listHighlightcolors)
            #                                     )
            return(dfHighlightedWords)
          })
                      
    # observeEvent(input$addHighlight,{
      output$currentAbstract <-
        renderText({
          # listHighlightwords <- list(input$HighlightWord1, input$HighlightWord2)
          # listHighlightcolors <- list(input$HighlightColor1, input$HighlightColor2)
          reviewFile <- data.frame(lapply(reviewFile(), function(x, y,...) {
                                          # reviewFile <-
                                          # lapply(listHighlightwords, function(x) {paste0("<b><span style =\"color:", listHighlightcolors, "\">", listHighlightwords, "</span></b>")})
                                         
                                            if (input$HighlightWord1 != ""){
                                            x = gsub(input$HighlightWord1, paste("<b><span style =\"color:", input$HighlightColor1, "\">", input$HighlightWord1, "</span></b>", sep = ""), x, ignore.case = TRUE) # seems to get overwritten by the second gsub function
                                            } 
                                            if (input$HighlightWord2 != ""){
                                            y = gsub(input$HighlightWord2, paste("<b><span style =\"color:", input$HighlightColor2, "\">", input$HighlightWord2, "</span></b>", sep = ""), x, ignore.case = TRUE)
                                            }
                                            else x
                                          ## Following FUN gives error: argument 'replacement' has length > 1 and only the first element will be used
                                          # gsub(listHighlightwords, paste("<b><span style =\"color:", listHighlightcolors, "\">", listHighlightwords, "</span></b>", sep = ""), x, ignore.case = TRUE)
                                          
                                          ## part of EXPERIEMENT: Alternative 2 above
                                          # do.call(gsub, highlightWords()) 
                                          # gsub(input$HighlightWord1,
                                          #      paste("<b><span style =\"color:", input$HighlightColor1, "\">", input$HighlightWord1, "</span></b>", sep = ""),
                                          #      x, ignore.case = TRUE)
                                      }),
                                   stringsAsFactors = FALSE)

          paste(reviewFile[input$CurrentItem, "Abstract"])
          })
      # })
  
        
  ### END:    Mark keywords in current Abstract (ISSUE: only second Highlightword gets highlighted) ####
  
  ### START:  Mark keywords in current Title (Working Original) #### ####################################
  
  output$reviewFileCurrentTitle <- renderText({
    reviewFile <- data.frame(lapply(reviewFile(), function(x) {reviewFile <-
                                          gsub(input$HighlightWord1, paste("<b><span style =\"color:", input$HighlightColor1, "\">", input$HighlightWord1, "</span></b>", sep = ""), x, ignore.case = TRUE) # seems to get overwritten by the second gsub function
                                          gsub(input$HighlightWord2, paste("<b><span style =\"color:", input$HighlightColor2, "\">", input$HighlightWord2, "</span></b>", sep = ""), x, ignore.case =TRUE)
                                      }),
                              stringsAsFactors = FALSE)
                                  
    paste(reviewFile[input$CurrentItem, "Title"])
  })
  ### END:    Mark keywords in current Title (Working Original) ####
 
  ### START:  Mark keywords in current Keywords #### ####
  
  output$currentKeywords <- renderText({
    reviewFile <- data.frame(lapply(reviewFile(), function(x) {reviewFile <-
                                          gsub(input$HighlightWord1, paste("<b><span style =\"color:", input$HighlightColor1, "\">", input$HighlightWord1, "</span></b>", sep = ""), x, ignore.case = TRUE) # seems to get overwritten by the second gsub function
                                          gsub(input$HighlightWord2, paste("<b><span style =\"color:", input$HighlightColor2, "\">", input$HighlightWord2, "</span></b>", sep = ""), x, ignore.case =TRUE)
                                      }),
                              stringsAsFactors = FALSE)
                                        
    paste(reviewFile[input$CurrentItem, "Author.Keywords"])
  })
  ### END:    Mark keywords in current Keywords ####
  
  ### START: Show locally stored files, based on list from imported CSV #### ######################

  currentPDF <- reactive({
    
    reviewFile <- data.frame(reviewFile(),
                             stringsAsFactors = FALSE)
    fileName <- paste(word(reviewFile[input$CurrentItem, "Authors"],1), # word()-funciton requires stringr-library
                      reviewFile[input$CurrentItem, "Year"],
                      reviewFile[input$CurrentItem, "Title"],
                      sep = "_")
    fileName <- gsub("\"", "'", fileName)
    fileName <- gsub("‘‘", "'", fileName)
    fileName <- gsub(":", " -", fileName)
    fileName <- gsub("/", "-", fileName)
    fileName <- gsub("  ", " ", fileName)
    
    paste("localfiles/", fileName, ".pdf", sep = "")
    })
  
  output$pdfviewer <- renderUI({
    tags$iframe(style = "height: 600px; width: 100%; scrolling = yes",
                src = currentPDF()
    )
  })
  ### END:   Show locally stored files, based on list from imported CSV ####
  
  ### START: SHOW PDF from Sci-Hub based on DOI (WORKING!!!), can maybe be adapted to University accounts?? ### #########################
  
  currentDOI <- reactive({
    DOI <- data.frame(reviewFile(), stringsAsFactors = FALSE)
    paste("http://sci-hub.tw/", DOI[input$CurrentItem, "DOI"], sep = "")
  })

  output$pdfviewer2 <- renderUI({
    tags$iframe(style = "height: 600px; width: 100%; scrolling = yes",
                src = currentDOI()
    )
  })
  
  
      ###### START: EXPERIMENT: same with Google Scholar (not yet working) #### #####################
     # observe({
     #    DOI <- data.frame(reviewFile(), stringsAsFactors = FALSE)
     #    currentDOI <- DOI[input$CurrentItem, "DOI"]
     #    googleScholarLink <<- paste("https://scholar.google.de/scholar?hl=de&as_sdt=0%2C5&q=",
     #                                currentDOI, 
     #                                "&btnG=",
     #                                sep = "")
     #  }) 
     #  
     #  # https://scholar.google.de/scholar?hl=de&as_sdt=0%2C5&q=10.1016%2Fj.jeap.2012.10.003&btnG=
     #  # 10.1016/j.jeap.2012.10.003
     #  # https://scholar.google.de/scholar?hl=de&as_sdt=0%2C5&q=10.1016%2Fj.ijer.2012.09.005&btnG=
     #  # 10.1016/j.ijer.2012.09.005
     # 
     #  output$pdfviewer2 <- renderUI({
     #    input$CurrentItem
     #    myGoogle <- tags$iframe(style = "height: 600px; width: 100%; scrolling = yes",
     #                src = googleScholarLink
     #                )
     #  })
      ###### END:   EXPERIMENT: same with Google Scholar (not yet working) ####
  
      ###### START: Show, if included (working!) #### ####
      output$currentItemIncluded <- renderTable({
          reviewFile <- data.frame(reviewFile(), stringsAsFactors = FALSE)
          paste(reviewFile[input$CurrentItem,"INCLUDE"])
          })
      ###### End:   Show, if included (working!) ####
  ### END:   SHOW PDF from Sci-Hub based on DOI (WORKING!!!), can maybe be adapted to University accounts?? ###
}

########################################       Open App       ##################################################################
shinyApp(ui = ui, server = server)

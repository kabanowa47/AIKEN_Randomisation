library(shiny)
library(shinythemes)

# Define UI for app
ui <- fluidPage(
  theme = shinytheme("superhero"),
  # App title
  titlePanel("AIKEN Randomisation"),
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      # Input: Select a file
      fileInput(
        "file1",
        "Choose AIKEN quiz file",
        multiple = FALSE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".txt")
      ),
      # Horizontal line
      tags$hr(),
      
      # Input: Select before or sorted
      radioButtons(
        "disp",
        "Display",
        choices = c(Before = "before",
                    Sorted = "sorted"),
        selected = "before"
      ),
      # Button
      downloadButton("downloadData", "Download")
      
    ),
    # Main panel for displaying outputs
    mainPanel(tabsetPanel(id = "tabset",
                          tabPanel(
                            "quiz", tableOutput("sorted")
                          )))
  ),
  #Print info
  helpText(
    "Note: this app works only wiht AIKEN formated quiz that contains 'A' to 'E' possible answers.",
    "All the questions should be separated by an empty row.",
    "Upload only plain text file (your_file.txt)"
  ),
  helpText(
    "visit my github: https://github.com/kabanowa47/AIKEN_Randomisation.git"
  )
)

# Define server part
server <- function(input, output) {
  sorted <-  shiny::reactive({
    req(input$file1)
    tryCatch({
      df <-
        readLines(input$file1$datapath)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    
    if (input$disp == "before") {
      return(df)
    }
    else {
      file <- (df)
      i <- 1
      j <- NULL
      y <- NULL
      odp <- NULL
      sub <- rep(NA, length(file) / 8)
      for (i in 1:(length(file) / 8)) {
        j <- i * 8
        y <- j - 7
        s <- file[y:j]
        sub[i] <- list(
          list(
            text = s[1],
            A = stringr::str_replace(s[2], "A. ", ""),
            B = stringr::str_replace(s[3], "B. ", ""),
            C = stringr::str_replace(s[4], "C. ", ""),
            D = stringr::str_replace(s[5], "D. ", ""),
            E = stringr::str_replace(s[6], "E. ", ""),
            ODP = stringr::str_replace(s[grep(paste(stringr::str_replace(s[7], "ANSWER: ", ""), '. ', sep =
                                                      ''),  s[1:6])], paste(
                                                        stringr::str_replace(s[7], "ANSWER: ", ""), '. ', sep = ''
                                                      ), "")
            
          )
        )
      }
      sorted <- sample(sub[1:(length(sub))])
      for (i in 1:(length(sorted))) {
        sorted[[i]][2:6] <- sample(sorted[[i]][2:6])
        sorted[[i]][7] <-
          names(sorted[[i]][match(sorted[[i]][7], sorted[[i]][2:6]) + 1])
        sorted[[i]][8] <- c('')
      }
      
      sorted_df <- as.data.frame(do.call(cbind, sorted))
      sorted_df <- data.frame(x = unlist(sorted_df))
      sorted_df$val <-
        rep(c('', 'A:', 'B:', 'C:', 'D:', "E:", "ANSWER:", ''),
            length(rownames(sorted_df)) / 8)
      sorted_df <- sorted_df[, c(2, 1)]
      return(sorted_df)
    }
  })
  
  output$sorted <- renderTable(data.frame(sorted()), colnames = F)
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('new_file', ".txt", sep = "")
    },
    content = function(file) {
      write.table(
        sorted(),
        file,
        row.names = FALSE,
        quote = F,
        col.names = F
      )
    }
  )
}

shinyApp(ui = ui, server = server)

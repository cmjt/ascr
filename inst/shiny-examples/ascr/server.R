#


library(shiny)


shinyServer(function(input, output) {

  output$traps <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    traps <- read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
    if(input$disp == "head") {
        return(head(traps))
    }
    else {
        return(traps)
    }

  },
  striped = TRUE)
    output$detections <- renderTable({

    # input$file2 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file2)

    detections <- read.csv(input$file2$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)

    if(input$disp == "head") {
        return(head(detections))
    }
    else {
        return(detections)
    }

    },
    striped = TRUE)

})

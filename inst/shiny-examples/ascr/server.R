#


library(shiny)


shinyServer(function(input, output) {

  output$traps <- renderTable({

    # input$file1 (traps) will be NULL initially. After the user selects
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
    # code to plot trap locations
    output$trapsPlot <- renderPlot({
        req(input$file1)
        
        traps <- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = input$sep,
                          quote = input$quote)
        plot(traps$x,traps$y)
    })
        
    output$detections <- renderTable({
        
    # input$file2 (detections) will be NULL initially. After the user selects
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
    # plot of mask 
    output$maskPlot <- renderPlot({
        req(input$file1)
        
        traps <- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = input$sep,
                          quote = input$quote)
        traps <- as.matrix(cbind(traps$x,traps$y))
        mask <- create.mask(traps,input$buffer,input$spacing)
        plot(mask)
    })
                                        # model fit and coeficients
    output$coefs <- renderTable({
        capt.hist <-list(bincapt = get.capt.hist(detections))
        traps <- as.matrix(cbind(traps$x,traps$y))
        fit <- fit.ascr(capt = capt.hist,traps = traps,mask = mask,detfn =  input$select,
                        fix = list(g0 = 1))
        return(fit)
    })
    
    
})


library(shiny)
library(ascr)


shinyServer(function(input, output,session) {
    ## read in input data
    traps <- reactive({
        if(input$example == TRUE){
            file1 <- system.file("inst/shiny-examples/ascr/data/exampletraps.csv", package = "ascr")
            traps <- read.csv(file1)

        }else{
            req(input$file1)
            
            traps <- read.csv(input$file1$datapath,
                              header = input$header,
                              sep = input$sep,
                              quote = input$quote)
        }
    })
    detections <- reactive({
        if(input$example == TRUE){
            file2 <- system.file("inst/shiny-examples/ascr/data/exampledetect.csv", package = "ascr")
            detections <- read.csv(file2)

        }else{
         req(input$file2)

         detections <- read.csv(input$file2$datapath,
                                header = input$header,
                                sep = input$sep,
                                quote = input$quote)
         }
    })
    # output trap locations
  output$traps <- renderTable({
      traps <- traps()
   
    if(input$disp == "head") {
        return(head(traps))
    }else{
        return(traps)
    }

  },
  striped = TRUE)
    # code to plot trap locations
    output$trapsPlot <- renderPlot({
        traps <- traps()
        if(!is.null(traps$post)){
            plot(traps$x,traps$y,asp = 1,type = "n",xlab = "Longitude",ylab = "Latitude")
            text(traps$x,traps$y,traps$post,lwd = 2)
        }else{
            plot(traps$x,traps$y,asp = 1,pch = 4,cex = 2,lwd = 3,xlab = "Longitude",ylab = "Latitude")
            }
    })
        
    output$detections <- renderTable({
        detections <- detections()

        if(input$disp == "head") {
            return(head(detections))
    }else{
        return(detections)
    }

    },
    striped = TRUE)

    output$capt.hist <- renderTable({
        detections <- detections()
        capt.hist <- get.capt.hist(detections)
        colnames(capt.hist[[1]]) <- names(table(detections$post))
        rownames(capt.hist[[1]]) <- unique(paste("occasion",detections$occasion, "group", detections$group))
        if(input$disp == "head") {
            return(head(capt.hist[[1]]))
        }else{
            return(capt.hist[[1]])
        }
    },striped = TRUE,rownames = TRUE,colnames = TRUE,digits = 0)
    # chage buffer slider based on trap range
    observe({
        infile <- traps()# user input file upload
        if(!is.null(infile)) {
            traps <- traps()
            maxdistance <- 4*diff(range(traps$x,traps$y)) 
            updateSliderInput(session, "buffer", max = maxdistance,value = maxdistance/2) 
        }
    })
   
    # chage spacing slider based on trap range
    observe({
        infile <- detections() # user input file upload
        if(!is.null(infile)) {
            traps <- traps()
            maxdistance <- diff(range(traps$x,traps$y))/4
            updateSliderInput(session, "spacing", max = maxdistance, value = maxdistance/2) 
        }
        })
    # plot of mask 
    output$maskPlot <- renderPlot({
        traps <- traps()
        traps <- as.matrix(cbind(traps$x,traps$y))
        validate(need(input$buffer > input$spacing,"The mask buffer cannot be less than the spacing"))
        mask <- create.mask(traps,input$buffer,input$spacing)
        plot.mask(mask,traps)
        
    },width = 500, height = 500)
    # chose which parameters of which detection function to fit, conditional numeric input for fixing param values
    output$fixedParamSelection <- renderUI({
        params.fix <- cbind(c("g0","sigma","g0","sigma","z","shape","scale"),
                            c("hn","hn","hr","hr","hr","th","th"))
        checkboxGroupInput("parameter", "Fix which parameters:",
                           choices = as.character(params.fix[params.fix[,2]==input$select,1]),inline = TRUE)
       
    })
    output$fixedg0 <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('g0')",       
                         numericInput("g0","fix g0 to:",value=1,min=1,max=100,step=1)
                         )
    })
    output$fixedsigma <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('sigma')",       
                         numericInput("sigma","fix sigma to:",value=1,min=1,max=100,step=1)
                         )
    })
    output$fixedz <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('z')",       
                         numericInput("z","fix z to:",value=1,min=1,max=100,step=1)
                         )
    })
    output$fixedshape <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('shape')",       
                         numericInput("shape","fix shape to:",value=1,min=1,max=100,step=1)
                         )
    })
    
    # Fit model based on inputs of user and output parameter estimates and plots
    fit <- eventReactive(input$fit,{
        withProgress(message = 'Fitting model', value = 0,{
        
        detections <- detections()
        traps <- traps()
        traps <- as.matrix(cbind(traps$x,traps$y))
        mask <- create.mask(traps,input$buffer,input$spacing)
        nms <- names(detections)
        
        capt.hist <- get.capt.hist(detections)
                
        param.fix <- input$parameter
        param.fix.value <- list(g0 = input$g0,sigma = input$sigma,z = input$z,shape = input$shape,
                                scale = input$scale, shape.1 = input$shape.1,shape.2 = input$shape.2)
        idx <- match(param.fix,names(param.fix.value))
        fix <- param.fix.value[idx]
        fit <- NULL
        fit <- tryCatch({fit.ascr(capt = capt.hist,traps = traps,mask = mask,detfn =  input$select,
                                  fix = fix)},
                        warning = function(w) print("fit.ascr convergence issues"))
        })
    })
    # coefficients
    output$coefs <- renderTable({
        fit <- fit()
            if(class(fit)[1]=="ascr"){
                res <- data.frame(Estimate = summary(fit)$coefs,Std.Error = summary(fit)$coefs.se)
                rownames(res) <- names(coef(fit))
                return(res)
            }
        },rownames = TRUE)
    # Detection function plots and location estimate plots
    output$detectionPlot <- renderPlot({
        fit <- fit()
        if(class(fit)[1]=="ascr"){
            validate(need(input$call.num,"please provide a call number"))
            if(input$call.num > nrow(fit$args$capt$bincapt)){
                layout(matrix(c(1,1,1,2,2,2,1,1,1,2,2,2,3,3,3,3,3,3),byrow = TRUE,ncol = 6))
                show.detsurf(fit)
                plot(1,1,col="white",axes = FALSE,xlab = "",ylab = "")
                text(1,1,paste("There are only",nrow(fit$args$capt$bincapt),"calls",collapse = " "),col = "red",cex = 2)
                show.detfn(fit)
            }else{
                layout(matrix(c(1,1,1,2,2,2,1,1,1,2,2,2,3,3,3,3,3,3),byrow = TRUE,ncol = 6))
                show.detsurf(fit)
                locations(fit,input$call.num)
                legend("top",legend = paste("call",input$call.num,sep = " "),bty = "n")
                show.detfn(fit)
            }
        }else{
            plot(1,1,col="white",axes = FALSE,xlab = "",ylab = "")
            text(1,1,paste("convergence issues"),col = "red",cex = 2)
        }
    },width = 700,height = 700)
    ## code to produce downloadable objects (i.e., plots and report)
    output$downloadMask <- downloadHandler(
      filename = "ascrMask.png",
      content = function(file) {
          png(file)
          
          traps <- traps()
          traps <- as.matrix(cbind(traps$x,traps$y))
          mask <- create.mask(traps,input$buffer,input$spacing)
          plot.mask(mask,traps)
          dev.off()
      })
    output$downloadModelPlot <- downloadHandler(
        filename = "ascrModelPlots.png",
        content = function(file) {
            png(file)
            fit <- fit()
            if(class(fit)[1]=="ascr"){
                validate(need(input$call.num,"please provide a call number"))
                if(input$call.num > nrow(fit$args$capt$bincapt)){
                    layout(matrix(c(1,1,1,2,2,2,1,1,1,2,2,2,3,3,3,3,3,3),byrow = TRUE,ncol = 6))
                    show.detsurf(fit)
                    plot(1,1,col="white",axes = FALSE,xlab = "",ylab = "")
                    text(1,1,paste("There are only",nrow(fit$args$capt$bincapt),"calls",collapse = " "),col = "red",cex = 2)
                    show.detfn(fit)
                }else{
                    layout(matrix(c(1,1,1,2,2,2,1,1,1,2,2,2,3,3,3,3,3,3),byrow = TRUE,ncol = 6))
                    show.detsurf(fit)
                    locations(fit,input$call.num)
                    legend("top",legend = paste("call",input$call.num,sep = " "),bty = "n")
                    show.detfn(fit)
                }
            }else{
                plot(1,1,col="white",axes = FALSE,xlab = "",ylab = "")
                text(1,1,paste("convergence issues"),col = "red",cex = 2)
            }
            dev.off()
        })
    
    output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.html",
        content = function(file) {
            withProgress(message = 'Generating report', min = 0,max = 100,{
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(buffer = input$buffer,
                           spacing = input$spacing,
                           fit = fit())
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
                              )
            })
            
        })
    session$onSessionEnded(stopApp)
})
    

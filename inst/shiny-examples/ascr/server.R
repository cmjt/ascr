
library(shiny)
library(ascr)

withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "message")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

shinyServer(function(input, output,session) {
    
    
    ## read in input data
     traps <- reactive({
        if(input$example == TRUE){
            file <- system.file("inst/shiny-examples/ascr/data/exampletraps.csv", package = "ascr")
            traps <- read.csv(file)
            
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
            file <- system.file("inst/shiny-examples/ascr/data/exampledetect.csv", package = "ascr")
            detections <- read.csv(file)

        }else{
         req(input$file2)

         detections <- read.csv(input$file2$datapath,
                                header = input$header,
                                sep = input$sep,
                                quote = input$quote)
         }
    })
    observe({
        if(input$example == TRUE){
            disable("file1")
            disable("file2")
            disable("header")
            disable("sep")
            disable("quote")
            hide("file1")
            hide("file2")
            hide("header")
            hide("sep")
            hide("quote")
        }else{
            enable("file1")
            enable("file2")
            enable("header")
            enable("sep")
            enable("quote")
            show("file1")
            show("file2")
            show("header")
            show("sep")
            show("quote")
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
    # chage spacing slider based on trap range
    observe({
        infile <- traps() # user input file upload
        if(!is.null(infile)) {
            traps <- traps()
            maxdistance <- diff(range(traps$x,traps$y))/4
            updateSliderInput(session, "spacing", max = maxdistance, value = maxdistance/2)
            maxdistance <- 4*diff(range(traps$x,traps$y))
            updateSliderInput(session, "buffer", max = maxdistance,value = maxdistance/2) 
        }
    })
    # change buffer sliding in advanced increase buffer option chosen
    observe({
        if("increase mask buffer" %in% input$advancedOptions) {
        maxdistance <- input$incmaskbuffer
        updateSliderInput(session, "buffer", max = maxdistance,value = maxdistance/2)
        }
    })
    # plot of mask 
    output$maskPlot <- renderPlot({
        
        traps <- traps()
        
        traps <- as.matrix(cbind(traps$x,traps$y))
        validate(need(input$buffer > input$spacing,"The mask buffer cannot be less than the spacing"))
        validate(need(input$buffer/input$spacing < 80, "Infeasibly fine mask"))
        mask <- create.mask(traps,input$buffer,input$spacing)
        plot.mask(mask,traps)
        
    },width = 500, height = 500)
    # chose which parameters of which detection function to fit, conditional numeric input for fixing param values
    output$fixedParamSelection <- renderUI({
        params.fix <- cbind(c("g0","sigma","g0","sigma","z","shape","scale"),
                            c("hn","hn","hr","hr","hr","th","th"))
        checkboxGroupInput("parameter", "Fix which parameters:",
                           choices = as.character(params.fix[params.fix[,2] == input$select,1]),inline = TRUE)
       
    })
    output$fixedg0 <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('g0')",       
                         numericInput("g0","fix g0 to:",value = 1,min = 1,max = 100,step = 1)
                         )
    })
    output$fixedsigma <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('sigma')",       
                         numericInput("sigma","fix sigma to:",value = 1,min = 1,max = 100,step = 1)
                         )
    })
    output$fixedz <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('z')",       
                         numericInput("z","fix z to:",value = 1,min = 1,max = 100,step = 1)
                         )
    })
    output$fixedshape <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('shape')",       
                         numericInput("shape","fix shape to:",value = 1,min = 1,max = 100,step = 1)
                         )
    })

    output$startParamSelection <- renderUI({
        params.fix <- cbind(c("g0","sigma","g0","sigma","z","shape","scale"),
                            c("hn","hn","hr","hr","hr","th","th"))
        checkboxGroupInput("parset", "Set starting values for which parameters:",
                           choices = as.character(params.fix[params.fix[,2] == input$select,1]),inline = TRUE)
       
    })
    output$svg0 <- renderUI({
        conditionalPanel(condition = "input.parset.includes('g0') && !input.parameter.includes('g0')",
                         numericInput("svg0","g0 start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) # set starting value of g0 ensure it isn't already fixed
     output$svsigma <- renderUI({
        conditionalPanel(condition = "input.parset.includes('sigma') && !input.parameter.includes('sigma')",
                         numericInput("svsigma","sigma start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) # set starting value of sigma ensure it isn't already fixed
    output$svz <- renderUI({
        conditionalPanel(condition = "input.parset.includes('z') && !input.parameter.includes('z')",
                         numericInput("svz","z start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) # set starting value of z ensure it isn't already fixed
    output$svshape <- renderUI({
        conditionalPanel(condition = "input.parset.includes('shape') && !input.parameter.includes('shape')",
                         numericInput("svshape","shape start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) # set starting value of shape ensure it isn't already fixed
    output$svscale <- renderUI({
        conditionalPanel(condition = "input.parset.includes('scale') && !input.parameter.includes('scale')",
                         numericInput("svscale","scale start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) # set starting value of scale ensure it isn't already fixed

    output$svshape.1 <- renderUI({
        conditionalPanel(condition = "input.parset.includes('shape.1') && !input.parameter.includes('shape.1')",
                         numericInput("svshape.1","shape.1 start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) # set starting value of shape.1 ensure it isn't already fixed
    output$svshape.2 <- renderUI({
        conditionalPanel(condition = "input.parset.includes('shape.2') && !input.parameter.includes('shape.2')",
                         numericInput("svshape.2","shape.2 start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) # set starting value of shape.2 ensure it isn't already fixed
    
    
    # Fit model based on inputs of user and output parameter estimates and plots
    
    
    fit <- eventReactive(input$fit,{
        detections <- detections()
        traps <- traps()
        if("bearing" %in% names(detections)){
            validate(need(detections$bearing >= 0 & detections$bearing <= 2*pi |
                          "bd" %in% input$advancedOptions,
                          "Bearings should be in radians. To change see advanced options."))
        }
        if("bd" %in% input$advancedOptions){
            detections$bearing <- (2*pi/360)*detections$bearing
        }
        
        traps <- as.matrix(cbind(traps$x,traps$y))
        mask <- create.mask(traps,input$buffer,input$spacing)
        nms <- names(detections)
        
        capt.hist <- get.capt.hist(detections)
        ## fixed values
        param.fix <- input$parameter
        param.fix.value <- list(g0 = input$g0,sigma = input$sigma,z = input$z,shape = input$shape,
                                scale = input$scale, shape.1 = input$shape.1,shape.2 = input$shape.2)
        idx <- match(param.fix,names(param.fix.value))
        fix <- param.fix.value[idx]
        ## starting values
        param.sv <- input$parset
        param.sv.value <- list(g0 = input$svg0,sigma = input$svsigma,z = input$svz,svshape = input$svshape,
                               scale = input$svscale, shape.1 = input$svshape.1,shape.2 = input$svshape.2)
        idsv <- match(param.sv,names(param.sv.value))
        sv <- param.sv.value[idsv]
        fit <- NULL
        disable("fit")
        disable("side-panel")
        show("processing") # stuff to disable fitting button
        withConsoleRedirect("console", {
            fit <-  tryCatch({
            fit.ascr(capt = capt.hist,traps = traps,mask = mask,detfn =  input$select,
                     fix = fix, sv = sv,trace = TRUE) 
            },warning = function(w) print("fit.ascr convergence issues"))
            })
        enable("fit")
        enable("side-panel")
        hide("processing")
        return(fit)
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
    # AIC and log Likelihood
    output$AIClL <- renderTable({
        fit <- fit()
        if(class(fit)[1]=="ascr"){
            tab <- rbind(AIC = AIC(fit),logLik = fit$loglik)
            colnames(tab) <- "value"
            return(tab)
        }
    },rownames = TRUE)
    # Detection function plots and location estimate plots
    
    output$detectionsurf <- renderPlot({
        
        fit <- fit()
        
        if(class(fit)[1] == "ascr"){
            par(mfrow = c(1,2))
            show.detsurf(fit)
            show.detsurf(fit,surface = FALSE)    
        }else{
            plot(1,1,col="white",axes = FALSE,xlab = "",ylab = "")
            text(1,1,paste("convergence issues try advanced options"),col = "grey")
        }
    })
    output$detfn <- renderPlot({
        
        fit <- fit()
        
            if(class(fit)[1]=="ascr"){
                show.detfn(fit)
            }else{
                plot(1,1,col="white",axes = FALSE,xlab = "",ylab = "")
                text(1,1,paste("convergence issues try advanced options"),col = "grey")
            }
    })
    output$locs <- renderPlot({
       
        fit <- fit()
        
        if(class(fit)[1]=="ascr"){
            validate(need(input$call.num,"Please provide a call number"))
            if(input$call.num > nrow(fit$args$capt$bincapt)){
                plot(1,1,col="white",axes = FALSE,xlab = "",ylab = "")
                text(1,1,paste("There are only",nrow(fit$args$capt$bincapt),"calls",collapse = " "),col = "grey")
            }else{
                if("build finer mask for plotting" %in% input$advancedOptions){
                    traps <- traps()
                    validate(need(input$plotmaskspacing,
                                  "Please provide a spacing for the (plotting) mask or uncheck this option"))
                    validate(need(input$plotmaskspacing > 0,
                                  "Cannnot have a spacing of zero meters"))
                    validate(need(input$buffer > input$plotmaskspacing,
                                  "The mask buffer cannot be less than the (plotting) mask spacing"))
                    validate(need(input$plotmaskspacing < input$spacing,
                                  "To obtain a smooth plot the (plotting) mask spacing should be finer than the model fit mask "))
                    msk <- create.mask(traps,input$buffer,input$plotmaskspacing)
                    locations(fit,input$call.num,mask = msk)
                    legend("top",legend = paste("call",input$call.num,sep = " "),bty = "n")
                }else{
                    locations(fit,input$call.num)
                    legend("top",legend = paste("call",input$call.num,sep = " "),bty = "n")
                }
            }
        }else{
            plot(1,1,col="white",axes = FALSE,xlab = "",ylab = "")
            text(1,1,paste("convergence issues try advanced options"),col = "grey")
        }
    },width = 700,height = 700)
    output$bearing_pdf <- renderPlot({
        fit <- fit()
        validate(need(!is.null(fit$args$capt$bearing),"No bearing data provided"))
        validate(need(is.null(fit$args$capt$bearing),"TODO"))

    })
    output$distance_pdf <- renderPlot({
        fit <- fit()
        validate(need(!is.null(fit$args$capt$distance),"No distance data provided"))
        validate(need(is.null(fit$args$capt$distance),"TODO"))

    })
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
    output$downloadSurfPlot <- downloadHandler(
        filename = "ascr_detection_surface_plot.png",
        content = function(file) {
            
            png(file)
            fit <- fit()
            if(class(fit)[1]=="ascr"){
                show.detsurf(fit)
            }else{
                NULL
            }
            dev.off()
        })
    output$downloadContPlot <- downloadHandler(
        filename = "ascr_detection_contour_plot.png",
        content = function(file) {
            png(file)
            fit <- fit()
            if(class(fit)[1]=="ascr"){
                show.detsurf(fit,surface = FALSE)
            }else{
                NULL
            }
            dev.off()
        })
    output$downloadDetPlot <- downloadHandler(
        filename = "ascr_detection_function_plot.png",
        content = function(file) {
            
            png(file)
            fit <- fit()
            if(class(fit)[1]=="ascr"){
                show.detfn(fit)
            }else{
                NULL
            }
            dev.off()
        })
    output$downloadModel <- downloadHandler(
        filename = paste("ascr_",date(),".RData",sep = ""),
        content = function(file){
            
            fit <- fit()
            save(fit,file = file)
        }
    )
    output$report <- downloadHandler(
          
        filename = "report.html",
        content = function(file) {
            disable("report")
            show("proc_report")
            
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
                                        # can happen when deployed).
                             
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
                             
                                        # Set up parameters to pass to Rmd document
            params <- list(buffer = input$buffer,
                           spacing = input$spacing,
                           fit = fit(),
                           anispeed = input$anispeed)
            # Knit the document, passing in the `params` list, and eval it in a
            # child of the global environment (this isolates the code in the document
            # from the code in this app).
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
                              )
                         
            enable("report")
            hide("proc_report")
        })
    observeEvent(input$reset_input, {
        updateSliderInput(session, "spacing", max = 1000, value = 250)
        updateSliderInput(session, "buffer", max =10000, value = 1000)
        updateCheckboxInput(session, "example",  value = FALSE)
        reset("side-panel")
    })
    observe({
        if (input$close > 0) {
            stopApp()
            }
    })
    session$onSessionEnded(stopApp)
})
    

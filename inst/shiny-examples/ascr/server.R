
library(shiny)
library(ascr)


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
        if(!is.null(traps$post)){
            plot(traps$x,traps$y,asp = 1,type = "n",xlab = "Longitude",ylab = "Latitude")
            text(traps$x,traps$y,traps$post,lwd = 2)
        }else{
            plot(traps$x,traps$y,asp = 1,pch = 4,cex = 2,lwd = 3,xlab = "Longitude",ylab = "Latitude")
            }
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
        plot.mask(mask,traps)
        
    },width = 500, height = 500)
    # chose which parameters of which detection function to fit, conditional numeric input for fixing param values
    output$fixedParamSelection <- renderUI({
        params.fix <- cbind(c("g0","sigma","g0","sigma","z","shape","scale","shape.1","shape.2","scale"),
                            c("hn","hn","hr","hr","hr","th","th","lth","lth","lth"))
        checkboxGroupInput("parameter", "Fix which parameters:",
                           choices = as.character(params.fix[params.fix[,2]==input$select,1]))
       
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
    output$fixedscale <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('scale')",       
                         numericInput("scale","fix scale to:",value=1,min=1,max=100,step=1)
                         )
    })
    output$fixedshape.1 <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('shape.1')",       
                         numericInput("shape.1","fix shape.1 to:",value=1,min=1,max=100,step=1)
                         )
    })
    output$fixedshape.2 <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('shape.2')",       
                         numericInput("shape.2","fix shape.2 to:",value=1,min=1,max=100,step=1)
                         )
    })
    # Fit model based on inputs of user and output parameter estimates and plots
    output$coefs <- renderTable({
        req(input$file1)
        req(input$file2)
        detections <- read.csv(input$file2$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
        traps <- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = input$sep,
                          quote = input$quote)
        traps <- as.matrix(cbind(traps$x,traps$y))
        mask <- create.mask(traps,input$buffer,input$spacing)
        capt.hist <-list(bincapt = get.capt.hist(detections))
        param.fix <- input$parameter
        param.fix.value <- list(g0 = input$g0,sigma = input$sigma,z = input$z,shape = input$shape,
                             scale = input$scale, shape.1 = input$shape.1,shape.2 = input$shape.2)
        idx <- match(param.fix,names(param.fix.value))
        fix <- param.fix.value[idx]
        fit <- NULL
        fit <- tryCatch({fit.ascr(capt = capt.hist,traps = traps,mask = mask,detfn =  input$select,
                                  fix = fix)},
                        warning = function(w) print("fit.ascr convergence issues"))
        if(class(fit)[1]=="ascr"){
            res <- data.frame(Estimate = summary(fit)$coefs,Std.Error = summary(fit)$coefs.se)
            rownames(res) <- names(coef(fit))
            return(res)
        }
        
    },rownames = TRUE)
                                        # Detection function plots and location estimate plots
    output$detectionPlot <- renderPlot({
        req(input$file1)
        req(input$file2)
        detections <- read.csv(input$file2$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote)
        traps <- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = input$sep,
                          quote = input$quote)
        traps <- as.matrix(cbind(traps$x,traps$y))
        mask <- create.mask(traps,input$buffer,input$spacing)
        capt.hist <-list(bincapt = get.capt.hist(detections))
        param.fix <- input$parameter
        param.fix.value <- list(g0 = input$g0,sigma = input$sigma,z = input$z,shape = input$shape,
                             scale = input$scale, shape.1 = input$shape.1,shape.2 = input$shape.2)
        idx <- match(param.fix,names(param.fix.value))
        fix <- param.fix.value[idx]
        fit <- NULL
        fit <- tryCatch({fit.ascr(capt = capt.hist,traps = traps,mask = mask,detfn =  input$select,
                                  fix = fix)},
                        warning = function(w) print("fit.ascr convergence issues"))
        if(class(fit)[1]=="ascr"){
            if(input$call.num > nrow(capt.hist$bincapt)){
                layout(matrix(c(1,1,1,2,2,2,1,1,1,2,2,2,3,3,3,3,3,3),byrow = TRUE,ncol = 6))
                show.detsurf(fit)
                plot(1,1,col="white",axes = FALSE,xlab = "",ylab = "")
                text(1,1,paste("There are only",nrow(capt.hist$bincapt),"calls",collapse = " "),col = "red",cex = 2)
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
})

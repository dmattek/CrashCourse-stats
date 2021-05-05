# Shiny app to demonstrate the concept of 
# confidence intervals for the sample mean.
#
# Draw samples from a normal distribution, 
# calculate the sample mean and CI,
# plot them horizontally and compare to 
# the population mean.
#
# Author: Maciej Dobrzynski, University of Bern


library(data.table)
library(ggplot2)

#' Calculate confidence intervals of a sample
#'
#' @param x a numeric vector.
#' @param ... parameters passed to the `t.test` function.
#'
#' @return a named list with the sample mean and confidence intervals.
#' @export
#'
#' @examples
myCalcCI = function(x, ...) {
  locRes = t.test(x, ...)
  locCI = as.list(c(locRes$estimate, locRes$conf.int))
  names(locCI) = c("sampleMean", "CIlower", "CIupper")
  
  return(locCI)
} 


shinyApp(
  # UI
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6,
                 numericInput("niSampleN", label = "# samples:",
                              min = 1, value = 10, step = 1, width = "100%"),
                 numericInput("niPopMn", label = "Popul. mean:",
                              value = 0, step = .1, width = "100%")
                 ),
          column(6,
                 numericInput("niSampleSz", label = "Sample size:",
                              min = 2, value = 5, step = 1, width = "100%"),
                 numericInput("niPopSD", label = "Popul. SD:",
                              min = .1, value = 1, step = .1, width = "100%")
                 )
        ),
        
        sliderInput("slCL", label = "Confidence level:",
                    min = 50, max = 100, value = 95, step = 1),
        
        actionButton("butRedraw", "Draw a sample"),
        actionButton("butReset", "Reset")
        
      ),
      
      mainPanel(
        plotOutput("distPlot"),
        textOutput("percText")
      )
    )
  ),
  
  ## SERVER
  server = function(input, output) {
    
    rv = reactiveValues(datAggr = NULL)
    
    #v = reactiveValues(datAggr = NULL)
    
    # Generate a collection of niSampleN samples, with each sample of size slSampleSz.
    # Samples drawn from a normal distribution with slPopMn and slPopSD.
    #
    # Return:
    #    data.table with 5 columns: 
    #       group, 
    #       sample mean, 
    #       CI upper and lower, 
    #       logical whether the population mean is within CIs
    
    newSample = reactive({
      
      locDummy = input$butRedraw

      req(input$niSampleN)
      req(input$niSampleSz)
      req(input$niPopMn)
      req(input$niPopSD)
            
      locNrowCurr = nrow(rv$datAggr)
      
      
      locDT = data.table(x = rnorm(input$niSampleN * input$niSampleSz, 
                                   mean = input$niPopMn,
                                   sd = input$niPopSD),
                         gr = rep(seq(1, input$niSampleN, 1) + ifelse(is.null(locNrowCurr), 
                                                                      0, 
                                                                      locNrowCurr), 
                                  each = input$niSampleSz))
      
      locDTaggr = locDT[,
                        myCalcCI(x, 
                                 conf.level = input$slCL * 0.01),
                        by = gr]
      
      locDTaggr[,
                popMeanIncl := between(input$niPopMn, 
                                       CIlower, 
                                       CIupper)]
      
      return(locDTaggr)
    })
    
    observeEvent(input$butRedraw, {
      rv$datAggr = rbind(rv$datAggr,
                         newSample())
    })
    
    observeEvent(input$butReset, {
      rv$datAggr = NULL
    })
    
    # Text
    output$percText = renderText({
      
      if (!is.null(rv$datAggr)) {
        sprintf("%.2f%% of CI contains the population mean", 
                100*mean(rv$datAggr[["popMeanIncl"]]))
      }      
    })
    
    # Plot
    
    output$distPlot = renderPlot({
      
      validate(
        need(rv$datAggr, "Draw a sample!")
      )
      
      ggplot(rv$datAggr,
             aes(x = gr,
                 y = sampleMean,
                 ymin = CIlower,
                 ymax = CIupper)) +
        geom_pointrange(aes(color = popMeanIncl)) +
        geom_hline(yintercept = input$niPopMn, 
                   color = "red", 
                   linetype = "dashed") +
        scale_color_manual(name = "Population mean included in CI:", 
                           values = c("TRUE" = "#4E79A7",
                                      "FALSE" = "#F28E2B")) +
        theme_minimal() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.title.y = element_blank(),
              legend.position = "top")
    })
    
  }
)
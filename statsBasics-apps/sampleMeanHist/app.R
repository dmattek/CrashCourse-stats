# Shiny app to demonstrate the concept of 
# the sample distribution.
#
# Draw samples from a normal distribution, 
# calculate the sample mean and
# plot them as a historgram.
#
# Author: Maciej Dobrzynski, University of Bern


#' Generate sample means from normal distribution
#'
#' @param inSampleSz an integer, sample size.
#' @param inSampleN an integer, number of samples.
#' @param inMn a float, mean of the normal distribution.
#' @param inSD a float, sd of the normal distribution.
#'
#' @return a numeric vector.
#' @export
#'
#' @examples
fGenSamples = function(inSampleSz, inSampleN,
                       inMn, inSD) {
  sapply(1:inSampleN, 
         function(x) 
           mean(rnorm(inSampleSz, inMn, inSD)))
}


shinyApp(
  # UI
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6,
                 numericInput("niPopMn", label = "Popul. mean:",
                              value = 5., step = .1),
                 numericInput("niSampleSz", label = "Sample size:",
                              min = 2, value = 5, step = 1)
          ),
          column(6,
                 numericInput("niPopSD", label = "Popul. SD:",
                              min = .1, value = 1, step = .1),
                 numericInput("niSampleN", label = "# samples:",
                              min = 1, value = 100, step = 1)
          )
        ),
        
        actionButton("butRedraw", "Draw a sample"),
        
        tags$hr(),
        verbatimTextOutput("resText")
      ),
      
      mainPanel(
        plotOutput("distPlot")
      )
    )
  ),
  
  ## SERVER
  server = function(input, output) {
    
    getSample = reactive({
      locDummy = input$butRedraw
      
      req(input$niSampleSz)
      req(input$niSampleN)
      req(input$niPopMn)
      req(input$niPopSD)
      
      locV = fGenSamples(input$niSampleSz, 
                         input$niSampleN, 
                         input$niPopMn, 
                         input$niPopSD)
      return(locV)
    })
    
    output$distPlot = renderPlot({


      
      hist(getSample(), 
           freq = F, 
           main = sprintf("%d FOVs, %d cells per FOV", 
                          input$niSampleN, 
                          input$niSampleSz), 
           xlab = "Mean FI in a FOV", 
           xlim = c(input$niPopMn - 2*input$niPopSD,
                    input$niPopMn + 2*input$niPopSD))
      abline(v = input$niPopMn, 
             col = "#de3765", 
             lty = 2, lwd = 2)
    })
    
    output$resText = renderText({
      
      locSample = getSample()
      
      sprintf("Mean of sample means = %.2f\nSD of sample means   = %.2f", 
              mean(locSample),
              sd(locSample))
    })
    
  }
)
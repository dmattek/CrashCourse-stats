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
                              value = 0, step = .1),
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
        
        actionButton("butRedraw", "Draw a sample")
      ),
      
      mainPanel(
        plotOutput("distPlot")
      )
    )
  ),
  
  ## SERVER
  server = function(input, output) {
    
    output$distPlot = renderPlot({
      
      locDumm = input$butRedraw
      
      locV = fGenSamples(input$niSampleSz, 
                         input$niSampleN, 
                         input$niPopMn, 
                         input$niPopSD)
      
      hist(locV, 
           freq = F, 
           main = sprintf("%d FOVs, %d cells per FOV", 
                          input$niSampleN, 
                          input$niSampleSz), 
           xlab = "Mean FI in a FOV")
      abline(v = input$niPopMn, 
             col = "#de3765", 
             lty = 2, lwd = 2)
    })
    
  }
)
# Shiny app to demonstrate the concept of
# confidence intervals.
#
# Draw a sample from a normal distribution, 
# calculate the sample mean and
# plot the sampling distribution with the CL% of the inner region shaded.
#
# Author: Maciej Dobrzynski, University of Bern


#' Plot t-distribution with shaded region
#'
#' The x-axis is transformed according to $x' = \frac{x - \bar{x}}{\sigma / \sqrt{n}}$.
#' 
#' @param nSz an integer, sample size.
#' @param nMn a float, mean for x-axis transformation.
#' @param nSD a float, sd for x-axis transformation.
#' @param vertLines a vector with x coordinates of vertical dashed lines.
#' @param vertLinesCol a vector with colours for vertical lines.
#' @param nFrac a float, fraction of the distribution to shade.
#' @param xlim a two-element vector with x-axis limits.
#' @param xn an integer with the number of points on the x-axis.
#' @param inv logical, FALSE => inner region of the distribution is shaded; TRUE => tails of the distribution is shaded; missing => no shading
#' @param ... additioal parameters passed to the `plot` function.
#'
#' @return a plot.
#' @export
#'
#' @examples
fPlotShadedTdist = function(nSz, 
                            nMn, nSD, 
                            vertLines, vertLinesCol, 
                            nFrac, 
                            xlim, 
                            xn = 100, inv, ...) {
  
  x = seq(xlim[1], xlim[2], (xlim[2] - xlim[1])/xn )
  xT = (x - nMn) / (nSD / sqrt(nSz))
  y = dt(xT, df = nSz-1)
  
  plot(x, 
       y,
       type = "l",
       xlim = xlim,
       ...)
  
  if (!missing(inv)) {
    if (inv) {
      xL = qt(nFrac/2, df = nSz-1) * nSD / sqrt(nSz) + nMn
      xR = qt(1 - nFrac/2, df = nSz-1) * nSD / sqrt(nSz) + nMn
      
      polygon(c(min(x), x[x<=xL], max(x[x<=xL])), 
              c(0, y[x<=xL], 0), 
              col="grey80")
      polygon(c(x[x>=xR], max(x), min(x[x>=xR])), 
              c(y[x>=xR], 0, 0), 
              col="grey80")
    } else {
      xL = qt((1-nFrac)/2, df = nSz-1) * nSD / sqrt(nSz) + nMn
      xR = qt((1 + nFrac)/2, df = nSz-1) * nSD / sqrt(nSz) + nMn
      
      polygon(c(min(x[x>=xL]), x[x>=xL & x<=xR], max(x[x<=xR])), 
              c(0,             y[x>=xL & x<=xR], 0), 
              col="grey80")
    }
  }
  
  if (!missing(vertLines) & !(missing(vertLinesCol))) {
    abline(v = vertLines, 
           col = vertLinesCol, 
           lty = 2, lwd = 2)
  }
  
}

shinyApp(
  # UI
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6,
                 numericInput("slPopMn", label = "Popul. mean:",
                              value = 0, step = .1),
                 numericInput("slSampleSz", label = "Sample size:",
                              min = 2, value = 5, step = 1)
                 ),
          column(6,
                 numericInput("slPopSD", label = "Popul. SD:",
                              min = .1, value = 1, step = .1)
                 )
        ),
        sliderInput("slCL", label = "Confidence level:",
                    min = 50, max = 100, value = 95, step = 1),
        
        actionButton("butRedraw", "Draw a sample")
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
      return(rnorm(n = input$slSampleSz, 
                   mean = input$slPopMn, 
                   sd = input$slPopSD))
    })
    
    output$distPlot = renderPlot({
      locSample = getSample()
      locMn = mean(locSample)
      locSD = sd(locSample)
      locL = length(locSample)
      locXlim = c(input$slPopMn - 3*input$slPopSD,
                  input$slPopMn + 3*input$slPopSD)
      
      fPlotShadedTdist(nSz = locL,
                       nMn = locMn, 
                       nSD = locSD,
                       vertLines = c(locMn, input$slPopMn),
                       vertLinesCol = c("#7cb5ec", "#de3765"),
                       nFrac = input$slCL * 0.01,
                       xlim = locXlim,
                       xn = 1000,
                       inv = F,
                       xlab = "",
                       ylab = "Density")
    })
    
  }
)
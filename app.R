library(shiny)
if (!require('devtools')) install.packages('devtools'); library('devtools')
source("C:/Users/Craig/Dropbox/_UNF/Craig Thesis/R-Roughness/R/Roughness/L1.R")


# GUI Interface -----------------------------------------------------------

ui <- basicPage(
    fluidRow(
      column(width=6,
        plotOutput("main"
          , height = "350px"
          , click = "mce"
          , dblclick = "mde"
          , hover = "mhe"
          , brush = "mbe"
        )#plotOutput
      )#column
      
      , column(width=3,
          plotOutput("right"
            , height="350px"
            , click="rce"
            , dblclick = "rde"
            , hover = "rhe"
            , brush="rbe"
          )#plotOutput
        )#column
      )#fluidRow
    
    , fluidRow(
        column(width=6,
          plotOutput("bottom"
            , height="250px"
            , click="bce"
            , dblclick="bde"
            , hover="bhe"
            , brush=brushOpts(id="bbe", direction="x")
          )#plotOutput
        )#column
    )#fluidRow
    
    , fluidRow(
        column(width=9,
               verbatimTextOutput("info")
        )#column
    )#fluidRow
  
  )# basicPage


# Server ------------------------------------------------------------------

server <- function(input, output) {
    w <- dim(z)[1]
    h <- dim(z)[2]
  
    mca <- reactiveValues(x = 100, y= 100)
    mcr <- reactiveValues(x = 0.5, y=0.5)
    
    observeEvent(input$mce$x, {
        mcr$x <- input$mce$x
        mca$x <- round(mcr$x*w,0)
    })
    
    observeEvent(input$mce$y, {
        mcr$y <- input$mce$y
        mca$y <- round(mcr$y*h,0)
    })
  

  output$main <- renderPlot({
    image(z, axes=F, useRaster=T)
    abline(v=mcr$x, h=mcr$y)
    box(lwd=3)
  })#main
  
  output$bottom <- renderPlot({
    plot(z[,mca$y], type="l", lwd=2)
    abline(v=mca$x)
  })
  
  output$right <- renderPlot({
    plot(z[mca$x,],1:dim(z)[2], type="l", lwd=2)
    abline(h=mca$y)
    
  })
   
  output$info <- renderText({
    paste0(  "x: ", input$mcr$x, "\n"
           , "y: ", input$mcr$y, "\n"
           , "x2:", mca$x, "\n"
           , "y2:", mca$y, "\n"
           )
  })#renderText
}#server

# Run Application ---------------------------------------------------------

shinyApp(ui = ui, server = server)


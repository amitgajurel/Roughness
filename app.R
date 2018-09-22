library(shiny)
if (!require('devtools')) install.packages('devtools'); library('devtools')
source("C:/Users/Craig/Dropbox/_UNF/Craig Thesis/R-Roughness/R/Roughness/L1.R")

# Define UI for application that draws a histogram
ui <- basicPage(
  fluidRow(
    column(width=6,
      plotOutput("img"
                 , click = "plot_click"
                 , hover = "plot_hover"
                 , height = "350px"
                 )
    ),
    column(width=3,
           plotOutput("yaxs", height="350px")
    )
  ),
  fluidRow(
    column(width=6,
           plotOutput("xaxs"
                      , height="250px"
                      , brush=brushOpts(id="xbrush", direction="x")
                      , hover="x_hover"
           )
           )
  )
  
  )# fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {
    crd <- reactiveValues(x = 100, y= 100)
    
    observeEvent(input$plot_click$x,
                 {crd$x <- round(input$plot_click$x*dim(z)[1],0)}
                 )
    
    observeEvent(input$plot_click$y,
                 {crd$y <- round(input$plot_click$y*dim(z)[2],0)}
    )
  
  # reactive({
  #   xx <- round(input$plot_click$x*dim(z)[1],0)
  #   yy <- round(input$plot_click$y*dim(z)[2],0)
  #   
  # })
  #xx <- reactive(round(input$plot_click$x*dim(z)[1],0))
    
  
  output$img <- renderPlot({
    #xx <- round(input$plot_click$x*dim(z)[1],0)
    #yy <- round(input$plot_click$y*dim(z)[2],0)
    
    image(z, axes=F, useRaster=T)
    abline(v=crd$x/dim(z)[1], h=crd$y/dim(z)[2])
    box(lwd=3)
  })#output$img
  
  output$xaxs <- renderPlot({
    # yy <- round(input$plot_click$y*dim(z)[2],0)
    yy <- crd$y
    plot(z[,yy], type="l", lwd=2)
    abline(v=crd$x)
  })
  
  output$yaxs <- renderPlot({
    # xx <- round(input$plot_click$x*dim(z)[1],0)
    xx <- crd$x
    
    plot(z[xx,],1:dim(z)[2], type="l", lwd=2)
    abline(h=crd$y)
    
  })
   
  output$info <- renderText({
    #xx <- round(input$plot_click$x*dim(z)[1],0)
    yy <- round(input$plot_click$y*dim(z)[2],0)
    
    paste0(  "x: ", crd$x, "\n"
           , "y: ", crd$y, "\n"
           , "z: ", round(z[crd$x,crd$y],2), "\n"
           )
  })#renderText
}#server

# Run the application 
shinyApp(ui = ui, server = server)


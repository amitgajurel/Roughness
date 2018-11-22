library(shiny)
if (!require('devtools')) install.packages('devtools'); library('devtools')
source("C:/Users/Craig/Dropbox/_UNF/Craig Thesis/R-Roughness/R/Roughness/L1.R")
source("C:/Users/Craig/Dropbox/_UNF/Craig Thesis/R-Roughness/R/Roughness/Roughness-Geo.R")


# GUI Interface -----------------------------------------------------------
# See Notes at EOF for Variable Naming Scheme

ui <- basicPage(
  fluidRow(
    column(width=2,
      checkboxInput("image", "Show Depth", TRUE)),
    column(width=3,
      selectInput("colors", "Color Scheme: "
          , c(
            "heat.colors",
            "terrain.colors",
            "topo.colors",
            "cm.colors",
            "rainbow"
          )
      ),
      checkboxInput("rev","reverse colors", FALSE)
    ),
    column(width=3,
      sliderInput("range", "Range of Depth",
                  min=floor(min(z, na.rm=TRUE)/10)*10,
                  max=ceiling(max(z, na.rm=TRUE)/10)*10,
                  value=c(min(z, na.rm=TRUE),max(z, na.rm=TRUE))
                  )
    )
  ),
    fluidRow(
      column(width=6,
        plotOutput("main"
          , height = "350px"
          , click = "mcd"
          , dblclick = "mdd"
          , hover = "mhd"
          , brush = "mbd"
        )#plotOutput
      )#column
      
      , column(width=3,
          plotOutput("right"
            , height="350px"
            , click="rcd"
            , dblclick = "rdd"
            , hover = "rhd"
            , brush=brushOpts(id="rbd", direction="y")
          )#plotOutput
        )#column
      )#fluidRow
    
    , fluidRow(
        column(width=6,
          plotOutput("bottom"
            , height="250px"
            , click="bcd"
            , dblclick="bdd"
            , hover="bhd"
            , brush=brushOpts(id="bbd", direction="x")
          )#plotOutput
        )#column

         , column(width=3,
                tableOutput("info"),
                verbatimTextOutput("bpnl")
         )#column
    )#fluidRow
  , fluidRow(
      column(width=9,
          tableOutput("xtbl")
             )
  )
  
  )# basicPage


# Server ------------------------------------------------------------------

server <- function(input, output) {

    w <- dim(z)[1]
    h <- dim(z)[2]
    
    rbr.min <- reactive({round(input$rbd$ymin,0)})
    rbr.max <- reactive({round(input$rbd$ymax,0)})
    
    mz <- reactive({round(z[mca$x, mca$y],2)})
    mc <- reactive({matrix(c(mca$x, mca$y, mz())
                           , nrow=1
                           , dimnames=list("main", c("x","y","z"))
          )})
    
    rdf <- reactive({data.frame(x=z[mca$x,], y=1:ncol(z))})
    rha <- reactive({nearPoints(rdf(), input$rhd, xvar="x", yvar="y", threshold=10,maxpoints=1)})
    
    bdf <- reactive({data.frame(y=z[,mca$y], x=1:nrow(z))})
    bha <- reactive({nearPoints(bdf(), input$bhd, xvar="x", yvar="y", threshold=10,maxpoints=1)})
    
    color <- reactive({
        colVal <- 1:5
        names(colVal) <- c("heat.colors","terrain.colors","topo.colors","cm.colors","rainbow")
        if(input$rev) {
          switch(colVal[input$colors],
                 rev(heat.colors(100)),
                 rev(terrain.colors(100)),
                 rev(topo.colors(100)),
                 rev(cm.colors(100)),
                 rev(rainbow(100))
          )   
        }
        else{
            switch(colVal[input$colors],
                   heat.colors(100),
                   terrain.colors(100),
                   topo.colors(100),
                   cm.colors(100),
                   rainbow(100)
                   )   
        }
    })
    
    mca <- reactiveValues(x = 100, y= 100)
    mcr <- reactiveValues(x = 0.5, y=0.5)
    ref <- reactiveValues(w = dim(z)[1], h = dim(z)[2])
    
    # observeEvent(input$image, {
    #   if(input$image)
    #     {w=dim(z)[1]
    #      h=dim(z)[2]}
    #   else
    #     {w=1
    #      h=1}
    # })
    
    observeEvent(input$mcd$x, {
        mcr$x <- input$mcd$x
        mca$x <- round(mcr$x*w,0)
    })
    
    observeEvent(input$mcd$y, {
        mcr$y <- input$mcd$y
        mca$y <- round(mcr$y*h,0)
    })


  output$main <- renderPlot({
    if(input$image) {
       image(z, axes=T, useRaster=T, col=color(), zlim=c(input$range[1],input$range[2]))
       abline(v=mcr$x, h=mcr$y)
    } else {
       plot(C, axes=T, useRaster=T)
       abline(v=mca$x, h=mca$y)
    }
    
    box(lwd=3)
    
    #points(mcr$x, rha()[2]/h, pch=3)
    #points(bha()[2]/w, mcr$y, pch=3)
  })#main
  
  output$bottom <- renderPlot({
    plot(z[,mca$y], type="l", lwd=2)
    abline(v=mca$x)
  })
  
  output$right <- renderPlot({
    plot(z[mca$x,],1:dim(z)[2], type="l", lwd=2)
    abline(h=mca$y)
  })
   
  output$info <- renderTable({
    mc()
  })#renderText
  
  output$bpnl <- renderText({
    
  paste0("Bottom Panel Hover:\n",
         "x: ", bha()[2], "\n",
         "y: ", mca$y, "\n",
         "z: ", round(bha()[1],2),"\n",
         "ref :", w
        )
  
  })#renderText
  
  output$xtbl <- renderTable({
    df_ <- z[mca$x,rbr.min():rbr.max()]
    tbl <- data.frame(x=rbr.min():rbr.max(), y=df_)
    
    Geom(tbl)
  })
}#server

# Run Application ---------------------------------------------------------

shinyApp(ui = ui, server = server)


# Notes -------------------------------------------------------------------

### Naming Scheme
#
#       Event variable names are defined as a three-letter word.
#         First letter defines the panel:
#             • m = main
#             • r = right (y-axis)
#             • b = bottom (x-axis)
#         Second letter defines the event:
#             • c = click
#             • d = double-click
#             • h = hover
#             • b = brush (aka window select)
#         Third value defines the coordinate system
#             • a = absolute (values from 0 to ...)
#             • r = relative (values from 0% to 100%)
#             • d = direct (direct, unaltered input from the event)

library(shiny)

# Load Packages, Helper Functions, and Data -------------------------------

if (!require('devtools')) install.packages('devtools'); library('devtools')
if (!require('imager')) install.packages('imager'); library('imager')

source("Roughness-Geo.R")
source("Roughness-Frac.R")

load("data/Limestone_1mm.RData")

# GUI Interface -----------------------------------------------------------
# See Notes at EOF for Variable Naming Scheme

ui <- basicPage(
  fluidRow(
    column(width=1,
      checkboxInput("image", "Show Depth", FALSE)
    ), # column (left)
    
    column(width=2,
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
    ), # column (middle)
    
    column(width=3,offset=2,
      selectInput("core", "Select Core Image: "
          , c(
            "L1" ,
            "L2" ,
            "O1" ,
            "O4" ,
            "V5" ,
            "V21"
          )
      )# selectInput
    ) # column (right)
  ),
    fluidRow(
      column(width=6,
        plotOutput("main"
          , height = "350px"
          , click = "mcd"
          #, dblclick = "mdd"
          #, hover = "mhd"
          #, brush = "mbd"
        )#plotOutput
      )#column
      
      , column(width=3,
          plotOutput("right"
            , height="350px"
            , click="rcd"
            #, dblclick = "rdd"
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
            #, dblclick="bdd"
            , hover="bhd"
            , brush=brushOpts(id="bbd", direction="x")
          )#plotOutput
        )#column

         , column(width=3,
                HTML("Main Panel (click)"),
                tableOutput("main_ref"),
                HTML("Right Panel (hover)"),
                tableOutput("right_ref"),
                HTML("Bottom Panel (hover)"),
                tableOutput("bottom_ref")
         )#column
    )#fluidRow
  
   , fluidRow(
       column(width=9,
           HTML("X-Axis Geometric"),
           tableOutput("xtbl_Geom"),
           HTML("X-Axis Fractal"),
           tableOutput("xtbl_Frac"),
           HTML("Y-Axis Geometric"),
           tableOutput("ytbl_Geom"),
           HTML("Y-Axis Fractal"),
           tableOutput("ytbl_Frac")
       )#column
   )#fluidrow
  
  )# basicPage


# Server ------------------------------------------------------------------

server <- function(input, output, session) {

# Server - Events ---------------------------------------------------------

    # scaling parameters for width (w) and height (h) of plot
    w <- reactive(if(input$image) {dim(dep())[1]} else {1})
    h <- reactive(if(input$image) {dim(dep())[2]} else {1})
    
    # Main panel coordinate system
    mca <- reactiveValues(x = 314/2, y=214/2)
    mcr <- reactiveValues(x = 0.5, y=0.5)
    
        # Side Panel Event
        observeEvent(input$bcd$x, {
          mcr$x <- input$bcd$x
          mca$x <- round(mcr$x*w(),0)
        })
        
        observeEvent(input$rcd$y, {
          mcr$y <- input$rcd$y
          mca$y <- round(mcr$y*h(),0)
        })
        
        # Main Panel Event
        observeEvent(input$mcd$x, {
          mcr$x <- input$mcd$x
          mca$x <- round(mcr$x*w(),0)
        })
        
        observeEvent(input$mcd$y, {
          mcr$y <- input$mcd$y
          mca$y <- round(mcr$y*h(),0)
        })

    # Min/Max for Brush in the side panels
    rbr.min <- reactive({round(input$rbd$ymin,0)})
    rbr.max <- reactive({round(input$rbd$ymax,0)})
    
    bbr.min <- reactive({round(input$bbd$xmin,0)})
    bbr.max <- reactive({round(input$bbd$xmax,0)})
    
    # Point value for Click in the main panel
    mz <- reactive({round(dep()[mca$x, mca$y],2)})
    mc <- reactive({matrix(c(mca$x, mca$y, mz())
                           , nrow=1
                           , dimnames=list("main", c("x","y","z"))
          )})
    
    # Point values for Hover in the side panels
    rdf <- reactive({data.frame(y=1:ncol(dep()), z=dep()[mca$x,])})
    rha <- reactive({nearPoints(rdf(), input$rhd, xvar="z", yvar="y", threshold=10,maxpoints=1)})
    
    bdf <- reactive({data.frame(x=1:nrow(dep()), z=dep()[,mca$y])})
    bha <- reactive({nearPoints(bdf(), input$bhd, xvar="x", yvar="z", threshold=10,maxpoints=1)})
    

# Server - Params ---------------------------------------------

    # Load color scheme from UI
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
    }) #reactive (color)
    
    
    # Load Core Image depth from UI
    dep <- reactive({
      colVal <- 1:6
      names(colVal) <- c("L1", "L2", "O1", "O4", "V5", "V21")
      
      switch(colVal[input$core],
             L1.z,
             L2.z,
             O1.z,
             O4.z,
             V5.z,
             V21.z)
    }) # reactive (z)
    
    # Load Core Image raster from UI
    img <- reactive({
      colVal <- 1:6
      names(colVal) <- c("L1", "L2", "O1", "O4", "V5", "V21")
      
      switch(colVal[input$core],
             L1.RGB,
             L2.RGB,
             O1.RGB,
             O4.RGB,
             V5.RGB,
             V21.RGB)
    }) # reactive (z)

# Server - Output ---------------------------------------------------------


  output$main <- renderPlot({
    if(input$image) {
       image(dep()
             , axes=F
             , col=color()
             , ylim=c(1,0)
             , xlim=c(-0.05, 1.05)
             , xaxt="s"
        )
       axis(1, at=seq(0,300/nrow(dep()),length.out=7), labels = seq(0,300,50))
       axis(2, at=seq(0,200/ncol(dep()),length.out=5), labels = seq(0,200,50))
       abline(v=mcr$x, h=mcr$y)
    } else {
       plot(img()
            , axes=T
            #, useRaster=T
            , xlab = "circumferential (mm)"
            , ylab = "longitudinal (mm)"
       )
       abline(v=mca$x, h=mca$y)
    }
    
    box(lwd=3)
  })#main
  
  output$bottom <- renderPlot({
    plot(dep()[,mca$y]
         , type="l"
         , lwd=2
         , xlab="circumferential (mm)"
         , ylab="depth (mm)"
    )
    abline(v=mca$x)
  })
  
  output$right <- renderPlot({
    plot(dep()[mca$x,]
         , 1:dim(dep())[2]
         , type="l"
         , lwd=2
         , xlim=c(max(dep()[mca$x,], na.rm=T),min(dep()[mca$x,], na.rm=T))
         , ylim=c(dim(dep())[2],0)
         , ylab = "longitudinal (mm)"
         , xlab = "depth (mm)"
    )
    abline(h=mca$y)
  })
   
 
  output$main_ref <- renderTable({
    mc()
  })
  
  output$right_ref <- renderTable({
    rha()
  })
  
  output$bottom_ref <- renderTable({
    bha()
  })
  
  
  output$xtbl_Geom <- renderTable({
    if (is.null(input$bbd$xmin)) {
      df_ <- dep()[,mca$y]
      tbl <- data.frame(x=1:nrow(dep()), y=df_)
      tbl <- na.omit(tbl)
    } else {
      df_ <- dep()[bbr.min():bbr.max(), mca$y]
      tbl <- data.frame(x=bbr.min():bbr.max(), y=df_)
      tbl <- na.omit(tbl)
    }
    
    Geom(tbl)
  }, spacing="xs")
  
  
  output$xtbl_Frac <- renderTable({
    if (is.null(input$bbd$xmin)) {
      df_ <- dep()[,mca$y]
      tbl <- data.frame(x=1:nrow(dep()), y=df_)
      tbl <- na.omit(tbl)
    } else {
      df_ <- dep()[bbr.min():bbr.max(), mca$y]
      tbl <- data.frame(x=bbr.min():bbr.max(), y=df_)
      tbl <- na.omit(tbl)
    }
    
    Frac(tbl)
  }, spacing="xs", rownames = TRUE)
  
  
  output$ytbl_Geom <- renderTable({
    if (is.null(input$rbd$xmin)) {
        df_ <- dep()[mca$x,]
        tbl <- data.frame(x=1:ncol(dep()), y=df_)
        tbl <- na.omit(tbl)
    } else {
        df_ <- dep()[mca$x,rbr.min():rbr.max()]
        tbl <- data.frame(x=rbr.min():rbr.max(), y=df_)
        tbl <- na.omit(tbl)
    }
    
    Geom(tbl)
    }, spacing="xs")
  
  
  output$ytbl_Frac <- renderTable({
    if (is.null(input$rbd$xmin)) {
      df_ <- dep()[mca$x, ]
      tbl <- data.frame(x = 1:ncol(dep()), y = df_)
      tbl <- na.omit(tbl)
    } else {
      df_ <- dep()[mca$x, rbr.min():rbr.max()]
      tbl <- data.frame(x = rbr.min():rbr.max(), y = df_)
      tbl <- na.omit(tbl)
    }
    
    Frac(tbl)
  }, spacing = "xs", rownames = TRUE)
  
  
  
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

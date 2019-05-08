library(shiny)
library(leaflet)
library(colorRamps)

setwd("G:\\My Drive\\PhD\\Climate Velocity\\Data\\CRU\\")
file <- c("TMP_World.nc")
TMP <- brick(file)
TMP <- brick(TMP[[81:114]])
allyears <- rep(1, nlayers(TMP))
mnTMP <- stackApply(TMP, indices = allyears, fun = mean)

setwd("G:\\My Drive\\PhD\\Climate Velocity\\Data\\Phenology\\")
file <- c("SOS_World_0.5deg_QA_Smooth.nc")
SOS <- brick(file)
allyears <- rep(1, nlayers(SOS))
mnSOS <- stackApply(SOS, indices = allyears, fun = mean)

pal_tmpano <- colorNumeric(palette = matlab.like(10),  domain = c(-2,2), na.color = "transparent")
pal_sosano <- colorNumeric(palette = rev(matlab.like(10)),  domain = c(-15,15), na.color = "transparent")

ui<-fluidPage(
  selectInput("layer", "Layer",
              choices = c("TMP Anomaly", "SOS Anomaly")),
  sliderInput("year", "Year",
              min=1981, max=2014, value=1, ticks=T),
  
  mainPanel(leafletOutput('raster_map', width="100%"))
)


server<-function(input, output){
  
  output$raster_map = renderLeaflet({leaflet(width = "100%") %>% addTiles()})
  
  observe({
    if(input$layer=="TMP Anomaly") {
      reactiveRaster <- reactive({TMP[[input$year-1980]]-mnTMP})
      leafletProxy("raster_map") %>%
        clearImages() %>%
        clearControls() %>% 
        addRasterImage(reactiveRaster(),colors = pal_tmpano, opacity = 1, layerId = "TMP")%>%
        addLegend(pal = pal_tmpano, values = seq(-2,2,by=1),
                  title = "MAT Anomaly (°C)")
    }
    if(input$layer=="SOS Anomaly") {
      reactiveRaster <- reactive({SOS[[input$year-1980]]-mnSOS})
      leafletProxy("raster_map") %>%
        clearImages()%>%
        clearControls() %>% 
        addRasterImage(reactiveRaster(),colors = pal_sosano, opacity = 1, layerId = "SOS")%>%
        addLegend(pal = pal_sosano, values = seq(-15,15,by=5),
                  title = "SOS Anomaly (day)")
    }
  })
  
  #Show popup on click
  observeEvent(input$raster_map_click, {
    click <- input$raster_map_click
    text<-paste("Lattitude ", round(click$lat,2), "Longtitude ", round(click$lng,2))
    
    leafletProxy("raster_map") %>%
      clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })
}
# 
# showPopup <- function(lat, lng) {
#   content <- as.character(tagList(
#     tags$strong(HTML(sprintf("%s, %s %s",
#                              lat,lng,
#     ))), tags$br(),
#     #   sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
#     #   sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
#     #   sprintf("Adult population: %s", selectedZip$adultpop)
#   ))
#   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = popup)
# }

shinyApp(ui, server, options = list(height=600,width=1200))

library(readr)
library(leaflet)
library(shiny)
library(dplyr)
library(ggplot2)
library(here)

# system dependencies "jq", "v8", "gdal", "proj", "geos", "udunits", "gcc-fortran"

path <- here()
setwd(paste(path, "/data", sep=''))

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    # leafletOutput("estados", width = "100%", height = "100%"),
    absolutePanel(
        dateRangeInput("daterange6", "Date range:",
                       startview = "decade"),
        fixed=TRUE,
        top="25px",
        right="25px"
    ),
    absolutePanel(
        textOutput("currentTime"),
        fixed=TRUE,
        bottom="25px",
        left="25px"
    )
)


server <- function(input, output, session) {
    output$currentTime <- renderText({
        invalidateLater(1000, session)
        paste("UTC Time Zone:", Sys.time())
    })

    output$map <- renderLeaflet({
        # states <- geojsonio::geojson_read("../geojsonBrasil/brazil-states.geojson", what = "sp")
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        leaflet(states) %>%
            fitBounds(-73.99024, -33.75136, -32.390875, 5.270972) %>%
            setMaxBounds(-73.99024, -33.75136, -32.390875, 5.270972) %>%
            # addTiles() %>%
            addTiles(options=providerTileOptions(minZoom=4.3))
    })

    observe({
        fileData <- reactiveFileReader(1, NULL,
                                       '201812171908200G16.GLM--FLASH.txt',
                                       read_delim, delim=',', col_names=c("lat", "lon", "value"), col_types = c("ccc"))
        leafletProxy("map") %>%
        clearMarkers() %>% # comment to keep old points on the map
        addCircleMarkers(lng=as.numeric(fileData()$lon), lat=as.numeric(fileData()$lat),
                         color="dark_red", weight = 1, radius = 5,  opacity = 0.9, fill = TRUE,
                         fillColor = "red", fillOpacity = 0.6)
    })
}

shinyApp(ui, server)
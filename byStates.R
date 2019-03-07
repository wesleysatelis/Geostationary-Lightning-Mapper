library(readr)
library(leaflet)
library(shiny)
library(dplyr)
library(ggplot2)

# system dependencies "jq", "v8", "gdal", "proj", "geos", "udunits", "gcc-fortran"

path <- "/home/wesley/Dropbox/IC Cepagri/Global-Lightning-Mapper/data/"
setwd(path)

############ COLOR MAP FOR NUMBER OF INCIDENCES ############
states <- geojsonio::geojson_read("../geojsonBrasil/brazil-states.geojson", what = "sp")
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$id, bins = bins)
labels <- sprintf("<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
                  states$name, states$id) %>% lapply(htmltools::HTML)

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    # leafletOutput("map", width = "100%", height = "100%"),
    leafletOutput("estados", width = "100%", height = "100%"),
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
    
    ############ COLOR MAP FOR NUMBER OF INCIDENCES ############
    output$estados <- renderLeaflet({
        leaflet(states) %>%
            fitBounds(-73.99024, -33.75136, -32.390875, 5.270972) %>%
            setMaxBounds(-73.99024, -33.75136, -32.390875, 5.270972) %>%
            # addTiles() %>%
            addTiles(options=providerTileOptions(minZoom=4.3)) %>%
            addPolygons(fillColor = ~pal(id),
                        weight = 2,
                        opacity = 1,
                        color = "red",
                        dashArray = "3",
                        fillOpacity = 1,
                        highlight = highlightOptions(
                            weight = 3,
                            color = "red",
                            dashArray = "",
                            fillOpacity = 1,
                            bringToFront = TRUE),
                        label = labels,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>%
            addLegend(pal = pal, values = ~id, opacity = 0.7, title = NULL, position = "bottomright")
    })
}

shinyApp(ui, server)

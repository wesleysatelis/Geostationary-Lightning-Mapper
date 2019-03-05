library(readr)
library(leaflet)
library(shiny)
library(dplyr)
# system dependencies "jq", "v8", "gdal", "proj", "geos", "udunits", "gcc-fortran"
path <- "/home/wesley/Dropbox/IC Cepagri/GLM - Global Lightning Mapper/data/"
setwd(path)

# states <- geojsonio::geojson_read("../geojsonBrasil/brazil-states.geojson", what = "sp")
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
# pal <- colorNumeric("viridis", NULL)
pal <- colorBin("YlOrRd", domain = states$id, bins = bins)
labels <- sprintf("<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
                  states$name, states$id) %>% lapply(htmltools::HTML)

ui <- bootstrapPage(
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        leafletOutput("map", width = "100%", height = "100%")
        # leafletOutput("estados", width = "100%", height = "100%")
        # textOutput("currentTime")
    )


server <- function(input, output, session) {
    # output$currentTime <- renderText({
    #     invalidateLater(1000, session)
    #     paste("The current time is", Sys.time())
    # })
    
    # states <- geojsonio::geojson_read("../geojsonBrasil/brazil-states.geojson", what = "sp")
    # class(states)
    
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

    # observe({
    #     leafletProxy("map") %>%
    #     addPolygons(fillColor = ~pal(id),
    #                 weight = 2,
    #                 opacity = 1,
    #                 color = "red",
    #                 dashArray = "3",
    #                 fillOpacity = 1,
    #                 highlight = highlightOptions(
    #                     weight = 3,
    #                     color = "red",
    #                     dashArray = "",
    #                     fillOpacity = 1,
    #                     bringToFront = TRUE),
    #                 label = labels,
    #                 labelOptions = labelOptions(
    #                     style = list("font-weight" = "normal", padding = "3px 8px"),
    #                     textsize = "15px",
    #                     direction = "auto")) %>%
    #     addLegend(pal = pal, values = ~id, opacity = 0.7, title = NULL, position = "bottomright")
    # })
    
    observe({
        fileData <- reactiveFileReader(1, NULL,
                                       '201812171233000G16.GLM--FLASH.txt',
                                       read_delim, delim=',', col_names=c("lat", "lon", "value"), col_types = c("ccc"))
        # pal <- colorpal()
        leafletProxy("map") %>%
        clearMarkers() %>% # comment to keep old points on the map
        addCircleMarkers(lng=as.numeric(fileData()$lon), lat=as.numeric(fileData()$lat),
                         color="dark_red", weight = 1, radius = 5,  opacity = 0.9, fill = TRUE,
                         fillColor = "red", fillOpacity = 0.6)
    })
}

shinyApp(ui, server)

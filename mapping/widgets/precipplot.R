# precipplot(stations, precip)
#
# Shiny megawidget that combines a Leaflet map to show station locations,
# and Plotly plots of the precipitation amounts.

precipplot <- function(stations.df,precip.df) {

  library(dplyr)
  library(ggplot2)
  library(leaflet)
  library(plotly)
  library(shiny)


  stationName2stationId <- function(stationName) {
    stationId <- (stations.df %>%
                    filter(Station.Name == stationName) %>%
                    select(Station.ID)
                 )[[1]]
    return(stationId)
  }

  stationId2stationName <- function(stationId) {
    stationName <- as.character(
                     (stations.df %>%
                        filter(Station.ID == stationId) %>%
                        select(Station.Name)
                     )[[1]]
                   )
    return(stationName)
  }

  stationLatLon2stationId <- function(lat,lng) {
    stationId <- (stations.df %>%
                    filter(latitude == lat,
                           longitude == lng) %>%
                    select(Station.ID)
                 )[[1]]
    return(stationId)
  }

  stationId2stationLatLon <- function(stationId) {
    stationLatLon <- (stations.df %>%
                        filter(Station.ID == stationId) %>%
                        select(latitude,longitude)
                     )[,1:2]
    return(stationLatLon)
  }

  shinyApp(

    # Build the Shiny application user interface
    ui = bootstrapPage(
      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
      leafletOutput("map", width = "100%", height = "100%"),
      absolutePanel (top = 5, right = 2,
                     draggable = TRUE,
                     style = "opacity: 0.92",
                     wellPanel(
                       plotlyOutput("plot.plotly", height=250, width=300),
                       selectInput("station_select", "Station:",
                                   stations.df[,'Station.Name'])
                     )
      )
    ),

    server = function(input, output, session) {
      data <- reactiveValues(
        clickedMarker=NULL,
        station.df=data.frame(Time=c(0),Precipitation=c(0))
      )

      # update the plot based on the station the user
      # selected from the dropdown menu
      observeEvent(input$station_select, {
        station.id <- stationName2stationId(input$station_select)

        # create a Time/Precip dataframe for the selected marker
        # this will be used later to generate our plot
        data$station.df <- precip.df %>%
          filter(Station.ID == station.id)

        #    # popup a placard at the lat/lng
        #    latlon <- stationId2stationLatLon(station.id)
        #    proxy <- leafletProxy("map")
        #    proxy %>%
        #      clearPopups() %>% 
        #      addPopups(lng=latlon$longitude,
        #                lat=latlon$latitude,
        #                layerId="selected",
        #                popup=popupContent(station.id))
      })

      # monitor for clicks on map markers
      # save information about which marker was clicked
      # trigger an update to the plot
      # update the dropdown menu
      observeEvent(input$map_marker_click, {
        p <- input$map_marker_click
        data$clickedMarker <- p

        # remove any previously selected popups
        proxy <- leafletProxy("map")
        proxy %>% removePopup(layerId="selected")

        # lookup the station id of the selected marker
        # could have also filtered by p$id if we set the
        # layerId parameter in the addCircleMarkers() function.
        station.id <- stationLatLon2stationId(p$lat,p$lng)

        # create a Time/Precip dataframe for the selected marker
        # this will be used later to generate our plot
        data$station.df <- precip.df %>%
          filter(Station.ID == station.id)

        # update the selectInput widget with the marker name
        if(!is.null(p$id)){
          if(is.null(input$station_select) || input$station_select!=p$id) {
            stationName = stationId2stationName(p$id)
            updateSelectInput(session,
                              "station_select",
                              selected=stationName)
          }
        }
      })

      # monitor for clicks on the map (not on markers)
      # clicks on non-marker items do nothing
      observeEvent(input$map_click, {
        data$clickedMarker <- NULL
        proxy = leafletProxy("map")
        proxy %>% clearPopups()
      })

      # generate our Leaflet based map output 
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addCircleMarkers(
            data = stations.df,
            lng = ~longitude,
            lat = ~latitude,
            radius = 4,
            fillOpacity = 0.8,
            layerId = ~Station.ID,
            popup = paste(
              "Station Name: ", htmlEscape(stations.df$Station.Name), "<br>",
              "Station ID: ", htmlEscape(stations.df$Station.ID), "<br>",
              "Latitude: ", htmlEscape(stations.df$latitude), "<br>",
              "Longitude: ", htmlEscape(stations.df$longitude), "<br>"
            )
          )
      })

      # generate our lattice based xy curve
      # based on the data in our reactive dataframe data$station.df
      output$plot.lattice <- renderPlot({
        if (is.null(data$station.df)) {
          return(NULL)
        }
        print(xyplot(Precipitation ~ Time, data = data$station.df))
      })

      # generate our Plotly based xy curve
      # based on the data in our reactive dataframe data$station.df
      output$plot.plotly <- renderPlotly({
        if (is.null(data$station.df)) {
          return(NULL)
        }

        # using the ggplotly() function (it's faster)
        p = ggplot(data$station.df,aes(Time,Precipitation)) +
          geom_line()
        ggplotly(p)
      })

    },

    options = list(height = 500)
  )
}

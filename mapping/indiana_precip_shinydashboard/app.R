library(dplyr)
library(tibble)
library(readr)
library(htmltools)
library(leaflet)
library(shiny)
library(shinydashboard)
library(DT)
library(dygraphs)

base.dir = '..'
data.dir = file.path(base.dir,'data')
precip.dir = file.path(data.dir,'indiana_precipitation')

# read in location and description of the precip monitor stations
# station.csv has duplicate entries in it (121739).
stations.df <- read_csv( file.path(precip.dir,'station.csv.clean') ) %>%
                  rename(Station.ID = `Station ID`) %>%
                  rename(Station.Name = `Station Name`)

# read in the precipitation data
precip.details.df = tibble()
precip.files = list.files(path=precip.dir,pattern="hr.*.csv")

for (i in 1:length(precip.files)) {
  # get the station id from the file path
  matches = gregexpr("[0-9]+",precip.files[i])
  
  station.id = as.integer(regmatches(precip.files[i],matches)[[1]])
  
  # read the precipitation data into a dataframe
  p = file.path(precip.dir,precip.files[i])
  df = read_csv(p)

  # create a row with StationID, StationName, lat, lon information
  # from stations.df. Add to it some metrics about the precipitation
  # data like min, max, mean amounts. store the precipitation dataframe
  # in a listcolumn so we can quickly grab it later by indexing by the
  # Station ID.
  station <- (stations.df %>%
              filter(Station.ID == station.id) %>%
              mutate(
                min = min(df$Precipitation),
                max = max(df$Precipitation),
                mean = mean(df$Precipitation),
                data = list(df)
              )
             )
  
  # build up a new tibble, row by row, that links in the min, max,
  # mean, and precipitation dataframe.
  precip.details.df = rbind(precip.details.df,station)
}

# remove the old stations dataframe
rm(stations.df)

# setup map marker colors

qpal = colorQuantile("RdYlBu",precip.details.df$mean,10)
precip.details.df <- precip.details.df %>%
                       mutate(color = qpal(mean))

# some helper functions that make finding data in
# precip.details.df simple

stationName2stationId <- function(stationName) {
  stationId <- (precip.details.df %>%
                  filter(Station.Name == stationName) %>%
                  select(Station.ID)
               )[[1]]
  return(stationId)
}

stationId2stationName <- function(stationId) {
  stationName <- as.character(
                   (precip.details.df %>%
                      filter(Station.ID == stationId) %>%
                      select(Station.Name)
                   )[[1]]
                 )
  return(stationName)
}

stationLatLon2stationId <- function(lat,lng) {
  stationId <- (precip.details.df %>%
                  filter(latitude == lat,
                         longitude == lng) %>%
                  select(Station.ID)
               )[[1]]
  return(stationId)
}

stationId2stationLatLon <- function(stationId) {
  stationLatLon <- (precip.details.df %>%
                      filter(Station.ID == stationId) %>%
                      select(latitude,longitude)
                   )[,1:2]
  return(stationLatLon)
}

stationId2precipdf <- function(stationId) {
  precipdf <- (precip.details.df %>%
                 filter(Station.ID == stationId) %>%
                 select(data)
              )[[1]]
  return(precipdf)
}

rowNumber2stationId <- function(rownum) {
  stationId <- precip.details.df[rownum,"Station.ID"][[1]]
  return(stationId)
}

popupContent <- function(stationId) {
  s <- precip.details.df %>% filter(Station.ID == stationId)
  content = paste(
              "Station Name: ", htmlEscape(s$Station.Name), "<br>",
              "Station ID: ", htmlEscape(s$Station.ID), "<br>" #,
#              "Latitude: ", htmlEscape(s$latitude), "<br>",
#              "Longitude: ", htmlEscape(s$longitude), "<br>"
            )
  return(content)
}

# Build the Shiny application user interface
# using the shiny dashboard

# if you change the box height, also change the hardcoded
# dataTables_scrollBody height and map height below in the
# custom CSS
box.height = 450

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Indiana Precipitation Explorer",
    titleWidth = 450
  ),
  
  dashboardSidebar(disable=TRUE),
  
  dashboardBody(
    
    # Hint from https://rstudio.github.io/shinydashboard/appearance.html#long-titles
    # Add some custom CSS to make the title background area the same
    # color as the rest of the header.
    tags$head(tags$style(HTML('
      .skin-blue .main-header .logo {
        background-color: #3c8dbc;
        # text-align: left;
        # font-size: 24px;
      }
      .skin-blue .main-header .logo:hover {
        background-color: #3c8dbc;
      }
      .dataTables_scrollBody {
        height: 250px !important;
      }
      #map {
        height: 375px !important;
      }
    '))),
    
    fluidRow(

      box(title="Station Details",
          solidHeader=FALSE,
          width=6,
          height=box.height,
          DT::dataTableOutput('tbl'),
          downloadButton("downloadData","Download Station Precipitation Data")
      ),
    
      box(title="Station Locations",
          solidHeader=FALSE,
          width=6,
          height=box.height,
          leafletOutput('map')
      )
    ),
    
    fluidRow(
      box(width=12,
#          height=box.height,
          dygraphOutput('plot.dygraph')
      )
    )
    
  )
)


server <- function(input, output, session) {

    # setup our reactive values
  data <- reactiveValues(
    clickedMarker=NULL,
    station.id=NULL,
    station.df=data.frame(Time=c(0),Precipitation=c(0))
  )

  updateMapSelectedStation <- function(station.id) {
    # update the map widget with the newly selected station
    # popup a placard at the lat/lng
    latlon <- stationId2stationLatLon(station.id)
    proxy <- leafletProxy("map")
    proxy %>%
      clearPopups() %>%
      addPopups(lng=latlon$longitude,
                lat=latlon$latitude,
                layerId="selected",
                popup=popupContent(station.id))  
  }

  # updateSelectSelectedStation <- function(stationId) {
  #   # update the selectInput widget with the selected station id
  #   if(!is.null(stationId)){
  #     if(is.null(input$station_select) || input$station_select!=stationId) {
  #       stationName = stationId2stationName(stationId)
  #       updateSelectInput(session,"station_select",selected=stationName)
  #     }
  #   }
  # }
  
  updateDataTableSelectedStation <- function(stationId) {
    # update the datatable widget with the selected station id
    # only handles single row selection
    if (!is.null(stationId)) {
      indicies = which(precip.details.df$Station.ID == stationId)
      if (!(indicies %in% input$tbl_rows_selected)) {
        indicies <- switch(as.character(length(indicies)),
                           "0" = NULL,
                           c(indicies))
        proxy <- dataTableProxy("tbl")
        proxy %>% selectRows(indicies)
      }
    }
  }
  
  
  # setup observers for reactive values of our input widgets
  
  # update the plot and map based on the station the user
  # selected from the dropdown menu
  # observeEvent(input$station_select, {
  #   data$station.id <- stationName2stationId(input$station_select)
  # 
  #   # create a Time/Precip dataframe for the selected marker
  #   # this will be used later to generate our plot
  #   data$station.df <- stationId2precipdf(data$station.id)
  # 
  #   # update the selected station on the map
  #   updateMapSelectedStation(data$station.id)
  # })
  
  # update the plot and map based on the station the user
  # selected from the table
  # this only supports a single row being selected
  observeEvent(input$tbl_rows_selected, {
    data$station.id <- rowNumber2stationId(input$tbl_rows_selected)

    # create a Time/Precip dataframe for the selected marker
    # this will be used later to generate our plot
    data$station.df <- stationId2precipdf(data$station.id)

    # update the selected station on the map
    updateMapSelectedStation(data$station.id)
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
    data$station.id <- stationLatLon2stationId(p$lat,p$lng)
    
    # update the select input widget with the new station id
#    updateSelectSelectedStation(data$station.id)
    
    updateDataTableSelectedStation(data$station.id)
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
        data = precip.details.df %>% select(Station.ID,Station.Name,latitude,longitude,color),
        label = ~Station.Name,
        lng = ~longitude,
        lat = ~latitude,
        radius = 4,
        fillOpacity = 0.8,
        layerId = ~Station.ID,
        color = ~color
      ) #%>%
      #addLegend(pal = qpal, values = ~mean, opacity = 1)
  })
  
  # generate our DyGraph based xy curve
  # based on the data in our reactive dataframe data$station.df
  output$plot.dygraph <- renderDygraph({
    if (is.null(data$station.df)) {
      return(NULL)
    }

    station.name = stationId2stationName(data$station.id)
    graph.title = paste(station.name," Station Data")
    dygraph(as.data.frame(data$station.df), main=graph.title) %>%
      dySeries(name="Precipitation", label="Precipitation") %>%
      dyRangeSelector()
  })
  
  # generate our DataTables table
  # hide the longitude and latitude columns
  output$tbl <- DT::renderDataTable({
    #hideCols = list(3,4,8)
    hideCols = list()

    datatable(
      precip.details.df %>% select(Station.ID,Station.Name,min,max,mean),
      selection=list(mode="single",selected=c(1)),
      extensions=c("Scroller","Responsive"),
      style="bootstrap",
      class="compact",
      width="100%",
      options=list(
        deferRender=TRUE,
        scrollY=300,
        scroller=TRUE,
        columnDefs = list(list(
          visible=FALSE,
          targets=hideCols))
      )
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      file.path(precip.dir,paste('hr',data$station.id,'.csv',sep=''))
    },
    content = function(file) {
      write_csv(as.data.frame(data$station.df),file)
    }
  )
  
}

# Run the application 
shinyApp(ui,server)

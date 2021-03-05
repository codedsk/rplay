library(shiny)
library(ggplot2)
library(plotly)
library(tibble)
library(DBI)
library(dplyr)
library(glue)

  
spiro <- function(n1,n2,n3) {
  cached_result <- cached_runs %>%
    filter((n1 == n1), (n2 == n2), (n3 == n3)) %>%
    collect()
  
  if (cached_result %>% nrow() == 0) {
    
    print(glue("calculating new spirograph for n1 = {n1}, n2 = {n2}, n3 = {n3}"))
    
    # calculate the new spirograph points
    t <- seq(0,1,length.out=1000)
    z <- exp(1i*2*pi*n1*t) + exp(1i*2*pi*n2*t) + exp(1i*2*pi*n3*t)
    result <- tibble(x=Re(z),y=Im(z))
    
    # store the new result in the cache
    cached_runs %>%
      add_rows(n1, n2, n3, result$x, result$y)
    
  } else {
    
    print(glue("retrieving cached spirograph for n1 = {n1}, n2 = {n2}, n3 = {n3}"))
    
    # retrieve the x,y points from the database
    result <- cached_result %>% select(x,y)
  }
  
  return (result)
}

# Define UI for application that draws a spirograph
ui <- fluidPage(

  # Application title
  titlePanel("Spirograph"),

  sidebarLayout(

    sidebarPanel(
      # input sliders
      sliderInput(inputId="n1",label="n1",value=13,min=-10,max=20,step=1),
      sliderInput(inputId="n2",label="n2",value=-7,min=-10,max=20,step=1),
      sliderInput(inputId="n3",label="n3",value=-3,min=-10,max=20,step=1)
    ),

    mainPanel(
      # output plot
      plotlyOutput("spirograph")
    )
  )
)

# Define server logic required to draw a spirograph
server <- function(input, output) {
  
  output$spirograph <- renderPlotly({
    result <- spiro(input$n1,input$n2,input$n3)
    ggplot(data=result,aes(x=x,y=y)) +
        geom_path() +
        xlab("Real(z)") +
        ylab("Imag(z)")
  })
}

# Connect to the database
con <- dbConnect(
    RPostgres::Postgres(),
    dbname = "spirographs",
    host = "localhost",
    port = 5432,
    password = "postgres",
    user = "postgres"
)

# setup the initial database table structure
c <- spiro(0,0,0)
cached_runs <- tribble(
  ~n1, ~n2, ~n3, ~x, ~y,
  0, 0, 0, c$x, c$y
)

copy_to(con, cached_runs, name="cached_runs", temporary=FALSE, overwrite=TRUE)
cached_runs <- tbl(con,"cached_runs")

# Run the application
shinyApp(ui,server)

dbDisconnect(con)
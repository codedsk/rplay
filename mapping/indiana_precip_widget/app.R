library(dplyr)
library(htmltools)

base.dir = './playground/R/mapping/'
data.dir = file.path(base.dir,'data')
widgets.dir = file.path(base.dir,'widgets')
precip.dir = file.path(data.dir,'indiana_precipitation')

# read in location and description of the precip monitor stations
stations.df <- read.csv( file.path(precip.dir,'station.csv') )

# read in the precipitation data
precip.df = data.frame()
precip.files = list.files(path=precip.dir,pattern="hr.*.csv")

for (i in 1:length(precip.files)) {
  # get the station id from the file path
  matches = gregexpr("[0-9]+",precip.files[i])
  station.id = regmatches(precip.files[i],matches)[[1]]

  # save the file to our dataframe,
  # associating the station id with each row
  p = file.path(precip.dir,precip.files[i])
  df = read.csv(p)
  df$Station.ID = station.id
  precip.df = rbind(precip.df,df)
}


# Use our prebuilt Shiny widget
source(file.path(widgets.dir,'precipplot.R'))

# Call the precipplot Shiny widget as our main application
precipplot(stations.df,precip.df)

# R and Geospacial Data Visualization

## Examples:

**leaflet\_plotly\_examples.Rmd**

A R Markdown Notebook that shows basic usage of the Leaflet and Plotly packages. This notebook contains a number of maps and x-y graphs, and does take a long time to "knit" or render as an HTML page. When viewing this file, try running all of the code blocks and viewing it as an interactive notebook. Inside RStudio, use the Run -> Run All menu option to run the code blocks.

**shiny\_mapping\_examples.Rmd**

A R Markdown Notebook that uses the Shiny runtime for rendering. This is often referred to as an interactive document. It still combines narritive, code blocks, and output just like a regular R Markdown file, but also include a Shiny application (or widgets) that require a running Shiny server to support the interactive widgets. This file builds upon leaflet\_plotly\_examples.Rmd by showing how to use the `shinyApp()` function to combine the Leaflet map and Plotly graph into a single application where the user can manipulate the elements, and the elements react to each other. Inside Rstudio, use the Run Document button to convert the notebook with Shiny application into a HTML web page.

**indiana\_precip\_shiny/app.R**

A Shiny application hosted in an R script. The R script also includes code to read the data into data frames that are used by the application. The Shiny user interface is described by the `ui` variable and the server code is implemented in the `server()` function. These two pieces are passed as parameters to the `shinyApp()` function to create the Shiny application. When the user interface and server are hosted in the same R script, the script should be named app.R. In Rstudio, use the Run App button to launch the Shiny application.

**widgets/precipplot.R**

A Shiny widget hosted in an R script. Shiny applications can also be used as Shiny widgets, in the megawidget sense, inside of interactive documents or other Shiny applications. This example takes the Leaflet map and Plotly graph from the previous examples, and adds a dropdown menu that allows users to choose the observation station they would like to highlight on the map. The output graph is controlled by user interaction with both the dropdown menu options and makers on the map. This widget can be included in an R Markdown file as a Shiny widget or a Shiny application, or embedded in another Shiny application.

**indiana\_precip\_widget/app.R**

An R script that holds a Shiny application. This script calls the `precipplot()` Shiny widget from the widgets directory which launches a server to host the Shiny application. Update the `base.dir` variable to be the path to your mapping directory. In RStudio, highlight the code and use the Run button to run the code.

**indiana\_precip\_widget\_embedded/app.R**

A Shiny application with the precipplot Shiny widget embedded within it. This example demonstrates how to use a megawidget inside of a Shiny application. In RStudio, use the Run App button to launch the Shiny application.

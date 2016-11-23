is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 
if (!is.installed("RCurl")) install.packages("RCurl")
if (!is.installed("RJSONIO")) install.packages("RJSONIO")
if (!is.installed("shiny")) install.packages("shiny")

library(shiny)
runApp("shiny_ui", port = 9000)
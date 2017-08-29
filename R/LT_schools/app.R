rm(list=ls())
library(data.table)
library(RSelenium)
library(tidyr)
require(RJSONIO)
library(magrittr)
library(dplyr)
library(foreach)
library(plyr)
library(ggmap)
library(leaflet)
library(shiny)
library(shinydashboard)
source("00functions.R")


load("add_miss_year.Rdata")#mhsd
vdatr[is.na(date), date:=daten1]
# 
# theme_set(theme_bw(16))
# map <- qmap("lithuania", zoom = 7,
#                    color = "bw", legend = "topleft")
# map
# map+geom_point(aes(x=y, y=x), data=vdatr)

#setnames(vdatr, c("x", "y"), c("long", "lat"))
setnames(vdatr, c("x", "y"), c("lat","long"))

vdatr[, date1:=date]
vdatr[]
vdatr[is.na(date), date1:=2016 ]
vdatr[, date1:=as.numeric(date1)]


# ui <- fluidPage(
#   sidebarPanel(
#     # Simple integer interval
#     sliderInput("inslider","Slider", 
#                 min=min(vdatr[,date1]), max=max(vdatr[,date1]), value=c(min(vdatr[,date1]),max(vdatr[,date1])),
#                 step = 10, animate=animationOptions(interval=100, loop=TRUE))),
#   leafletOutput("mymap"),
#   p()
# )

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(disable=TRUE),
    # Simple integer interval
  dashboardBody(
    sliderInput("inslider","", 
                min=min(vdatr[,date1]), max=max(vdatr[,date1]), value=c(min(vdatr[,date1]),max(vdatr[,date1])),
                step = 10, animate=animationOptions(interval=100, loop=TRUE)),
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
   leafletOutput("mymap")
  )
)


server <- function(input, output, session) {
  
  data <- reactive({
    filteredData<-vdatr
    if(!is.null(input$inslider)){
      filteredData<-filteredData %>%
        filter(
               date1 <= input$inslider[2] )
    }
      filteredData
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      setView(lng = mean(vdatr[, long], na.rm=TRUE), lat = mean(vdatr[, lat], na.rm=TRUE), zoom = 07)%>%
      addMarkers(data = data(), data()$long, data()$lat, popup = HTML(paste0("<a href='",data()$href,"'>", data()$name,"</a>")), label = as.character(data()$name) )#)
  })
}


shinyApp(ui, server)

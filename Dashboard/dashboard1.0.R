library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(leafsync)
library(rgdal)
library(rgeos)


########################
# Read in Cleaned Data #
########################

##############
#     UI     #
##############

ui = fluidPage(
    theme = "style.css",
    div(style = "padding: 1px 0px; width: '100%'",
        titlePanel(
            title = "",
            windowTitle = "Dashboard for Comsumer Complaints"
        )
    ),
    navbarPage(
        # Application title.
        title = div(span(
            "Dashboard for Comsumer Complaints",
            style = "position: relative; top: 50%; transform: translateY(-50%);")),
        
        tabPanel(
            "EDA Overview",
            tags$h1("Overview of Project for Consumer Complaints"),
            tags$p(HTML('&nbsp;')),
            tags$h2("Intro"),
            tags$p(HTML('&nbsp;')),
            tags$p(tags$a(href="https://www.consumerfinance.gov/data-research/consumer-complaints/", "Click here check out our data")),
            tags$p(tags$a(href="https://github.com/QMSS-Spring-2020", "Click here to see the code")),
            tags$p(HTML('&nbsp;')),
            tags$p("by Jiaqing Ge, Nikky Xiong, Richa Gupta")
        ),
        
        tabPanel(
            "Text Mining and Visualization for Narrative",
            tabPanel("", includeHTML("model2017_2.html"))
        ),
        
        tabPanel(
            "Unsupervised Learning - LDA",
            tabPanel("", includeHTML("LDA.html"))
        )
    )
)

##########################
#   Data Preprocessing   #
##########################

##################
#     Server     #
##################

server = function(input, output, session){}

#######################
# Run the Application #
#######################

shinyApp(ui = ui, server = server)


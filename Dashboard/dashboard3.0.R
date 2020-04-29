library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(DT)
library(lubridate)
library(ggthemes)
library(scales)
library(plotly)
library(stringr)
library(TTR)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(tidytext)
library(stringr)
library(tidyr)
library(geojsonio)
library(usmap)
library(tidycensus)
library(gapminder)

########################
# Read in Cleaned Data #
########################

df_raw <- read_csv('complaints.csv') 

##########################
#   Data Preprocessing   #
##########################

# EDA Overview #

# line chart
df <- df_raw %>%
    mutate(year = as.integer(substr(`Date received`, 
                                    start = 1, stop = 4))) %>%
    mutate(month = as.integer(substr(`Date received`, 
                                     start = 6, stop = 7))) %>%
    mutate(day = as.integer(substr(`Date received`, start =9 , stop = 10)))
df$`Date received` <- ymd(df$`Date received`)

# data table
df_new <- df %>% 
    mutate(date = as.POSIXct(paste(month , day , sep = "." ), 
                             format = "%m.%d" )) %>%
    group_by(date, Company, Product, State, 
             `Submitted via`, `Company response to consumer`) %>%
    summarise(number_of_complaints = n()) %>% 
    arrange(desc(number_of_complaints)) %>% 
    head(100)

pretty_headers <- 
    gsub("[.]", " ", colnames(df_new)) %>%
    str_to_title()

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
            tags$h4("by Jiaqing Ge, Nikky Xiong, Richa Gupta"),
            tags$p(HTML('&nbsp;')),
            tags$p(tags$a(href="https://www.consumerfinance.gov/data-research/consumer-complaints/", "Click here check out our data")),
            tags$p(tags$a(href="https://github.com/QMSS-Spring-2020", "Click here to see the code")),
            tags$p(HTML('&nbsp;')),
            dataTableOutput('ex1', width = 900, height = 400),
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

##################
#     Server     #
##################

server = function(input, output, session){
    output$ex1 <- DT::renderDataTable(
        DT::datatable(df_new, 
                      caption = 'Table 1: This is a simple data table for the complaints.',
                      rownames = FALSE,
                      class = 'cell-border stripe',
                      colnames = pretty_headers,
                      filter = list(position = "top"),
                      options = list(
                          pageLength = 5,
                          dom = "Bfrtip",
                          buttons = I("colvis"),
                          language = list(sSearch = "Filter:")
                      ),
                      extensions = c("Buttons", "Responsive"))
    )
}

#######################
# Run the Application #
#######################

shinyApp(ui = ui, server = server)


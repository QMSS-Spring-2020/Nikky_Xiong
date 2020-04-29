library(shiny)
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

########################
# Read in Cleaned Data #
########################

df_raw <- read_csv('complaints.csv') 

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
            tags$h1("Overview of Project for Consumer Complaint"),
            tags$p(HTML('&nbsp;')),
            tags$h2("Intro"),
            tags$p(HTML('&nbsp;')),
            tags$p(tags$a(href="https://www.consumerfinance.gov/data-research/consumer-complaints/", "Click here check out our data")),
            tags$p(tags$a(href="https://github.com/QMSS-Spring-2020", "Click here to see the code")),
            tags$p(HTML('&nbsp;')),
            tags$p("by Jiaqing Ge, Nikky Xiong, Richa Gupta"),
            tags$p(HTML('&nbsp;')),
            tags$h2("Overall Number of Complaints throughout a Year"),
            plotlyOutput("linechart", width = 800, height = 500),
            tags$p(HTML('&nbsp;')),
            dataTableOutput('ex1'),
            
            sidebarLayout(
                sidebarPanel(
                    conditionalPanel(
                        'input.dataset === "df_new"',
                        checkboxGroupInput("show_vars", "Columns to show:",
                                           names(df_new), selected = names(df_new))
                    )
                )
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
)))

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

##################
#     Server     #
##################

server = function(input, output, session) {
    
    # EDA Overview #
    
    p <- df %>% 
        mutate(date = as.POSIXct(paste(month , day , sep = "." ), 
                                 format = "%m.%d" )) %>%
        group_by(date, `Company response to consumer`) %>%
        summarise(number_of_complaints = n())
    
    pp <- ggplot(p, aes(x = date, y = number_of_complaints,
                        color = `Company response to consumer`)) +
        geom_line()+
        ylab("Number of complaints") +
        theme_tufte() +
        scale_x_datetime(labels= date_format("%b"), 
                         date_breaks = '1 month') + 
        theme(plot.title = element_text(size=14, face="bold", hjust = 0.5),
              legend.text = element_text(size=8),
              legend.title = element_text(size=8),
              axis.text.x = element_text(angle = 45)) +
        ggtitle("Overall Number of Complaints throughout a Year")
    
    output$linechart <- renderPlotly(pp)
    
    output$ex1 <- DT::renderDataTable(
        DT::datatable(df_new, 
                      caption = 'Table 1: This is a simple data table for the complaints.',
                      rownames = FALSE,
                      class = 'cell-border stripe',
                      colnames = pretty_headers,
                      filter = list(position = "top"),
                      options = list(
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

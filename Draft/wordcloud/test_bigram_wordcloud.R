library(shiny)
library(tidytext)
library(shinythemes)
library(tidyverse)
library(DT)
library(wordcloud)
library(readr) 
library(dplyr)
library(tm)
library(lubridate)
library(quanteda)

setwd('/Users/nikkyxiong/Desktop/IBM_Dashboard')
df <- read_csv('complaints.csv') %>% 
    filter(`Consumer complaint narrative` != 'NA') %>% 
    filter(`Company response to consumer` == 'Closed with monetary relief') %>%
    mutate(year = as.integer(substr(`Date received`, 
                                    start = 1, stop = 4))) %>%
    mutate(month = as.integer(substr(`Date received`, 
                                     start = 6, stop = 7))) %>%
    mutate(day = as.integer(substr(`Date received`, start =9 , stop = 10))) 
df$`Date received` <- ymd(df$`Date received`) 
df <- df %>% filter(year == '2017')

ui <- fluidPage(theme = shinytheme("simplex"),
                titlePanel("Word Cloud for the Narrative of Consumer Complaints in 2017"),
                # Sidebar layout with input and output definitions
                sidebarLayout(
                    # Sidebar panel for inputs
                    sidebarPanel(
                        h4('N-gram Data Table'),
                        p('This will change the number of grams the data table shows.'),
                        br(),
                        sliderInput('ngramCount', '# of Grams', min = 1, max = 6, value = 1),
                        hr(),
                        h4('N-gram Wordcloud'),
                        p('This will change the max words shown for the word cloud.'),
                        br(),
                        sliderInput('cloudCount', '# of Words', min = 20, max = 400, value = 100),
                        hr(),
                        plotOutput("wordcloud")
                    ),
                    # Main panel for displaying outputs
                    mainPanel(
                        # Output -> Data file
                        DT::dataTableOutput("contents")
                    )
                )
)

server <- function(input, output) {
    
    output$wordcloud  <- renderPlot({
            tidytext::unnest_tokens(ngram, df$`Consumer complaint narrative`, 
                                    token="ngrams", n=input$ngramCount) %>%
            count(ngram) %>%
            with(wordcloud(ngram, n, max.words=input$cloudCount, rot.per=.1, 
                           random.color=FALSE, random.order=FALSE, 
                           colors=c("#000000", "#FF1A1A")))
    })
    
    output$contents <- DT::renderDataTable({
        DT::datatable(head(tidytext::unnest_tokens(ngram, df$`Consumer complaint narrative`, 
                                                       token="ngrams", n=input$ngramCount) %>%
                               dplyr::group_by(ngram) %>%
                               dplyr::filter(!is.na(ngram)) %>%
                               dplyr::summarize(num = n()) %>%
                               dplyr::arrange(desc(num)), 100),
                      class = "stripe hover compact order-column",
                      extensions = 'Buttons',
                      caption = 'You can sort and filter by column and export to a variety of options.',
                      options = list(
                          dom = 'Bfrtip',
                          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                          pageLength = 50
                      ),
                      filter = 'top'
        )
    })
}

shinyApp(ui, server)
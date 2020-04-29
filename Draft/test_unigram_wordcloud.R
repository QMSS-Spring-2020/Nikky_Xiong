library(shiny)
library(readr) 
library(dplyr)
library(tm)
library(wordcloud)
library(quanteda)
require(RWeka)
library(stringr)
library(lubridate)

setwd('/Users/nikkyxiong/Desktop/IBM Dashboard')
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

clean_corpus <- function(reviews){
    reviews <- gsub("+", "", reviews) 
    reviews <- gsub("X", "", reviews) 
    reviews <- gsub("x", "", reviews) 
    return(reviews)
}

getTermMatrix <- function() {
    corpus_raw <- clean_corpus(df$`Consumer complaint narrative`)
    myCorpus = Corpus(VectorSource(corpus_raw))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,
                      c(stopwords("SMART")))

    myDTM = TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    
    m = as.matrix(myDTM)
    
    sort(rowSums(m), decreasing = TRUE)
}

ui <- fluidPage(
    # Application title
    titlePanel("Word Cloud for the Narrative of Consumer Complaints in 2017"),
    
    sidebarLayout(
        # Sidebar with a slider 
        sidebarPanel(
            actionButton("update", "Change"),
            hr(),
            sliderInput("freq",
                        "Minimum Frequency:",
                        min = 1000,  max = 5000, value = 200),
            sliderInput("max",
                        "Maximum Number of Words:",
                        min = 10,  max = 100,  value = 10)
        ),
        
        # Show Word Cloud
        mainPanel(
            plotOutput("plot")
        )
    )
)

server <- function(input, output) {
    # Define a reactive expression for the document term matrix
    terms <- reactive({
        # Change when the "update" button is pressed...
        input$update
        # ...but not for anything else
        isolate({
            withProgress({
                setProgress(message = "Processing corpus...")
                getTermMatrix()
            })
        })
    })
    
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
        v <- terms()
        wordcloud_rep(names(v), v, scale=c(4,0.5),
                      min.freq = input$freq, max.words=input$max,
                      colors=brewer.pal(8, "Dark2"))
    })
}

shinyApp(ui = ui, server = server)
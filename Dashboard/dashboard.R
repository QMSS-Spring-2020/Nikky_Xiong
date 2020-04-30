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
library(leaflet)


########################
# Read in Cleaned Data #
########################

df_raw <- read_csv('complaints.csv') 
complaints_test <- readRDS('complaints_sub.rds')
complaints_narrative_corp <- readRDS('complaints_narrative_corp.rds')

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

# More
complaints_test <- complaints_test %>%
    mutate(year = as.integer(substr(Date.received, start = 1, stop = 4))) %>%
    mutate(month = as.integer(substr(Date.received, start = 6, stop = 7))) %>%
    mutate(day = as.integer(substr(Date.received, start =9 , stop = 10)))
complaints_test$Date.received <- ymd(complaints_test$Date.received)
# create overall complaint level graphs
complaints_pattern <- function(x){
    if (x=="All"){
        p_ <- complaints_test %>% 
            mutate(date = as.POSIXct(paste(month , day , sep = "." )  , format = "%m.%d" )) %>%
            group_by(date) %>%
            summarise(number_of_complaints = n()) %>%
            ggplot(aes(x = date, y = number_of_complaints))+ geom_point()+ylab("Number of complaints")+
            theme_economist()+
            scale_x_datetime(labels=  date_format("%b-%d"),date_breaks = '1 month')+
            geom_smooth(lwd=1, se=FALSE,color = 'red')+
            geom_line(aes(x=date, y=SMA(number_of_complaints,10), color = 'red'))+ 
            theme(legend.position="none")+
            ggtitle(paste("Number of Complaints throughout a Year (Overall level):",simpleCap(tolower(x)), sep = ' '))
    } else{
        p_ <-  complaints_test %>% 
            filter(Company %in% x)  %>%
            mutate(date = as.POSIXct(paste(month , day , sep = "." )  , format = "%m.%d" )) %>%
            group_by(date) %>%
            summarise(number_of_complaints = n()) %>%
            ggplot(aes(x = date, y = number_of_complaints))+ geom_point()+ylab("Number of complaints")+
            theme_economist()+
            scale_x_datetime(labels=  date_format("%b-%d"),date_breaks = '1 month')+
            geom_smooth(lwd=1, se=FALSE,color = 'red')+
            theme(legend.position="none")+
            ggtitle(paste("Number of Complaints throughout a Year of selected companies"))+theme(plot.title = element_text(size=10))}
    ggplotly(p_)}
company_names <- complaints_test %>%
    select(Company)%>%
    distinct()
Top_10_comanies <- complaints_test %>%
    group_by(Company) %>%
    summarise(number_of_complaints = n())%>%
    arrange(desc(number_of_complaints))%>%
    head(10)%>%
    select(Company) # This line select the companies with most complaints
Top_10_comanies_plot <- complaints_test %>%
    filter(Company %in% Top_10_comanies$Company) %>%
    group_by(Company,Product)%>%
    summarise(number_of_complaints = n())%>%
    ungroup()%>%
    mutate(Company = factor(Company, levels=Top_10_comanies$Company))%>%
    ggplot(aes(fill=Product, y=number_of_complaints, x=Company)) + 
    geom_bar(position="stack", stat="identity")+ylab('Nunber of Complaints by Company')+coord_flip()+theme(legend.position="right")+guides(fill=guide_legend(nrow=15))
Top_10_comanies_plot <- ggplotly(Top_10_comanies_plot,width = 1500, height = 500, type = 'bar')
Top_10_product <- complaints_test %>%
    group_by(Product) %>%
    summarise(number_of_complaints = n())%>%
    arrange(desc(number_of_complaints))%>%
    head(10)%>%
    select(Product) # This line select the companies with most complaints
Top_10_product_plot <- complaints_test %>%
    filter(Product %in% Top_10_product$Product) %>%
    group_by(Product,Submitted.via)%>%
    summarise(number_of_complaints = n())%>%
    ungroup()%>%
    mutate(Product = factor(Product, levels=Top_10_product$Product))%>%
    ggplot(aes(fill=Submitted.via, y=number_of_complaints, x=Product)) + 
    geom_bar(position="stack", stat="identity")+ylab('Nunber of Complaints by Product')+coord_flip()+theme(legend.position="bottom")
Top_10_product_plot <- ggplotly(Top_10_product_plot,width = 1200, height = 500, type = 'bar')
share_of_products_ <- complaints_test %>%
    group_by(Product) %>%
    summarize(count = n()) %>%
    plot_ly(labels = ~Product, values = ~count,width = 1200, height = 500) %>%
    add_pie(hole = 0.6) %>%
    layout(title = "Share of Products",  showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),legend = list(x = 100, y = 0.5))

top_word_response <- complaints_narrative_corp %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>% 
    group_by(Company.response.to.consumer) %>% 
    top_n(10) %>% 
    ungroup() %>%
    ggplot(aes(word, tf_idf, fill = Company.response.to.consumer)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~Company.response.to.consumer, ncol = 2, scales = "free") +
    coord_flip()
top_word_response <- ggplotly(top_word_response,width = 1000, height = 1000, type = 'bar')
company_response <- complaints_test %>%
    select(Company.response.to.consumer)%>%
    distinct()
wordcloud_rep <- repeatable(wordcloud)
word_cloud <- function(max_word,freq,company_res){
    sub_set <- complaints_narrative_corp %>%
        filter(Company.response.to.consumer == company_res)
    print(wordcloud_rep(words = sub_set$word, freq = sub_set$n,
                        min.freq = freq,max.words=max_word, random.order=FALSE, rot.per=0.35, 
                        colors=brewer.pal(8, "Dark2")))                  
}
states <- 
    geojson_read( 
        x = "https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json"
        , what = "sp"
    )
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

labels <- sprintf(
    "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
    states$name, states$density
) %>% lapply(htmltools::HTML)

leaflet(states) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
    addPolygons(
        fillColor = ~pal(density),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
    addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
              position = "bottomright")

library(reticulate)
if(!("r-reticulate" %in% conda_list()$name)){ ## r-reticulate is not in conda environment
    conda_create("r-reticulate", packages = c("python", "scikit-learn", "pandas"))
}

conda_install("r-reticulate", "scikit-learn")
conda_install('r-reticulate', 'pandas')
use_condaenv("r-reticulate", required = TRUE)

#conda_remove("r-reticulate")
use_condaenv("r-reticulate", required = TRUE)
#irtualenv_create("r-reticulate")
#virtualenv_install("r-reticulate", "scikit-learn")
#virtualenv_install("r-reticulate", "pandas")
#use_virtualenv("r-reticulate", required = TRUE)
source_python('predict_narrative.py')


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
        
        tabPanel("Overall Complaint level through out a year",
                 
                 sidebarLayout(
                     sidebarPanel(
                         
                         pickerInput("level_select", "Level:",   
                                     choices = c("Overall","Company"), 
                                     selected = c("Company"),
                                     multiple = FALSE),
                         
                         pickerInput("company_select", "Company",   
                                     choices = as.character(company_names$Company), 
                                     options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                     selected = c('BANK OF AMERICA, NATIONAL ASSOCIATION'),multiple = TRUE)),
                     
                     mainPanel(
                         tabPanel('Complaint Throughout a Year',plotlyOutput("complaints_pattern_plot")))
                 )
        ),
        
        tabPanel("Shares of Complaints",
                 mainPanel(
                     tabsetPanel(
                         tabPanel('Top 10 Companies of number of complaints',plotlyOutput("Top_10_comanies_plot")),
                         tabPanel('Top 10 Products of highest number of complaints',plotlyOutput("Top_10_product_plot")),
                         tabPanel('Share of Complaints by Product',plotlyOutput("share_of_products_")))
                 )
        ),
        
        tabPanel("Company response text mining",
                 
                 sidebarLayout(
                     sidebarPanel(
                         
                         
                         sliderInput("max_word", step = 1,
                                     "Max number of words to show",
                                     min = 1,
                                     max = 500,
                                     value = 100),
                         pickerInput("response", "Company response to consumer",   
                                     choices = as.character(company_response$Company.response.to.consumer), 
                                     selected = c('Closed with monetary relief'),
                                     multiple = FALSE),
                         sliderInput("freq",
                                     "Minimum Frequency:",
                                     min = 1,  max = 1000, value = 100),
                     ),
                     
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Frequentest words by Company response", plotlyOutput("top_word_response")),
                             tabPanel("Word cloud", plotOutput("word_cloud_plot"))
                         )
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
        ),
        
        # Tab for Supervised Learning- LinearSVC Model
        tabPanel("Supervised Learning- LinearSVC",
            sidebarLayout(
                sidebarPanel(
                    
                    # Drop down to choose product
                    pickerInput("product_select", "Product:",   
                                choices = c("Mortgage","Credit Card"), # Credit Card is not working right now.
                                selected = c("Mortgage"),
                                multiple = FALSE),
                    
                    # Text box to enter the narrative
                    textInput(
                        inputId = 'narrative_input',
                        label = 'Narrative',
                        value = 'Enter narrative'
                    ),
                    
                    # This button will activate the model
                    actionButton(inputId = "clicks", "Submit")
                    
                ),
                    
                    mainPanel(
                    tabPanel('Predicted Response', textOutput(outputId = "predicted_response")))
            )
        )
    )
)

##################
#     Server     #
##################

server = function(input, output, session){
    
    # The filler value to be shown to user
    predicted_text <- reactiveValues(t = "Waiting...")
    
    # Collect the narrative into new_narrative when Submit is clicked.
    observeEvent(input$clicks, 
                 {
                     #### To enable python script to work in R #####
                     # library(reticulate)
                     #use_condaenv("r-reticulate")
                     #conda_create("r-reticulate")
                     # These libraries will need to be installed in this environment
                     # conda_install("r-reticulate", "scikit-learn")
                     # reticulate::conda_install("r-reticulate", "pandas")
                     # reticulate::conda_install("r-reticulate", "numpy")
                     # py_install("pandas")
                     # py_install("scikit-learn")
                     # use_condaenv("r-reticulate")
                     # source_python('predict_narrative.py')
                     predicted_text$t <- predict_narrative(input$narrative_input)
                 }
                 )
    ## 1 new_narrative <- eventReactive(input$clicks, {input$narrative_input})
    
    # Send the new_narrative to python model to get prediction back.
    output$predicted_response <- renderText(predicted_text$t)
    
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
    
    # update company selections
    observeEvent(input$level_select, {
        if (input$level_select=="Overall") {
            updatePickerInput(session = session, inputId = "company_select", 
                              choices = "All", selected = "All")
        }
        
        if (input$level_select=="Company") {
            updatePickerInput(session = session, inputId = "company_select", 
                              choices = as.character(company_names$Company), 
                              selected = c('BANK OF AMERICA, NATIONAL ASSOCIATION'))
        }
    }, ignoreInit = TRUE)
    
    output$complaints_pattern_plot <- renderPlotly({
        complaints_pattern(input$company_select)
    })
    output$Top_10_comanies_plot <- renderPlotly({
        Top_10_comanies_plot
    })
    output$Top_10_product_plot <- renderPlotly({
        Top_10_product_plot
    })
    output$share_of_products_ <- renderPlotly({
        share_of_products_
    })
    output$top_word_response <- renderPlotly({
        top_word_response
    })
    output$word_cloud_plot <- renderPlot({
        word_cloud(input$max_word,input$freq ,input$response)
    })
}

#######################
# Run the Application #
#######################

shinyApp(ui = ui, server = server)


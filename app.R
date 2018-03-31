# Shiny App UI
library(shiny)
library(dplyr)
library(shinythemes)
library(data.table)
library(ggplot2)
library(DT)
library(xml2)
library(rvest)
library(purrr)
library(stringi)
library(tidytext)
library(textclean)
library(stringr)

######
##UI##
######
ui <- fluidPage(
  
  shinythemes::themeSelector(),
  #theme = shinytheme('slate'),
  
  # App title 'Name_Assignment'
  titlePanel("IndrajitHaridas_NLP"),
  
  sidebarLayout(position = 'right',
                # Sidebar layout to upload file and select input
                sidebarPanel(
                  textInput('main_url', 'Enter url', value = 'https://www.cars.com/research/toyota-camry/'),
                  width = 3
                ),
                mainPanel(
                  width = 9,
                  tabsetPanel(
                    tabPanel("Instructions", uiOutput("instructions")),
                    tabPanel("Training Data Review", dataTableOutput('train_rev')),
                    tabPanel("Testing Data Review", dataTableOutput('test_rev')),
                    tabPanel('Normalization', tabsetPanel(
                      tabPanel("Training Data", column(12, dataTableOutput("train_norm"))),
                      tabPanel("Testing Data", column(12, dataTableOutput("test_norm"))))),
                    tabPanel("Data Tagging", tabsetPanel(
                      tabPanel('Training Data', column(12, dataTableOutput("train_tag"))),
                      tabPanel('Testing Data', column(12, dataTableOutput("test_tag"))))),
                    tabPanel("Sentiment Analysis", tabsetPanel(
                      tabPanel('Training Data', column(12, dataTableOutput("train_sent"))),
                      tabPanel('Testing Data', column(12, dataTableOutput("test_sent"))))),
                    
                    tabPanel("Sentiment Comparisons", tabsetPanel(
                      tabPanel('Sentiment vs Star Rating', fluidRow(
                        column(12, dataTableOutput("table1")),
                        
                        column(12, plotOutput("star_com")),
                        column(12, plotOutput("sent_com")), 
                        sidebarPanel(sliderInput("bins",
                                                 "Number of Bins of Sentiment's Histogram:",
                                                 min = 5,
                                                 max = 35,
                                                 value = 20)))),
                      tabPanel('Tags: Sentiment vs Star Rating', fluidRow(
                                column(12, dataTableOutput("table2")),
                                column(12, plotOutput("tag_hist")),
                                sidebarPanel(sliderInput("bins1",
                                                         "Number of Bins:",
                                                         min = 5,
                                                         max = 35,
                                                         value = 20))
                              
                              ))))
                    
                  )
                )
  ))

## Server Code
server <- function(input, output) {
  
  
  #####################
  ## Helper Reactives##
  #####################
  
  #####################
  ### Data Scrapping###
  #####################
  #main_url <- 'https://www.cars.com/research/toyota-camry/'
  
  get_data_train <- reactive({
    df=data.frame()
    main_url <- input$main_url#'https://www.cars.com/research/toyota-camry/'
    for (year in 2012:2016)
    {
      url <- paste(substr(main_url,1,nchar(main_url)-1),"-",year,"/","consumer-reviews" , "/", sep="")
      page <- read_html(url)
      
      n_revs <- as.numeric(html_text(html_nodes(page,'[itemprop = "reviewCount"]')))
      base_url <- paste(url,"?pg=%d&nr=10","/",sep="")
      
      map_df(1:ceiling(n_revs/10), function(i) {
        pg <- read_html(sprintf(base_url,i))
        data.frame(years = year , 
                   description  = html_text(html_nodes(pg,'[itemprop = "description"]')),
                   rating = as.numeric(html_text(html_nodes(pg,'[itemprop = "reviewRating"] [itemprop = "ratingValue"]'))),stringsAsFactors=FALSE)
      }) -> Toyota 
      
      df <- rbind(df,Toyota)
    }
    return(df)
  })
  
  get_data_test <- reactive({
    df=data.frame()
    main_url <- input$main_url#'https://www.cars.com/research/toyota-camry/'
    year = 2017
      url <- paste(substr(main_url,1,nchar(main_url)-1),"-",year,"/","consumer-reviews" , "/", sep="")
      page <- read_html(url)
      
      n_revs <- as.numeric(html_text(html_nodes(page,'[itemprop = "reviewCount"]')))
      base_url <- paste(url,"?pg=%d&nr=10","/",sep="")
      
      map_df(1:ceiling(n_revs/10), function(i) {
        pg <- read_html(sprintf(base_url,i))
        data.frame(years = year , 
                   description  = html_text(html_nodes(pg,'[itemprop = "description"]')),
                   rating = as.numeric(html_text(html_nodes(pg,'[itemprop = "reviewRating"] [itemprop = "ratingValue"]'))),stringsAsFactors=FALSE)
      }) -> Toyota 
      
      df <- rbind(df,Toyota)
    
    return(df)
  }) 
  
  
  # This reactive function will take out all the punctuations and normalize the text
  normalizer_train <- reactive({
    x1 <- get_data_train()
    for (i in 1:nrow(x1)){
    x1[i, 'Normalized_Review'] <- strip(x1[i, 'description'], char.keep = "~~", digit.remove = TRUE, apostrophe.remove = FALSE,
                                       lower.case = TRUE)
    }
    return(x1)
    })
  
  # This reactive function will take out all the punctuations and normalize the tex1t
  normalizer_test <- reactive({
    x1 <- get_data_test()
    for (i in 1:nrow(x1)){
      x1[i, 'Normalized_Review'] <- strip(x1[i, 'description'], char.keep = "~~", digit.remove = TRUE, apostrophe.remove = FALSE,
                                         lower.case = TRUE)
    }
      return(x1)
  })
  
  # This reactive function will tag the reviews for train data
  tag_train <- reactive({
    
    keywords <- c('service', 'price', 'handling', 'interior')
    x2 <- normalizer_train()
    for (i in 1:nrow(x2)){
      
      #x2[i, 'Tags'] <- ifelse(length(keywords[str_detect(x2[i, 'Normalized_Review'], keywords)]) == 0, '', keywords[str_detect(x2[i, 'Normalized_Review'], keywords)])
      x2[i, 'Tags'] <- ifelse(length(keywords[str_detect(x2[i, 'Normalized_Review'], keywords)]) == 0, '', 
                              paste(ifelse(is.na(keywords[str_detect(x2[i, 'Normalized_Review'], keywords)][1]) == FALSE,keywords[str_detect(x2[i, 'Normalized_Review'], keywords)][1], '')
                                    , ifelse(is.na(keywords[str_detect(x2[i, 'Normalized_Review'], keywords)][2]) == FALSE,keywords[str_detect(x2[i, 'Normalized_Review'], keywords)][2], '')
                                    , ifelse(is.na(keywords[str_detect(x2[i, 'Normalized_Review'], keywords)][3]) == FALSE,keywords[str_detect(x2[i, 'Normalized_Review'], keywords)][3], '')
                                    , ifelse(is.na(keywords[str_detect(x2[i, 'Normalized_Review'], keywords)][4]) == FALSE,keywords[str_detect(x2[i, 'Normalized_Review'], keywords)][4], ''), sep = ','))
    }
    return(x2)
  })
  
  # This reactive function will tag the reviews for train data
  tag_test <- reactive({
    
    keywords <- c('service', 'price', 'handling', 'interior')
    x2 <- normalizer_test()
    for (i in 1:nrow(x2)){
      
      x2[i, 'Tags'] <- ifelse(length(keywords[str_detect(x2[i, 'Normalized_Review'], keywords)]) == 0, '', keywords[str_detect(x2[i, 'Normalized_Review'], keywords)])
    }
    return(x2)
  })

  # This reactive function will evaluate sentiment of the reviews for train data
  sent_train <- reactive({
    
    x3 <- tag_train()
    #x3 <- x3['Normalized_Review', 'reviewRating']
    
    for (i in 1:nrow(x3)){
      
      tokens <- x3[i,] %>% unnest_tokens(word, Normalized_Review)
      join <- inner_join(x = tokens, y = get_sentiments("afinn")) # pull out only sentiment words
      x3[i, 'Sentiment'] <- lapply(join['score'], mean, na.rm = TRUE)
      x3[i, 'Sentiment'] <- round(x3[i, 'Sentiment'], digits = 3)
    }
    return(x3)
  })
  
  # This reactive function will evaluate sentiment of the reviews for train data
  sent_test <- reactive({
    
    x3 <- tag_test()
    #x3 <- x3['Normalized_Review', 'reviewRating']
    
    for (i in 1:nrow(x3)){
      
      tokens <- x3[i,] %>% unnest_tokens(word, Normalized_Review)
      join <- inner_join(x = tokens, y = get_sentiments("afinn")) # pull out only sentiment words
      x3[i, 'Sentiment'] <- lapply(join['score'], mean, na.rm = TRUE)
      x3[i, 'Sentiment'] <- round(x3[i, 'Sentiment'], digits = 3)
    }
    return(x3)
  })
  
  output$instructions <- renderUI(HTML("<p><ul><li> <b><b>Data_Snippet:</b></b> Gives data overview and distribution. </li>
                                       <p><li> <b><b> Network of n connections:</b></b> It simply shows first 'n' connections from the links dataset.</li></p>
                                       <p><li> <b><b>2 Hop neighbors:</b></b> This tab displays 5 '1-hops' and 10 '2-hops' corresponding to each 1-hop.</li></p>
                                       <p><li> <b><b>Centralities:</b></b> This will display 5 '1-hops' with highest centrality and respective 2-hops with top centralities. Color indicates towards the department whereas node size is the magnitude of the centrality.</li></p>
                                       <p><li> <b><b>Rank:</b></b> Rank of node when arranged from top to bottom using the key parameter, e.g. Centrality or number of emails.</li></p>
                                       <p><li> <b><b>Department wise mails:</b></b> Displays tabulated magnitude of number of intere\action between the departments. Graph shows 2-hop connections originating from the top 5 1-hops, width of the link is the magnitude. </li></p>
                                       </ul>
                                       "))
  
  
  
  # Review of train data
  output$train_rev <- renderDataTable({
    
    datatable(get_data_train())
    
  })
  
  # Review of test data
  output$test_rev <- renderDataTable({
    
    datatable(get_data_test())
    
  })
  
  # Normalization of train data
  output$train_norm <- renderDataTable({
    
    datatable(normalizer_train())
    
      
  })
  
  # Normalization of test data
  output$test_norm <- renderDataTable({
    
    datatable(normalizer_test())
  })
  
  # Tagging of train data
  output$train_tag <- renderDataTable({
    
    tag_tr <- tag_train()
    datatable(tag_tr)
  })
  
  # Tagging of test data
  output$test_tag <- renderDataTable({
    
    tag_t <- tag_test()
    datatable(tag_t)
  })
  
  # Sentiment analysis of train data
  output$train_sent <- renderDataTable({
    
    sent_tr <- sent_train()
    sent_tr <- sent_tr[,c('Normalized_Review', 'rating', 'Tags', 'Sentiment')]
    datatable(sent_tr)
  })
  
  # Sentiment analysis of test data
  output$test_sent <- renderDataTable({
  
    sent_t <- sent_test()
    sent_t <- sent_t[,c('Normalized_Review', 'rating', 'Tags', 'Sentiment')]
    datatable(sent_t)
  })
  ##########################################################
  # Sentiment vs average rating comparison
  
  # Sentiment Comparison with rating
  output$table1 <- renderDataTable({
    
    x <- sent_test()
    x <- x$Sentiment
    mx <- mean(x, na.rm = TRUE)
    
    y <- sent_test()
    y <- y$rating
    my <- mean(y, na.rm = TRUE)
    
    
    
    datatable(cbind(c('Mean Sentiment', 'Mean Rating'), c(round(mx, 3), round(my, 3))), colnames = c('Type of Mean', 'Value'))
  })
  
  # Sentiment histogram
  output$sent_com <- renderPlot({
    
    x <- sent_test()
    x <- x$Sentiment
    mx <- mean(x, na.rm = TRUE)
    
    #referring input bins in ui.r as input$bins
    bins <- seq(ifelse(is.finite(min(x)) == TRUE, min(x), -5), ifelse(is.finite(max(x)) == TRUE, max(x), 5), length.out = input$bins + 1)
    
    hist(x,
         breaks = bins,
         col = 'orange',
         border = 'white', 
         xlab = 'Sentiments', 
         main = 'Histogram of Ratings')
    
    abline(v = mx, col = "black", lwd = 2)
  })
    
  output$star_com <- renderPlot({
    
      
      
    # Histogram for ratings
    y <- sent_test()
    y <- y$rating
    
    bins <- seq(ifelse(is.finite(min(y)) == TRUE, min(y), 0), ifelse(is.finite(max(y)) == TRUE, max(y), 5), length.out = 10)
    
    hist(y,
         breaks = bins,
         col = 'pink',
         border = 'white', 
         xlab = 'Ratings', 
         main = 'Histogram of Ratings')
    
    abline(v = mean(y, na.rm = TRUE), col = "black", lwd = 2)
  })
  
    ##########################################################
    # tag-wise comparison between sentiments and ratings
     output$table2 <- renderDataTable({
    
       x <- sent_train()
       #x <- x[,c('Tags', 'Sentiment')]
       
       ms <- list()
       mp <- list()
       mh <- list()
       mi <- list()
       
       for (i in 1:nrow(x)){
         
         ms[i] <- ifelse(grep('service', x[i,'Tags']) == 1, x[i, 'Sentiment'],NULL)
         mp[i] <- ifelse(grep('price', x[i,'Tags']) == 1, x[i, 'Sentiment'],NULL)
         mh[i] <- ifelse(grep('handling', x[i,'Tags']) == 1, x[i, 'Sentiment'],NULL)
         mi[i] <- ifelse(grep('interior', x[i,'Tags']) == 1, x[i, 'Sentiment'],NULL)
         
       }
       
       ms <- mean(unlist(ms))
       mp <- mean(unlist(mp))
       mh <- mean(unlist(mh))
       mi <- mean(unlist(mi))
       
       x1 <- datatable(cbind(c('service', 'price', 'handling', 'interior'), c(round(ms, 2), round(mp, 2)), round(mh, 2), round(mi, 2)), colnames = c('Tag', 'Mean Sentiment'))
       
       
       # mx <- mean(x, na.rm = TRUE)
       # 
       # y <- sent_test()
       # y <- y$rating
       # my <- mean(y, na.rm = TRUE)
    
    
        datatable(x1)
       #datatable(cbind(c('Mean Sentiment', 'Mean Rating'), c(round(mx, 3), round(my, 3))), colnames = c('Type of Mean', 'Value'))
     })
    
  
}

shinyApp(ui, server)
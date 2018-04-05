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
library(Rstem)
library(quanteda)
library(ggplot2)
library(nnet)
library(caret)
library(stringr)
library(tidyr)
library(tidyverse)


######
##UI##
######
ui <- fluidPage(
  #shinythemes::themeSelector(),
  theme = shinytheme('paper'),
  
  # App title 'Name_Assignment'
  titlePanel("Reviews Analysis"),
  
  sidebarLayout(
    position = 'right',
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
          tabPanel("Testing Data", column(12, dataTableOutput("test_norm")))
        )),
        tabPanel("Data Tagging", tabsetPanel(
          tabPanel('Training Data', column(12, dataTableOutput("train_tag"))),
          tabPanel('Testing Data', column(12, dataTableOutput("test_tag")))
        )),
        tabPanel("Sentiment Analysis", tabsetPanel(
          tabPanel('Training Data', column(12, dataTableOutput("train_sent"))),
          tabPanel('Testing Data', column(12, dataTableOutput("test_sent")))
        )),
        
        tabPanel("Sentiment Comparisons", tabsetPanel(
          tabPanel(
            'Sentiment vs Star Rating',
            fluidRow(
              column(12, dataTableOutput("table1")),
              
              column(12, plotOutput("star_com")),
              column(12, plotOutput("sent_com")),
              sidebarPanel(
                sliderInput(
                  "bins",
                  "Number of Bins of sentiment's Histogram:",
                  min = 5,
                  max = 35,
                  value = 20
                )
              )
            )
          ),
          tabPanel(
            'Tags: Sentiment vs Star Rating',
            fluidRow(
              column(12, dataTableOutput("table2")),
              column(12, plotOutput("sent_box"))
              
            )
          )
        )),
        
        tabPanel("Model Training", fluidRow(
                                  column(12, verbatimTextOutput('model_summary')),
                                  column(12, plotOutput('dcn_bndry')))),
        
        tabPanel("Model Testing", fluidRow(
          column(12, verbatimTextOutput('model_test')),
          column(12, dataTableOutput('pred_values')))),
        
        tabPanel("TF-IDF", fluidRow(
              sidebarPanel(selectInput("tag", "Tag:", choices = c('service', 'price', 'handling', 'interior'))),
              
              column(12, dataTableOutput("tfidf_train")),
              
              column(12, plotOutput("tfidf_train_viz"))
              )
            )
          
      )
    )
  )
)


## Server Code
server <- function(input, output) {

  #####################
  ### Data Scrapping###
  #####################
  #main_url <- 'https://www.cars.com/research/toyota-camry/'
  
  #### Reactive function to scrpe the train data
  get_data_train <- reactive({
    df = data.frame()
    main_url <-
      input$main_url#'https://www.cars.com/research/toyota-camry/'
    for (year in 2012:2016)
    {
      url <-
        paste(substr(main_url, 1, nchar(main_url) - 1),
              "-",
              year,
              "/",
              "consumer-reviews" ,
              "/",
              sep = "")
      page <- read_html(url)
      
      n_revs <-
        as.numeric(html_text(html_nodes(page, '[itemprop = "reviewCount"]')))
      base_url <- paste(url, "?pg=%d&nr=10", "/", sep = "")
      
      map_df(1:ceiling(n_revs / 10), function(i) {
        pg <- read_html(sprintf(base_url, i))
        data.frame(
          years = year ,
          description  = html_text(html_nodes(pg, '[itemprop = "description"]')),
          rating = as.numeric(html_text(
            html_nodes(
              pg,
              '[itemprop = "reviewRating"] [itemprop = "ratingValue"]'
            )
          )),
          stringsAsFactors = FALSE
        )
      }) -> Toyota
      
      df <- rbind(df, Toyota)
    }
    return(unique(df))
  })
  
  #### Reactive function to scrpe the test data
  get_data_test <- reactive({
    df = data.frame()
    main_url <-
      input$main_url#'https://www.cars.com/research/toyota-camry/'
    year = 2017
    url <-
      paste(substr(main_url, 1, nchar(main_url) - 1),
            "-",
            year,
            "/",
            "consumer-reviews" ,
            "/",
            sep = "")
    page <- read_html(url)
    
    n_revs <-
      as.numeric(html_text(html_nodes(page, '[itemprop = "reviewCount"]')))
    base_url <- paste(url, "?pg=%d&nr=10", "/", sep = "")
    
    map_df(1:ceiling(n_revs / 10), function(i) {
      pg <- read_html(sprintf(base_url, i))
      data.frame(
        years = year ,
        description  = html_text(html_nodes(pg, '[itemprop = "description"]')),
        rating = as.numeric(html_text(
          html_nodes(
            pg,
            '[itemprop = "reviewRating"] [itemprop = "ratingValue"]'
          )
        )),
        stringsAsFactors = FALSE
      )
    }) -> Toyota
    
    df <- rbind(df, Toyota)
    
    return(unique(df))
  })
  
  
  # This reactive function will take out all the punctuations and normalize the text
  normalizer_train <- reactive({
    x1 <- get_data_train()                    # Get data
    for (i in 1:nrow(x1)) {                   # loop to iterate through each row and normalize and remove punctuations
      x1[i, 'normalized_review'] <-
        strip(
          x1[i, 'description'],
          char.keep = "~~",
          digit.remove = TRUE,
          apostrophe.remove = FALSE,
          lower.case = TRUE
        )
    }
    return(x1)                                # Return the dataframe
  })
  
  # This reactive function will take out all the punctuations and normalize the text in test data
  normalizer_test <- reactive({
    x1 <- get_data_test()                     # Get data
    for (i in 1:nrow(x1)) {
      x1[i, 'normalized_review'] <-
        strip(
          x1[i, 'description'],
          char.keep = "~~",
          digit.remove = TRUE,
          apostrophe.remove = FALSE,
          lower.case = TRUE
        )
    }
    return(x1)                              # Return the dataframe
  })
  
  # This reactive function will tag the reviews for train data
  tag_train <- reactive({
    keywords <- c('service', 'price', 'handling', 'interior')
    x2 <- normalizer_train()
    
    # this loop will iterate through every row and identify the keyword and paste them seperated by comma ','
    for (i in 1:nrow(x2)) {
      x2[i, 'Tags'] <-
        ifelse(length(keywords[str_detect(x2[i, 'normalized_review'], keywords)]) == 0,
               '',
               paste(
                 ifelse(is.na(keywords[str_detect(x2[i, 'normalized_review'], keywords)][1]) == FALSE, keywords[str_detect(x2[i, 'normalized_review'], keywords)][1], '')
                 ,
                 ifelse(is.na(keywords[str_detect(x2[i, 'normalized_review'], keywords)][2]) == FALSE, keywords[str_detect(x2[i, 'normalized_review'], keywords)][2], '')
                 ,
                 ifelse(is.na(keywords[str_detect(x2[i, 'normalized_review'], keywords)][3]) == FALSE, keywords[str_detect(x2[i, 'normalized_review'], keywords)][3], '')
                 ,
                 ifelse(is.na(keywords[str_detect(x2[i, 'normalized_review'], keywords)][4]) == FALSE, keywords[str_detect(x2[i, 'normalized_review'], keywords)][4], ''),
                 sep = ','
               ))
      
      # this line will remove extra comma aquired while concatinating keywords
      x2[i, 'Tags'] <-
        gsub("^,*|(?<=,),|,*$", "", x2[i, 'Tags'], perl = T)
    }
    return(x2)
  })
  
  # This reactive function will tag the reviews for train data
  # Similar to the above function, just for test dataset
  tag_test <- reactive({
    keywords <- c('service', 'price', 'handling', 'interior')
    x2 <- normalizer_test()
    # this loop will iterate through every row and identify the keyword and paste them seperated by comma ','
    for (i in 1:nrow(x2)) {
      #x2[i, 'Tags'] <- ifelse(length(keywords[str_detect(x2[i, 'normalized_review'], keywords)]) == 0, '', keywords[str_detect(x2[i, 'normalized_review'], keywords)])
      x2[i, 'Tags'] <-
        ifelse(length(keywords[str_detect(x2[i, 'normalized_review'], keywords)]) == 0,
               '',
               paste(
                 ifelse(is.na(keywords[str_detect(x2[i, 'normalized_review'], keywords)][1]) == FALSE, keywords[str_detect(x2[i, 'normalized_review'], keywords)][1], '')
                 ,
                 ifelse(is.na(keywords[str_detect(x2[i, 'normalized_review'], keywords)][2]) == FALSE, keywords[str_detect(x2[i, 'normalized_review'], keywords)][2], '')
                 ,
                 ifelse(is.na(keywords[str_detect(x2[i, 'normalized_review'], keywords)][3]) == FALSE, keywords[str_detect(x2[i, 'normalized_review'], keywords)][3], '')
                 ,
                 ifelse(is.na(keywords[str_detect(x2[i, 'normalized_review'], keywords)][4]) == FALSE, keywords[str_detect(x2[i, 'normalized_review'], keywords)][4], ''),
                 sep = ','
               ))
      # this line will remove extra comma aquired while concatinating keywords
      x2[i, 'Tags'] <-
        gsub("^,*|(?<=,),|,*$", "", x2[i, 'Tags'], perl = T)
    }
    return(x2)
  })
  
  # This reactive function will evaluate sentiment of the reviews for train data
  sent_train <- reactive({
    x3 <- tag_train()

    # Loop to iterate throught the column, each step is explained below
    for (i in 1:nrow(x3)) {
      tokens <-
        x3[i, ] %>% unnest_tokens(word, normalized_review) # Tokenize
      tokens <-
        data.frame(wordStem(tokens$word, language = 'english'))    # Stemming
      colnames(tokens) <- 'word'
      tokens <-
        tokens %>% anti_join(stop_words)    # Remove stope-words
      join <-
        inner_join(x = tokens, y = get_sentiments("afinn")) # pull out only sentiment words
      x3[i, 'sentiment'] <-
        lapply(join['score'], mean, na.rm = TRUE)
      x3[i, 'sentiment'] <- round(x3[i, 'sentiment'], digits = 3)
    }
    return(x3)
  })
  
  # This reactive function will evaluate sentiment of the reviews for train data
  sent_test <- reactive({
    x3 <- tag_test()
    #x3 <- x3['normalized_review', 'reviewRating']
    
    for (i in 1:nrow(x3)) {
      tokens <-
        x3[i, ] %>% unnest_tokens(word, normalized_review) # Tokenize
      tokens <-
        data.frame(wordStem(tokens$word, language = 'english'))    # Stemming
      colnames(tokens) <- 'word'
      tokens <-
        tokens %>% anti_join(stop_words)    # Remove stope-words
      join <-
        inner_join(x = tokens, y = get_sentiments("afinn")) # pull out only sentiment words
      x3[i, 'sentiment'] <-
        lapply(join['score'], mean, na.rm = TRUE)
      x3[i, 'sentiment'] <- round(x3[i, 'sentiment'], digits = 3)
    }
    return(x3)
  })
  
  # Reactive function trains SVM on training data
  model <- reactive({
    
    # Data prep for fitting the model
    df_train <- sent_train()
    df_nb <- df_train[,c('rating', 'sentiment')]
    
    # Remove NaN's from data
    df_nb <- na.omit(df_nb)
    
    # Fitting the model
    model2 <- glm(rating ~ sentiment, data = df_nb)
    
    return(model2)
    
  })
  
  output$instructions <-
    renderUI(
      HTML(
        "<p><ul><li> <b><b>Training and Testing Data Reviews:</b></b> Gives data overview and distribution. </li>
        <p><li> <b><b> Normalization:</b></b> We removed all the punctuations and lower cased all the words, it is reflected in normalized_review caloumn in Normalization tab.</li></p>
        <p><li> <b><b>Keywords Tagging:</b></b> Given keywrods wer tagged after normalization and stemming.</li></p>
        <p><li> <b><b>Sentiment Analysis:</b></b> Sentiment analysis was done after stemming and stop words removal. Lexicon used was afinn score</li></p>
        <p><li> <b><b>Sentiment Comparison:</b></b> Sentiments and ratings were compared. Objective was to identify relationship, data was not scaled for the same.</li></p>
        <p><li> <b><b>Model:</b></b> We implemented multinomial logistic regression Traing data accuracy was 65.88% whereas testing data accuracy was 79.71% </li></p>
        <p><li> <b><b>Duplicate Reviews:</b></b> We have removed the duplicate reviews, hence datasetsize may vary as compared to others. </li></p>
        </ul>
        "
      )
      )
  
  
  
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
    datatable(normalizer_train()) %>% formatStyle('normalized_review',
      backgroundColor = 'lightgrey')
    
    
  })
  
  # Normalization of test data
  output$test_norm <- renderDataTable({
    datatable(normalizer_test()) %>% formatStyle('normalized_review',
                                                backgroundColor = 'lightgrey')
  })
  
  # Tagging of train data
  output$train_tag <- renderDataTable({
    tag_tr <- tag_train()
    datatable(tag_tr) %>% formatStyle('Tags',
                                      backgroundColor = 'lightgrey')
  })
  
  # Tagging of test data
  output$test_tag <- renderDataTable({
    tag_te <- tag_test()
    datatable(tag_te) %>% formatStyle('Tags',
                                     backgroundColor = 'lightgrey')
  })
  
  # sentiment analysis of train data
  output$train_sent <- renderDataTable({
    sent_tr <- sent_train()
    sent_tr <-
      sent_tr[, c('normalized_review', 'rating', 'Tags', 'sentiment')]
    
    # Returning datatable with formatting
    datatable(sent_tr, class = 'cell-border stripe') %>% formatStyle('sentiment',
                                      background = styleColorBar(range(sent_tr['rating']), 'lightblue'),
                                      backgroundSize = '98% 88%',
                                      backgroundRepeat = 'no-repeat',
                                      backgroundPosition = 'center',
                                      color = 'black')
  })
  
  # sentiment analysis of test data
  output$test_sent <- renderDataTable({
    sent_t <- sent_test()
    sent_t <-
      sent_t[, c('normalized_review', 'rating', 'Tags', 'sentiment')]
    
    # Returning datatable with formatting
    datatable(sent_t, class = 'cell-border stripe') %>% formatStyle('sentiment',
                                      background = styleColorBar(range(sent_t['rating']), 'lightblue'),
                                      backgroundSize = '98% 88%',
                                      backgroundRepeat = 'no-repeat',
                                      backgroundPosition = 'center',
                                      color = 'black')
  })
  ##########################################################
  # sentiment vs average rating comparison
  
  # sentiment Comparison with rating- overall
  output$table1 <- renderDataTable({
    x <- sent_test()
    x <- x$sentiment
    mx <- mean(x, na.rm = TRUE)       # Overall mean sentiment
    sdx <- sd(x, na.rm = TRUE)        # Overall sentiment standard deviation for comparison
    
    y <- sent_test()
    y <- y$rating
    my <- mean(y, na.rm = TRUE)       # Overall mean rating
    sdy <- sd(y, na.rm = TRUE)        # Overall rating standard deviation for comparison
    
    rng <- range(mx-1,mx,sdx,my,my, my+1)
    
    # Creating dataframe (datatable) for display in shiny
    dtdf <- datatable(cbind(c('Mean sentiment', 'Std. Deviation sentiment', 'Mean Rating', 'Std. Deviation Rating'), (c(round(mx, 3), round(sdx, 3), round(my, 3), round(sdy, 3)))), colnames = c('Type of Mean', 'Value'))

    })
  
  # sentiment histogram
  output$sent_com <- renderPlot({
    x <- sent_test()
    x <- x$sentiment
    mx <- mean(x, na.rm = TRUE)
    
    #creating bins for histogram
    bins <-
      seq(
        ifelse(is.finite(min(x)) == TRUE, min(x),-5),
        ifelse(is.finite(max(x)) == TRUE, max(x), 5),
        length.out = input$bins + 1
      )
    
    hist(
      x,
      breaks = bins,
      col = 'orange',
      border = 'white',
      xlab = 'sentiments',
      main = 'Histogram of sentiments'
    )
    
    # Line for the mean
    abline(v = mx, col = "black", lwd = 2)
  })
  
  output$star_com <- renderPlot({
    # Histogram for ratings
    y <- sent_test()
    y <- y$rating
    
    # Creating bins
    bins <-
      seq(
        ifelse(is.finite(min(y)) == TRUE, min(y), 0),
        ifelse(is.finite(max(y)) == TRUE, max(y), 5),
        length.out = 10
      )
    
    hist(
      y,
      breaks = bins,
      col = 'pink',
      border = 'white',
      xlab = 'Ratings',
      main = 'Histogram of Ratings'
    )
    
    # Line for the mean
    abline(v = mean(y, na.rm = TRUE),
           col = "black",
           lwd = 2)
  })
  
  # tag-wise comparison between sentiments and ratings
  output$table2 <- renderDataTable({
    
    # Get data with sentiments populated
    x <- sent_train()
   
    # Create empty list for all the keywords
    # This scheme will be followed here onwards.
    ms <- list()  # list for service sentiment
    mp <- list()  # list for price sentiment
    mh <- list()  # list for handling sentiment
    mi <- list()  # list for interior sentiment
    mms <- list()  # list for service rating
    mmp <- list()  # list for price rating
    mmh <- list()  # list for handling rating
    mmi <- list()  # list for interior rating
    
    for (i in 1:nrow(x)) {
      ## Gathering sentiments for each tag
      if (length(grep('service', x[i, 'Tags'])) != 0) {
        ms[i] <- x[i, 'sentiment']
      }
      if (length(grep('price', x[i, 'Tags'])) != 0) {
        mp[i] <- x[i, 'sentiment']
      }
      if (length(grep('handling', x[i, 'Tags'])) != 0) {
        mh[i] <- x[i, 'sentiment']
      }
      if (length(grep('interior', x[i, 'Tags'])) != 0) {
        mi[i] <- x[i, 'sentiment']
      }
      
      ## Gathering ratings for each tag
      if (length(grep('service', x[i, 'Tags'])) != 0) {
        mms[i] <- x[i, 'rating']
      }
      if (length(grep('price', x[i, 'Tags'])) != 0) {
        mmp[i] <- x[i, 'rating']
      }
      if (length(grep('handling', x[i, 'Tags'])) != 0) {
        mmh[i] <- x[i, 'rating']
      }
      if (length(grep('interior', x[i, 'Tags'])) != 0) {
        mmi[i] <- x[i, 'rating']
      }
      
    }
    
    ## Calculating mean sentiments
    ms1 <- mean(unlist(ms), na.rm = TRUE)
    mp1 <- mean(unlist(mp), na.rm = TRUE)
    mh1 <- mean(unlist(mh), na.rm = TRUE)
    mi1 <- mean(unlist(mi), na.rm = TRUE)
    
    ## Calculating std deviation sentiments
    ms2 <- sd(unlist(ms), na.rm = TRUE)
    mp2 <- sd(unlist(mp), na.rm = TRUE)
    mh2 <- sd(unlist(mh), na.rm = TRUE)
    mi2 <- sd(unlist(mi), na.rm = TRUE)
    
    ## Calculating mean ratings
    mms1 <- mean(unlist(mms), na.rm = TRUE)
    mmp1 <- mean(unlist(mmp), na.rm = TRUE)
    mmh1 <- mean(unlist(mmh), na.rm = TRUE)
    mmi1 <- mean(unlist(mmi), na.rm = TRUE)
    
    ## Calculating std dev ratings
    mms2 <- sd(unlist(mms), na.rm = TRUE)
    mmp2 <- sd(unlist(mmp), na.rm = TRUE)
    mmh2 <- sd(unlist(mmh), na.rm = TRUE)
    mmi2 <- sd(unlist(mmi), na.rm = TRUE)
    
    # Creating datatable for display in shiny
    datatable(cbind(
      c('Overall', 'service', 'price', 'handling', 'interior'),
      c(round(mean(x$sentiment, na.rm = TRUE), 3), round(sd(x$sentiment, na.rm = TRUE), 3), round(mean(x$rating, na.rm = TRUE), 3), round(sd(x$rating, na.rm = TRUE), 3)),
      c(round(ms1, 3), round(mp1, 3), round(mh1, 3), round(mi1, 3)),
      c(round(ms2, 3), round(mp2, 3), round(mh2, 3), round(mi2, 3)),
      c(round(mms1, 3), round(mmp1, 3), round(mmh1, 3), round(mmi1, 3)),
      c(round(mms2, 3), round(mmp2, 3), round(mmh2, 3), round(mmi2, 3))
    ),
    colnames = c('Tag', 'Mean sentiment', 'Std. Deviation sentiment', 'Mean Rating', 'Std. Deviation rating'))
    
    
  })
  
  # Box plot for comparison
  output$sent_box <- renderPlot({

    ### List of values for each tag
    x <- sent_train()
    #x <- x[,c('Tags', 'sentiment')]

    ms <- list()
    mp <- list()
    mh <- list()
    mi <- list()
    mms <- list()
    mmp <- list()
    mmh <- list()
    mmi <- list()

    for (i in 1:nrow(x)) {
      ## Gathering sentiments for each tag
      if (length(grep('service', x[i, 'Tags'])) != 0) {
        ms[i] <- x[i, 'sentiment']
      }
      if (length(grep('price', x[i, 'Tags'])) != 0) {
        mp[i] <- x[i, 'sentiment']
      }
      if (length(grep('handling', x[i, 'Tags'])) != 0) {
        mh[i] <- x[i, 'sentiment']
      }
      if (length(grep('interior', x[i, 'Tags'])) != 0) {
        mi[i] <- x[i, 'sentiment']
      }

      ## Gathering ratings for each tag
      if (length(grep('service', x[i, 'Tags'])) != 0) {
        mms[i] <- x[i, 'rating']
      }
      if (length(grep('price', x[i, 'Tags'])) != 0) {
        mmp[i] <- x[i, 'rating']
      }
      if (length(grep('handling', x[i, 'Tags'])) != 0) {
        mmh[i] <- x[i, 'rating']
      }
      if (length(grep('interior', x[i, 'Tags'])) != 0) {
        mmi[i] <- x[i, 'rating']
      }

    }

    sentiment_Service <- data.frame(group = "Mean Service sentiment", value = unlist(ms))
    sentiment_price <- data.frame(group = "Mean price sentiment", value = unlist(mp))
    sentiment_handling <- data.frame(group = "Mean handling sentiment", value = unlist(mh))
    sentiment_interior <- data.frame(group = "Mean interior sentiment", value = unlist(mi))

    sentiment_Service <- na.omit(sentiment_Service)
    sentiment_price <- na.omit(sentiment_price)
    sentiment_handling <- na.omit(sentiment_handling)
    sentiment_interior <- na.omit(sentiment_interior)

    Rating_Service <- data.frame(group = "Mean Service rating", value = unlist(mms))
    Rating_price <- data.frame(group = "Mean price rating", value = unlist(mmp))
    Rating_handling <- data.frame(group = "Mean handling rating", value = unlist(mmh))
    Rating_interior <- data.frame(group = "Mean interior rating", value = unlist(mmi))

    Rating_Service <- na.omit(Rating_Service)
    Rating_price <- na.omit(Rating_price)
    Rating_handling <- na.omit(Rating_handling)
    Rating_interior <- na.omit(Rating_interior)


    plot_data <- rbind(sentiment_Service, Rating_Service, sentiment_price, Rating_price, sentiment_handling, Rating_handling, sentiment_interior, Rating_interior)

    #Plots
    ggplot(plot_data, aes(x=group, y=value, fill=group)) + geom_boxplot()

  })
#################################################################  
  #### Multinomial Logistic Regression
  output$model_summary <- renderPrint({
    
    # Data Prep
    df_nb <- sent_train()
    df_nb <- df_nb[,c('rating', 'sentiment')]
    df_nb <- na.omit(df_nb)
    df_nb$rating <- as.factor(df_nb$rating)
    
    # Fitting the model
    model1 <- multinom(rating ~ sentiment, data = df_nb)#,family=binomial(link="logit"))
    new1 <- data.frame(sentiment = df_nb$sentiment)
    res1 <- predict(model1, newdata =  new1)
    #res1 <- round(res1)
    
    # factorizing
    y1 <- as.factor(res1)
    x <- as.factor(df_nb$rating)
    
    # Creating confusion matrix to display predictions and evaluation metrics
    u = union(y1, x)
    t = table(factor(y1, u), factor(x, u))
    confusionMatrix(t)
    
  })
  
  ## model_test
  output$model_test <- renderPrint({
    
    # Data Prep
    df_nb <- sent_train()
    test <- sent_test()
    
    df_nb <- df_nb[,c('rating', 'sentiment')]
    df_nb <- na.omit(df_nb)
    df_nb$rating <- as.factor(df_nb$rating)
    
    # Fitting the model
    model1 <- multinom(rating ~ sentiment, data = df_nb)#,family=binomial(link="logit"))
    new1 <- data.frame(sentiment = test$sentiment)
    res1 <- predict(model1, newdata =  new1)
    #res1 <- round(res1)
    
    # factorizing
    y1 <- as.factor(res1)
    x <- as.factor(test$rating)
    
    # Creating confusion matrix to display predictions and evaluation metrics
    u = union(y1, x)
    t = table(factor(y1, u), factor(x, u))
    confusionMatrix(t)
    
  })
  
  output$pred_values <- renderDataTable({
    
    # Data Prep
    df_nb <- sent_train()
    test <- sent_test()
    
    df_nb <- df_nb[,c('rating', 'sentiment')]
    df_nb <- na.omit(df_nb)
    df_nb$rating <- as.factor(df_nb$rating)
    
    # Fitting the model
    model1 <- multinom(rating ~ sentiment, data = df_nb)#,family=binomial(link="logit"))
    new1 <- data.frame(sentiment = test$sentiment)
    res1 <- predict(model1, newdata =  new1)
    
    test$Prediction <- res1
    
    datatable(test)
  })
  
 
  
  ##############################################
  ##TF-IDF
  
  output$tfidf_train <- renderDataTable({
    
    # Get data
    myTable <- sent_train()
    
    # Calculating TF-IDF
    myTable <- myTable %>%
      mutate(Tags = strsplit(as.character(Tags), ",")) %>% # Spliting tags
      unnest(Tags) %>%
      filter(!is.na(Tags)) %>%                            # filtering out na's
      unnest_tokens(word, normalized_review) %>%          # tokenizing
      filter(!word %in% stop_words$word,                  # removing the stop words
             str_detect(word, "^[a-z']+$")) %>%
      count(Tags, word, sort = TRUE) %>% ungroup() %>%
      bind_tf_idf(word,Tags,n)                            # This function calculates TF-IDF
    
    myTable <- filter(myTable, Tags == input$tag)         # Filter the input tag
    myTable <- myTable[order(myTable$tf_idf, decreasing = TRUE),] # Orderingin descending order of TF-IDF
    datatable(myTable)
  
   })
  
  ##TFIDF plot output
  output$tfidf_train_viz <- renderPlot({
    
    # Get data
    myTable <- sent_train()
    
    # Creating dataframe for words, tags and tf-idf as earlier done above
    myTable <- myTable %>%
      mutate(Tags = strsplit(as.character(Tags), ",")) %>% 
      unnest(Tags) %>%
      filter(!is.na(Tags)) %>%
      unnest_tokens(word, normalized_review) %>%
      filter(!word %in% stop_words$word,
             str_detect(word, "^[a-z']+$")) %>%
      count(Tags, word, sort = TRUE) %>% ungroup() %>%
      bind_tf_idf(word,Tags,n)
    
    myTable <- filter(myTable, Tags == input$tag)
    x <- myTable[order(myTable$tf_idf, decreasing = TRUE),]
    x <- top_n(x, 10)   # Get top 10

    #Plot
    theme_set(theme_light())

    # Draw plot
    ggplot(x, aes(x=word, y=tf_idf)) + 
      geom_bar(stat="identity", width=.5, fill="tomato3") + 
      labs(title="Ordered Bar Chart", 
           subtitle="TF-IDF for top 10 words!") + 
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
   
  })
  
}

shinyApp(ui, server)
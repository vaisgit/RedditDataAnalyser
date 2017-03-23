

shinyServer(
  
  function(input, output) {
    library(rjson)
    library("devtools")
    library(gtools) # if problems calling library, install.packages("gtools", dependencies = T)
    library(qdap) # qualitative data analysis package (it masks %>%)
    library(tm) # framework for text mining; it loads NLP package
    library(SnowballC);   # wordStem is masked from SnowballC
    library(stringr)
    library(dplyr)
    library(tidyr)
    library(tidytext)
    library(ggplot2)
    library(RColorBrewer)
    library(wordcloud)
    library(reshape2)
    library(RedditExtractoR)
    library(BBmisc)
    library(RTextTools)
    library(e1071)
    
    ntext <- eventReactive(input$submit, {
 
      
      querry <- input$search
      
      querry_edited = gsub(" ","_", querry)
      
      reddit_links <- reddit_urls(search_terms= querry_edited,page_threshold = 1)
      str(reddit_links)
      
      d=gsub('http:','https:',reddit_links$URL[1])
      reddit_thread <- reddit_content(URL=d)
      str(reddit_thread)
      
      af <-as.data.frame(reddit_thread$comment, stringsAsFactors=FALSE)
      
      colnames(af) = c("data")
   #   af$data <- tolower(af$data)
      af$data <- tm::removeNumbers(af$data)
      af$data <- str_replace_all(af$data, "  ", "") # replace double spaces with single space
      af$data <- str_replace_all(af$data, pattern = "[[:punct:]]", " ")
      
      af$data <- tm::removeWords(x = af$data ,stopwords(kind = "SMART"))
      View(af)
      
      ul <- unlist(strsplit(as.character(af$data), "\\s+|[[:punct:]]"))
      my<-as.data.frame(ul, stringsAsFactors=FALSE)
      my$Index<-seq.int(nrow(my))
      colnames(my) <- c("data","Index")
      
      my <- my[c("Index","data")]
      View(my)
      sent <- my %>%
        inner_join(get_sentiments("bing"),by = c("data" = "word")) %>%
        count(data, sentiment) %>%
        spread(sentiment, n, fill = 0) %>%
        mutate(sentiment = positive - negative)
      View(sent)
      
      bing_word_counts <- my["data"] %>%
        inner_join(get_sentiments("bing"),by = c("data" = "word")) %>%
        count(data,sentiment,sort=TRUE) %>%
        ungroup()
      
      View(bing_word_counts)
      
      sum(sent$sentiment)
      t=sum(bing_word_counts$n) 
      p=sum(bing_word_counts$n[which(bing_word_counts$sentiment=='positive')])
      n=sum(bing_word_counts$n[which(bing_word_counts$sentiment=='negative')])
      
      p= p/t * 100
      n= n/t*100
      
      
      output$result <- renderPlot({
        
        bing_word_counts %>%
          group_by(sentiment) %>%
          top_n(10) %>%
          mutate(word = reorder(data, n)) %>%
          ggplot(aes(word, n, fill = sentiment)) +
          geom_bar(stat = "identity", show.legend = FALSE) +
          #  annotate("text", label=p ,x=10,y=10) + 
          facet_wrap(~sentiment, scales = "free_y") +
          labs(y = sprintf("Contribution to sentiment \n Positive : %s\n Negative : %s",p,n),x = NULL) +
          coord_flip()
      })
      
      output$result2 <- renderPlot({
        
        my %>%
          inner_join(get_sentiments("bing"), by =c("data" = "word")) %>%
          count(data, sentiment, sort = TRUE) %>%
          acast(data ~ sentiment, value.var = "n", fill = 0) %>%
          comparison.cloud(colors = c("#F8766D", "#00BFC4"),max.words = 100)  
        
      })
      
    })
    
    
    output$text1 <- renderText({ 
      ntext()
      paste("Selected Query : ", input$search)
    })
    
    
    
  
  }
)
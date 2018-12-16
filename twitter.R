
#recalling packages from library
library(twitteR)
library(ROAuth)
library(RCurl)
library(tm)
library(httr)
library(curl)
library(SnowballC)
library(wordcloud)
library(reshape)
library(ggplot2)
library(dplyr)
library(tidytext)
library(topicmodels)
library(ldatuning)

#setting API connection 
api_key <- "Rw3RnZbfHalmdDAYTMqQ6YSc2"
api_secret <-"UOnERcMNQ0sk3ObugtetYOtWygwGhuSMCYmxMRGiG1lE1nvuUL"
access_token <- "39695155-iSYZqudk0DfOo0BBJsZofye6pNwxGfx76eQSp02YV"
access_token_secret <-"ad0Hff0rqVPDTUtPOd011pn4rnf43EvZZz8ZZSrKeCp4c"

setup_twitter_oauth(api_key,api_secret,access_token, access_token_secret)

tweets <- searchTwitter('#MPElection2018', n=1500, lang="en", since=NULL, until=NULL, retryOnRateLimit=10)


tweets.df <- twListToDF(tweets)

a<-tweets.df$text

#To remove non-ASCII Characters

aas<- unlist(strsplit(a, split=", "))
nonAscIDX<- grep("aas", iconv(aas, "latin1", "ASCII", sub="aas"))
aa<- aas[ - nonAscIDX]

myCorpus<-Corpus(VectorSource(aa))

# to convert to lowercase 
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ",
 x))})

 myCorpus<- tm_map(myCorpus,toSpace,"[^[:graph:]]")
myCorpus<-tm_map(myCorpus,toSpace,"/")

myCorpus<-tm_map(myCorpus,toSpace,"@")

myCorpus<-tm_map(myCorpus,toSpace,"\\|")

 myCorpus <- tm_map(myCorpus, content_transformer(tolower))

#To remove punctuation 

myCorpus <- tm_map(myCorpus, removePunctuation)

#To remove numbers 

myCorpus <- tm_map(myCorpus, removeNumbers)

# to remove URL 

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

#To remove stopwords and certain other words 
mystopwords<-c(stopwords("english"),"NRC","sukmaattack","airwesalerts","newslaundry", "read", "iamtssudhirs", "incisive", "report", "on","sukmaattack", "sukmaattackhoping" )

myCorpus <- tm_map(myCorpus, removeWords, mystopwords)

# To stem document 
myCorpus <- tm_map(myCorpus, stemDocument)

# To strip white space 
myCorpus <- tm_map(myCorpus, stripWhitespace)


# to convert to matrix 
dtm<-DocumentTermMatrix(myCorpus)

m<-as.matrix(dtm)

n<-m[ !rowSums(m[,colnames(m)[(3:ncol(m))]]==0)==ncol(m)-2, ]

# to find out frequent words 

findFreqTerms(dtm,lowfreq=30)

# for drawing word cloud 


set.seed(123)
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wordcloud(names(freq), freq, min.freq=10, colors=brewer.pal(6, "Dark2"))

# For sensitivity analysis 

hu.liu.pos<-scan('p.txt',what='character')

hu.liu.neg<-scan('n.txt',what='character')

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  {
    require(plyr);
    require(stringr);
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      sentence = gsub('[^A-z ]','', sentence)
      sentence = tolower(sentence);
      word.list = str_split(sentence, '\\s+');
      words = unlist(word.list);
      pos.matches = match(words, pos.words);
      neg.matches = match(words, neg.words);
      pos.matches = !is.na(pos.matches);
      neg.matches = !is.na(neg.matches);
      score = sum(pos.matches) - sum(neg.matches);
      return(score);
    }, pos.words, neg.words, .progress=.progress );
    scores.df = data.frame(score=scores, text=sentences);
    return(scores.df);
  }
  
  result<-score.sentiment(tweets.df$text,hu.liu.pos,hu.liu.neg)
  
  result$score
  
  hist(result$score)
  
  
  #to convert non-tidy to tidy format 
  
  terms<-Terms(dtm)
  
  ap_td<-tidy(dtm)
  
  ap_sentiments <- ap_td %>%
    inner_join(get_sentiments("bing"), by = c(term = "word"))
  
  ap_sentiments %>%
    count(sentiment, term, wt = count) %>%
    ungroup() %>%
    filter(n >= 1) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(term = reorder(term, n)) %>%
    ggplot(aes(term, n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    ylab("Contribution to sentiment") +
    coord_flip()
  
  #Number of topics code 
  
  
  
  
  result <- FindTopicsNumber(
    n,
    topics = seq(from = 2, to = 10, by = 1),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 77),
    mc.cores = 2L,
    verbose = TRUE
  )
  
  FindTopicsNumber_plot(result)
  
  ap_lda <- LDA(n, k = 2, control = list(seed = 1234))
  
  ap_topics <- tidy(ap_lda, matrix = "beta")
  
  ap_top_terms <- ap_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  
  ap_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
  
  
  


#### importing libraries ####
library(base64enc)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggraph)
library(httpuv)
library(httr)
library(igraph)
library(openssl)
library(plyr)
library(RCurl)
library(ROAuth)
library(rtweet)
library(sentimentr)
library(SnowballC)
library(stringr)
library(syuzhet)
library(textclean)
library(textmineR)
library(tidyr)
library(tidytext)
library(tidyverse)
library(tm)
library(topicmodels)
library(twitteR)
library(widyr)
library(wordcloud)


#### setting the working directory ####
setwd("/Users/Lily/Library/Mobile Documents/com~apple~CloudDocs/Trinity College Dublin/BU7147 Social Media Analysis/Group Assignment/Social-Media-Analytics")

#### initializing keys ####
bearer_token<-"#######"
consumer_key<-"#######"
consumer_secret<-"#######"
access_token<-"#######"
access_token_secret<-"#######"

setup_twitter_oauth(consumer_key=consumer_key,
                    consumer_secret=consumer_secret,
                    access_token=access_token,
                    access_secret=access_token_secret)


#### Getting Tweets about Samsung ####
stweets1 <- searchTwitter("#samsungs22 -filter:retweets", n=10000, lang="en", retryOnRateLimit = 100)
sdf1 <- twListToDF(stweets1)
View(sdf1)

stweets2 <- searchTwitter("Samsung + S22 -@ShopeeID -@_arllee -filter:retweets", n=10000, lang="en", retryOnRateLimit = 100)
sdf2 <- twListToDF(stweets2)
View(sdf2)

sd2_users <- sdf2 %>%
  distinct(screenName, id) %>%
  group_by(screenName) %>%
  summarise("number of tweets" = n())
View(sd2_users)
sum(sd2_users$`number of tweets`)

ad_tweets <- c("whitestonedome", "FromKorea5", "dome_glass", "Whitestone_DE", "whitestone_UK", "jp_whitestone", "Whitestone__FR", "WhitestoneJapan", "WhitestoneEU")

sdf2_filter <- sdf2 %>%
  filter(!screenName %in% ad_tweets)
View(sdf2_filter)

sd2_users2 <- sdf2_filter %>%
  distinct(screenName, id) %>%
  group_by(screenName) %>%
  summarise("number of tweets" = n())
View(sd2_users2)

samsung_df <- rbind(sdf1, sdf2_filter)
View(samsung_df)
write.csv(samsung_df,"Samsung_df.csv")

##### Samsung: Use this dataframe from now on so we work on same data #####
samsung_df <- read.csv("Samsung_df.csv")
View(samsung_df)


#### Getting Tweets about Apple ####
atweets1 <- searchTwitter("iPhone + 13 -filter:retweets", n=10000, lang="en", retryOnRateLimit = 100)
adf1 <- twListToDF(atweets1)
View(adf1)

adf_users <- adf1 %>%
  distinct(screenName, id) %>%
  group_by(screenName) %>%
  summarise("number of tweets" = n())
View(adf_users)

adf_replies <- adf1 %>%
  distinct(replyToSN, id) %>%
  group_by(replyToSN) %>%
  summarise("number of replies to user" = n())
View(adf_replies)

apple_ad_tweets <- c("whitestonedome", "FromKorea5", "domeglassapple")

adf_filter <- adf1 %>%
  filter(!screenName %in% apple_ad_tweets)
View(adf_filter)

atweets2 <- searchTwitter("#iphone13 -filter:retweets", n=10000, lang="en", retryOnRateLimit = 100)
adf2 <- twListToDF(atweets2)
View(adf2)

adf_users2 <- adf2 %>%
  distinct(screenName, id) %>%
  group_by(screenName) %>%
  summarise("number of tweets" = n())
View(adf_users2)

adf_replies2 <- adf2 %>%
  distinct(replyToSN, id) %>%
  group_by(replyToSN) %>%
  summarise("number of replies to user" = n())
View(adf_replies)

apple_df <- rbind(adf_filter, adf2)

#there is another competition which did not need retweets or replying to someone specifically 
#people copied the text to take part, therefore, the word "treasure" and "win" were exorbitant in our analysis
#therefore we excluded all rows that had this text
apple_df <- apple_df %>%
  filter(!grepl("Join the event to win an iPhone 13!",text))  %>%
  filter(!grepl("rolex",text))

write.csv(apple_df,"Apple_df.csv")

##### Apple: Use this dataframe from now on so we work on same data #####

################################################# 

setwd("/Users/mohit/Desktop/Trinity College/BA- Semester 2/Social Media Analytics/Group Assignment/Marie Code")
apple_df <- read.csv("Apple_df.csv")
samsung_df <- read.csv("Samsung_df.csv")
###Samsung####
##### statistics about tweets #####

stats_s <- samsung_df %>%
  mutate(date = substr(created,1,10))
stats_s <- aggregate(cbind(count_users = screenName, count_tweets = id) ~ date, 
                     data = stats_s, 
                     FUN = function(x){n_distinct(x)})
stats_s <- stats_s %>%
  mutate(avg_tweets_per_user = count_tweets / count_users) %>%
  mutate(weekday = weekdays(as.Date(date)))

##### Preprocessing Tweets #####
stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')

Samsung_df <- samsung_df$text %>%
  str_to_lower() %>% #all text to lower case
  replace_contraction() %>% #replaces contractions to longer form
  replace_internet_slang() %>% #replaces common internet slang
  replace_hash(replacement = "") %>% #removes hashtags
  replace_word_elongation() %>% #removes word elongation, e.g. "heeeeey" to "hey"
  replace_emoji() %>% #replaces emojis with the word form 
  replace_emoji_identifier() %>% #replaces emoji identifiers to word form 
  replace_non_ascii() %>% #replaces common non-ASCII characters. 
  str_squish() %>% #reduces repeated whitespace inside a string
  str_trim() %>% #removes whitespace from start and end of string
  {gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",.)} %>% #remove RT (retweets)
  {gsub("http[^[:blank:]]+","",.)} %>% #remove links that start with http
  {gsub("@\\u+","",.)} %>% #remove names 
  {gsub('@\\w+', '', .)} %>% # remove at people
  {gsub("[[:punct:]]"," ",.)} %>%#remove punctuation
  {gsub("[^[:alnum:]]"," ",.)}%>%#remove punctuation
  stringr::str_replace_all(stopwords_regex, '') %>% #remove stop words
  unique()#remove duplicates

##### sentiment analysis #####
mysentiment<- get_nrc_sentiment(Samsung_df)
sentimentscores<- data.frame(colSums(mysentiment[,]))

###### getting sentiment scores ######
names(sentimentscores)<-"score"
sentimentscores<-cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores)<-NULL

###### plotting sentiment scores ######
ggplot(data=sentimentscores,aes(x=sentiment,y=score))+
  geom_bar(aes(fill=sentiment),stat="identity")+ 
  theme(legend.position = "none")+
  xlab("sentiment") +ylab("score")+ ggtitle("total sentiment score based on tweets about Samsung")

##### Sentimentr score ######
sentimentr_samsung <- sentiment_by(Samsung_df, by=NULL)
ggplot(data=sentimentr_samsung,aes(x=element_id,y=ave_sentiment))+
  geom_line()

sentimentr_html_s <- sentimentr_samsung %>%
  sentiment_by(by=NULL)%>%
  highlight()
extract_sentiment_terms(Samsung_df)

##### building wordcloud #####
pal<- brewer.pal(8,"Dark2")
stopwords_regex <- paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex <- paste0('\\b', stopwords_regex, '\\b')

word_s <- samsung_df$text %>%
  str_to_lower() %>% #all text to lower case
  replace_contraction() %>% #replaces contractions to longer form
  replace_internet_slang() %>% #replaces common internet slang
  replace_hash(replacement = "") %>% #removes hashtags
  replace_word_elongation() %>% #removes word elongation, e.g. "heeeeey" to "hey"
  #replace_emoji() %>% #replaces emojis with the word form #we eliminate this from word preprocessing because we don't want emoji words to be within the word cloud
  #replace_emoji_identifier() %>% #replaces emoji identifiers to word form #we eliminate this from word preprocessing because we don't want emoji words to be within the word cloud
  #replace_non_ascii() %>% #replaces common non-ASCII characters. #we eliminate this from word preprocessing because we don't want emoji words to be within the word cloud
  str_squish() %>% #reduces repeated whitespace inside a string
  str_trim() %>% #removes whitespace from start and end of string
  {gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",.)} %>% #remove RT (retweets)
  {gsub("http[^[:blank:]]+","",.)} %>% #remove links that start with http
  {gsub("@\\u+","",.)} %>% #remove names 
  {gsub('@\\w+', '', .)} %>% # remove at people
  {gsub("[[:punct:]]"," ",.)} %>%#remove punctuation
  {gsub("[^[:alnum:]]"," ",.)}%>%#remove punctuation
  removeNumbers() %>%
  stringr::str_replace_all(stopwords_regex, '') %>% #remove stop words
  unique() #remove duplicates

wordcloud(word_s, min.freq = , max.words = 500, width=1000,
          height=1000, random.order = FALSE, color= pal)

##### Creating a plot of the positive words most frequently used #####
#ngram = 1
bg_df_s <- data.frame(word_s)

s_bigram <- bg_df_s %>%
  unnest_tokens(output=bigrams, input=word_s, token="words", format= "text")

s_bigram_counted <- s_bigram %>% dplyr::count(bigrams, sort = TRUE)
s_bigram_counted$sentiment <- get_sentiment(s_bigram_counted$bigrams)

ggplot(s_bigram_counted, aes(x=bigrams, y=n))+
  geom_col()
s_bigram_with_sentiment <- s_bigram_counted %>%
  mutate(weightage = sentiment*n) %>%
  arrange(desc(weightage)) %>%
  top_n(25)

ggplot(s_bigram_with_sentiment, aes(x=bigrams, y=weightage))+
  geom_col()

#bigram, n=2

s_bigram2 <- bg_df_s %>%
  unnest_tokens(output=bigrams, input=word_s, format= "text", token = "ngrams", n=2)

s_bigram2_counted <- s_bigram2 %>% dplyr::count(bigrams, sort = TRUE)

s_bigrams2_separated <- s_bigram2_counted %>%
  separate(bigrams, c("word1", "word2"), sep = " ")

s_bigrams2_counts <- s_bigrams2_separated %>% 
  dplyr::count(word1, word2, sort = TRUE)

s_bigrams2_united <- s_bigrams2_counts %>%
  unite(bigrams, word1, word2, sep = " ")

AFINN <- get_sentiments("afinn")

s_not_words <- s_bigrams2_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  dplyr::count(word2, value, sort = TRUE)
View(s_bigrams2_separated)
s_not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"") 


s_words_per_tweet <- bg_df_s %>%
  dplyr::mutate(tweet = row_number()) %>%
  unnest_tokens(output=bigrams, input=word_s, format= "text", token = "ngrams", n=1)

s_word_pairs <- s_words_per_tweet %>%
  pairwise_count(bigrams, tweet, sort = TRUE)

s_word_pairs %>%
  filter(item1=="samsung")

s_word_cors <- s_words_per_tweet %>%
  group_by(bigrams) %>%
  filter(n() >= 20) %>%
  pairwise_cor(bigrams, tweet, sort = TRUE)

View(s_word_cors)

s_word_cors %>%
  filter(item1 %in% c("battery", "financing", "security", "configurable")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

s_word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

##### Creating a plot of the negative words most frequently used #####


s_bigram_with_neg_sentiment <- s_bigram_counted %>%
  mutate(weightage = sentiment*n) %>%
  arrange(desc(weightage)) %>%
  tail(25)

ggplot(s_bigram_with_neg_sentiment, aes(x=bigrams, y=weightage))+
  geom_col()

##### LDA Topic Modelling #####
options(mc.cores = 10)
tm_parLapply_engine(parallel::mclapply)  # mclapply gets the number of cores from global options

# Create corpus object
s_corpus <- Corpus(VectorSource(bg_df_s))  

# Remove English stop words.
s_corpus <- tm_map(s_corpus, removeWords, stopwords("en"))  

# Remove numbers.
s_corpus <- tm_map(s_corpus, removeNumbers)

# Stem the words.
s_corpus <- tm_map(s_corpus, stemDocument)

# Remove the stems associated with our search terms!
s_corpus <- tm_map(s_corpus, removeWords, c("galaxi", "samsung", "galaxy", "ultra", "\"samsung", "\"galaxy", "iphone", "iphon", "\"iphon", "appl", "pro", "max"))

s_doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(s_corpus)))
s_dtm <- DocumentTermMatrix(s_corpus)

# Now for some topics
SEED = sample(1:1000000, 1)  # Pick a random seed for replication
k = 5 

# This might take a minute!
s_models <- list(
  CTM       = CTM(s_dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))),
  VEM       = LDA(s_dtm, k = k, control = list(seed = SEED)),
  VEM_Fixed = LDA(s_dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
  Gibbs     = LDA(s_dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
                                                                 thin = 100,    iter = 1000))
)

# There you have it. Models now holds 4 topics. See the topicmodels API documentation for details

# Top 10 terms of each topic for each model
# Do you see any themes you can label to these "topics" (lists of words)?
lapply(s_models, terms, 10)

s_assignments <- sapply(s_models, topics) 

s_assignments

#### Apple ####
##### Statistics about dataset #####
stats_a <- apple_df %>%
  mutate(date = substr(created,1,10))
stats_a <- aggregate(cbind(count_users = screenName, count_tweets = id) ~ date, 
                     data = stats_a, 
                     FUN = function(x){n_distinct(x)})
stats_a <- stats_a %>%
  mutate(avg_tweets_per_user = count_tweets / count_users) %>%
  mutate(weekday = weekdays(as.Date(date)))
stats_a

View(apple_df)

##### Preprocessing Tweets #####
#replace emojis with sentiment
Apple_df <- apple_df$text %>%
  str_to_lower() %>% #all text to lower case
  replace_contraction() %>% #replaces contractions to longer form
  replace_internet_slang() %>% #replaces common internet slang
  replace_hash(replacement = "") %>% #removes hashtags
  replace_word_elongation() %>% #removes word elongation, e.g. "heeeeey" to "hey"
  replace_emoji() %>% #replaces emojis with the word form 
  replace_emoji_identifier() %>% #replaces emoji identifiers to word form 
  replace_non_ascii() %>% #replaces common non-ASCII characters. 
  str_squish() %>% #reduces repeated whitespace inside a string
  str_trim() %>% #removes whitespace from start and end of string
  {gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",.)} %>% #remove RT (retweets)
  {gsub("http[^[:blank:]]+","",.)} %>% #remove links that start with http
  {gsub("@\\u+","",.)} %>% #remove names 
  {gsub('@\\w+', '', .)} %>% # remove at people
  {gsub("[[:punct:]]"," ",.)} %>%#remove punctuation
  {gsub("[^[:alnum:]]"," ",.)}%>%#remove punctuation
  stringr::str_replace_all(stopwords_regex, '') %>% #remove stop words
  unique() #remove duplicates

##### sentiment analysis #####
mysentiment_apple<- get_nrc_sentiment(Apple_df) 
sentimentscores_apple<- data.frame(colSums(mysentiment_apple[,]))

###### getting sentiment scores ######
names(sentimentscores_apple)<-"score"
sentimentscores_apple<-cbind("sentiment"=rownames(sentimentscores_apple),sentimentscores_apple)
rownames(sentimentscores_apple)<-NULL

###### plotting sentiment scores ######
ggplot(data=sentimentscores_apple,aes(x=sentiment,y=score))+
  geom_bar(aes(fill=sentiment),stat="identity")+ 
  theme(legend.position = "none")+
  xlab("sentiment") +ylab("score")+ ggtitle("total sentiment score based on tweets about Apple")

###### Sentimentr score #######
sentimentr_apple <- sentiment_by(Apple_df, by=NULL)
ggplot(data=sentimentr_apple,aes(x=element_id,y=ave_sentiment))+
  geom_line()

sentimentr_html_a <- sentimentr_apple %>%
  sentiment_by(by=NULL)%>%
  highlight()

sentiments_a <- get_sentences(Apple_df) %>%
  extract_sentiment_terms()

##### building wordcloud #####
pal<- brewer.pal(8,"Dark2")

word_a <- apple_df$text %>%
  str_to_lower() %>% #all text to lower case
  replace_contraction() %>% #replaces contractions to longer form
  replace_internet_slang() %>% #replaces common internet slang
  replace_hash(replacement = "") %>% #removes hashtags
  replace_word_elongation() %>% #removes word elongation, e.g. "heeeeey" to "hey"
  #replace_emoji() %>% #replaces emojis with the word form #we eliminate this from word preprocessing because we don't want emoji words to be within the word cloud
  #replace_emoji_identifier() %>% #replaces emoji identifiers to word form #we eliminate this from word preprocessing because we don't want emoji words to be within the word cloud
  #replace_non_ascii() %>% #replaces common non-ASCII characters. #we eliminate this from word preprocessing because we don't want emoji words to be within the word cloud
  str_squish() %>% #reduces repeated whitespace inside a string
  str_trim() %>% #removes whitespace from start and end of string
  {gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",.)} %>% #remove RT (retweets)
  {gsub("http[^[:blank:]]+","",.)} %>% #remove links that start with http
  {gsub("@\\u+","",.)} %>% #remove names 
  {gsub('@\\w+', '', .)} %>% # remove at people
  {gsub("[[:punct:]]"," ",.)} %>%#remove punctuation
  {gsub("[^[:alnum:]]"," ",.)}%>%#remove punctuation
  removeNumbers() %>%
  stringr::str_replace_all(stopwords_regex, '')%>% #remove stop words
  unique() #remove duplicates

wordcloud(word_a, min.freq = , max.words = 1000, width=1000,
          height=1000, random.order = FALSE, color= pal)

##### Creating a plot of the positive words most frequently used #####

bg_df_a <- data.frame(word_a)

a_bigram <- bg_df_a %>%
  unnest_tokens(output=bigrams, input=word_a, token="words", format= "text")

a_bigram_counted <- a_bigram %>% dplyr::count(bigrams, sort = TRUE)
a_bigram_counted$sentiment <- get_sentiment(a_bigram_counted$bigrams)

ggplot(a_bigram_counted, aes(x=bigrams, y=n))+
  geom_col()
a_bigram_with_sentiment <- a_bigram_counted %>%
  mutate(weightage = sentiment*n) %>%
  arrange(desc(weightage)) %>%
  top_n(25)

ggplot(a_bigram_with_sentiment, aes(x=bigrams, y=weightage))+
  geom_col()
View(apple_df)

##### Creating a plot of the negative words most frequently used #####

a_bigram_with_neg_sentiment <- a_bigram_counted %>%
  mutate(weightage = sentiment*n) %>%
  arrange(desc(weightage)) %>%
  tail(25)

ggplot(a_bigram_with_neg_sentiment, aes(x=bigrams, y=weightage))+
  geom_col()

## bigrams, n=2

a_bigram2 <- bg_df_a %>%
  unnest_tokens(output=bigrams, input=word_a, format= "text", token = "ngrams", n=2)

a_bigram2_counted <- a_bigram2 %>% dplyr::count(bigrams, sort = TRUE)

a_bigrams2_separated <- a_bigram2_counted %>%
  separate(bigrams, c("word1", "word2"), sep = " ")

a_bigrams2_counts <- a_bigrams2_separated %>% 
  dplyr::count(word1, word2, sort = TRUE)

a_bigrams2_united <- a_bigrams2_counts %>%
  unite(bigrams, word1, word2, sep = " ")

AFINN <- get_sentiments("afinn")

a_not_words <- a_bigrams2_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  dplyr::count(word2, value, sort = TRUE)
View(a_bigrams2_separated)
a_not_words %>%                                      ##########NOT WORKING FOR ME - Graph is blank
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"") 

a_words_per_tweet <- bg_df_a %>%
  dplyr::mutate(tweet = row_number()) %>%
  unnest_tokens(output=bigrams, input=word_a, format= "text", token = "ngrams", n=1)

a_word_pairs <- a_words_per_tweet %>%
  pairwise_count(bigrams, tweet, sort = TRUE)

a_word_pairs %>%
  filter(item1=="apple")

a_word_cors <- a_words_per_tweet %>%
  group_by(bigrams) %>%
  filter(n() >= 20) %>%
  pairwise_cor(bigrams, tweet, sort = TRUE)

View(a_word_cors)

a_word_cors %>%
  filter(item1 %in% c("battery", "update", "charge", "configurable")) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

a_word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


##### LDA Topic Modelling #####
options(mc.cores = 10)
tm_parLapply_engine(parallel::mclapply)  # mclapply gets the number of cores from global options

# Create corpus object
a_corpus <- Corpus(VectorSource(bg_df_a))  

# Remove English stop words.
a_corpus <- tm_map(a_corpus, removeWords, stopwords("en"))  

# Remove numbers.
a_corpus <- tm_map(a_corpus, removeNumbers)

# Stem the words.
a_corpus <- tm_map(a_corpus, stemDocument)

# Remove the stems associated with our search terms!
a_corpus <- tm_map(a_corpus, removeWords, c("galaxi", "samsung", "galaxy", "ultra", "\"samsung", "\"galaxy", "iphone", "iphon", "\"iphon", "appl", "pro", "max"))

a_doc.lengths <- rowSums(as.matrix(DocumentTermMatrix(a_corpus)))
a_dtm <- DocumentTermMatrix(a_corpus)

# Now for some topics
SEED = sample(1:1000000, 1)  # Pick a random seed for replication
k = 5 

# This might take a minute!
a_models <- list(
  CTM       = CTM(a_dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))),
  VEM       = LDA(a_dtm, k = k, control = list(seed = SEED)),
  VEM_Fixed = LDA(a_dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
  Gibbs     = LDA(a_dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
                                                                 thin = 100,    iter = 1000))
)

# Top 10 terms of each topic for each model
# Do you see any themes you can label to these "topics" (lists of words)?
lapply(a_models, terms, 10)

a_assignments <- sapply(a_models, topics) 

a_assignments

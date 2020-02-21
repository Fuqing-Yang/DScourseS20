library(xml2)
library(rvest)
web <- read_html("https://en.wikipedia.org/wiki/10-second_barrier") 
web
result <- web %>%
  html_nodes("#mw-content-text > div > table:nth-child(19)") %>%
  html_table(fill=TRUE)
table<-result[[1]]
table




#Question 4

library(twitteR)
library(tm)
requestURL = "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = "yzsG9mzthMbetyxVUWjnwkSFf"
consumerSecret = "8PlxlA11RU2DVav5i0K9Ouuh3IvHVk2kfePSe0TsHtYcNVW9t4"

accessToken = "1183838112486711298-BoipFrOKmCVvRPWc4oeysUeWfOTASo"
accessSecret = "9OXPD7EbRrY4kTmLxox9hey7y6kUufll3oc8BsLew69pq"

setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    accessToken,
                    accessSecret)


econ <- searchTwitter('economics', 
                      geocode='35.203601,-97.442209,100mi',  
                      n=1000, retryOnRateLimit=1)
econ.df <- twListToDF(econ) 
head(econ.df$text)
econ.df2 <- gsub("http.*","",econ.df$text)
econ.df2 <- gsub("https.*","",econ.df2)
econ.df2 <- gsub("#.*","",econ.df2)
econ.df2 <- gsub("@.*","",econ.df2)
econ.df2 <- gsub("RT","",econ.df2)
View(econ.df2)
data <- data.frame(econ = as.character(econ.df2), 
                   stringsAsFactors = FALSE)
data <- data %>% 
  unnest_tokens(word, econ)
library(tidytext)
library(ggplot2)
library(forcats)
library(textdata)

sentiments

sentiment <- get_sentiments("nrc")

data <- inner_join(data, sentiment, by = "word")

ggplot(data = data, aes(x = fct_rev(fct_infreq(sentiment)))) +
  geom_bar() +
  coord_flip()
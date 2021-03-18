
# options -----------------------------------------------------------------

library(sentimentr)
library(tidytext)
library(tidyverse)
library(R.utils)
library(qdapDictionaries)
library(scales)
options(max.print = 50)


# loading data ------------------------------------------------------------

set.seed(1)

con <- file("./en_US/en_US.twitter.txt", "r")
twitter <- readLines(con,500000)
close(con)

coin_flip <- sample(c(0,1,2), size=500000, replace = TRUE, prob = c(0.6,0.2,0.2))
twitter_training <- twitter[coin_flip==0]
twitter_validation <- twitter[coin_flip==1]
twitter_testing <- twitter[coin_flip==2]

con <- file("./en_US/en_US.blogs.txt", "r")
twitter <- readLines(con,500000)
close(con)

coin_flip <- sample(c(0,1,2), size=500000, replace = TRUE, prob = c(0.6,0.2,0.2))
blog_training <- twitter[coin_flip==0]
blog_validation <- twitter[coin_flip==1]
blog_testing <- twitter[coin_flip==2]

con <- file("./en_US/en_US.news.txt", "r")
twitter <- readLines(con,500000)
close(con)

coin_flip <- sample(c(0,1,2), size=500000, replace = TRUE, prob = c(0.6,0.2,0.2))
news_training <- twitter[coin_flip==0]
news_validation <- twitter[coin_flip==1]
news_testing <- twitter[coin_flip==2]


# cleaning data -----------------------------------------------------------

## correcting the â
twitter_corrected <- str_remove_all(twitter_training,"_+|â–\u0081/â|â‚¬|â˜…|â„¢|â˜¹|â˜€|âš½|â\u009d•|â\u009d¤|â\u009d|â€œ|â€\u009d|â™¥|â˜¼|â™|â˜º|â™|â€§|â˜‡|âœŠ|âœŒðŸ’¤ðŸŒ™|âœ|âœŒ|âˆž")
twitter_corrected <- str_replace_all(twitter_corrected,c("â€™"="'","â€"="-","âˆ†"="A"))

blog_corrected <- str_remove_all(blog_training,"_+|â–\u0081/â|â‚¬|â˜…|â„¢|â˜¹|â˜€|âš½|â\u009d•|â\u009d¤|â\u009d|â€œ|â€\u009d|â™¥|â˜¼|â™|â˜º|â™|â€§|â˜‡|âœŠ|âœŒðŸ’¤ðŸŒ™|âœ|âœŒ|âˆž")
blog_corrected <- str_replace_all(blog_corrected,c("â€™"="'","â€"="-","âˆ†"="A"))

news_corrected <- str_remove_all(news_training,"_+|â–\u0081/â|â‚¬|â˜…|â„¢|â˜¹|â˜€|âš½|â\u009d•|â\u009d¤|â\u009d|â€œ|â€\u009d|â™¥|â˜¼|â™|â˜º|â™|â€§|â˜‡|âœŠ|âœŒðŸ’¤ðŸŒ™|âœ|âœŒ|âˆž")
news_corrected <- str_replace_all(news_corrected,c("â€™"="'","â€"="-","âˆ†"="A"))

## correcting contractions
twitter_corrected <- str_replace_all(twitter_corrected,
                                     c("[Ii]'ll"="i will","[Yy]ou'll"="you will","[Hh]e'll"="he will","[Ss]he'll"="she will","[Ii]t'll"="it will","[Ww]e'll"="we will","[Tt]hey'll"="they will"))
twitter_corrected <- str_replace_all(twitter_corrected,
                                     c("[Dd]on't"="do not","[Dd]oesn't"="does not","[Hh]aven't"="have not", "[Hh]asn't"="has not"))
twitter_corrected <- str_replace_all(twitter_corrected,
                                     c("[Ii]'m"="i am","[Yy]ou're"="you are","[Hh]e's"="he is", "[Ss]he's"="she is", "[Ww]e're" = "we are", "[Tt]hey're" = "they are"))
twitter_corrected <- str_replace_all(twitter_corrected,c("^[Uu] " = "you ", " [Uu] " = " you "," [Uu]$" = " you"))
twitter_corrected <- str_replace_all(twitter_corrected, c("^[Rr] you " = "are you ", " you [Rr] " = " you are ", " you [Rr]$" = " you are"))


blog_corrected <- str_replace_all(blog_corrected,
                                     c("[Ii]'ll"="i will","[Yy]ou'll"="you will","[Hh]e'll"="he will","[Ss]he'll"="she will","[Ii]t'll"="it will","[Ww]e'll"="we will","[Tt]hey'll"="they will"))
blog_corrected <- str_replace_all(blog_corrected,
                                     c("[Dd]on't"="do not","[Dd]oesn't"="does not","[Hh]aven't"="have not", "[Hh]asn't"="has not"))
blog_corrected <- str_replace_all(blog_corrected,
                                     c("[Ii]'m"="i am","[Yy]ou're"="you are","[Hh]e's"="he is", "[Ss]he's"="she is", "[Ww]e're" = "we are", "[Tt]hey're" = "they are"))
blog_corrected <- str_replace_all(blog_corrected,c("^[Uu] " = "you ", " [Uu] " = " you "," [Uu]$" = " you"))
blog_corrected <- str_replace_all(blog_corrected, c("^[Rr] you " = "are you ", " you [Rr] " = " you are ", " you [Rr]$" = " you are"))


news_corrected <- str_replace_all(news_corrected,
                                     c("[Ii]'ll"="i will","[Yy]ou'll"="you will","[Hh]e'll"="he will","[Ss]he'll"="she will","[Ii]t'll"="it will","[Ww]e'll"="we will","[Tt]hey'll"="they will"))
news_corrected <- str_replace_all(news_corrected,
                                     c("[Dd]on't"="do not","[Dd]oesn't"="does not","[Hh]aven't"="have not", "[Hh]asn't"="has not"))
news_corrected <- str_replace_all(news_corrected,
                                     c("[Ii]'m"="i am","[Yy]ou're"="you are","[Hh]e's"="he is", "[Ss]he's"="she is", "[Ww]e're" = "we are", "[Tt]hey're" = "they are"))
news_corrected <- str_replace_all(news_corrected,c("^[Uu] " = "you ", " [Uu] " = " you "," [Uu]$" = " you"))
news_corrected <- str_replace_all(news_corrected, c("^[Rr] you " = "are you ", " you [Rr] " = " you are ", " you [Rr]$" = " you are"))

## remove profanity

profanity_list <- str_remove_all(lexicon::profanity_alvarez, "^\\*|\\*$")
profanity_list <- str_replace_all(profanity_list, c("\\!"="\\\\!", "\\+"="\\\\+", "\\$"="\\\\$", "\\."="\\\\.","\\("="\\\\(","\\-"="\\\\-"))
profanity_regex <- str_c(" ",profanity_list," ", collapse = "|")

twitter_corrected <- str_remove_all(twitter_corrected,profanity_regex)

blog_corrected <- str_remove_all(blog_corrected,profanity_regex)

news_corrected <- str_remove_all(news_corrected,profanity_regex)

## tibbles

tibble_twitter_train <- tibble(line = 1:length(twitter_corrected), text = twitter_training)
tibble_blog_train <- tibble(line = 1:length(blog_corrected), text = blog_training)
tibble_news_train <- tibble(line = 1:length(news_corrected), text = news_training)

# tidyiing data -----------------------------------------------------------

twitter_train_count_freq <- tibble_twitter_train %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% lexicon::profanity_alvarez) %>%
  count(word, sort = TRUE) %>%
  mutate(freqs = n/sum(n)) %>%
  mutate(word = reorder(word,n))

blog_train_count_freq <- tibble_blog_train %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% lexicon::profanity_alvarez) %>%
  count(word, sort = TRUE) %>%
  mutate(freqs = n/sum(n)) %>%
  mutate(word = reorder(word,n))

news_train_count_freq <- tibble_news_train %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% lexicon::profanity_alvarez) %>%
  count(word, sort = TRUE) %>%
  mutate(freqs = n/sum(n)) %>%
  mutate(word = reorder(word,n))

unified_dataset <- bind_rows(mutate(blog_train_count_freq, source = "Blog"),
                                    mutate(twitter_train_count_freq, source = "Twitter"),
                                    mutate(news_train_count_freq, source = "News"))%>%
  select(-n)%>%
  spread(source, freqs)%>%
  gather(source, freqs, Blog:News) %>%
  arrange(desc(freqs))


ggplot(unified_dataset, aes(x = freqs, y = Twitter, color = abs(Twitter - freqs)))+
  geom_abline(color = "gray40", lty = 2)+
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3)+
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5)+
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels = percent_format())+
  scale_color_gradient(limits = c(0,0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~source, ncol = 2) +
  theme(legend.position="none")+
  labs(y = "Twitter", x = NULL)


# 2-gram model ------------------------------------------------------------

twitter_train_bigram <- tibble_twitter_train %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) 

twitter_train_bigram_freqs <- twitter_train_bigram %>%
  separate(bigram, c("word1","word2"), sep =" ") %>%
  count(word1,word2,sort=TRUE) %>%
  mutate(freqs = n/sum(n)) %>%
  mutate(word1 = reorder(word1,n))

for (i in 1:10) {
  c <- i
  n_c <- sum(twitter_train_bigram_freqs$n==i)
  n_c_1 <- sum(twitter_train_bigram_freqs$n==i+1)
  c_star <- ((i+1) * n_c_1) / n_c
  freq_new <- c_star/sum(twitter_train_bigram_freqs$n)
  for (j in i:i) {
    twitter_train_bigram_freqs$freqs[twitter_train_bigram_freqs$n == j] <- freq_new
  }
}

predict_bigram <- function(word1) {
  x <- word1
  z <- twitter_train_bigram_freqs %>%
    filter(word1 == x) %>%
    arrange(desc(n))
  z$word2[1]  
}

predict_bigram("in")

# 3-gram model ------------------------------------------------------------

twitter_train_trigram <- tibble_twitter_train %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) 

twitter_train_trigram_freqs <- twitter_train_trigram %>%
  separate(trigram, c("word1","word2","word3"), sep =" ") %>%
  count(word1,word2,word3,sort=TRUE) %>%
  mutate(freqs = n/sum(n)) %>%
  mutate(word1 = reorder(word1,n))

for (i in 1:10) {
  c <- i
  n_c <- sum(twitter_train_trigram_freqs$n==i)
  n_c_1 <- sum(twitter_train_trigram_freqs$n==i+1)
  c_star <- ((i+1) * n_c_1) / n_c
  freq_new <- c_star/sum(twitter_train_trigram_freqs$n)
  for (j in i:i) {
    twitter_train_trigram_freqs$freqs[twitter_train_trigram_freqs$n == j] <- freq_new
  }
}

predict_trigram <- function(word1,word2) {
  x <- word1
  y <- word2
  z <- twitter_train_trigram_freqs %>%
    filter(word1 == x & word2 == y) %>%
    arrange(desc(n))
  z$word3[1]  
}

predict_trigram("in","the")

# 4-gram model ------------------------------------------------------------

twitter_train_fourgram <- tibble_twitter_train %>%
  unnest_tokens(fourgram, text, token = "ngrams", n=4) 

twitter_train_fourgram_freqs <- twitter_train_fourgram %>%
  separate(fourgram, c("word1","word2","word3","word4"), sep =" ") %>%
  count(word1,word2,word3,word4,sort=TRUE) %>%
  mutate(freqs = n/sum(n)) %>%
  mutate(word1 = reorder(word1,n))

for (i in 1:10) {
  c <- i
  n_c <- sum(twitter_train_fourgram_freqs$n==i)
  n_c_1 <- sum(twitter_train_fourgram_freqs$n==i+1)
  c_star <- ((i+1) * n_c_1) / n_c
  freq_new <- c_star/sum(twitter_train_fourgram_freqs$n)
  for (j in i:i) {
    twitter_train_fourgram_freqs$freqs[twitter_train_fourgram_freqs$n == j] <- freq_new
  }
}

predict_fourgram <- function(word1,word2,word3) {
  x <- word1
  y <- word2
  z <- word3
  w <- twitter_train_fourgram_freqs %>%
    filter(word1 == x & word2 == y & word3 == z) %>%
    arrange(desc(n))
  w$word4[1]  
}

predict_fourgram("in","the","world")

# 5-gram model ------------------------------------------------------------

twitter_train_fivegram <- tibble_twitter_train %>%
  unnest_tokens(fivegram, text, token = "ngrams", n=5) 

twitter_train_fivegram_freqs <- twitter_train_fivegram %>%
  separate(fivegram, c("word1","word2","word3","word4","word5"), sep =" ") %>%
  count(word1,word2,word3,word4,word5,sort=TRUE) %>%
  mutate(freqs = n/sum(n)) %>%
  mutate(word1 = reorder(word1,n))

for (i in 1:10) {
  c <- i
  n_c <- sum(twitter_train_fivegram_freqs$n==i)
  n_c_1 <- sum(twitter_train_fivegram_freqs$n==i+1)
  c_star <- ((i+1) * n_c_1) / n_c
  freq_new <- c_star/sum(twitter_train_fivegram_freqs$n)
  for (j in i:i) {
    twitter_train_fivegram_freqs$freqs[twitter_train_fivegram_freqs$n == j] <- freq_new
  }
}

predict_fivegram <- function(word1,word2,word3,word4) {
  x <- word1
  y <- word2
  z <- word3
  w <- word4
  v <- twitter_train_fivegram_freqs %>%
    filter(word1 == x & word2 == y & word3 == z & word4 == w) %>%
    arrange(desc(n))
  v$word5[1]  
}

predict_fivegram("keep","up","the","good")

# model -------------------------------------------------------------------

word_prediction <- function(word1,word2){
  if (sum(twitter_train_bigram$bigram == paste(word1,word2),na.rm = TRUE)>1) {
    predict_trigram(word1,word2)
  } else {
    predict_bigram(word2)
  }
}

word_prediction <- function(word1,word2,word3,word4){
  if (sum(twitter_train_fourgram$fourgram == paste(word1,word2,word3,word4),na.rm = TRUE)>1) {
    predict_fivegram(word1,word2,word3,word4)
  } else if (sum(twitter_train_trigram$trigram == paste(word2,word3,word4),na.rm = TRUE)>1) {
    predict_fourgram(word2,word3,word4)
  } else if (sum(twitter_train_bigram$bigram == paste(word3,word4),na.rm = TRUE)>1) {
    predict_trigram(word3,word4)
  } else if (sum(twitter_train_bigram_freqs$word1 == word4,na.rm = TRUE)>1) {
    predict_bigram(word4)
  } else {
    "not sufficient data"
  }
}

word_prediction("if","can","sis","of")
predict_fourgram("can","world","of")
predict_fivegram("if","can","world","of")
predict_trigram("world","of")
predict_bigram("of")


# validation twitter ------------------------------------------------------

## correcting the â
twitter_corrected_v <- str_remove_all(twitter_validation,"_+|â–\u0081/â|â‚¬|â˜…|â„¢|â˜¹|â˜€|âš½|â\u009d•|â\u009d¤|â\u009d|â€œ|â€\u009d|â™¥|â˜¼|â™|â˜º|â™|â€§|â˜‡|âœŠ|âœŒðŸ’¤ðŸŒ™|âœ|âœŒ|âˆž")
twitter_corrected_v <- str_replace_all(twitter_corrected_v,c("â€™"="'","â€"="-","âˆ†"="A"))

## correcting contractions
twitter_corrected_v <- str_replace_all(twitter_corrected_v,
                                     c("[Ii]'ll"="i will","[Yy]ou'll"="you will","[Hh]e'll"="he will","[Ss]he'll"="she will","[Ii]t'll"="it will","[Ww]e'll"="we will","[Tt]hey'll"="they will"))
twitter_corrected_v <- str_replace_all(twitter_corrected_v,
                                     c("[Dd]on't"="do not","[Dd]oesn't"="does not","[Hh]aven't"="have not", "[Hh]asn't"="has not"))
twitter_corrected_v <- str_replace_all(twitter_corrected_v,
                                     c("[Ii]'m"="i am","[Yy]ou're"="you are","[Hh]e's"="he is", "[Ss]he's"="she is", "[Ww]e're" = "we are", "[Tt]hey're" = "they are"))
twitter_corrected_v <- str_replace_all(twitter_corrected_v,c("^[Uu] " = "you ", " [Uu] " = " you "," [Uu]$" = " you"))
twitter_corrected_v <- str_replace_all(twitter_corrected_v, c("^[Rr] you " = "are you ", " you [Rr] " = " you are ", " you [Rr]$" = " you are"))

## remove profanity

profanity_list <- str_remove_all(lexicon::profanity_alvarez, "^\\*|\\*$")
profanity_list <- str_replace_all(profanity_list, c("\\!"="\\\\!", "\\+"="\\\\+", "\\$"="\\\\$", "\\."="\\\\.","\\("="\\\\(","\\-"="\\\\-"))
profanity_regex <- str_c(" ",profanity_list," ", collapse = "|")

twitter_corrected_v <- str_remove_all(twitter_corrected_v,profanity_regex)

## tibbles

tibble_twitter_train_v <- tibble(line = 1:length(twitter_corrected_v), text = twitter_corrected_v)

# error -------------------------------------------------------------------

twitter_val_sixgram <- tibble_twitter_train %>%
  unnest_tokens(sixgram, text, token = "ngrams", n=6)

twitter_val_sixgram <- twitter_val_sixgram %>%  
  separate(sixgram, c("word1","word2","word3","word4","word5","word6"), sep =" ") %>%
  unite("fivegram",c("word1","word2","word3","word4","word5"),sep = " ")

head(twitter_val_sixgram)

test <- 
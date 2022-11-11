# Load packages
library(tidyr)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(readr)
library(textdata)

library(wordcloud)
library(tm)
library(SnowballC)
library(viridis)

# Import data
online_well <- read_csv("Online Well.csv")

online_not_well <- read_csv("Online not well.csv")

inperson_good <- read_csv("inperson good.csv")

inperson_bad <- read_csv("inperson bad.csv")

online_vs_inperson <- read_csv("Online vs inperson.csv")


# Clean out comments and convert them all to one word 
online_well <- online_well %>% 
  dplyr::select(Comments) %>%
  unnest_tokens(word, Comments)

online_not_well <- online_not_well %>% 
  dplyr::select(Comments) %>%
  unnest_tokens(word, Comments)

inperson_good <- inperson_good %>% 
  dplyr::select(Comments) %>%
  unnest_tokens(word, Comments)

inperson_bad <- inperson_bad %>% 
  dplyr::select(Comments) %>%
  unnest_tokens(word, Comments)

online_vs_inperson <- online_vs_inperson %>% 
  dplyr::select(Comments) %>%
  unnest_tokens(word, Comments)

# Remove stop words (things like 'the', 'and', etc)
data("stop_words")
online_well <- online_well %>%
  anti_join(stop_words)

data("stop_words")
online_not_well <- online_not_well %>%
  anti_join(stop_words)

data("stop_words")
inperson_good <- inperson_good %>%
  anti_join(stop_words)

data("stop_words")
inperson_bad <- inperson_bad %>%
  anti_join(stop_words)

data("stop_words")
online_vs_inperson <- online_vs_inperson %>%
  anti_join(stop_words)


# Online Well

# Filter out non needed words
online_well <- subset(online_well, word != "classes")
online_well <- subset(online_well, word != "class")
online_well <- subset(online_well, word != "online")
online_well <- subset(online_well, word != "person")
online_well <- subset(online_well, word != "students")
online_well <- subset(online_well, word != "student")
online_well <- subset(online_well, word != "lectures")
online_well <- subset(online_well, word != "lecture")
online_well <- subset(online_well, word != "lot")
online_well <- subset(online_well, word != "zoom")
online_well <- subset(online_well, word != "professor")
online_well <- subset(online_well, word != "professors")

# Top used words (adjust top_n)
online_well %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Frequency of Words",
       x = "words",
       title = "Top most used words")

# Word Cloud
online.well <- unlist(online_well, use.name= FALSE)
wordcloud(online.well, scale=c(7,0.5)    
          , max.words=50    
          , random.order=FALSE 
          , rot.per=0.25       
          , use.r.layout=FALSE 
          , colors=mako(200, direction = -1))


# Online Not Well

# Filter out non needed words
online_not_well <- subset(online_not_well, word != "classes")
online_not_well <- subset(online_not_well, word != "class")
online_not_well <- subset(online_not_well, word != "online")
online_not_well <- subset(online_not_well, word != "person")
online_not_well <- subset(online_not_well, word != "students")
online_not_well <- subset(online_not_well, word != "student")
online_not_well <- subset(online_not_well, word != "lectures")
online_not_well <- subset(online_not_well, word != "lecture")
online_not_well <- subset(online_not_well, word != "lot")
online_not_well <- subset(online_not_well, word != "zoom")
online_not_well <- subset(online_not_well, word != "professor")
online_not_well <- subset(online_not_well, word != "professors")
online_not_well <- subset(online_not_well, word != "top")
online_not_well <- subset(online_not_well, word != "pre")
online_not_well <- subset(online_not_well, word != "it's")

# Top used words (adjust top_n)
online_not_well %>%
  count(word, sort = TRUE) %>%
  top_n(50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Frequency of Words",
       x = "words",
       title = "Top most used words")

# Word Cloud
online.not.well <- unlist(online_not_well, use.name= FALSE)
wordcloud(online.not.well, scale=c(5,0.5)     # Set min and max scale
          , max.words=50      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.25       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=plasma(150, direction = -1))


# In person good

# Filter out non needed words
inperson_good <- subset(inperson_good, word != "classes")
inperson_good <- subset(inperson_good, word != "class")
inperson_good <- subset(inperson_good, word != "online")
inperson_good <- subset(inperson_good, word != "person")
inperson_good <- subset(inperson_good, word != "students")
inperson_good <- subset(inperson_good, word != "student")
inperson_good <- subset(inperson_good, word != "lectures")
inperson_good <- subset(inperson_good, word != "lecture")
inperson_good <- subset(inperson_good, word != "lot")
inperson_good <- subset(inperson_good, word != "zoom")
inperson_good <- subset(inperson_good, word != "professor")
inperson_good <- subset(inperson_good, word != "professors")

# Top used words (adjust top_n)
inperson_good %>%
  count(word, sort = TRUE) %>%
  top_n(50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Frequency of Words",
       x = "words",
       title = "Top most used words")

# Word Cloud
inperson.good <- unlist(inperson_good, use.name= FALSE)
wordcloud(inperson.good, scale=c(5,0.5)     # Set min and max scale
          , max.words=50      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.25       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=mako(50, direction = -1))


# In person bad

# Filter out non needed words
inperson_bad <- subset(inperson_bad, word != "classes")
inperson_bad <- subset(inperson_bad, word != "class")
inperson_bad <- subset(inperson_bad, word != "online")
inperson_bad <- subset(inperson_bad, word != "person")
inperson_bad <- subset(inperson_bad, word != "students")
inperson_bad <- subset(inperson_bad, word != "student")
inperson_bad <- subset(inperson_bad, word != "lectures")
inperson_bad <- subset(inperson_bad, word != "lecture")
inperson_bad <- subset(inperson_bad, word != "lot")
inperson_bad <- subset(inperson_bad, word != "zoom")
inperson_bad <- subset(inperson_bad, word != "professor")
inperson_bad <- subset(inperson_bad, word != "professors")
inperson_bad <- subset(inperson_bad, word != "it")
inperson_bad <- subset(inperson_bad, word != "lot")
inperson_bad <- subset(inperson_bad, word != "'s")

# Top used words (adjust top_n)
inperson_bad %>%
  count(word, sort = TRUE) %>%
  top_n(50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Frequency of Words",
       x = "words",
       title = "Top most used words")

# Word Cloud
inperson.bad <- unlist(inperson_bad, use.name= FALSE)
wordcloud(inperson.bad, scale=c(5,0.5)     # Set min and max scale
          , max.words=50      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.25       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=plasma(50, direction = -1))


# Online vs in person

# Filter out non needed words
online_vs_inperson <- subset(online_vs_inperson, word != "classes")
online_vs_inperson <- subset(online_vs_inperson, word != "class")
online_vs_inperson <- subset(online_vs_inperson, word != "online")
online_vs_inperson <- subset(online_vs_inperson, word != "person")
online_vs_inperson <- subset(online_vs_inperson, word != "students")
online_vs_inperson <- subset(online_vs_inperson, word != "student")
online_vs_inperson <- subset(online_vs_inperson, word != "lectures")
online_vs_inperson <- subset(online_vs_inperson, word != "lecture")
online_vs_inperson <- subset(online_vs_inperson, word != "lot")
online_vs_inperson <- subset(online_vs_inperson, word != "zoom")
online_vs_inperson <- subset(online_vs_inperson, word != "professor")
online_vs_inperson <- subset(online_vs_inperson, word != "professors")
online_vs_inperson <- subset(online_vs_inperson, word != "dont")

# Top used words (adjust top_n)
online_vs_inperson %>%
  count(word, sort = TRUE) %>%
  top_n(50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Frequency of Words",
       x = "words",
       title = "Top most used words")

# Word Cloud
online.vs.inperson <- unlist(online_vs_inperson, use.name= FALSE)
wordcloud(online.vs.inperson, scale=c(7,0.7)     # Set min and max scale
          , max.words=50      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.25       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=mako(50, direction = -1))

# Other sentiment analysis

# Sentiment analysis (bing)
sentiments_comments_bing <- online_vs_inperson %>%
   inner_join(get_sentiments("bing"), by = "word") %>%
   count(word, sentiment, sort = TRUE) %>%
   ungroup() %>%
   group_by(sentiment) %>%
   top_n(10) %>%
   ungroup() %>%
   mutate(word = reorder(word, n)) %>%
   ggplot(aes(word, n, fill = sentiment)) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~sentiment, scales = "free_y") +
   ylim(0, 50) +
   labs(y = NULL, x = NULL) +
   coord_flip() +
   theme_minimal()

sentiments_comments_bing

# Sentiment analysis (nrc)
 sentiments_comments_nrc <- online_vs_inperson %>%
   inner_join(get_sentiments("nrc"), by = "word") %>%
   count(word, sentiment, sort = TRUE) %>%
   ungroup() %>%
   group_by(sentiment) %>%
   top_n(5) %>%
   ungroup() %>%
   mutate(word = reorder(word, n)) %>%
   ggplot(aes(word, n, fill = sentiment)) +
   geom_col(show.legend = FALSE) +
   facet_wrap(~sentiment, scales = "free_y") +
   ylim(0, 50) +
   labs(y = NULL, x = NULL) +
   coord_flip() +
   theme_minimal() +
   theme(axis.text.x = element_blank())

 sentiments_comments_nrc



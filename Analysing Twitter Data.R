# ANALYSING TWITTER DATA
library(rtweet)
library(tidyverse)
library(tidytext)

rm(list = ls(all.names = TRUE))
dev.off()
options(digits = 3)

#________________________________________________________________________
# OBTAINING THE DATA
#________________________________________________________________________
Tweets <- search_tweets(q = "#WajingaNyinyi OR Wajinga OR KING KAKA",
                        n = 100,
                        type = "mixed") 

head(Tweets$text)
#________________________________________________________________________
# TIME SERIES PLOTS
#________________________________________________________________________

Tweets %>% ts_plot(by="days",trim = 1L)+
  geom_point()+
  theme_minimal()+
  theme(plot.title = element_text(size=15, 
                                  hjust=0.5, 
                                  face="bold", 
                                  colour="red", 
                                  vjust=-1),
        plot.subtitle=element_text(size=10, 
                                   hjust=0.5, 
                                   face="italic", 
                                   color="red"))+
  labs(x="Wajinga Nyinyi Tweets",
       y="Tweet Count",
       title = "King Kaka Spoken Word",
       subtitle = "Tweet Count Generated on a daily basis")
#________________________________________________________________________
# CLEANING THE DATA
#________________________________________________________________________

# REMOVING THE URL IN THE TWEET
Tweets$text <- gsub("http.*", "", Tweets$text) 
Tweets <- as.data.frame(Tweets)
class(Tweets)

save_as_csv(Tweets, file_name = "Twitter Data.csv")

# TIDYNG THE TEXT
Clean_text <- Tweets %>% 
  select(text) %>% 
  unnest_tokens(word,text) # generate a vector of words and store in "word"

#________________________________________________________________________
# PLOTTING TOP 20 MOST APPEARING WORDS
#________________________________________________________________________

Clean_text %>% 
  count(word,sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word,
             y = n))+
  geom_col()+
  coord_flip()+
  theme_minimal()+
  theme(plot.title = element_text(size = 15, 
                                  hjust = 0.5, 
                                  face = "bold", 
                                  colour = "red", 
                                  vjust = -1))+
  labs(x = "Unique Words", 
       y = "Word Count",
       title = "Count Of Unique Words Used")


# REMOVING STOP WORDS (to, in, the, at, etc)
Clean_text <-Clean_text %>% 
  anti_join(stop_words)

#________________________________________________________________________
# PLOTTING TOP 10 MOST APPEARING WORDS WITHOUT STOP WORDS
#________________________________________________________________________

Clean_text %>% 
  count(word,
        sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(x = word, 
             y = n))+
  geom_col()+
  coord_flip()+
  theme_minimal()+
  theme(plot.title = element_text(size=15, 
                                  hjust=0.5, 
                                  face="bold", 
                                  colour="blue", 
                                  vjust=-1),
        plot.caption = element_text(hjust = 0, 
                                    face = "italic"))+
  labs(x = "Unique Words", 
       y = "Word Count",
       title = "Wajinga Nyinyi",
       caption = "Data source: Twitter")

#________________________________________________________________________
# PLOTTING TOP 10 MOST APPEARING WORDS WITHOUT STOP WORDS
#________________________________________________________________________
library(widyr)
Tweet_pairs <- Tweets %>% 
  select(text) %>% 
  unnest_tokens(paired_words,text,
                token = "ngrams",
                n = 5)

Tweet_pairs %>% 
  count(paired_words, 
        sort = TRUE) %>% 
  top_n(10) %>%
  mutate(paired_words = reorder(paired_words, n)) %>%
  ggplot(aes(x = paired_words, 
             y = n))+
  geom_col()+
  coord_flip()+
  theme_minimal()+
  theme(plot.title = element_text(size = 15, 
                                  hjust = 0.5, 
                                  face = "bold", 
                                  colour = "red", 
                                  vjust = -1))+
  labs(x = "Unique Words", 
       y = "Word Count",
       title = "Count Of Unique Words Used")

#________________________________________________________________________
# END
#________________________________________________________________________





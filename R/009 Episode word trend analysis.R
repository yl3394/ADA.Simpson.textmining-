library(dplyr)
library(DBI)
library(tidyr)
library(NLP)
library(tm)
library(SnowballC)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)
library(scales)
library(sqldf)
# load data
load("/Users/wangyue/Desktop/2016 fall/gr5291bada/project/simpson/dat_myDTM.RData")
dat.episodes <- readRDS("/Users/wangyue/GitHub/ADA.Simpson.textmining-/data/dat_episodes.rds")
dat.characters <- readRDS("/Users/wangyue/GitHub/ADA.Simpson.textmining-/data/dat_characters.rds")
dat.scripts <- readRDS("/Users/wangyue/GitHub/ADA.Simpson.textmining-/data/dat_scripts.rds")
dat.locations <- readRDS("/Users/wangyue/GitHub/ADA.Simpson.textmining-/data/dat_locations.rds")
# DTM by episode
dat_DTMfrq_episode <- dat.myDTM %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (episode_id) %>%
  summarise(num=n()) %>%
  arrange(episode_id)
# merge season number
dat.DTMfrq.season<-dat.DTMfrq.episode %>%
  merge(dat.episodes,by = "episode_id") %>%
  arrange(episode_id) %>%
  select(season,num) %>%
  group_by(season) %>%
  summarise(num.total=sum(num)) %>%
  arrange(season)
# study the relationship between the number of words spoken and rating
episode_rating <- select(dat.episodes, episode_id, imdb_rating, season)
rating.num <- sqldf("SELECT episode_rating.season, episode_rating.episode_id, episode_rating.imdb_rating, dat_DTMfrq_episode.num
from episode_rating, dat_DTMfrq_episode where episode_rating.episode_id = dat_DTMfrq_episode.episode_id")
# visualization 
ggplot(rating.num, aes(x = num, y = imdb_rating, color = season)) + geom_point() + 
  xlab("Total number of words spoeken in each episode") + ylab("imdb_rating") + ggtitle("Scatterplot of total number of words spoken in each episode and rating")
# fit a linear regression
fit <- lm(imdb_rating ~ num, data = rating.num )
summary(fit) 
# look at the top 3 episodes which has highest ratings
rating.num[order(rating.num$imdb_rating, decreasing = T)[1:3], ]
# episodes 155, 176 and 179 have the highest rating 9.2, 9.2 and 9.1
# look at words spoken by each episode
# episode 155
dat_DTMfrq_episode155 <- dat.myDTM[which(dat.myDTM$episode_id == 155), ] %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (word) %>%
  summarise(num=n()) %>%
  arrange(desc(num))
wordcloud(dat_DTMfrq_episode155$word, dat_DTMfrq_episode155$num, min.freq = 300, 
          colors = brewer.pal(6,"Dark2"))
text(x = 0.5, y = 1 , "Wordcloud for Episode 155")
# episode 176
dat_DTMfrq_episode176 <- dat.myDTM[which(dat.myDTM$episode_id == 176), ] %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (word) %>%
  summarise(num=n()) %>%
  arrange(desc(num))
wordcloud(dat_DTMfrq_episode176$word, dat_DTMfrq_episode176$num,
          colors = brewer.pal(6,"Dark2"))
text(x = 0.5, y = 1 , "Wordcloud for Episode 176")
# episode 179
dat_DTMfrq_episode179 <- dat.myDTM[which(dat.myDTM$episode_id == 179), ] %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (word) %>%
  summarise(num=n()) %>%
  arrange(desc(num))
wordcloud(dat_DTMfrq_episode179$word, dat_DTMfrq_episode179$num, min.freq = 300, 
          colors = brewer.pal(6,"Dark2"))
text(x = 0.5, y = 1 , "Wordcloud for Episode 179")


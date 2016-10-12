### Top 4 characters word trend analysis
# Homer, Marge, Bart and Lisa
library(RColorBrewer)
library(wordcloud)
# 1. Homer id=2
dat.DTMfrq.homer<-dat.myDTM[which(dat.myDTM$character_id==2),] %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (word) %>%
  summarise(num=n()) %>%
  arrange(desc(num))
wordcloud(dat.DTMfrq.homer$word,dat.DTMfrq.homer$num,min.freq = 400,colors=brewer.pal(6, "Dark2"))

# 2. Marge id=1
dat.DTMfrq.marge<-dat.myDTM[which(dat.myDTM$character_id==1),] %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (word) %>%
  summarise(num=n()) %>%
  arrange(desc(num))
wordcloud(dat.DTMfrq.marge$word,dat.DTMfrq.marge$num,min.freq = 150,colors=brewer.pal(6, "Dark2"))

# 3. Bart id=8
dat.DTMfrq.bart<-dat.myDTM[which(dat.myDTM$character_id==8),] %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (word) %>%
  summarise(num=n()) %>%
  arrange(desc(num))
wordcloud(dat.DTMfrq.bart$word,dat.DTMfrq.bart$num,min.freq = 150,colors=brewer.pal(6, "Dark2"))

# 4. Lisa id=9
dat.DTMfrq.lisa<-dat.myDTM[which(dat.myDTM$character_id==9),] %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (word) %>%
  summarise(num=n()) %>%
  arrange(desc(num))
wordcloud(dat.DTMfrq.lisa$word,dat.DTMfrq.lisa$num,min.freq = 120,colors=brewer.pal(6, "Dark2"))

### Top 4 characters spoken words percentage change over 1-26 seasons
# homer
dat.homer.episode<-dat.myDTM[which(dat.myDTM$character_id==2),] %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (episode_id) %>%
  summarise(num.episode=n()) %>%
  arrange(episode_id)
dat.homer.season<-dat.homer.episode %>%
  merge(dat.episodes,by = "episode_id") %>%
  arrange(episode_id) %>%
  select(season,num.episode) %>%
  group_by(season) %>%
  summarise(num.homer=sum(num.episode)) %>%
  arrange(season)
# marge
dat.marge.episode<-dat.myDTM[which(dat.myDTM$character_id==1),] %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (episode_id) %>%
  summarise(num.episode=n()) %>%
  arrange(episode_id)
dat.marge.season<-dat.marge.episode %>%
  merge(dat.episodes,by = "episode_id") %>%
  arrange(episode_id) %>%
  select(season,num.episode) %>%
  group_by(season) %>%
  summarise(num.marge=sum(num.episode)) %>%
  arrange(season)
# bart
dat.bart.episode<-dat.myDTM[which(dat.myDTM$character_id==8),] %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (episode_id) %>%
  summarise(num.episode=n()) %>%
  arrange(episode_id)
dat.bart.season<-dat.bart.episode %>%
  merge(dat.episodes,by = "episode_id") %>%
  arrange(episode_id) %>%
  select(season,num.episode) %>%
  group_by(season) %>%
  summarise(num.bart=sum(num.episode)) %>%
  arrange(season)
# lisa
dat.lisa.episode<-dat.myDTM[which(dat.myDTM$character_id==9),] %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (episode_id) %>%
  summarise(num.episode=n()) %>%
  arrange(episode_id)
dat.lisa.season<-dat.lisa.episode %>%
  merge(dat.episodes,by = "episode_id") %>%
  arrange(episode_id) %>%
  select(season,num.episode) %>%
  group_by(season) %>%
  summarise(num.lisa=sum(num.episode)) %>%
  arrange(season)

# combine main characters word spoken by season
dat.word.season<-data.frame(dat.DTMfrq.season$season,
                            dat.homer.season$num.homer/dat.DTMfrq.season$num.total,
                            dat.marge.season$num.marge/dat.DTMfrq.season$num.total,
                            dat.bart.season$num.bart/dat.DTMfrq.season$num.total,
                            dat.lisa.season$num.lisa/dat.DTMfrq.season$num.total)
colnames(dat.word.season)<-c("season","Homer","Marge","Bart","Lisa")
# gather percentage
dat.word.season<-gather(dat.word.season,Character,Word_Percentage,-season)

ggplot(dat.word.season,aes(x=season,y=Word_Percentage,col=Character))+
  geom_line(size=1.5)

ggplot(dat.word.season,aes(x=season,y=Word_Percentage,col=Character))+
  geom_line(size=1.5)+
  facet_grid(.~Character)
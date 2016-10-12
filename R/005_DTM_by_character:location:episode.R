# DTM by characters
dat.DTMfrq.character <- dat.myDTM %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (character_id) %>%
  summarise(num=n()) %>%
  arrange(desc(num))

# merge character name
dat.DTMfrq.character<-dat.DTMfrq.character %>%
  merge(dat.characters,by.x = "character_id",by.y = "id") %>%
  arrange(desc(num))

dat.DTMfrq.character<-dat.DTMfrq.character[,3:2]

# visulization
# main characters
top15<-dat.DTMfrq.character[1:15,]
ggplot(top15,aes(reorder(name,num), y = num))+
  geom_bar(stat = "identity",fill="gold")+
  coord_flip()+
  scale_y_continuous("Words spoken, seasons 1-26",labels = unit_format("k", scale = 1e-3))+
  scale_x_discrete("")+
  ggtitle("The Simpsons top characters---by number of words spoken")
# support characters
ggplot(dat.DTMfrq.character[5:50,],aes(reorder(name,num),y=num))+
  geom_bar(stat="identity",fill="gold")+
  coord_flip()+
  scale_y_continuous("Words spoken, seasons 1-26",labels = unit_format("k",scale = 1e-3))+
  scale_x_discrete("")+
  ggtitle("The Simpsons support characters---by number of words spoken")


# DTM by locations
dat.DTMfrq.location <- dat.myDTM %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (location_id) %>%
  summarise(num=n()) %>%
  arrange(desc(num))

# merge location name
dat.DTMfrq.location<-dat.DTMfrq.location %>%
  merge(dat.locations,by.x = "location_id",by.y = "id") %>%
  arrange(desc(num))

dat.DTMfrq.location<-dat.DTMfrq.location[,3:2]

# visulization 
ggplot(dat.DTMfrq.location[1:10,],aes(reorder(name,num), y = num))+
  geom_bar(stat = "identity",fill="gold")+
  coord_flip()+
  scale_y_continuous("Words spoken, seasons 1-26",labels = unit_format("k", scale = 1e-3))+
  scale_x_discrete("")+
  ggtitle("The Simpsons locations---by number of words spoken")


# DTM by episodes
dat.DTMfrq.episode <- dat.myDTM %>% 
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



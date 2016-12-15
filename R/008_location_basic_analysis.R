library(RColorBrewer)
library(wordcloud)
library(scales)

#Top 4 locations words trends analysis
## 1. Simpson Home;  location id = 5
dat.DTMfrq.loct.SH <- dat.myDTM[which(dat.myDTM$location_id == 5),] %>%
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by(word) %>%
  summarise(num = n()) %>%
  arrange(desc(num))
wordcloud(dat.DTMfrq.loct.SH$word, dat.DTMfrq.loct.SH$num, min.freq = 300, colors = brewer.pal(6,"Dark2"))

## 2. Springfield Elementary School;  location id = 3
dat.DTMfrq.loct.SES <- dat.myDTM[which(dat.myDTM$location_id == 3),] %>%
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by(word) %>%
  summarise(num = n()) %>%
  arrange(desc(num))
wordcloud(dat.DTMfrq.loct.SES$word, dat.DTMfrq.loct.SES$num, min.freq = 80, colors = brewer.pal(6,"Dark2"))

## 3. Moe's Tavern;  location id = 15
dat.DTMfrq.loct.MT <- dat.myDTM[which(dat.myDTM$location_id == 15),] %>%
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by(word) %>%
  summarise(num = n()) %>%
  arrange(desc(num))
wordcloud(dat.DTMfrq.loct.MT$word, dat.DTMfrq.loct.MT$num, min.freq = 50, colors = brewer.pal(6,"Dark2"))

## 4. Springfield Nuclear Power Plant; location id = 10
dat.DTMfrq.loct.SNPP <- dat.myDTM[which(dat.myDTM$location_id == 10),] %>%
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by(word) %>%
  summarise(num = n()) %>%
  arrange(desc(num))
wordcloud(dat.DTMfrq.loct.SNPP$word, dat.DTMfrq.loct.SNPP$num, min.freq = 40, colors = brewer.pal(6,"Dark2"))


###########################################
# top location word percentage changes before and after season 10 (change point)
# 1~10 (including 10) season: episode_id 1~226 
# 1st Simpson Home;  location id = 5
dat.loct.SH.episode <- dat.myDTM[which(dat.myDTM$location_id == 5),] %>%
  group_by(episode_id) %>%
  summarise(num = n()) %>%
  arrange(episode_id)

# 2nd Springfield Elementary School;  location id = 3
dat.loct.SES.episode <- dat.myDTM[which(dat.myDTM$location_id == 3),] %>%
  group_by(episode_id) %>%
  summarise(num = n()) %>%
  arrange(episode_id)

# 3rd Moe's Tavern;  location id = 15
dat.loct.MT.episode <- dat.myDTM[which(dat.myDTM$location_id == 15),] %>%
  group_by(episode_id) %>%
  summarise(num = n()) %>%
  arrange(episode_id)

# 4th Springfield Nuclear Power Plant; location id = 10
dat.loct.SNPP.episode <- dat.myDTM[which(dat.myDTM$location_id == 10),] %>%
  group_by(episode_id) %>%
  summarise(num = n()) %>%
  arrange(episode_id)

# 5th Simpson Living Room; location id = 25
dat.loct.SLR.episode <- dat.myDTM[which(dat.myDTM$location_id == 25),] %>%
  group_by(episode_id) %>%
  summarise(num = n()) %>%
  arrange(episode_id)

# Whole line number 
dat.loct.episode <- dat.myDTM%>%
  group_by(episode_id) %>%
  summarise(num = n()) %>%
  arrange(episode_id)

dat.location.episode.before <- rbind(sum(dat.loct.SH.episode[which(dat.loct.SH.episode[,1] <= 226),2])/sum(dat.loct.episode[1:226,2]),
                                     sum(dat.loct.SES.episode[which(dat.loct.SES.episode[,1] <= 226),2])/sum(dat.loct.episode[1:226,2]),
                                     sum(dat.loct.MT.episode[which(dat.loct.MT.episode[,1] <= 226),2])/sum(dat.loct.episode[1:226,2]),
                                     sum(dat.loct.SNPP.episode[which(dat.loct.SNPP.episode[,1] <= 226),2])/sum(dat.loct.episode[1:226,2]),
                                     sum(dat.loct.SLR.episode[which(dat.loct.SLR.episode[,1] <= 226),2])/sum(dat.loct.episode[1:226,2])
)
dat.location.episode.after <- rbind(sum(dat.loct.SH.episode[which(dat.loct.SH.episode[,1] > 226),2])/sum(dat.loct.episode[227:564,2]),
                                    sum(dat.loct.SES.episode[which(dat.loct.SES.episode[,1] > 226),2])/sum(dat.loct.episode[227:564,2]),
                                    sum(dat.loct.MT.episode[which(dat.loct.MT.episode[,1] > 226),2])/sum(dat.loct.episode[227:564,2]),
                                    sum(dat.loct.SNPP.episode[which(dat.loct.SNPP.episode[,1] > 226),2])/sum(dat.loct.episode[227:564,2]),
                                    sum(dat.loct.SLR.episode[which(dat.loct.SLR.episode[,1] > 226),2])/sum(dat.loct.episode[227:564,2])
)

dat.location.episode <- rbind(dat.location.episode.before,dat.location.episode.after)
dat.location.episode <- cbind(dat.location.episode,c(rep("Before season 10",5), rep("After season 10",5)),
                              rep(c("1st_SH","2nd_SES","3rd_MT","4th_SNPP","5th_SLP"),2))

colnames(dat.location.episode) <- c("perc","time","variables")

dat.location.episode <- as.data.frame(dat.location.episode)
dat.location.episode[,1] <- as.numeric(as.character(dat.location.episode[,1]))

str(dat.location.episode)

#Before season 10
ggplot(dat.location.episode[which(dat.location.episode[,2]=="Before season 10"),], aes(x=variables, y=perc))+
  geom_bar(stat="identity",position="dodge",fill="#E69F00", colour="black")+
  xlab("TOP 5 Locations")+ylab("Line Percentage")+
  ggtitle("Locations Before season 10")
#After season 10

ggplot(dat.location.episode[which(dat.location.episode[,2]=="After season 10"),], aes(x=variables, y=perc))+
  geom_bar(stat="identity",position="dodge",fill="#E69F00", colour="black")+
  xlab("TOP 5 Locations")+ylab("Line Percentage")+
  ggtitle("Locations After season 10")

# comparable plot before and after season 10
ggplot(dat.location.episode, aes(x=variables, y=perc, fill=factor(time))) + 
  stat_summary(fun.y=mean, geom="bar",position=position_dodge(1)) + 
  scale_color_discrete("time")+
  xlab("TOP 5 Locations")+ylab("Line Percentage")+
  ggtitle("Locations compared Before and After season 10")



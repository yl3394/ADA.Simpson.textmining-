# DTM by season
# first study the relationship between word number and season
# merge season number
dat.DTMfrq.season<-dat.DTMfrq.episode %>%
  merge(dat.episodes,by = "episode_id") %>%
  arrange(episode_id) %>%
  select(season,num) %>%
  group_by(season) %>%
  summarise(num.total=sum(num)) %>%
  arrange(season)
# boxplot of word number given season
ggplot(rating.num, aes(x = factor(season), y = num )) + geom_boxplot() + 
  xlab("Season") + ylab("Number of words spoken") + ggtitle("Boxplot of number of words spoken in each season")
# scatterplot
dat.DTMfrq.season$changepoint <- ifelse(dat.DTMfrq.season$season <= 10, "Before season 10", "After season 10")
dat.DTMfrq.season$changepoint <- factor(dat.DTMfrq.season$changepoint)
ggplot(dat.DTMfrq.season, aes(x = season, y = num.total, fill = changepoint)) + geom_bar(stat = "identity") + ylab("Total number of words spoken") +
  ggtitle("Bar plot of total number of words spoken in each season")
# mean number of words spoken before season 10
mean(dat.DTMfrq.season$num.total[1:10]) # 9996.8
# mean number of words spoken after season 10
mean(dat.DTMfrq.season$num.total[11:26]) # 8317.562
# Before season 10, it seems that they speak more words
# next we will study relative word spoken before season 10 and after season 10
# episode_id in 1:226 is before season 10
# order data
dat.myDTM <- dat.myDTM[order(dat.myDTM$episode_id), ]
which(dat.myDTM$episode_id == 226) # the first 64432 obs belong to first season 10
# arrange data for first 10 seasons
dat_DTMfrq_season1 <- dat.myDTM[1:64432, ] %>% 
  gather(word, frequency, -episode_id, -spoken_words, -spoken_words_clean,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (word) %>%
  summarise(num=n()) %>%
  arrange(desc(num))
# add percentage
dat_DTMfrq_season1$percentage <- dat_DTMfrq_season1$num / sum(dat_DTMfrq_season1$num)
# arrange data after season 10
dat_DTMfrq_season2 <- dat.myDTM[-(1:64432), ] %>% 
  gather(word, frequency, -episode_id, -spoken_words, -spoken_words_clean,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by (word) %>%
  summarise(num=n()) %>%
  arrange(desc(num))
# add percentage
dat_DTMfrq_season2$percentage <- dat_DTMfrq_season2$num / sum(dat_DTMfrq_season2$num)
# compare word frequency before and after season 10 
# pick the first 25 words
# before season 10
sum(dat_DTMfrq_season1$percentage[1:25]) # 0.4381602
ggplot(dat_DTMfrq_season1[1:25, ], aes( x = reorder(word, -percentage), y = percentage)) + geom_bar(stat = "identity") +
  xlab("Word") + ggtitle("The frequency of most frequent words Before season 10")
# after season 10
sum(dat_DTMfrq_season2$percentage[1:25]) # 0.4193085
ggplot(dat_DTMfrq_season2[1:25, ], aes( x = reorder(word, -percentage), y = percentage)) + geom_bar(stat = "identity") +
  xlab("Word") + ggtitle("The frequency of most frequent words After season 10")
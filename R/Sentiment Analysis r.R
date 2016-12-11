library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(scales)
library(xlsx)
library(stringr)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(RSentiment)
library(wordcloud)



dat_episodes.rds <- readRDS("/Users/yicheng/Github/STAT5291.ADA.untitled-/ADA.Simpson.textmining-/data/dat_episodes.rds")
dat_scripts.rds <- readRDS("/Users/yicheng/Github/STAT5291.ADA.untitled-/ADA.Simpson.textmining-/data/dat_scripts.rds")
dat_locations.rds <- readRDS("/Users/yicheng/Github/STAT5291.ADA.untitled-/ADA.Simpson.textmining-/data/dat_locations.rds")
dat.characters <- readRDS("/Users/yicheng/Github/STAT5291.ADA.untitled-/ADA.Simpson.textmining-/data/dat_characters.rds")

# Changing point at 10 season. Then we see the Top Charater before 10 season.
dat_change<- left_join(dat_scripts.rds, dat_episodes.rds,by="episode_id") %>%
  filter(season <= 10) %>%                                                                           
  group_by(raw_character_text, character_id)%>%
  summarise(n.change=n()) %>%
  filter(n.change != 6991) %>%
  filter(n.change >=747) 



p1<-plot_ly(x=~ dat_change$n.change, y=~ reorder(dat_change$raw_character_text,dat_change$n.change),
            type='bar', orientation='h',
            marker = list(color = 'rgba(11,167,181,0.7)'))%>%
  layout(xaxis=list(title="Sentences Spoken,season 1-10"),
         yaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE),
         title = 'The Simpsons top characters before the 10th season') %>%
  add_annotations(xref = 'paper', yref = 'dat_change$raw_character_text', x = 0.14, y = dat_change$raw_character_text,
                  xanchor = 'right',
                  text = dat_change$raw_character_text,
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(67, 67, 67)'),
                  showarrow = FALSE)

# Top Charactor after 10 Season.
dat_change1<- left_join(dat_scripts.rds, dat_episodes.rds,by="episode_id") %>%
  filter(season >10) %>%
  group_by(raw_character_text, character_id)%>%
  summarise(n.change1=n()) %>%
  filter(n.change1 != 10530) %>%
  filter(n.change1 >=1089) 

p2<-plot_ly(x=~ dat_change1$n.change1, y=~ reorder(dat_change1$raw_character_text,dat_change1$n.change1),
        type='bar', orientation='h',
        marker = list(color = 'rgba(11,167,181,0.7)'))%>%
  layout(xaxis=list(title="Sentences Spoken,season 10-26"),
         yaxis = list(title = "",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE),
         title = 'The Simpsons top characters after the 10th season') %>%
  add_annotations(xref = 'paper', yref = 'dat_change1$raw_character_text', x = 0.14, y = dat_change1$raw_character_text,
                  xanchor = 'right',
                  text = dat_change1$raw_character_text,
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(67, 67, 67)'),
                  showarrow = FALSE)

par(mfrow=c(2,2))
p1
p2

dat_whole<-left_join(dat_scripts.rds, dat_episodes.rds, by="episode_id") %>%
  group_by(season) %>%
  summarise(n.whole=n())

# Marge Simpson: percentage of show's total dialogue by season
dat_marge <- dat_scripts.rds %>% 
  filter(as.numeric(character_id) == 1) %>%
  mutate(n.word = as.numeric(word_count)) %>%
  mutate(n.word = ifelse(is.na(n.word), 0, n.word)) %>%
  filter(n.word != 409000)

dat_marge <- left_join(dat_marge, dat_episodes.rds, by = "episode_id") %>%  
  group_by(season) %>%
  summarise(n.dialog = n(), n.word = sum(n.word)) %>%
  mutate(pctg.dialog = n.dialog/dat_whole$n.whole) %>%
  mutate(pctg = percent(pctg.dialog))

# Bart Simpson: percentage of show's total dialogue by season
dat_bart<- dat_scripts.rds %>%
  filter(as.numeric(character_id) == 8)


dat_bart <- left_join(dat_bart,dat_episodes.rds, by= "episode_id") %>%
  group_by(season) %>%
  summarise(n.dialog= n()) %>%
  mutate(pctg.dialog=n.dialog/dat_whole$n.whole) %>%
  mutate(pctg = percent(pctg.dialog))
  
#PLOT
data<-data.frame(season = dat_bart$season, bart = dat_bart$pctg.dialog, marge = dat_marge$pctg.dialog) 

plot_ly(data, x= ~season, y= ~bart, type='bar', name= 'Bart Simpson', marker=list(color='rgb(235,104,65)')) %>%
  add_trace(y=~marge, name= 'Marge Simpson', marker= list(color='rgb(235,201,81)')) %>% 
  layout(xaxis=list(title="Season"),
         yaxis=list(title="Propotion"),
         title = 'Propotion of show’s total dialogue by season')

# Sentiment

#corpus= Corpus(VectorSource(dat_bart1$spoken_words))
#corpus=tm_map(corpus,removePunctuation)
#corpus=tm_map(corpus,content_transformer(tolower))
#corpus=tm_map(corpus,removeNumbers)
#corpus=tm_map(corpus,stripWhitespace)
#corpus=tm_map(corpus,removeWords, stopwords('english'))


# Sentiment for Marge
attach(myDTM_n1)

dtm<-myDTM_n1 %>%
  filter(character_id == 1)
dtm[1:7] <- list(NULL)

freq_marge<- colSums(as.matrix((dtm)))
sentiments_marge=calculate_sentiment(names(freq_marge))
sentiments_marge=cbind(sentiments_marge, as.data.frame(freq_marge))
marge<-sentiments_marge%>%filter(sentiment=="Positive"|sentiment=="Negative") %>%
  mutate(freq_marge=ifelse(sentiment=="Negative", -freq_marge, freq_marge)) 


# Plot for contribution to Sentiment for Marge
marge$text<- factor(marge$text, levels=marge$text[order(marge$freq_marge)])

ggplot(data=marge,aes(marge$text, marge$freq_marge, fill=marge$sentiment))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust =1))+
  labs(list(title = "Sentiment analysis for Marge", x ="",y= "Contribution to sentiment"))+
  scale_fill_discrete(name="Sentiment")

## Wordcloud of marge
sent_pos_marge = sentiments_marge[sentiments_marge$sentiment=='Positive',]
sent_neg_marge = sentiments_marge[sentiments_marge$sentiment=='Negative',]

wordcloud(sent_pos_homer$text, sent_pos_marge$freq_marge, min.freq = 10, scale=c(6,0.7), random.order=FALSE,
          rot.per=0.35,colors=brewer.pal(8,"Dark2"))
wordcloud(sent_neg_homer$text, sent_pos_marge$freq_marge, min.freq = 10, scale=c(6,0.7), random.order=FALSE,
          rot.per=0.35,colors=brewer.pal(8,"Dark2"))


# Plot for contribution to Sentiment for Bart
dtm1<-myDTM_n1 %>%
  filter(character_id == 8)
dtm1[1:7] <- list(NULL)

freq_bart<- colSums(as.matrix((dtm1)))
sentiments_bart=calculate_sentiment(names(freq_bart))
sentiments_bart=cbind(sentiments_bart, as.data.frame(freq_bart))
bart<-sentiments_bart%>%filter(sentiment=="Positive"|sentiment=="Negative") %>%
  mutate(freq_bart=ifelse(sentiment=="Negative", -freq_bart, freq_bart)) 


bart$text<- factor(bart$text, levels=bart$text[order(bart$freq_bart)])

ggplot(data=bart,aes(bart$text, bart$freq_bart, fill=bart$sentiment))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust =1))+
  labs(list(title = "Sentiment analysis for Bart", x ="",y= "Contribution to sentiment"))+
  scale_fill_discrete(name="Sentiment")


## Wordcloud of Bart
sent_pos_bart = sentiments_bart[sentiments_bart$sentiment=='Positive',]
sent_neg_bart = sentiments_bart[sentiments_bart$sentiment=='Negative',]

wordcloud(sent_pos_bart$text, sent_pos_bart$freq_bart, min.freq = 10, scale=c(6,0.7), random.order=FALSE,
          rot.per=0.35,colors=brewer.pal(8,"Dark2"))
wordcloud(sent_neg_bart$text, sent_pos_bart$freq_bart, min.freq = 10, scale=c(6,0.7), random.order=FALSE,
          rot.per=0.35,colors=brewer.pal(8,"Dark2"))


# Sentiment analysis for MOZ
dtm2<-myDTM_n1 %>%
  filter(character_id == 17)
dtm2[1:7] <- list(NULL)

freq_moz<- colSums(as.matrix((dtm2)))
sentiments_moz=calculate_sentiment(names(freq_moz))
sentiments_moz=cbind(sentiments_moz, as.data.frame(freq_moz))


sent_pos_moz = sentiments_moz[sentiments_moz$sentiment=='Positive',]
sent_neg_moz = sentiments_moz[sentiments_moz$sentiment=='Negative',]

wordcloud(sent_pos_moz$text, sent_pos_moz$freq_moz,scale=c(6,0.7), min.freq = 1, random.order=FALSE,
          rot.per=0.35,colors=brewer.pal(8,"Dark2"))
wordcloud(sent_neg_moz$text, sent_pos_moz$freq_moz,scale=c(6,0.7), min.freq = 1, random.order=FALSE,
          rot.per=0.35,colors=brewer.pal(8,"Dark2"))









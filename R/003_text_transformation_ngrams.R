# Author: YJ Li
#=================================================================================================================================================
# STEP 4. 2-gram Text Tansformation (Unweighted)
#=================================================================================================================================================
NGramDataframe <- function(doc, n, sparse) {
  # DESCRITPION: Return n-gram dataframes 
  # RETURN VALUES: dataframe
  return.list <- list()
  NGramTokenizer <- function(x) 
    unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
  
  ngram.dtm <- DocumentTermMatrix(doc, control = list(tokenize = NGramTokenizer))
  ngram.dtm.nonsparse <- removeSparseTerms(ngram.dtm, 1.00 - sparse)    # Use the original doc 
  ngram.dtm.df <- cbind(text_body = messages.clean$text_body,
                        conversation_id = messages.clean$conversation_id,
                        contact_type = messages.clean$contact_type,
                        vertical = messages.clean$vertical,
                        as.data.frame(as.matrix(ngram.dtm.nonsparse)))
  return.list[["dtm"]] <- ngram.dtm
  return.list[["dtm.df"]] <- ngram.dtm.df
  return(return.list)
}

#=================================================================================================================================================
# STEP 4. N-gram Text Tansformation (Unweighted, N > 1)
#=================================================================================================================================================
# N-gram Generator 
NGramDataframe <- function(corpus, n, sparsity) {
  # DESCRITPION: Return n-gram dataframes 
  # RETURN VALUES: dataframe
  return.list <- list()
  NGramTokenizer <- function(x) 
    unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)
  
  ngram.dtm <- DocumentTermMatrix(corpus, control = list(tokenize = NGramTokenizer))
  ngram.dtm.nonsparse <- removeSparseTerms(ngram.dtm, sparsity)    
  ngram.dtm.df <- cbind(spoken_words = dat.scripts$spoken_words,
                        spoken_words_clean = dat.scripts$spoken_words_clean,
                        episode_id = dat.scripts$episode_id, # Details join by dat.episodes 
                        speaking_line = dat.scripts$speaking_line,
                        character_id = dat.scripts$character_id, # Details join by dat.character 
                        location_id = dat.scripts$location_id, # Details join by dat.locations 
                        as.data.frame(as.matrix(ngram.dtm.nonsparse))) 
  return.list[["dtm"]] <- ngram.dtm
  return.list[["dtm.df"]] <- ngram.dtm.df
  return(return.list)
}

# 4.1 2-gram DTM Table 
# ----------------------------
gram2 <- NGramDataframe(myCorpus, 2, 0.9999) 
myDTM.2 <- gram2$dtm
dat.myDTM.2 <- gram2$dtm.df

# Get Frequency Table 
dat.DTMfrq.2 <- dat.myDTM.2 %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by(word) %>%    
  summarise(num = n()) %>%           
  arrange(desc(num)) 

write.csv(dat.DTMfrq.2, "exported_data/DTM_frq_2gram.csv")

# 4.2 3-gram DTM Tables 
# ----------------------------
gram3 <- NGramDataframe(myCorpus, 3, 0.9999) 
myDTM.3 <- gram3$dtm
dat.myDTM.3 <- gram3$dtm.df

# Get Frequency Table 
dat.DTMfrq.3 <- dat.myDTM.3 %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
         -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by(word) %>%    
  summarise(num = n()) %>%           
  arrange(desc(num)) 

write.csv(dat.DTMfrq.3, "exported_data/DTM_frq_3gram.csv")


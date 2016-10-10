# Author: YJ Li
#=================================================================================================================================================
# STEP 2. Text Cleaning  
#=================================================================================================================================================
# 2.1 Corpus Cleaning 
# ----------------------------
# Convert multibyte to bibyte   ????? Why do we always need this??????? 
dat.scripts$spoken_words <- iconv(enc2utf8(dat.scripts$spoken_words),sub="byte")
dat.scripts$word_count <- ifelse(!is.na(dat.scripts$word_count), as.numeric(dat.scripts$word_count), 0)

# Tansform script text lines into corpus (separated text files)
myCorpus <- Corpus(VectorSource(dat.scripts$spoken_words))
# Check original text data's meta-data
inspect(myCorpus)

# Convert all text to lowercase
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# Remove all numbers
myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
# Delete all english stopwords. See list: stopwords("english")
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
# Remove all punctuation
myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation))
# Delete common word endings, like -s, -ed, -ing, etc.
myCorpus <- tm_map(myCorpus, stemDocument, language = "english")
# Reduce any whitespace (spaces, newlines, etc) to single spaces
myCorpus <- tm_map(myCorpus, content_transformer(stripWhitespace))

# Check the clean text data's meta-data 
inspect(myCorpus)  # [[9999]] 5 -> 4; [[9998]] 30 -> 4; [[9997]] 5 -> 4; [[9996]] 67 -> 32;
myCorpus[[12]]$content 

# Add cleaned textlines to dataframe 
dat.scripts$spoken_words_clean <- as.vector(unlist(sapply(myCorpus, `[`, "content")))

#=================================================================================================================================================
# STEP 3. 1-gram Text Tansformation (Unweighted)
#=================================================================================================================================================
# 3.1 DTM Generating 
# ----------------------------
# Tansform corpus into Document Term Matrices
myDTM <- DocumentTermMatrix(myCorpus)
myDTM 
dim(myDTM) # dim = 158271 * 31331 (31331 different terms)
inspect(myDTM)

# Remove sparsity 
myDTM.nosparse <- removeSparseTerms(myDTM, 0.995) # dim = 127
# myDTM.nosparse <- removeSparseTerms(myDTM, 0.998) # dim = 341
# myDTM.nosparse <- removeSparseTerms(myDTM, 0.990) # dim = 49
dim(myDTM.nosparse)
inspect(myDTM.nosparse)

# 3.2 DTM Table
# ----------------------------
dat.myDTM <- cbind(spoken_words = dat.scripts$spoken_words,
                   spoken_words_clean = dat.scripts$spoken_words_clean,
                   episode_id = dat.scripts$episode_id, # Details join by dat.episodes 
                   speaking_line = dat.scripts$speaking_line,
                   character_id = dat.scripts$character_id, # Details join by dat.character 
                   location_id = dat.scripts$location_id, # Details join by dat.locations 
                   as.data.frame(as.matrix(myDTM.nosparse))) 

# QUESTIONS: By location/character/episodes (word trend analysis)

# 3.3 DTM Frequency Table 
# ----------------------------
dat.DTMfrq.1 <- dat.myDTM %>% 
  gather(word, frequency, -spoken_words, -spoken_words_clean, -episode_id,
                             -speaking_line, -character_id, -location_id) %>%
  filter(frequency != 0) %>%
  group_by(word) %>%    
  summarise(num = n()) %>%           
  arrange(desc(num)) 

write.csv(dat.DTMfrq.1, "exported_data/DTM_frq_1gram.csv")

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
# STEP 6. Text Tansformation (Weighted)
#=================================================================================================================================================
# 6.1 Weighted DTM Generating 
# ----------------------------
myDTM.wt <- DocumentTermMatrix(myCorpus, control = list(weighting = weightTfIdf))
myDTM.wt
dim(myDTM.wt) 
inspect(myDTM.wt[1:100, 1:100])

myDTM.wt.nosparse <- removeSparseTerms(myDTM.wt, 0.995) # dim = 127 
dim(myDTM.wt.nosparse) 

####### QUESTIONS: WHY DO WE NEED THE WEIGHTED IF-IDF? 











#=================================================================================================================================================
# STEP 99. Test Values
#=================================================================================================================================================
# 99.1 Test on the DTM generator
mtDTM.test <- DocumentTermMatrix(myCorpus[1:100])
mtDTM.test
dim(mtDTM.test)
inspect(mtDTM.test)
mtDTM.test.nosparse <- removeSparseTerms(mtDTM.test, 0.98)
inspect(mtDTM.test.nosparse)
dim(mtDTM.test.nosparse)

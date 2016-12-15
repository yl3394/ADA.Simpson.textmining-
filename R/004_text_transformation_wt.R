# Author: YJ Li
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

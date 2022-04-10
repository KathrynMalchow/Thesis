#Collocation

pacman::p_load(udpipe, quanteda)


####for DM
df_coll = collocation(dm_parsed, term = "lemma", group = c("doc_id", "sentence_id"), 
          ngram_max = 2, n_min = 3, sep = " ")
#look by top occurring collocations
df_coll = df_coll[order(df_coll$freq, decreasing = TRUE),] %>% 
  head(10)



####for Information Education
ie_coll = collocation(ie_parsed, term = "lemma", group = c("doc_id", "sentence_id"), 
                      ngram_max = 2, n_min = 3, sep = " ")
#look by top occurring collocations
ie_coll = ie_coll[order(ie_coll$freq, decreasing = TRUE),] 



####for Markets/Networks
mn_coll = collocation(mn_parsed, term = "lemma", group = c("doc_id", "sentence_id"), 
                      ngram_max = 4, n_min = 3, sep = " ")
#look by top occurring collocations
mn_coll = mn_coll[order(mn_coll$freq, decreasing = TRUE),] 


####for Advanced Technology
at_coll = collocation(at_parsed, term = "lemma", group = c("doc_id", "sentence_id"), 
                      ngram_max = 5, n_min = 2, sep = " ")
#look by top occurring collocations
at_coll = at_coll[order(at_coll$freq, decreasing = TRUE),]


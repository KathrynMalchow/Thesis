#Collocation

pacman::p_load(udpipe, quanteda)


#for DM
df_coll = collocation(dm_parsed, term = "lemma", group = c("doc_id", "sentence_id"), 
          ngram_max = 2, n_min = 3, sep = " ")


df_coll = df_coll[order(df_coll$freq, decreasing = TRUE),] %>% 
  head(10)

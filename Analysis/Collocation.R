#Collocation

pacman::p_load(udpipe, quanteda)


#for DM
collocation(dm_parsed, term = "lemma", group = c("doc_id", "sentence_id"), 
            ngram_max = 2, n_min = 3, sep = " ")


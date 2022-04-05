#Topic Modeling

pacman::p_load(quanteda, tidytext, tidyverse, dplyr, ggplot2, tidyr, textmineR, textclean)


#####tib_DataManagement

#preprocess cleaning function

reviews_into_words = function(x){
  
  x = x[,c(1, 4)]
  
  mystopwords = tibble(word = c("app", "star", "fix" , "please"), lexicon = "me") 
  allstopwords = stop_words %>%
    bind_rows(mystopwords)
  
    new = x %>% 
    unnest_tokens(word, reviews) %>% 
    filter(!(nchar(word) == 1)) %>%  
    anti_join(allstopwords)
  
  new = new %>% 
    mutate(ind = row_number()) %>% 
    group_by(names) %>%
    mutate(ind = row_number()) %>% 
    pivot_wider(names_from = ind, values_from = word)
  
  new[is.na(new)] = ""
  new = unite(new, reviews,-names,sep =" " )
  
  new$reviews = trimws(new$reviews)
  
  as.data.frame(new)
}


#run to words funciton for DM
dm_LDA = reviews_into_words(tib_DataManagement)




#create Document Term Matrix
dm_dtm = CreateDtm(dm_tokens$word, 
                 doc_names = dm_tokens$names, 
                 ngram_window = c(1, 2))  #let algorithm decide unigram or bigram
dm_tf = TermDocFreq(dm_dtm)
original_tf = dm_tf %>% 
  select(term, term_freq,doc_freq)
rownames(original_tf) =  1:nrow(original_tf)

#remove words appearing less than 2 times and those that appear in more than half the reviews
vocabulary = dm_tf$term[dm_tf$term_freq > 1 & dm_tf$doc_freq < nrow(dm_dtm) / 2 ]


#determine which number of k (terms) per topics gives the highest coherence score

k_list = seq(1, 20, by = 1)

model_dir = paste0("models_", digest::digest(vocabulary, algo = "sha1"))

if (!dir.exists(model_dir)) dir.create(model_dir)

model_list = TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path("model_dir", paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m = FitLdaModel(dtm = dm_dtm, k = k, iterations = 500)
    m$k = k
    m$coherence = CalcProbCoherence(phi = m$phi, dtm = dm_dtm, M = 5)
    save(m, file = filename)
  }else {
    load(filename)
  }
  
  m
}, export = c("dm_dtm", "model_dir"))





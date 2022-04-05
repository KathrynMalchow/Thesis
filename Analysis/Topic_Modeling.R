#Topic Modeling

pacman::p_load(quanteda, tidytext, tidyverse, dplyr, ggplot2, tidyr, textmineR)


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


#run reviews_into_words function for DM
dm_LDA = reviews_into_words(tib_DataManagement)





#create Document Term Matrix
dtm = CreateDtm(dm_LDA$reviews, 
                 doc_names = dm_LDA$names, 
                 ngram_window = c(1, 2))  #let algorithm decide unigram or bigram
#basic frequency
tf = TermDocFreq(dtm = dtm)
original_tf = tf %>% 
  select(term, term_freq,doc_freq)
rownames(original_tf) =  1:nrow(original_tf)

#remove words appearing < 2 times and those that appear in > half the reviews
vocabulary = tf$term[tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]

dtm = dtm



####determine which number of k (terms) per topics gives the highest coherence score

k_list = seq(30, 50, by = 1)

model_dir = paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)

model_list = TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m = FitLdaModel(dtm = dtm, k = k, iterations = 500)
    m$k = k
    m$coherence = CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  }else {
    load(filename)
  }
  
  m
}, export = c("dtm", "model_dir"))

#pick best model
coherence_mat = data.frame(k = sapply(model_list, function(x)
  nrow(x$phi)),
  coherence = sapply(model_list, function(x) 
  mean(x$coherence)), 
  stringsAsFactors = FALSE)

#plot
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(30,50,1)) + ylab("Coherence")


#highest coherence score for dm is 0.14 at 42 topics

####get 15 top terms per topic with highest phi(the most frequency in a topic)
model = model_list[which.max(coherence_mat$coherence)][[ 1 ]]
model$top_terms = GetTopTerms(phi = model$phi, M = 5) #top 5 terms per topic
top5_wide = as.data.frame(model$top_terms)


#coherence value for each topic
model$coherence

#prevalence
model$prevalence = colSums(model$theta)/sum(model$theta)*100
model$prevalence

#Number of documents in which each topic appears
model$assignments = model$theta
model$assignments[ model$assignments < 0.05 ] <- 0
model$assignments = model$assignments / rowSums(model$assignments)
model$assignments[ is.na(model$assignments) ] <- 0
model$num_docs = colSums(model$assignments > 0)

#compile into summary
model$summary = data.frame(topic = rownames(model$phi),
                                coherence = round(model$coherence,3),
                                prevalence = round(model$prevalence,3),
                                top_terms = apply(model$top_terms,2,function(x){paste(x,collapse = ", ")}),
                                frequency = model$num_docs)

modsum = model$summary %>%
  `rownames<-`(NULL)
modsum

save(modsum,file="Analysis/modsum.Rda")


ggplot(modsum[modsum$topic %in% c("t_4" , "t_16", "t_29", "t_35"),], aes(x = reorder(topic, -frequency), y = frequency))+
  geom_bar(stat = "identity", width = 0.65, fill = "lightblue")+
  scale_x_discrete(labels=c("Topic 1 \neasy, \nrecord, \nsimple, \nkeeping",
                            "Topic 2\n free, \nmonth, \npay, \nworth",
                            "Topic 3 \nphone, \ninput, \ngps, \ninterface",
                            "Topic 4 \naccurate, \nmeasuring, \ngps, \naccuracy"))+
  labs(x = "Topics with their most frequent terms", 
       y = "Number of Reviews",
       title = "Number of Reviews by Topic")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
  







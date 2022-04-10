#topic modeling 2

pacman::p_load(quanteda, tidytext, tidyverse, dplyr, ggplot2, tidyr, textmineR, topicmodels, seededlda, ggthemes, remotes)


#####tib_DataManagement

############preprocess cleaning function####################

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
  
  #as.data.frame(new)
}
##########################################################


#run reviews_into_words function for DM
dm_char = reviews_into_words(tib_DataManagement)


dm_tokens = tokens(dm_char,
                   remove_punct = TRUE,
                   remove_numbers = TRUE) %>%
  tokens_remove(stopwords()) %>%
  tokens_tolower() %>% 
  tokens_select(min_nchar = 1)

#create Document Feature Matrix

dm_dfm = dfm(dm_tokens)

dm_lda = textmodel_lda(dm_dfm, k = 15)


terms(dm_lda, 10)

#most important topic for each review
dm_dfm$topic = topics(dm_lda)

#most important terms for each topic
top_terms = terms(dm_lda, 4)
topic_names = apply(top_terms, 2, paste, collapse=",")

#extract prevalence of topics across all documents
topic_names_df = bind_rows(topic_names) %>%
  gather(topic, names)

topics_dm = as_tibble(dm_lda$theta) %>%
  mutate(document = rownames(.)) %>%
  gather(topic, gamma, -document) %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(topic_names_df, by = "topic")


#for theme
remotes::install_github("hrbrmstr/hrbrthemes")
library(hrbrthemes)

#color palette



#plot
topics_dm %>%
  ggplot(aes(reorder(topic, gamma), gamma, label = names, fill =
               topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.2),
                     labels = scales::percent_format()) +
  labs(x = NULL, y = expression(gamma),
       title = "Topic Prevalence",
       subtitle = "With the top words that contribute to each topic") +
  theme_minimal()+
  scale_fill_brewer(palette = "Dark2")



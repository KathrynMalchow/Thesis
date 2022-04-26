#Markets and Networks

pacman::p_load(quanteda, tidytext, tidyverse, dplyr, ggplot2, 
               tidyr, textmineR, topicmodels, seededlda, ggthemes, remotes,
               modelsummary, formattable, htmltools, webshot, simEd)

#Market_Networks_Final



############preprocess cleaning function####################

reviews_into_words = function(x){
  
  x = x[,c(1, 4)]
  
  mystopwords = tibble(word = c("app", "star", "fix" , "please", "farmers", "farm", "farmer", "bighaat", "kisan"), lexicon = "me") 
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


#run reviews_into_words function for mn
mn_char = reviews_into_words(Market_Networks_Final)

#tokenize
mn_tokens = tokens(mn_char,
                   remove_punct = TRUE,
                   remove_numbers = TRUE) %>%
  tokens_remove(stopwords()) %>%
  tokens_tolower() %>% 
  tokens_select(min_nchar = 1)

#create Document Feature Matrix
mn_dfm = dfm(mn_tokens)

#run lda
mn_lda = textmodel_lda(mn_dfm, k = 5)

terms(mn_lda, 10)

#most important topic for each review
mn_dfm$topic = topics(mn_lda)


#get top 10 most important terms for each topic 
ten_terms_mn = terms(mn_lda, 10)

#put together
ten_names_mn = apply(ten_terms_mn, 2, paste, collapse=", ")

#make into df
ten_topic_names_mn = bind_rows(ten_names_mn) %>%
  gather(topic, names)



###################
formattable(ten_topic_names_mn, align = c("l","r"), list(
  `Topic` = formatter("span",
                      style = x ~ ifelse(x == "topic1" | x == "topic2" | x == "topic3" | x == "topic4", style(font.weight = "bold"), NA))))


mn_tm_table_nostar = formattable(ten_topic_names_mn, align = c("l","r"), list(
  `Topic` = formatter("span",
                      style = x ~ ifelse(x == "topic1" | x == "topic2" | x == "topic3" | x == "topic4", style(font.weight = "bold"), NA)),
  `Terms` = formatter("span",
                      style = x ~ ifelse(x == "products, product, time, experience, service, quality, delivery, services, customer, team" 
                                         | x == "information, agriculture, application, helpful, agri, students, excellent, agricultural, related, knowledge" 
                                         | x == "market, awesome, prices, markets, cattle, local, helps, grain, time, close" | 
                                           x == "easy, love, user, friendly, lot, add, language, hindi, crashing, team", style(font.weight = "bold"), NA)
  )))
 #1st table idea, not using
###################

################
#export table function
export_formattable = function(f, file, width = "100%", height = NULL, 
                              background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

webshot::install_phantomjs()

export_formattable(mn_tm_table_nostar, "C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/Graphs_Tables/mn_table.png")
 #export table function, not using
###############

#######################

#most important terms for each topic
top_terms_mn = terms(mn_lda, 10)
topic_names_mn = apply(top_terms_mn, 2, paste, collapse=", ")

#extract prevalence of topics across all documents
topic_names_df_mn = bind_rows(topic_names_mn) %>%
  gather(topic, names)

topics_mn = as_tibble(mn_lda$theta) %>%
  mutate(document = rownames(.)) %>%
  gather(topic, gamma, -document) %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(topic_names_df_mn, by = "topic")
 #graph, not using
#########################


#######################
#for theme
#remotes::install_github("hrbrmstr/hrbrthemes")
#library(hrbrthemes)

#color palette
#farm_colors = c("#a47c48", "#99c140", "#ffce36", "#ee4035", "#fff2cc")
#green colors = c("#99c140","#4b752a", "#6f8f40", "#375225")

#plot
mn_chart = topics_mn %>%
  subset(topic == "topic1" | topic == "topic2" | topic == "topic3"| topic == "topic4") %>% 
  ggplot(aes(reorder(topic, gamma), gamma, label = names, fill =
               topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.30),
                     labels = scales::percent_format()) +
  labs(x = NULL, y = "Percent of Reviews",
       title = "Topic Prevalence",
       subtitle = "With the top words that contribute to each topic") +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_manual(values = c("#99c140","#4b752a", "#6f8f40", "#375225"))


ggsave(mn_chart, file="Analysis/Graphs_Tables/mn_graph.png") #graph, not using #graph, not using
########################

############################ stars and all gamma table

#make into tibble with prevalence (gamma)
all_topics_mn = as_tibble(mn_lda$theta) %>%
  mutate(document = rownames(.)) %>%
  gather(topic, gamma, -document) %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(ten_topic_names_mn, by = "topic")

#rename
colnames(all_topics_mn)[colnames(all_topics_mn) == 'names'] = 'Terms'
colnames(all_topics_mn)[colnames(all_topics_mn) == 'topic'] = 'Topic'


mn_tm_table_final = formattable(all_topics_mn, align = c("l","c", "r"), caption = "Market Place and Social Network", list(
  `Topic` = formatter("span",
                      style = x ~ ifelse(x == "topic3" | x == "topic4" | x == "topic5", style(font.weight = "bold"), NA),
                      x ~ icontext(ifelse(x == "topic3" | x == "topic4" | x == "topic5", "star", ""), x)),
  `Terms` = formatter("span",
                      style = x ~ ifelse(x == "products, product, time, service, experience, team, quality, delivery, services, customer" 
                                         | x == "update, search, equipment, time, load, location, error, version, list, lot" 
                                         | x == "information, application, agriculture, excellent, farming, informative, agri, students, helpful, knowledge",
                                         style(font.weight = "bold"), NA)
  )))



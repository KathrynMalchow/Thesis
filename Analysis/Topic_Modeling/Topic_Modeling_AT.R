#Advanced Technology

pacman::p_load(quanteda, tidytext, tidyverse, dplyr, ggplot2, 
               tidyr, textmineR, topicmodels, seededlda, ggthemes, remotes,
               modelsummary, formattable, htmltools, webshot)

#Advanced_Tech_Final

#####

############preprocess cleaning function####################

reviews_into_words = function(x){
  
  x = x[,c(1, 4)]
  
  mystopwords = tibble(word = c("app", "star", "fix" , "please", "farmers", "farm", "farmer", "bighaat"), lexicon = "me") 
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


#run reviews_into_words function for infoed
at_char = reviews_into_words(Advanced_Tech_Final)


#tokenize
at_tokens = tokens(at_char, 
                   remove_punct = TRUE,
                   remove_numbers = TRUE) %>%
  tokens_remove(stopwords()) %>%
  tokens_tolower() %>% 
  tokens_select(min_nchar = 1)


#create Document Feature Matrix
at_dfm = dfm(at_tokens)



#run lda
at_lda = textmodel_lda(at_dfm, k = 10)

terms(at_lda, 10)

#most important topic for each review
at_dfm$topic = topics(at_lda)


############## make table and graph - 1st idea ##########

#get top 10 most important terms for each topic 
ten_terms_at = terms(at_lda, 10)

#put together
ten_names_at = apply(ten_terms_at, 2, paste, collapse=", ")

#make into df
ten_topic_names_at = bind_rows(ten_names_at) %>%
  gather(topic, names)


#make into tibble with prevalence (gamma)
all_topics_at = as_tibble(at_lda$theta) %>%
  mutate(document = rownames(.)) %>%
  gather(topic, gamma, -document) %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(ten_topic_names_at, by = "topic")

#rename
colnames(all_topics_at)[colnames(all_topics_at) == 'names'] = 'Terms'
colnames(all_topics_at)[colnames(all_topics_at) == 'topic'] = 'Topic'

#save
write_csv(all_topics_at, 'Analysis/Topic_Modeling/all_topics_at.csv')
write_rds(all_topics_at, 'Analysis/Topic_Modeling/all_topics_at.rds')

#table
at_tm_table_final = formattable(all_topics_at, align = c("l","c", "r"), caption = "Advanced Technologies", list(
  `Topic` = formatter("span",
                      style = x ~ ifelse(x == "topic1" | x == "topic2" | x == "topic9", style(font.weight = "bold"), NA),
                      x ~ icontext(ifelse(x == "topic1" | x == "topic2" | x == "topic9", "star", ""), x)),
  `Terms` = formatter("span",
                      style = x ~ ifelse(x == "field, fields, track, data, save, maps, love, recommend, waste, note" 
                                         | x == "gps, phone, accurate, accuracy, free, love, map, garmin, close, external" 
                                         | x == "plant, plants, diseases, photo, disease, nice, identify, picture, time, application",
                                         style(font.weight = "bold"), NA))))




##########################
formattable(ten_topic_names_at, align = c("l","r"), list(
  `Topic` = formatter("span",
                      style = x ~ ifelse(x == "topic3" | x == "topic5" | x == "topic6" | x == "topic7", style(font.weight = "bold"), NA))))


at_tm_table_nostar = formattable(ten_topic_names_at, align = c("l","r"), list(
  `Topic` = formatter("span",
                      style = x ~ ifelse(x == "topic3" | x == "topic5" | x == "topic6" | x == "topic7", style(font.weight = "bold"), NA)),
  `Terms` = formatter("span",
                      style = x ~ ifelse(x == "field, gps, fields, phone, accurate, guidance, accuracy, garmin, day, fertilizer" 
                                         | x == "track, time, save, issues, recommend, lot, change, note, useless, version" 
                                         | x == "plant, plants, helpful, photo, disease, diseases, awesome, picture, identify, feature" | 
                                           x == "add, bad, option, hope, download, updated, properly, screen, aap, hours", style(font.weight = "bold"), NA)
  )))




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

export_formattable(at_tm_table_nostar, "C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/Graphs_Tables/at_table.png")


### making a graph

#most important terms for each topic
top_terms_at = terms(at_lda, 4)
topic_names_at = apply(top_terms_at, 2, paste, collapse=", ")

#extract prevalence of topics across all documents
topic_names_df_at = bind_rows(topic_names_at) %>%
  gather(topic, names)

topics_at = as_tibble(at_lda$theta) %>%
  mutate(document = rownames(.)) %>%
  gather(topic, gamma, -document) %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(topic_names_df_at, by = "topic")


#for theme
#remotes::install_github("hrbrmstr/hrbrthemes")
#library(hrbrthemes)

#color palette
#farm_colors = c("#a47c48", "#99c140", "#ffce36", "#ee4035", "#fff2cc")
#green colors = c("#99c140","#4b752a", "#6f8f40", "#375225")


#plot
at_chart = topics_at %>%
  subset(topic == "topic3" | topic == "topic5" | topic == "topic6"| topic == "topic7") %>% 
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





ggsave(at_chart, file="Analysis/Graphs_Tables/at_graph.png")



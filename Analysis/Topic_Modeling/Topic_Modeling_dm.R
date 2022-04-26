#topic modeling 2

pacman::p_load(quanteda, tidytext, tidyverse, dplyr, ggplot2, 
               tidyr, textmineR, topicmodels, seededlda, ggthemes, remotes,
                modelsummary, formattable, htmltools, webshot)


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

#tokenize
dm_tokens = tokens(dm_char,
                   remove_punct = TRUE,
                   remove_numbers = TRUE) %>%
  tokens_remove(stopwords()) %>%
  tokens_tolower() %>% 
  tokens_select(min_nchar = 1)

#create Document Feature Matrix
dm_dfm = dfm(dm_tokens)

#run lda
dm_lda = textmodel_lda(dm_dfm, k = 15)



#most important topic for each review
dm_dfm$topic = topics(dm_lda)


############## make table and graph - 1st idea ##########

#get top 10 most important terms for each topic 
ten_terms = terms(dm_lda, 10)

#put together
ten_names = apply(ten_terms, 2, paste, collapse=", ")

#make into df
ten_topic_names_df = bind_rows(ten_names) %>%
  gather(topic, names)

#rename
colnames(ten_topic_names_df)[colnames(ten_topic_names_df) == 'names'] = 'Terms'
colnames(ten_topic_names_df)[colnames(ten_topic_names_df) == 'topic'] = 'Topic'

#table
dm_tm_table = formattable(ten_topic_names_df, align = c("l","r"), list(
  `Topic` = formatter("span",
      style = x ~ ifelse(x == "topic1" | x == "topic5" | x == "topic7" | x == "topic13", style(font.weight = "bold"), NA),
      x ~ icontext(ifelse(x == "topic1" | x == "topic5" | x == "topic7" | x == "topic13", "star", ""), x)),
  `Terms` = formatter("span",
                      style = x ~ ifelse(x == "phone, accept, screen, terms, button, android, past, totals, time, acres" 
                              | x == "free, data, version, money, month, pay, tracking, worth, found, activities" 
                              | x == "easy, farm, keeping, record, makes, farming, easier, tool, recommend, track" | 
                               x == "accurate, measure, easy, measuring, features, gps, feature, measurement, ads, distance", style(font.weight = "bold"), NA)
)))


dm_tm_table_nostar = formattable(ten_topic_names_df, align = c("l","r"), list(
  `Topic` = formatter("span",
                      style = x ~ ifelse(x == "topic1" | x == "topic5" | x == "topic7" | x == "topic13", style(font.weight = "bold"), NA)),
  `Terms` = formatter("span",
                      style = x ~ ifelse(x == "phone, accept, screen, terms, button, android, past, totals, time, acres" 
                                         | x == "free, data, version, money, month, pay, tracking, worth, found, activities" 
                                         | x == "easy, farm, keeping, record, makes, farming, easier, tool, recommend, track" | 
                                           x == "accurate, measure, easy, measuring, features, gps, feature, measurement, ads, distance", style(font.weight = "bold"), NA)
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
             
export_formattable(dm_tm_table, "C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Graphs_Tables/dm_table.png")


### making a graph

#most important terms for each topic
top_terms = terms(dm_lda, 4)
topic_names = apply(top_terms, 2, paste, collapse=", ")

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
#farm_colors = c("#a47c48", "#99c140", "#ffce36", "#ee4035", "#fff2cc")
#green colors = c("#99c140","#4b752a", "#6f8f40", "#375225")


#plot
dm_chart = topics_dm %>%
  subset(topic == "topic7" | topic == "topic5" | topic == "topic1"| topic == "topic13") %>% 
  ggplot(aes(reorder(topic, gamma), gamma, label = names, fill =
               topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.15),
                     labels = scales::percent_format()) +
  labs(x = NULL, y = "Percent of Reviews",
       title = "Topic Prevalence",
       subtitle = "With the top words that contribute to each topic") +
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  scale_fill_manual(values = c("#99c140","#4b752a", "#6f8f40", "#375225"))


ggsave(dm_chart, file="Analysis/Graphs_Tables/dm_graph.png")








############### 2nd table idea - with gamma ################
#make into tibble with prevalence (gamma)
all_topics_dm = as_tibble(dm_lda$theta) %>%
  mutate(document = rownames(.)) %>%
  gather(topic, gamma, -document) %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(ten_topic_names_df, by = "topic")

#rename
colnames(all_topics_dm)[colnames(all_topics_dm) == 'names'] = 'Terms'
colnames(all_topics_dm)[colnames(all_topics_dm) == 'topic'] = 'Topic'




#table2
formattable(all_topics_dm, align = c("l","c", "r"),  caption = "Data and Management", list(
  gamma = color_tile("transparent", "pink")
))


############################ stars and all gamma table
dm_tm_table_final = formattable(all_topics_dm, align = c("l","c", "r"), caption = "Data and Management", list(
  `Topic` = formatter("span",
                      style = x ~ ifelse(x == "topic1" | x == "topic5" | x == "topic7" | x == "topic13", style(font.weight = "bold"), NA),
                      x ~ icontext(ifelse(x == "topic1" | x == "topic5" | x == "topic7" | x == "topic13", "star", ""), x)),
  `Terms` = formatter("span",
                      style = x ~ ifelse(x == "phone, accept, screen, terms, button, android, past, totals, time, acres" 
                                         | x == "free, data, version, money, month, pay, tracking, worth, found, activities" 
                                         | x == "easy, farm, keeping, record, makes, farming, easier, tool, recommend, track" | 
                                           x == "accurate, measure, easy, measuring, features, gps, feature, measurement, ads, distance", style(font.weight = "bold"), NA)
  )))











#Topic Modeling Information and Education

pacman::p_load(quanteda, tidytext, tidyverse, dplyr, ggplot2, 
               tidyr, textmineR, topicmodels, seededlda, ggthemes, remotes,
               modelsummary, formattable, htmltools, webshot)

#Information_Education_Final

#####

############preprocess cleaning function####################

reviews_into_words = function(x){
  
  x = x[,c(1, 4)]
  
  mystopwords = tibble(word = c("app", "star", "stars", "fix" , "please", "game", "kisan", 
                                "bharat", "farmers", "farmer", "indian", "agriculture", "farming"), lexicon = "me") 
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
infoed_char = reviews_into_words(Information_Education_Final)

#tokenize
ie_tokens = tokens(infoed_char,
                   remove_punct = TRUE,
                   remove_numbers = TRUE) %>%
  tokens_remove(stopwords()) %>%
  tokens_tolower() %>% 
  tokens_select(min_nchar = 1)

#create Document Feature Matrix
infoed_dfm = dfm(ie_tokens)

#run lda
ie_lda = textmodel_lda(infoed_dfm, k = 16)

terms(ie_lda, 10)

#most important topic for each review
infoed_dfm$topic = topics(ie_lda)

#get top 10 most important terms for each topic 
ten_terms_ie = terms(ie_lda, 10)

#put together
ten_names_ie = apply(ten_terms_ie, 2, paste, collapse=", ")

#make into df
ten_topic_names_ie = bind_rows(ten_names_ie) %>%
  gather(topic, names)


#make into tibble with prevalence (gamma)
all_topics_ie = as_tibble(ie_lda$theta) %>%
  mutate(document = rownames(.)) %>%
  gather(topic, gamma, -document) %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(ten_topic_names_ie, by = "topic")

#rename
colnames(all_topics_ie)[colnames(all_topics_ie) == 'names'] = 'Terms'
colnames(all_topics_ie)[colnames(all_topics_ie) == 'topic'] = 'Topic'

#save
write_csv(all_topics_ie, 'Analysis/Topic_Modeling/all_topics_ie.csv')
write_rds(all_topics_ie, 'Analysis/Topic_Modeling/all_topics_ie.rds')

#remove row 13 (topic6) due to translation error
all_topics_ie = slice(all_topics_ie, -c(13))

ie_tm_table_final = formattable(all_topics_ie, align = c("l","c", "r"), caption = "Information and Education", list(
  `Topic` = formatter("span",
                      style = x ~ ifelse(x == "topic4" | x == "topic5" | x == "topic16", style(font.weight = "bold"), NA),
                      x ~ icontext(ifelse(x == "topic4" | x == "topic5" | x == "topic16", "star", ""), x)),
  `Terms` = formatter("span",
                      style = x ~ ifelse(x == "crop, service, weather, advisory, india, smart, accurate, knowledge, prediction, purchases" 
                                         | x == "application, nice, easy, data, language, superb, english, hindi, malayalam, gujarati" 
                                         | x == "knowledge, experience, agrimedia, found, features, weeds, identify, product, wonderful, quickly",
                                         style(font.weight = "bold"), NA))))




############## make table and graph - 1st idea ##########


#table
ie_tm_table = formattable(ten_topic_names_ie, align = c("l","r"), list(
  `Topic` = formatter("span",
                      style = x ~ ifelse(x == "topic10" | x == "topic21" | x == "topic16" | x == "topic2", style(font.weight = "bold"), NA),
                      x ~ icontext(ifelse(x == "topic10" | x == "topic21" | x == "topic16" | x == "topic2", "star", ""), x)),
  `Terms` = formatter("span",
                      style = x ~ ifelse(x == "phone, accept, screen, terms, button, android, past, totals, time, acres" 
                                         | x == "free, data, version, money, month, pay, tracking, worth, found, activities" 
                                         | x == "easy, farm, keeping, record, makes, farming, easier, tool, recommend, track" | 
                                           x == "accurate, measure, easy, measuring, features, gps, feature, measurement, ads, distance", style(font.weight = "bold"), NA)
  )))


ie_tm_table_nostar = formattable(ten_topic_names_ie, align = c("l","r"), list(
  `Topic` = formatter("span",
                      style = x ~ ifelse(x == "topic10" | x == "topic21" | x == "topic16" | x == "topic2", style(font.weight = "bold"), NA)),
  `Terms` = formatter("span",
                      style = x ~ ifelse(x == "data, satellite, awesome, informative, usefull, guidance, project, crops, production, view" 
                                         | x == "helpful, agricultural, language, agriculture, english, aap, hindi, gujarati, agrimedia, simple" 
                                         | x == "crop, agriculture, weather, farmers, feature, related, accurate, advisory, india, prediction" | 
                                           x == "information, features, weeds, knowledge, found, identify, quickly, farming, sheep, technologies", style(font.weight = "bold"), NA)
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

export_formattable(ie_tm_table_nostar, "C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/Graphs_Tables/ie_table.png")


### making a graph

#most important terms for each topic
top_terms_ie = terms(ie_lda, 6)
topic_names_ie = apply(top_terms_ie, 2, paste, collapse=", ")

#extract prevalence of topics across all documents
topic_names_df_ie = bind_rows(topic_names_ie) %>%
  gather(topic, names)

topics_ie = as_tibble(ie_lda$theta) %>%
  mutate(document = rownames(.)) %>%
  gather(topic, gamma, -document) %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(topic_names_df_ie, by = "topic")


#for theme
remotes::install_github("hrbrmstr/hrbrthemes")
library(hrbrthemes)

#color palette
#farm_colors = c("#a47c48", "#99c140", "#ffce36", "#ee4035", "#fff2cc")
#green colors = c("#99c140","#4b752a", "#6f8f40", "#375225")


#plot
ie_chart = topics_ie %>%
  subset(topic == "topic2" | topic == "topic10" | topic == "topic16"| topic == "topic21") %>% 
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


ggsave(ie_chart, file="Analysis/Graphs_Tables/ie_graph.png")



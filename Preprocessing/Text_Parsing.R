#text preprocessing for feature extraction

pacman::p_load(quanteda, spacyr, reticulate, tidytext, tidyverse)


#set up spacyr

#spacy_install()

spacy_initialize()


#parse text and preprocess function

parse_fun = function(df){
  corp = corpus(df$reviews, 
                docid_field = "",
                text_field = "reviews")
  
  parsed = spacy_parse(
    corp,
    pos = TRUE,
    tag = FALSE,
    lemma = TRUE,
    entity = FALSE,
    dependency = FALSE,
    nounphrase = FALSE,
    multithread = FALSE,
    additional_attributes = NULL)
  
  mystopwords = tibble(word = c("app", "star", "fix" , "please"), lexicon = "me") 
  allstopwords = stop_words %>%
    bind_rows(mystopwords)
  
  parsed[parsed$pos %in% c("NOUN", "VERB", "ADJ"), ] %>%
    anti_join(allstopwords, by= c("token" = "word"))
}


#use function - Data Management
dm_parsed = parse_fun(tib_DataManagement)

#save df
saveRDS(dm_parsed, 'Preprocessing/Parsed_dfs/dm_parsed.RDS')
write_csv(dm_parsed, 'Preprocessing/Parsed_dfs/dm_parsed.csv')



#use function - Information/ Education
ie_parsed = parse_fun(Information_Education_Final)
###########remove "farmer" from list
ie_stopwords = tibble(word = c("farmer", "farming", "farmers", "Good", "good"), lexicon = "ie")

ie_parsed = ie_parsed %>% 
  anti_join(ie_stopwords, by= c("token" = "word"))

#save df
saveRDS(ie_parsed, 'Preprocessing/Parsed_dfs/ie_parsed.RDS')
write_csv(ie_parsed, 'Preprocessing/Parsed_dfs/ie_parsed.csv')





#use function - Market/ Networks
mn_parsed = parse_fun(Market_Networks_Final)

mn_stopwords = tibble(word = c("farmer", "farming", "farmers", "application"), lexicon = "mn")
mn_parsed = mn_parsed %>% 
  anti_join(mn_stopwords, by= c("token" = "word"))

#save df
saveRDS(mn_parsed, 'Preprocessing/Parsed_dfs/mn_parsed.RDS')
write_csv(mn_parsed, 'Preprocessing/Parsed_dfs/mn_parsed.csv')




#use function - Advanced Technology
at_parsed = parse_fun(Advanced_Tech_Final)

at_stopwords = tibble(word = c("farmer", "farming", "farmers"), lexicon = "at")
at_parsed = at_parsed %>% 
  anti_join(at_stopwords, by= c("token" = "word"))


#save df
saveRDS(at_parsed, 'Preprocessing/Parsed_dfs/at_parsed.RDS')
write_csv(at_parsed, 'Preprocessing/Parsed_dfs/at_parsed.csv')


spacy_finalize()














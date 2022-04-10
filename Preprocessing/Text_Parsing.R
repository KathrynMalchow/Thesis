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

#use function - Information/ Education
ie_parsed = parse_fun(Information_Education_Final)

#use function - Market/ Networks
mn_parsed = parse_fun(Market_Networks_Final)

#use function - Advanced Technology
at_parsed = parse_fun(Advanced_Tech_Final)

spacy_finalize()














#text preprocessing for feature extraction - Data and Management

pacman::p_load(quanteda, spacyr, reticulate)


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
  
  parsed[parsed$pos %in% c("NOUN", "VERB", "ADJ"), ] %>% 
    as.tokens(., use_lemma = T) %>% 
    tokens_remove(stopwords()) %>% 
    dfm
}


#use function
dm_dfm = parse_fun(tib_DataManagement)

  

spacy_finalize()














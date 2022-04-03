#text preprocessing for feature extraction - Data and Management

pacman::p_load(quanteda, spacyr, reticulate)

#make DM tibble into corpus

corp_DM = corpus(tib_DataManagement$reviews, 
                 docid_field = "",
                 text_field = "reviews")
#set up spacyr

#spacy_install()

spacy_initialize()


#parse text
DM_parsed = spacy_parse(
  corp_DM,
  pos = TRUE,
  tag = FALSE,
  lemma = TRUE,
  entity = FALSE,
  dependency = FALSE,
  nounphrase = FALSE,
  multithread = FALSE,
  additional_attributes = NULL) 

DM_parsed[DM_parsed$pos %in% c("NOUN", "VERB", "ADJ"), ]

dm_token_test = DM_parsed[DM_parsed$pos %in% c("NOUN", "VERB", "ADJ"), ] %>% 
  as.tokens(., use_lemma = T) %>% 
  tokens_remove(stopwords()) %>% 
  dfm
  

spacy_finalize()



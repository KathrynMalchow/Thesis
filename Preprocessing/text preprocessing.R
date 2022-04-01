#text preprocessing for feature extraction - Data and Management

pacman::p_load(quanteda, spacyr, remotes)

#Parsing the text (POS) and extracting noun, verbs, and adjectives

###First, have to make it TIF-compliant (text interchange formats) data.frame

#install_github("ropensci/tif")

#corp_dm = data.frame(doc_id = "tib_DataManagement",
                     #stringsAsFactors = FALSE)

#tif_is_corpus_df(corp_dm)

#maybe this works

corp_DM = corpus(tib_DataManagement$reviews, 
                 docid_field = "",
                 text_field = "reviews")
#set up spacyr
spacy_install()
spacy_initialize()

#parse text
spacy_parse(
  corp_DM,
  pos = TRUE,
  tag = FALSE,
  lemma = TRUE,
  entity = FALSE,
  dependency = FALSE,
  nounphrase = TRUE,
  multithread = FALSE,
  additional_attributes = NULL)


spacy_finalize()



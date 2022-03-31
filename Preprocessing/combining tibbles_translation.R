pacman::p_load(quanteda, tidyverse, dplyr, cld2, deeplr, DSSAT, remotes, translater)

#Combine Tibbles

list_DataManagement = list(Canopeo, FarmersWallet, farmlogs, FarmManagementPro, FertilizerRemoval, fieldmargin, GPS_FieldsArea_Measure, Grazing_Calculator, LandPKS, MyCropManager, SoilSampler, Tank_Mix_Calculator) 
tib_DataManagement = reduce(list_DataManagement, bind_rows)

list_InfoEduc = list(Agri_Farming, AgriApp, AgriMedia_TV, BharatAgri, Extension_Manager, Farming_Solution, IDWeeds, LandPKS, PlantSat, SoilWeb, Yara_CheckIT)
tib_InfoEduc = reduce(list_InfoEduc, bind_rows)
  
list_marketNetworks = list(AgMobile, AgriMedia_TV, AgriSetu, BigHaat, Cattle_Market, EzyAgric, Farmpost, Kisaan_Suvidha, Tractor_Zoom, TractorHouse) 
tib_marketNetworks = reduce(list_marketNetworks, bind_rows)

list_AdvTech = list(AgriBus, Agrio, BigHaat, Field_Navigator, FieldBee, OneSoil, Plantix, SCOUTING, xarvio)
tib_AdvTech = reduce(list_AdvTech, bind_rows)

###Translating

#Identifying rows with languages other than English

detected_language_DM = tib_DataManagement$reviews %>% 
  sapply(., map_chr, detect_language) %>% 
  data.frame(check.names = FALSE) %>% 
  subset(. != "en")

detected_language_Info = tib_InfoEduc$reviews %>% 
  sapply(., map_chr, detect_language) %>% 
  data.frame(check.names = FALSE) %>% 
  subset(. != "en")

detected_language_MN = tib_marketNetworks$reviews %>% 
  sapply(., map_chr, detect_language) %>% 
  data.frame(check.names = FALSE) %>% 
  subset(. != "en")

detected_language_AT = tib_AdvTech$reviews %>% 
  sapply(., map_chr, detect_language) %>% 
  data.frame(check.names = FALSE) %>% 
  subset(. != "en")



#translating selected languages into English with DeepL API
install_github("paulcbauer/deeplr")


AT_Final = tib_AdvTech %>% 
  mutate_cond(reviews_trans =  tib_AdvTech$reviews[c(80,  326, 327, 329, 364, 367, 372, 377, 421, 423, 436, 439, 440, 704, 705, 706)],
  translate(tib_AdvTech$reviews,
  target_lang = "EN",
  source_lang = NULL,
  split_sentences = FALSE,
  preserve_formatting = TRUE,
  get_detect = FALSE,
  auth_key = "946600e0-4915-33c2-98d0-6e5c33eda626"))

install_github("ChristopherLucas/translateR")
library(translateR)


test = translate(dataset = tib_AdvTech,
          content.field = "Reviews",
          microsoft.client.id = "1202a93a-a5a1-48f6-a219-b1a40e9e80d4",
          microsoft.client.secret = "044290fb8ff54bb1ba8827d71f082041",
          source.lang = "hi",
          target.lang = "en")



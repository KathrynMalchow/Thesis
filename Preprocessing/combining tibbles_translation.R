pacman::p_load(quanteda, tidyverse, dplyr, cld2, deeplr, DSSAT, remotes, purrr)

#Combine Tibbles and save

list_DataManagement = list(Canopeo, FarmersWallet, farmlogs, FarmManagementPro, FertilizerRemoval, fieldmargin, GPS_FieldsArea_Measure, Grazing_Calculator, LandPKS, MyCropManager, SoilSampler, Tank_Mix_Calculator) 
tib_DataManagement = reduce(list_DataManagement, bind_rows)


list_InfoEduc = list(Agri_Farming, AgriApp, AgriMedia_TV, BharatAgri, Extension_Manager, Farming_Solution, IDWeeds, LandPKS, PlantSat, SoilWeb, Yara_CheckIT)
tib_InfoEduc = reduce(list_InfoEduc, bind_rows)

saveRDS(tib_InfoEduc, 'Objects/Information_Education/tib_InfoEduc.RDS')
write_csv(tib_InfoEduc, 'data/Information_Education/tib_InfoEduc.csv')
  
list_marketNetworks = list(AgMobile, AgriMedia_TV, AgriSetu, BigHaat, Cattle_Market, EzyAgric, Farmpost, Kisaan_Suvidha, Tractor_Zoom, TractorHouse) 
tib_marketNetworks = reduce(list_marketNetworks, bind_rows)

saveRDS(tib_marketNetworks, 'Objects/Markets_Social_Networks/tib_marketNetworks.RDS')
write_csv(tib_marketNetworks, 'data/Markets_Social_Networks/tib_marketNetworks.csv')

list_AdvTech = list(AgriBus, Agrio, BigHaat, Field_Navigator, FieldBee, OneSoil, Plantix, SCOUTING, xarvio)
tib_AdvTech = reduce(list_AdvTech, bind_rows)

saveRDS(tib_AdvTech, 'Objects/Advanced_Tech/tib_AdvTech.RDS')
write_csv(tib_AdvTech, 'data/Advanced_Tech/tib_AdvTech.csv')

write.table(tib_AdvTech, file = "tib_AdvTech.txt", sep = ",",
            row.names = FALSE, col.names = TRUE)


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

#manual translate for data management
tib_DataManagement$reviews[which(tib_DataManagement$reviews == "Giellies Dis vrek kwaai")] <- "Giellies It's damn bad"
tib_DataManagement$reviews[which(tib_DataManagement$reviews == "ek dink dit is baie help pende app die love it dit werk cool sal dit meer gebruik.")] <- "i think it's very helpful pende app the love it it works cool will use it more."

#save final version
saveRDS(tib_DataManagement, 'Objects/Data_Collection_Management/tib_DataManagement.RDS')
write_csv(tib_DataManagement, 'data/Data_Collection_Management/tib_DataManagement.csv')


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


#with TranslateR
install_github("ChristopherLucas/translateR")
library(translateR)


test = translate(content.vec = tib_AdvTech$reviews,
          microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
          source.lang = "mr",
          target.lang = "en")

AT_test = tib_AdvTech %>% 
  mutate_cond(reviews_trans =  tib_AdvTech$reviews[c(80,  326, 327, 329, 364, 367, 372, 377, 421, 423, 436, 439, 440, 704, 705, 706)],
              translate(tib_AdvTech$reviews,
                        target_lang = "EN",
                        source_lang = NULL,
                        split_sentences = FALSE,
                        preserve_formatting = TRUE,
                        get_detect = FALSE,
                        auth_key = "946600e0-4915-33c2-98d0-6e5c33eda626"))

#adding language specification to dfs
language_AT = tib_AdvTech %>% 
  mutate(lang = tib_AdvTech$reviews %>% 
  sapply(., map_chr, detect_language) %>% 
  data.frame(check.names = FALSE))

translate_func = function(reviews){
  if(lang == "en"){
          next
  }else if (lang == "NA"){
          next
  }else if (lang == "ar"){
    translate(content.vec = tib_AdvTech$reviews,
             microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
             source.lang = c("ar"),
             target.lang = "en")}
}

mapply(X = language_AT$lang, FUN = translate_func)


##try a function
for (i in 1:ncol(tib_AdvTech)){
  tib_AdvTech[,i][detected_language_AT[,i] == "ar"] =
  data.frame(translate(tib_AdvTech[,i][detected_language_AT[,i]], 
          microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
          source.lang = c("ar"),
          target.lang = "en"))
}



trans_fun = function(dataframe, x){
  dataframe %>% 
  mutate(source_lang = x %>% 
    sapply(., map_chr, detect_language) %>% 
    data.frame(check.names = FALSE)) %>%  #works
if (dataframe$source_lang == "en"){
    return ("yes")
}else {
  return ("nope")}
}

 # next
  #}else {replace(source.lang, with(api_func(.)))}



language_AT = tib_AdvTech %>% 
  mutate(lang = tib_AdvTech$reviews %>% 
           sapply(., map_chr, detect_language) %>% 
           data.frame(check.names = FALSE))

tib_test = tib_AdvTech

testtt = trans_fun(tib_test,tib_test$reviews)


api_func = function(source.lang){
  translate(content.vec = df,
  microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
  source.lang = source.lang,
  target.lang = "en")
}



#str_replace(string, pattern, replacement)


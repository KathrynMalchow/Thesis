pacman::p_load(quanteda, tidyverse, dplyr, cld2, deeplr, DSSAT, remotes, purrr)

#Combine Tibbles and save

############## Data and Management

list_DataManagement = list(Canopeo, FarmersWallet, farmlogs, FarmManagementPro, FertilizerRemoval, fieldmargin, GPS_FieldsArea_Measure, Grazing_Calculator, LandPKS, MyCropManager, SoilSampler, Tank_Mix_Calculator) 
tib_DataManagement = reduce(list_DataManagement, bind_rows)

#should actually save after translation, see below

#################### Information and Education

list_InfoEduc = list(Agri_Farming, AgriApp, AgriMedia_TV, BharatAgri, Extension_Manager, Farming_Solution, IDWeeds, LandPKS, PlantSat, SoilWeb, Yara_CheckIT)
tib_InfoEduc = reduce(list_InfoEduc, bind_rows)

saveRDS(tib_InfoEduc, 'Objects/Information_Education/tib_InfoEduc.RDS')
write_csv(tib_InfoEduc, 'data/Information_Education/tib_InfoEduc.csv')

#################### Markets and Networks  
list_marketNetworks = list(AgMobile, AgriMedia_TV, AgriSetu, BigHaat, Cattle_Market, EzyAgric, Farmpost, Kisaan_Suvidha, Tractor_Zoom, TractorHouse) 
tib_marketNetworks = reduce(list_marketNetworks, bind_rows)

saveRDS(tib_marketNetworks, 'Objects/Markets_Social_Networks/tib_marketNetworks.RDS')
write_csv(tib_marketNetworks, 'data/Markets_Social_Networks/tib_marketNetworks.csv')

#################### Advanced Technology
list_AdvTech = list(AgriBus, Agrio, BigHaat, Field_Navigator, FieldBee, OneSoil, Plantix, SCOUTING, xarvio)
tib_AdvTech = reduce(list_AdvTech, bind_rows)

saveRDS(tib_AdvTech, 'Objects/Advanced_Tech/tib_AdvTech.RDS')
write_csv(tib_AdvTech, 'data/Advanced_Tech/tib_AdvTech.csv')



###Translating

#Identifying rows with languages other than English (just for checking purposes)

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

# manual translation for data management
tib_DataManagement$reviews[which(tib_DataManagement$reviews == "Giellies Dis vrek kwaai")] <- "Giellies It's damn bad"
tib_DataManagement$reviews[which(tib_DataManagement$reviews == "ek dink dit is baie help pende app die love it dit werk cool sal dit meer gebruik.")] <- "i think it's very helpful pende app the love it it works cool will use it more."

# save final version of data management
saveRDS(tib_DataManagement, 'Objects/Data_Collection_Management/tib_DataManagement.RDS')
write_csv(tib_DataManagement, 'data/Data_Collection_Management/tib_DataManagement.csv')


##################################### non manual translations


#with TranslateR
install_github("ChristopherLucas/translateR")
library(translateR)

#AdvTech Reviews to translate [c(80,  326, 327, 329, 364, 367, 372, 377, 421, 423, 436, 439, 440, 704, 705, 706)]

##############################functions for identifying non English languages

#works
trans_fun = function(df, x){
  df %>% 
  mutate(source_lang = x %>% 
    sapply(., map_chr, detect_language) %>% 
    data.frame(check.names = FALSE))  
}
 
#doesn't recognize Im trying to call column "source_lang" for my source language, because it can only do one language at a time, need to use loop?
api_fun = function(transdf_source_lang, source_lang){
  translate(content.vec = transdf_source_lang,
            microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
            source.lang = "source_lang", #does not work
            target.lang = "en")
}


#loop


test_loop = for (i in test_f1$source_lang) {
  test_f1$source_lang != "en" & !is.na(test_f1$source_lang) = 
    data.frame(translate(dataset = test_f1, content.field = reviews, microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                         source.lang = test_f1$source_lang,
                         target.lang = "en"))
}







#######################test dfs wih AdvTech

tib_test = tib_AdvTech
test_f1 = trans_fun(tib_test,tib_test$reviews)
test_f2 = api_fun(test_f1$source_lang, source_lang)

########################






#Translation Part 2
pacman::p_load(quanteda, tidyverse, dplyr, cld2, deeplr, DSSAT, remotes, purrr, translateR, plyr)

tib_test = tib_AdvTech
test_f1 = trans_fun(tib_test,tib_test$reviews)
test_f2 = api_fun(test_f1$source_lang, source_lang)


detect_fun = function(df, x){
  df %>% 
    mutate(source_lang = x %>% 
             sapply(., map_chr, detect_language) %>% 
             data.frame(check.names = FALSE)) 
}


detected_language_AT = tib_AdvTech$reviews %>% 
  sapply(., map_chr, detect_language) %>% 
  data.frame(check.names = FALSE)

write.table(tib_AdvTech[4], file = "Preprocessing/Translation Txts/AdvTech.txt", sep = "\t",
            row.names = FALSE, col.names = TRUE)


#326, 327, 329, 364, 367, 372, 377, 421, 423, 436, 439, 440, 704, 705, 706

############hmmm

translate = function(content.vec, source){
  x = translate(content.vec = content.vec,
            microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
            source.lang = source, #does not work
            target.lang = "en")
}

translate(tib_test$reviews[c(327)], ro)



x = translateR::translate(content.vec = tib_test$reviews[c(326)],
              microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
              source.lang = "ro"
              target.lang = "en")

for (reviews in tib_AdvTech){
  tib_AdvTech[,reviews][detected_language_AT[,4]== "ro" & !is.na(detected_language_AT[,4])]<-
  data.frame(translateR::translate(tib_AdvTech[,reviews][detected_language_AT[,4]=="ro" & !is.na(detected_language_AT[,4])], content.vec = tib_test$reviews, microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041", source.lang = "ro", target = "en"))[,1]
}


  






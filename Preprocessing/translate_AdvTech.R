#Advanced Technology - only keeps the last translation
pacman::p_load(quanteda, tidyverse, dplyr, cld2, deeplr, DSSAT, remotes, purrr, translateR, plyr)

detected_language_AT %>% 
  unique()

#languages:
# ar - NA
# ro
# pt
# es
# lt
# mr
# ru
# uk
# de
# kn
# hr

################ ro

detected_language_AT %>% 
  subset(. == "ro")

#[c(326)]

advtech_ro = translateR::translate(content.vec = tib_AdvTech$reviews[c(326)], 
                              microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                              source.lang = "ro",
                              target.lang = "en")

advtech_ro_df = as.data.frame(advtech_ro)

rownames(advtech_ro_df) <- c(326)

advtech_df = merge(tib_AdvTech, advtech_ro_df, by=0, all=TRUE)

advtech_df$reviews <- ifelse(is.na(advtech_df$advtech_ro), advtech_df$reviews, advtech_df$advtech_ro)
advtech_df$advtech_ro <- NULL

rownames(advtech_df) <- advtech_df[,1]
advtech_df[,1] <- NULL


################ pt

detected_language_AT %>% 
  subset(. == "pt")

#[c(327)]


advtech_pt = translateR::translate(content.vec = tib_AdvTech$reviews[c(327)], 
                                   microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                                   source.lang = "pt",
                                   target.lang = "en")

advtech_pt_df = as.data.frame(advtech_pt)

rownames(advtech_pt_df) <- c(327)

advtech_df = merge(tib_AdvTech, advtech_pt_df, by=0, all=TRUE)

advtech_df$reviews <- ifelse(is.na(advtech_df$advtech_pt), advtech_df$reviews, advtech_df$advtech_pt)
advtech_df$advtech_pt <- NULL

rownames(advtech_df) <- advtech_df[,1]
advtech_df[,1] <- NULL



################ es
detected_language_AT %>% 
  subset(. == "es")

#[c(329, 367, 423, 436, 439)]


advtech_es = translateR::translate(content.vec = tib_AdvTech$reviews[c(329, 367, 423, 436, 439)], 
                                   microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                                   source.lang = "es",
                                   target.lang = "en")

advtech_es_df = as.data.frame(advtech_es)

rownames(advtech_es_df) <- c(329, 367, 423, 436, 439)

advtech_df = merge(tib_AdvTech, advtech_es_df, by=0, all=TRUE)

advtech_df$reviews <- ifelse(is.na(advtech_df$advtech_es), advtech_df$reviews, advtech_df$advtech_es)
advtech_df$advtech_es <- NULL

rownames(advtech_df) <- advtech_df[,1]
advtech_df[,1] <- NULL



################ lt
detected_language_AT %>% 
  subset(. == "lt")

#[c(364)]

advtech_lt = translateR::translate(content.vec = tib_AdvTech$reviews[c(364)], 
                                   microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                                   source.lang = "lt",
                                   target.lang = "en")

advtech_lt_df = as.data.frame(advtech_lt)

rownames(advtech_lt_df) <- c(364)

advtech_df = merge(tib_AdvTech, advtech_lt_df, by=0, all=TRUE)

advtech_df$reviews <- ifelse(is.na(advtech_df$advtech_lt), advtech_df$reviews, advtech_df$advtech_lt)
advtech_df$advtech_lt <- NULL

rownames(advtech_df) <- advtech_df[,1]
advtech_df[,1] <- NULL



################ mr
detected_language_AT %>% 
  subset(. == "mr")

#[c(372, 377)]


advtech_mr = translateR::translate(content.vec = tib_AdvTech$reviews[c(372, 377)], 
                                   microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                                   source.lang = "mr",
                                   target.lang = "en")

advtech_mr_df = as.data.frame(advtech_mr)

rownames(advtech_mr_df) <- c(372, 377)

advtech_df = merge(tib_AdvTech, advtech_mr_df, by=0, all=TRUE)

advtech_df$reviews <- ifelse(is.na(advtech_df$advtech_mr), advtech_df$reviews, advtech_df$advtech_mr)
advtech_df$advtech_mr <- NULL

rownames(advtech_df) <- advtech_df[,1]
advtech_df[,1] <- NULL



################ ru
detected_language_AT %>% 
  subset(. == "ru")

#[c(421)]


advtech_ru = translateR::translate(content.vec = tib_AdvTech$reviews[c(421)], 
                                   microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                                   source.lang = "ru",
                                   target.lang = "en")

advtech_ru_df = as.data.frame(advtech_ru)

rownames(advtech_ru_df) <- c(421)

advtech_df = merge(tib_AdvTech, advtech_ru_df, by=0, all=TRUE)

advtech_df$reviews <- ifelse(is.na(advtech_df$advtech_ru), advtech_df$reviews, advtech_df$advtech_ru)
advtech_df$advtech_ru <- NULL

rownames(advtech_df) <- advtech_df[,1]
advtech_df[,1] <- NULL



################ uk
detected_language_AT %>% 
  subset(. == "uk")

#[c(440)]


advtech_uk = translateR::translate(content.vec = tib_AdvTech$reviews[c(440)], 
                                   microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                                   source.lang = "uk",
                                   target.lang = "en")

advtech_uk_df = as.data.frame(advtech_uk)

rownames(advtech_uk_df) <- c(440)

advtech_df = merge(tib_AdvTech, advtech_uk_df, by=0, all=TRUE)

advtech_df$reviews <- ifelse(is.na(advtech_df$advtech_uk), advtech_df$reviews, advtech_df$advtech_uk)
advtech_df$advtech_uk <- NULL

rownames(advtech_df) <- advtech_df[,1]
advtech_df[,1] <- NULL



################ de
detected_language_AT %>% 
  subset(. == "de")

#[c(704)]

advtech_de = translateR::translate(content.vec = tib_AdvTech$reviews[c(704)], 
                                   microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                                   source.lang = "de",
                                   target.lang = "en")

advtech_de_df = as.data.frame(advtech_de)

rownames(advtech_de_df) <- c(704)

advtech_df = merge(tib_AdvTech, advtech_de_df, by=0, all=TRUE)

advtech_df$reviews <- ifelse(is.na(advtech_df$advtech_de), advtech_df$reviews, advtech_df$advtech_de)
advtech_df$advtech_de <- NULL

rownames(advtech_df) <- advtech_df[,1]
advtech_df[,1] <- NULL




################ kn
detected_language_AT %>% 
  subset(. == "kn")

#[c(705)]

advtech_kn = translateR::translate(content.vec = tib_AdvTech$reviews[c(705)], 
                                   microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                                   source.lang = "kn",
                                   target.lang = "en")

advtech_kn_df = as.data.frame(advtech_kn)

rownames(advtech_kn_df) <- c(705)

advtech_df = merge(tib_AdvTech, advtech_kn_df, by=0, all=TRUE)

advtech_df$reviews <- ifelse(is.na(advtech_df$advtech_kn), advtech_df$reviews, advtech_df$advtech_kn)
advtech_df$advtech_kn <- NULL

rownames(advtech_df) <- advtech_df[,1]
advtech_df[,1] <- NULL



################ hr
detected_language_AT %>% 
  subset(. == "hr")

#[c(706)]


advtech_hr = translateR::translate(content.vec = tib_AdvTech$reviews[c(706)], 
                                   microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                                   source.lang = "hr",
                                   target.lang = "en")

advtech_hr_df = as.data.frame(advtech_hr)

rownames(advtech_hr_df) <- c(706)

advtech_df = merge(tib_AdvTech, advtech_hr_df, by=0, all=TRUE)

advtech_df$reviews <- ifelse(is.na(advtech_df$advtech_hr), advtech_df$reviews, advtech_df$advtech_hr)
advtech_df$advtech_hr <- NULL

rownames(advtech_df) <- advtech_df[,1]
advtech_df[,1] <- NULL

Advanced_Tech_Final = advtech_df

#save!
saveRDS(Advanced_Tech_Final, 'Analysis/Final_DFs/Advanced_Tech_Final.RDS')
write_csv(Advanced_Tech_Final, 'Analysis/Final_DFs/Advanced_Tech_Final.csv')

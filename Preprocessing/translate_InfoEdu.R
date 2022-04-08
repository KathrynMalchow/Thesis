#InfoEducation
pacman::p_load(quanteda, tidyverse, dplyr, cld2, deeplr, DSSAT, remotes, purrr, translateR, plyr)



# languages:
################  hi - done
#  no - NA
#  gl - NA
################# gu - done
# st - NA
# bh - NA
# es - NA
################## ne - done
# it - NA
# ny -NA
# hmn - NA
# jw - NA
# mr - NA
#################### ml - done
#  tl - NA 
#  da -NA


### hi
test = tib_InfoEduc

detected_language_Info %>% 
  subset(. == "hi")

#[c(429 ,675 , 737  ,741  ,766  ,836 ,838 ,839 ,842 ,925  ,980  ,990  ,1002 ,1007 ,1012 ,1030 ,1065 ,1086 ,1089,1109, 1120)]

Info_hi = translateR::translate(content.vec = tib_InfoEduc$reviews[c(429, 675, 737, 741, 766, 836, 838, 839, 842,
                                                                     925, 980, 990, 1002, 1007, 1012, 1030, 1065, 1086, 1089, 1109, 1120)], 
                          microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                          source.lang = "hi",
                          target.lang = "en")

Info_hi_df = as.data.frame(Info_hi)

rownames(Info_hi_df) <- c(429, 675, 737, 741, 766, 836, 838, 839, 842,
                                 925, 980, 990, 1002, 1007, 1012, 1030, 1065, 1086, 1089, 1109, 1120)

merge <- merge(test, Info_hi_df, by=0, all=TRUE)

merge$reviews <- ifelse(is.na(merge$Info_hi), merge$reviews, merge$Info_hi)
merge$Info_hi <- NULL

rownames(merge) <- merge[,1]
merge[,1] <- NULL




####### gu

detected_language_Info %>% 
  subset(. == "gu")

#[c(566,567,568,569,570,571,572,573,574,576,577,578,579,581,631,649,673,674,676,677,679,683,690,702,703,704,705,706,707,708,715,731, 736,738,739,742,765,770,771,772,832, 834, 835,837, 843, 846,850,851,881,920,921,923,924,926,930)]

Info_no = translateR::translate(content.vec = tib_InfoEduc$reviews[c(566,567,568,569,570,571,572,573,574,576,577,578,579,581,631,
                                                                     649,673,674,676,677,679,683,690,702,703,704,705,706,707,708,715,
                                                                     731, 736,738,739,742,765,770,771,772,832, 834, 835,837, 843, 846,
                                                                     850,851,881,920,921,923,924,926,930)], 
                                microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                                source.lang = "gu",
                                target.lang = "en")

Info_gu_df = as.data.frame(Info_no)

rownames(Info_gu_df) <- c(566,567,568,569,570,571,572,573,574,576,577,578,579,581,631,
                          649,673,674,676,677,679,683,690,702,703,704,705,706,707,708,715,
                          731, 736,738,739,742,765,770,771,772,832, 834, 835,837, 843, 846,
                          850,851,881,920,921,923,924,926,930)

merge = merge(merge, Info_gu_df, by=0, all=TRUE)

merge$reviews <- ifelse(is.na(merge$Info_no), merge$reviews, merge$Info_no)
merge$Info_no <- NULL

rownames(merge) <- merge[,1]
merge[,1] <- NULL



######################### ne

detected_language_Info %>% 
  subset(. == "ne")

#[c(775, 786, 1064, 1094)]

Info_ne = translateR::translate(content.vec = tib_InfoEduc$reviews[c(775, 786, 1064, 1094)], 
                                microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                                source.lang = "ne",
                                target.lang = "en")

Info_ne_df = as.data.frame(Info_ne)

rownames(Info_ne_df) <- c(775, 786, 1064, 1094)

merge = merge(merge, Info_ne_df, by=0, all=TRUE)

merge$reviews <- ifelse(is.na(merge$Info_ne), merge$reviews, merge$Info_ne)
merge$Info_ne <- NULL

rownames(merge) <- merge[,1]
merge[,1] <- NULL


#################### ml
detected_language_Info %>% 
  subset(. == "ml")

#[c(1129, 1151, 1152, 1153, 1155)]

Info_ml = translateR::translate(content.vec = tib_InfoEduc$reviews[c(1129, 1151, 1152, 1153, 1155)], 
                                microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                                source.lang = "ml",
                                target.lang = "en")

Info_ml_df = as.data.frame(Info_ml)

rownames(Info_ml_df) <- c(1129, 1151, 1152, 1153, 1155)

merge = merge(merge, Info_ml_df, by=0, all=TRUE)

merge$reviews <- ifelse(is.na(merge$Info_ml), merge$reviews, merge$Info_ml)
merge$Info_ml <- NULL

rownames(merge) <- merge[,1]
merge[,1] <- NULL

Information_Education_Final = merge

#save!
saveRDS(Information_Education_Final, 'Analysis/Final_DFs/Information_Education_Final.RDS')
write_csv(Information_Education_Final, 'Analysis/Final_DFs/Information_Education_Final.csv')



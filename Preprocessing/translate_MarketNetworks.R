#Markets and Networks
pacman::p_load(quanteda, tidyverse, dplyr, cld2, deeplr, DSSAT, remotes, purrr, translateR, plyr)

detected_language_MN %>% 
  unique()

#languages
#  no -NA
#  et - NA
##############   hi - done
#   gl - NA
##############   gu - done
#   st - NA
#   bh - NA
#   es - NA
#   ne - NA
#   it - NA
#   ny - NA
#  hmn - NA
#   jw - NA
#  ta - NA
#  mr- NA


################ hi

detected_language_MN %>% 
  subset(. == "hi")

#[c(392,638,700,704,729,799,801,802,805,888,950,966,968,1322)]

MN_hi = translateR::translate(content.vec = tib_marketNetworks$reviews[c(392,638,700,704,729,799,801,802,805,888,950,966,968,1322)], 
                                microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                                source.lang = "hi",
                                target.lang = "en")

MN_hi_df = as.data.frame(MN_hi)

rownames(MN_hi_df) <- c(392,638,700,704,729,799,801,802,805,888,950,966,968,1322)

MN_df = merge(tib_marketNetworks, MN_hi_df, by=0, all=TRUE)

MN_df$reviews <- ifelse(is.na(MN_df$MN_hi), MN_df$reviews, MN_df$MN_hi)
MN_df$MN_hi <- NULL

rownames(MN_df) <- MN_df[,1]
MN_df[,1] <- NULL



################# gu
detected_language_MN %>% 
  subset(. == "gu")

#[c(529,53,531,532,533,534,535,536,537,539,540,541,542,544,594,612,636,637,639,640,642,646,653,665,666,667,668,669,670,671,678,694,699,701,702,705,728,733,734,735,795,797,798,800,806,809,813,814,844,883,884,886,887,889,893)]

MN_gu = translateR::translate(content.vec = tib_marketNetworks$reviews[c(529,53,531,532,533,534,535,536,537,539,540,541,
                                                                         542,544,594,612,636,637,639,640,642,646,653,665,
                                                                         666,667,668,669,670,671,678,694,699,701,702,705,
                                                                         728,733,734,735,795,797,798,800,806,809,813,814,
                                                                         844,883,884,886,887,889,893)], 
                              microsoft.api.key = "044290fb8ff54bb1ba8827d71f082041",
                              source.lang = "gu",
                              target.lang = "en")

MN_gu_df = as.data.frame(MN_gu)

rownames(MN_gu_df) <- c(529,53,531,532,533,534,535,536,537,539,540,541,
                        542,544,594,612,636,637,639,640,642,646,653,665,
                        666,667,668,669,670,671,678,694,699,701,702,705,
                        728,733,734,735,795,797,798,800,806,809,813,814,
                        844,883,884,886,887,889,893)

MN_df = merge(tib_marketNetworks, MN_gu_df, by=0, all=TRUE)

MN_df$reviews <- ifelse(is.na(MN_df$MN_gu), MN_df$reviews, MN_df$MN_gu)
MN_df$MN_gu <- NULL

rownames(MN_df) <- MN_df[,1]
MN_df[,1] <- NULL


#save!

Market_Networks_Final = MN_df

saveRDS(Market_Networks_Final, 'Analysis/Final_DFs/Market_Networks_Final.RDS')
write_csv(Market_Networks_Final, 'Analysis/Final_DFs/Market_Networks_Final.csv')


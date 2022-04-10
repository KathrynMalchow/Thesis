#Advanced Technology

pacman::p_load(remotes, readr, dplyr, tidyverse, quanteda, spacyr, reticulate, tidytext, stringr)


################### save track, save field, field spray,

# collect relevant reviews
at_1.1 = Advanced_Tech_Final[4] %>% 
  filter(str_detect(reviews, "save")) %>% 
  filter(str_detect(reviews, "track"))

at_1.2 = Advanced_Tech_Final[4] %>% 
  filter(str_detect(reviews, "save")) %>% 
  filter(str_detect(reviews, "field"))

at_1.3 = Advanced_Tech_Final[4] %>% 
  filter(str_detect(reviews, "field")) %>%
  filter(str_detect(reviews, "spray"))


at_1 = bind_rows(at_1.1, at_1.2, at_1.3) %>% 
  unique()

# make into .txt file
write.table(at_1, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/at_1.txt', col.names=TRUE, row.names = FALSE)

# back into df
at_1_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/at_1+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(at_1_scored)
# 2.666667
# -1.8

# mean star rating 
star1 = Advanced_Tech_Final %>% 
  filter(str_detect(reviews, "save")) %>% 
  filter(str_detect(reviews, "track"))

star2 = Advanced_Tech_Final %>% 
  filter(str_detect(reviews, "save")) %>% 
  filter(str_detect(reviews, "field"))

star3 = Advanced_Tech_Final %>% 
  filter(str_detect(reviews, "field")) %>%
  filter(str_detect(reviews, "spray"))


star = bind_rows(star1, star2, star3) %>% 
  unique()

mean(star$stars)
# 4.066667


################### pest disease, diagnose plant

# collect relevant reviews
at_2.1 = Advanced_Tech_Final[4] %>% 
  filter(str_detect(reviews, "pest")) %>% 
  filter(str_detect(reviews, "disease"))

at_2.2 = Advanced_Tech_Final[4] %>% 
  filter(str_detect(reviews, "diagnose")) %>% 
  filter(str_detect(reviews, "plant"))


at_2 = bind_rows(at_2.1, at_2.2) %>% 
  unique()

# make into .txt file
write.table(at_2, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/at_2.txt', col.names=TRUE, row.names = FALSE)

# back into df
at_2_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/at_2+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(at_2_scored)
# 2.75
# -2.75

# mean star rating 
star1 = Advanced_Tech_Final %>% 
  filter(str_detect(reviews, "pest")) %>% 
  filter(str_detect(reviews, "disease"))

star2 = Advanced_Tech_Final %>% 
  filter(str_detect(reviews, "diagnose")) %>% 
  filter(str_detect(reviews, "plant"))


star = bind_rows(star1, star2) %>% 
  unique()

mean(star$stars)
# 4







################### cool feature

# collect relevant reviews
at_3 = Advanced_Tech_Final[4] %>% 
  filter(str_detect(reviews, "cool")) %>% 
  filter(str_detect(reviews, "feature"))


# make into .txt file
write.table(at_3, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/at_3.txt', col.names=TRUE, row.names = FALSE)

# back into df
at_3_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/at_3+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(at_3_scored)
# 3.25
# -1.25

# mean star rating 
star = Advanced_Tech_Final %>% 
  filter(str_detect(reviews, "cool")) %>% 
  filter(str_detect(reviews, "feature"))


mean(star$stars)
# 4.5

################### reasonable price, pay version

# collect relevant reviews
at_4.1 = Advanced_Tech_Final[4] %>% 
  filter(str_detect(reviews, "reasonable")) %>% 
  filter(str_detect(reviews, "price"))

at_4.2 = Advanced_Tech_Final[4] %>% 
  filter(str_detect(reviews, "pay")) %>% 
  filter(str_detect(reviews, "version"))


at_4 = bind_rows(at_4.1, at_4.2) %>% 
  unique()

# make into .txt file
write.table(at_4, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/at_4.txt', col.names=TRUE, row.names = FALSE)

# back into df
at_4_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/at_4+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(at_4_scored)
# 2.5
# -1.25

# mean star rating 
star1 = Advanced_Tech_Final %>% 
  filter(str_detect(reviews, "reasonable")) %>% 
  filter(str_detect(reviews, "price"))

star2 = Advanced_Tech_Final %>% 
  filter(str_detect(reviews, "pay")) %>% 
  filter(str_detect(reviews, "version"))


star = bind_rows(star1, star2) %>% 
  unique()

mean(star$stars)
# 4.75



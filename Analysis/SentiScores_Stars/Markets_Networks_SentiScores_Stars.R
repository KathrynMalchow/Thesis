#Markets and Networks SentiStrength

pacman::p_load(remotes, readr, dplyr, tidyverse, quanteda, spacyr, reticulate, tidytext, stringr)


################### agriculture student

# collect relevant reviews
mn_1 = Market_Networks_Final[4] %>% 
  filter(str_detect(reviews, "agriculture")) %>% 
  filter(str_detect(reviews, "student"))

# make into .txt file
write.table(mn_1, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/mn_1.txt', col.names=TRUE, row.names = FALSE)

# back into df
mn_1_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/mn_1+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(mn_1_scored)
# 2.33
# -1.07

# mean star rating 
star = Market_Networks_Final %>% 
  filter(str_detect(reviews, "agriculture")) %>% 
  filter(str_detect(reviews, "student"))

mean(star$stars)


################### customer care

# collect relevant reviews
mn_2 = Market_Networks_Final[4] %>% 
  filter(str_detect(reviews, "customer")) %>% 
  filter(str_detect(reviews, "care"))

# make into .txt file
write.table(mn_2, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/mn_2.txt', col.names=TRUE, row.names = FALSE)

# back into df
mn_2_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/mn_2+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(mn_2_scored)
# 2.00
# -1.4

# mean star rating 
star = Market_Networks_Final %>% 
  filter(str_detect(reviews, "customer")) %>% 
  filter(str_detect(reviews, "care"))

mean(star$stars)


################### product service

# collect relevant reviews
mn_3 = Market_Networks_Final[4] %>% 
  filter(str_detect(reviews, "product")) %>% 
  filter(str_detect(reviews, "service"))

# make into .txt file
write.table(mn_3, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/mn_3.txt', col.names=TRUE, row.names = FALSE)

# back into df
mn_3_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/mn_3+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(mn_3_scored)
# 2.538462
# -1.230769

# mean star rating 
star = Market_Networks_Final %>% 
  filter(str_detect(reviews, "product")) %>% 
  filter(str_detect(reviews, "service"))

mean(star$stars)


################### piece equipment

# collect relevant reviews
mn_4 = Market_Networks_Final[4] %>% 
  filter(str_detect(reviews, "piece")) %>% 
  filter(str_detect(reviews, "equipment"))

# make into .txt file
write.table(mn_4, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/mn_4.txt', col.names=TRUE, row.names = FALSE)

# back into df
mn_4_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/mn_4+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(mn_4_scored)
# 1.5
# -1.5

# mean star rating 
star = Market_Networks_Final %>% 
  filter(str_detect(reviews, "piece")) %>% 
  filter(str_detect(reviews, "equipment"))

mean(star$stars)



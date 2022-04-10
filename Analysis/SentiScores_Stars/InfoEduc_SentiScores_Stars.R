#information education sentistrength scores

pacman::p_load(remotes, readr, dplyr, tidyverse, quanteda, spacyr, reticulate, tidytext, stringr)



################### crop information

# collect relevant reviews
ie_1 = Information_Education_Final[4] %>% 
  filter(str_detect(reviews, "crop")) %>% 
  filter(str_detect(reviews, "information"))

# make into .txt file
write.table(ie_1, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/ei_1.txt', col.names=TRUE, row.names = FALSE)

# back into df
ei_1_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/ei_1+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(ei_1_scored)
# 2.727273
# -1.227273

# mean star rating 
star = Information_Education_Final %>% 
  filter(str_detect(reviews, "crop")) %>% 
  filter(str_detect(reviews, "information"))

mean(star$stars)


################### crop advisory

# collect relevant reviews
ie_2 = Information_Education_Final[4] %>% 
  filter(str_detect(reviews, "crop")) %>% 
  filter(str_detect(reviews, "advisory"))

# make into .txt file
write.table(ie_2, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/ie_2.txt', col.names=TRUE, row.names = FALSE)

# back into df
ie_2_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/ie_2+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(ie_2_scored)
# 2.625
# -1

# mean star rating 
star = Information_Education_Final %>% 
  filter(str_detect(reviews, "crop")) %>% 
  filter(str_detect(reviews, "advisory"))

mean(star$stars)


################### weather prediction

# collect relevant reviews
ie_3 = Information_Education_Final[4] %>% 
  filter(str_detect(reviews, "weather")) %>% 
  filter(str_detect(reviews, "prediction"))

# make into .txt file
write.table(ie_3, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/ie_3.txt', col.names=TRUE, row.names = FALSE)

# back into df
ie_3_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/ie_3+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(ie_3_scored)
# 2.625
# -1

# mean star rating 
star = Information_Education_Final %>% 
  filter(str_detect(reviews, "weather")) %>% 
  filter(str_detect(reviews, "prediction"))

mean(star$stars)


################### local language

# collect relevant reviews
ie_4 = Information_Education_Final[4] %>% 
  filter(str_detect(reviews, "local")) %>% 
  filter(str_detect(reviews, "language"))

# make into .txt file
write.table(ie_4, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/ie_4.txt', col.names=TRUE, row.names = FALSE)

# back into df
ie_4_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/ie_4+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(ie_4_scored)
# 2.25
# -1

# mean star rating 
star = Information_Education_Final %>% 
  filter(str_detect(reviews, "local")) %>% 
  filter(str_detect(reviews, "language"))

mean(star$stars)

################### internet connection

# collect relevant reviews
ie_5 = Information_Education_Final[4] %>% 
  filter(str_detect(reviews, "internet")) %>% 
  filter(str_detect(reviews, "connection"))

# make into .txt file
write.table(ie_5, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/ie_5.txt', col.names=TRUE, row.names = FALSE)

# back into df
ie_5_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/ie_5+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(ie_5_scored)
# 3.25
# -3

# mean star rating 
star = Information_Education_Final %>% 
  filter(str_detect(reviews, "internet")) %>% 
  filter(str_detect(reviews, "connection"))

mean(star$stars)







pacman::p_load(remotes, readr, dplyr, tidyverse, quanteda, spacyr, reticulate, tidytext, stringr)

#df

#tib_DataManagement

#df_coll

#collect relevent reviews
dm_1 = tib_DataManagement[4] %>% 
  filter(str_detect(reviews, "user")) %>% 
  filter(str_detect(reviews, "friendly"))


#run through sentistrength - make into .txt
write.table(dm_1, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/dm_1.txt', col.names=TRUE, row.names = FALSE)


#send through sentistrength

#after sentiscoring - back into df
dm_1_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/dm_1+results.txt', sep="\t", quote="", comment.char="")


senti_func(dm_1_scored)

#keep only scores and scoring section
dm_1_scored = dm_1_scored[3:5] 
#rename and remove fist row
dm_1_scored = rename(dm_1_scored,
                  Positive = V3,
                  Negative = V4,
                  Rational = V5)

dm_1_scored = dm_1_scored[-c(1), ]
#make scores numeric
dm_1_scored = dm_1_scored %>% 
  mutate_at(vars(Positive, Negative), as.numeric)



### Average score of positive and negative? 

mean(dm_1_scored$Positive) #  2.55
mean(dm_1_scored$Negative) # -1.36

### Average of Stars

dm_1_star = tib_DataManagement %>% 
  filter(str_detect(reviews, "user")) %>% 
  filter(str_detect(reviews, "friendly"))

mean(dm_1_star$stars) #3.82

############################## function for average sentiments - working
senti_func = function(df_scored){
  #keep only scores and scoring section
  df_scored = df_scored[3:5] 
  #rename and remove fist row
  df_scored = rename(df_scored,
                       Positive = V3,
                       Negative = V4,
                       Rational = V5)
  
  df_scored = df_scored[-c(1), ]
  #make scores numeric
  df_scored = df_scored %>% 
    mutate_at(vars(Positive, Negative), as.numeric)
  print(mean(df_scored$Positive))
  print(mean(df_scored$Negative))
}


########################### function for stars - not working
star_func = function(df, wa, wb){
  star_df =  df %>%
    filter(str_detect(reviews, "wa")) %>% 
    filter(str_detect(reviews, "wb")) 
  print(mean(star_df$stars))
}

##########################


################### rain_fall

# collect relevant reviews
dm_2 = tib_DataManagement[4] %>% 
  filter(str_detect(reviews, "rain")) %>% 
  filter(str_detect(reviews, "fall"))

# make into .txt file
write.table(dm_2, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/dm_2.txt', col.names=TRUE, row.names = FALSE)

# back into df
dm_2_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/dm_2+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(dm_2_scored)


# mean star rating 
star = tib_DataManagement %>% 
  filter(str_detect(reviews, "rain")) %>% 
  filter(str_detect(reviews, "fall"))

mean(star$stars)

################## waste_time

# collect relevant reviews
dm_3 = tib_DataManagement[4] %>% 
  filter(str_detect(reviews, "time")) %>%
  filter(str_detect(reviews, "waste")) 
  

# make into .txt file
write.table(dm_3, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/dm_3.txt', col.names=TRUE, row.names = FALSE)

# back into df
dm_3_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/dm_3+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(dm_3_scored)


# mean star rating 
star = tib_DataManagement %>% 
  filter(str_detect(reviews, "time")) %>% 
  filter(str_detect(reviews, "waste"))

mean(star$stars)



################### keep_track

# collect relevant reviews
dm_4 = tib_DataManagement[4] %>% 
  filter(str_detect(reviews, "keep")) %>%
  filter(str_detect(reviews, "track")) 


# make into .txt file
write.table(dm_4, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/dm_4.txt', col.names=TRUE, row.names = FALSE)

# back into df
dm_4_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/dm_4+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(dm_4_scored)


# mean star rating 
star = tib_DataManagement %>% 
  filter(str_detect(reviews, "keep")) %>% 
  filter(str_detect(reviews, "track"))

mean(star$stars)


################## free_version

# collect relevant reviews
dm_5 = tib_DataManagement[4] %>% 
  filter(str_detect(reviews, "free")) %>%
  filter(str_detect(reviews, "version")) 


# make into .txt file
write.table(dm_5, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/dm_5.txt', col.names=TRUE, row.names = FALSE)

# back into df
dm_5_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/txt_files/dm_5+results.txt', sep="\t", quote="", comment.char="")

# average sentiment function
senti_func(dm_5_scored)


# mean star rating 
star = tib_DataManagement %>% 
  filter(str_detect(reviews, "free")) %>% 
  filter(str_detect(reviews, "version"))

mean(star$stars)
  
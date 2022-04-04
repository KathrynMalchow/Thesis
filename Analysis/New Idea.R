
pacman::p_load(remotes, readr, dplyr, tidyverse, quanteda, spacyr, reticulate, tidytext)

#df

tib_DataManagement

df_coll

#collect dfs that contain most frequent (top 5) collocations of df

df_1 = tib_DataManagement[4]
df_1 = df_1 %>% 
  filter(str_detect(reviews, c("user", "friendly")))



#run through sentistrength - make into .txt

write.table(df_1, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/df_1.txt', col.names=TRUE, row.names = FALSE)



#send through sentistrength

#after sentiscoring - back into df
df_1_scored = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/df_1+results.txt', sep="\t", quote="", comment.char="")


#keep only scores and scoring section
df_1_scored = df_1_scored[3:5] 
#rename and remove fist row
df_1_scored = rename(df_1_scored,
                  Positive = V3,
                  Negative = V4,
                  Rational = V5)

df_1_scored = df_1_scored[-c(1), ]
#make scores numeric
df_1_scored = df_1_scored %>% 
  mutate_at(vars(Positive, Negative), as.numeric)



### Average score of positive and negative? 

mean(df_1_scored$Positive) #  2.333
mean(df_1_scored$Negative) # -1.458


#####send through sentiscore

#take only reviews
dm_new = tib_DataManagement[4] # or maybe not remove names and shit
#turn into .txt
write.table(dm_new, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/dm_new.txt', sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)


write.table(tib_DataManagement, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/test.txt', sep = " ", row.names = FALSE, col.names = FALSE)


#after sentiscoring - back into df
senti_dm = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/dm_new+results.txt', sep="\t", quote="", comment.char="")



#keep only scores and scoring section
senti_dm = senti_dm[3:5] 
#rename and remove fist row
senti_dm = rename(senti_dm,
    Positive = V3,
    Negative = V4,
    Rational = V5)

senti_dm = senti_dm[-c(1), ]
#make scores numeric
senti_dm = senti_dm %>% 
  mutate_at(vars(Positive, Negative), as.numeric)



#####add new column for overall sentiscore
senti_dm = senti_dm %>% 
  mutate(score = Negative + Positive)


##### combine with orgininal df



###make new positive and negative dfs
negative_reviews = senti_dm %>% 
  subset(score < 0)

positive_reviews = senti_dm %>% 
  subset(score > 0)


###use parsing function on both dfs
spacy_initialize()


parse_fun()



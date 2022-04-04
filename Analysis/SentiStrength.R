#Sentiment Analysis with SentiStrength

pacman::p_load(remotes, readr)

dm_new = tib_DataManagement[4]


write.table(dm_new, 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/dm_new.txt', sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)


senti_dm = read.table(file = 'C:/Users/Kathryn/Desktop/Master_Thesis/Thesis_Analysis/Analysis/dm_new+results.txt', sep="\t", quote="", comment.char="")

senti_dm = senti_dm[3:5]

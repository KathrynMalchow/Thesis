#Collocation

pacman::p_load(udpipe, quanteda, knitr, xtable, formattable)


####for DM
dm_coll = collocation(dm_parsed, term = "lemma", group = c("doc_id", "sentence_id"), 
          ngram_max = 3, n_min = 3, sep = " ")
#look by top occurring collocations
dm_coll = df_coll[order(df_coll$freq, decreasing = TRUE),] %>% 
  head(10)



####for Information Education
ie_coll = collocation(ie_parsed, term = "lemma", group = c("doc_id", "sentence_id"), 
                      ngram_max = 3, n_min = 3, sep = " ")
#look by top occurring collocations
ie_coll = ie_coll[order(ie_coll$freq, decreasing = TRUE),] 



####for Markets/Networks
mn_coll = collocation(mn_parsed, term = "lemma", group = c("doc_id", "sentence_id"), 
                      ngram_max = 3, n_min = 3, sep = " ")
#look by top occurring collocations
mn_coll = mn_coll[order(mn_coll$freq, decreasing = TRUE),] 


####for Advanced Technology
at_coll = collocation(at_parsed, term = "lemma", group = c("doc_id", "sentence_id"), 
                      ngram_max = 3, n_min = 2, sep = " ")
#look by top occurring collocations
at_coll = at_coll[order(at_coll$freq, decreasing = TRUE),]





##### make tables ########    kabler

dm_tab = kable(dm_coll[1:5], format='html', row.names = FALSE, caption = "Data and Management") %>% 
  kable_styling(html_font = "Calibri (Light)", full_width = F) %>% 
  remove_column(c(2, 3, 4))

mn_tab = kable(head(mn_coll[1:5], 10), format='html', row.names = FALSE, caption = "Market Place and Social Network") %>% 
  kable_styling(html_font = "Calibri (Light)", full_width = F) %>% 
  remove_column(c(2, 3, 4))

ie_tab = kable(head(ie_coll[1:5], 10), format='html', row.names = FALSE, caption = "Information and Education") %>% 
  kable_styling(html_font = "Calibri (Light)", full_width = F) %>% 
  remove_column(c(2, 3, 4))

at_tab = kable(head(at_coll[1:5], 10), format='html', row.names = FALSE, caption = "Advanced Technologies") %>% 
  kable_styling(html_font = "Calibri (Light)", full_width = F) %>% 
  remove_column(c(2, 3, 4))


##### merging tables?

t1 = dm_coll[,c(1,5)]
t2 = head(mn_coll[,c(1,5)], 10)
t3 = head(ie_coll[,c(1,5)], 10)
t4 = head(at_coll[,c(1,5)], 10)

table = cbind(t1, t2, t3, t4)


######### formattable ######

formattable(table,  format='html', row.names = FALSE)

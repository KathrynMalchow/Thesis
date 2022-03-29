#Yara_CheckIT

pacman::p_load(RSelenium, tidyverse, lubridate, rvest, xml2)

#start up navigator

rD = rsDriver(browser="firefox", port=4545L, verbose=F)
remDr = rD[["client"]]

url = "https://play.google.com/store/apps/details?id=com.yara.checkit&hl=en&showAllReviews=true"
remDr$navigate(url)


webElem = remDr$findElement("css", "body")

#scroll to the End
for(i in 1:20){
  message(paste("Iteration",i))
  webElem$sendKeysToElement(list(key = "end"))
  #Check for the Show More Button
  element<- try(unlist(remDr$findElement("class name", "RveJvd")$getElementAttribute('class')),
                silent = TRUE)
  
  #If Button Is There Then Click It
  Sys.sleep(2)
  if(str_detect(element, "RveJvd") == TRUE){
    buttonElem <- remDr$findElement("class name", "RveJvd")
    buttonElem$clickElement()
  }
  
  #Sleep to Let Things Load
  Sys.sleep(3)
}

##Scrape in HTML Objects
html_obj <- remDr$getPageSource(header = TRUE)[[1]] %>% read_html()

#Shut Down Client and Server
remDr$close()
rD$server$stop()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)


#Extracting what I need from Reviews

# 1) Reviewer Name
names_Yara_CheckIT = html_obj %>% html_elements(".kx8XBd .X43Kjb") %>% html_text()

# 2) Number of Stars
stars_Yara_CheckIT = html_obj %>% html_elements(".kx8XBd .nt2C1d [role='img']")%>% 
  html_attr("aria-label") %>% 
  #Remove everything that's not numeric
  str_remove_all('\\D+') %>% 
  # Convert to Integer
  as.integer()

# 3) Date of Review
dates_Yara_CheckIT = html_obj %>% html_elements(".kx8XBd .p2TkOb") %>% 
  html_text() %>% 
  # Convert to a Date
  mdy()

# 4) Full Text of the Review
reviews_Yara_CheckIT = html_obj %>% html_elements(".UD7Dzf") %>% html_text() 
###Deal with the "Full Review" Issue where text is duplicated
reviews_Yara_CheckIT = if_else(
  #If the review is truncated
  str_detect(reviews_Yara_CheckIT, '\\.\\.\\.Full Review'),
  #Grab all the Text After the string '...Full Review'
  str_sub(reviews_Yara_CheckIT, 
          start = str_locate(reviews_Yara_CheckIT, '\\.\\.\\.Full Review')[, 2]+1
  ),
  #Else remove the leading space from the review as is
  str_trim(reviews_Yara_CheckIT))


#Put into table
Yara_CheckIT = tibble(
  names = names_Yara_CheckIT, 
  stars = stars_Yara_CheckIT, 
  dates = dates_Yara_CheckIT, 
  reviews = reviews_Yara_CheckIT) 
saveRDS(Yara_CheckIT, 'Objects/Information_Education/Yara_CheckIT.RDS')
write_csv(Yara_CheckIT, 'data/Information_Education/Yara_CheckIT.csv')

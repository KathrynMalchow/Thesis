#FieldBee tractor GPS navigation

pacman::p_load(RSelenium, tidyverse, lubridate, rvest, xml2)

#start up navigator

rD = rsDriver(browser="firefox", port=4545L, verbose=F)
remDr = rD[["client"]]

url = "https://play.google.com/store/apps/details?id=com.kmware.efarmer&showAllReviews=true"
remDr$navigate(url)


webElem = remDr$findElement("css", "body")

#scroll to the End
for(i in 1:30){
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
names_FieldBee = html_obj %>% html_elements(".kx8XBd .X43Kjb") %>% html_text()

# 2) Number of Stars
stars_FieldBee = html_obj %>% html_elements(".kx8XBd .nt2C1d [role='img']")%>% 
  html_attr("aria-label") %>% 
  #Remove everything that's not numeric
  str_remove_all('\\D+') %>% 
  # Convert to Integer
  as.integer()

# 3) Date of Review
dates_FieldBee = html_obj %>% html_elements(".kx8XBd .p2TkOb") %>% 
  html_text() %>% 
  # Convert to a Date
  mdy()

# 4) Full Text of the Review
reviews_FieldBee = html_obj %>% html_elements(".UD7Dzf") %>% html_text() 
###Deal with the "Full Review" Issue where text is duplicated
reviews_FieldBee = if_else(
  #If the review is truncated
  str_detect(reviews_FieldBee, '\\.\\.\\.Full Review'),
  #Grab all the Text After the string '...Full Review'
  str_sub(reviews_FieldBee, 
          start = str_locate(reviews_FieldBee, '\\.\\.\\.Full Review')[, 2]+1
  ),
  #Else remove the leading space from the review as is
  str_trim(reviews_FieldBee))


#Put into table
FieldBee = tibble(
  names = names_FieldBee, 
  stars = stars_FieldBee, 
  dates = dates_FieldBee, 
  reviews = reviews_FieldBee) 
saveRDS(FieldBee, 'Objects/Advanced_Tech/FieldBee.RDS')
write_csv(FieldBee, 'data/Advanced_Tech/FieldBee.csv')

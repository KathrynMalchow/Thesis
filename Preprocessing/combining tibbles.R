pacman::p_load(quanteda, tidyverse, dplyr)

#Combine Tibbles

list_DataManagement = 
  
list_InfoEduc =
  
list_marketNetworks = list(AgMobile, AgriMedia_TV, AgriSetu, BigHaat, Cattle_Market, EzyAgric, Farmpost, Kisaan_Suvidha, Tractor_Zoom, TractorHouse) 
tib_marketNetworks = reduce(list_marketNetworks, bind_rows)

list_AdvTech = 
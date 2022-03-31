pacman::p_load(quanteda, tidyverse, dplyr)

#Combine Tibbles

list_DataManagement = list(Canopeo, FarmersWallet, farmlogs, FarmManagementPro, FertilizerRemoval, fieldmargin, GPS_FieldsArea_Measure, Grazing_Calculator, LandPKS, MyCropManager, SoilSampler, Tank_Mix_Calculator) 
tib_DataManagement = reduce(list_DataManagement, bind_rows)

list_InfoEduc = list(Agri_Farming, AgriApp, AgriMedia_TV, BharatAgri, Extension_Manager, Farming_Solution, IDWeeds, LandPKS, PlantSat, SoilWeb, Yara_CheckIT)
tib_InfoEduc = reduce(list_InfoEduc, bind_rows)
  
list_marketNetworks = list(AgMobile, AgriMedia_TV, AgriSetu, BigHaat, Cattle_Market, EzyAgric, Farmpost, Kisaan_Suvidha, Tractor_Zoom, TractorHouse) 
tib_marketNetworks = reduce(list_marketNetworks, bind_rows)

list_AdvTech = list(AgriBus, Agrio, BigHaat, Field_Navigator, FieldBee, OneSoil, Plantix, SCOUTING, xarvio)
tib_AdvTech = reduce(list_AdvTech, bind_rows)

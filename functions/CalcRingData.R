#### CalcRingData.R 
#### Rudolf Cesaretti, 11/9/2022

#### "CalcRingData.R"
#### 
#### 
#### 
#### 

pak <- c("rgdal", "sp", "sf", "GISTools", "lwgeom", "tidyverse", "tidyr")
# Install packages not yet installed
ip <- pak %in% rownames(installed.packages())
if (any(ip == FALSE)) {
  install.packages(pak[!ip])
}
# load packages
invisible(lapply(pak, library, character.only = TRUE))
rm(pak,ip)


###############################################################
#######################  CalcRingData  ########################
###############################################################

CalcRingData = function(PolyData, # = Teo_Poly_Data
                        RingsPoly, # = RingsPoly_Oval
                        #dist_lim, 
                        RingVersion = c("Ring_Oval","Ring_Circle"), 
                        output_arch_data = F,
                        output_ring_data = T){
  
  #nrings <- nrow(RingsPoly)
  
  #Create binary land use classification helper variables
  
  PolyData@data <- PolyData@data %>%  mutate(
    XM_OB_bi = ifelse(XM_c == 0, 1, 0),
    XM_Low_bi = ifelse(XM_c == 1, 1, 0),
    XM_IM_bi = ifelse(XM_c == 2, 1, 0),
    XM_UncertStat_bi = ifelse(XM_c == 3, 1, 0),
    XM_High_bi = ifelse(XM_c == 4, 1, 0),
    XM_PlatOther_bi = ifelse(XM_c == 5, 1, 0),
    XM_Altar_bi = ifelse(XM_c == 6, 1, 0),
    XM_PlatformRelig_bi = ifelse(XM_c == 9, 1, 0),
    XM_Religious_bi = ifelse(XM_c == 10, 1, 0),
    XM_ProbTemple_bi = ifelse(XM_c == 16, 1, 0),
    XM_Civic_bi = ifelse(XM_c == 17, 1, 0),
    XM_AoD_bi = ifelse(XM_c == 18, 1, 0),
    XM_Plaza_bi = ifelse(XM_c == 28, 1, 0),
    XM_Other_bi = ifelse(XM_c == 99, 1, 0),
    XM_Unk_bi = ifelse(XM_c == 100, 1, 0),
    XM_Unoccu_bi = ifelse(XM_c == -2, 1, 0),
    XM_grp_OB_bi = ifelse(XM_group_c == 0, 1, 0),
    XM_grp_Low_bi = ifelse(XM_group_c == 1, 1, 0),
    XM_grp_IM_bi = ifelse(XM_group_c == 2, 1, 0),
    XM_grp_UncertStat_bi = ifelse(XM_group_c == 3, 1, 0),
    XM_grp_High_bi = ifelse(XM_group_c == 4, 1, 0),
    XM_grp_DomPlat_bi = ifelse(XM_group_c == 5, 1, 0),
    XM_grp_CCFeat_bi = ifelse(XM_group_c == 6, 1, 0),
    XM_grp_AoD_bi = ifelse(XM_group_c == 7, 1, 0),
    XM_grp_Plaza_bi = ifelse(XM_group_c == 9, 1, 0),
    XM_grp_Other_bi = ifelse(XM_group_c == 10, 1, 0),
    XM_grp_Unk_bi = ifelse(XM_c == 11, 1, 0),
    XM_grp_Unoccu_bi = ifelse(XM_c == 12, 1, 0),
    XM_macro_OB_bi = ifelse(XM_macro_c == 0, 1, 0),
    XM_macro_DStr_bi = ifelse(XM_macro_c == 1, 1, 0),
    XM_macro_DomPlat_bi = ifelse(XM_macro_c == 2, 1, 0),
    XM_macro_CCFeatO_bi = ifelse(XM_macro_c == 3, 1, 0),
    XM_macro_Unk_bi = ifelse(XM_macro_c == 4, 1, 0),
    XM_macro_Unoccu_bi = ifelse(XM_macro_c == 5, 1, 0))
  
  #sum(is.na(TeoDataPts_Rings[,36:227]))
  
  # Calculate area and pop for each arch feature using binary helper vars
  
  PolyData@data <- PolyData@data %>%  mutate(
    XM_Pop_Low = XM_Low_bi * XM_Pop,
    XM_Area_Low = XM_Low_bi * Area_m2,
    XM_Pop_IM = XM_IM_bi * XM_Pop,
    XM_Area_IM = XM_IM_bi * Area_m2,
    XM_Pop_High = XM_High_bi * XM_Pop,
    XM_Area_High = XM_High_bi * Area_m2,
    XM_Pop_UncertStat = XM_UncertStat_bi * XM_Pop,
    XM_Area_UncertStat = XM_UncertStat_bi * Area_m2)
  
  PolyData@data <- PolyData@data %>%  mutate(
    XM_Area_CCFeat = XM_grp_CCFeat_bi * Area_m2,
    XM_Area_CCFeatO = XM_macro_CCFeatO_bi * Area_m2,
    XM_Area_DStr = XM_macro_DStr_bi * Area_m2,
    XM_Area_OB = XM_grp_OB_bi * Area_m2,
    XM_Area_Unoccu = XM_macro_Unoccu_bi * Area_m2,
    XM_Area_DomPlat = XM_grp_DomPlat_bi * Area_m2,
    XM_Area_Unk = XM_macro_Unk_bi * Area_m2,
    XM_Pop = XM_Pop_Low + XM_Pop_IM + XM_Pop_High + XM_Pop_UncertStat)

  
  ## Summarize Data By Ring
  
  
  Teo_Rings <- PolyData@data %>%
    group_by(!!sym(RingVersion)) %>%
    summarise(
      Pop_TOT = sum(XM_Pop, na.rm=T),
      DStr_TOT = sum(XM_macro_DStr_bi, na.rm=T),
      Area_DStr = sum(XM_Area_DStr, na.rm=T) * 0.0001,
      NonCC_Feat_TOT = sum(XM_macro_Unoccu_bi, na.rm=T) + sum(XM_macro_Unk_bi, na.rm=T) + sum(XM_macro_DStr_bi, na.rm=T) + sum(XM_macro_DomPlat_bi, na.rm=T) + sum(XM_macro_OB_bi, na.rm=T),
      
      NonCC_Feat_Area = (sum(XM_Area_Unoccu, na.rm=T) + sum(XM_Area_Unk, na.rm=T) + sum(XM_Area_DStr, na.rm=T) + sum(XM_Area_OB, na.rm=T) + sum(XM_Area_DomPlat, na.rm=T)) * 0.0001,
      
      NonCCDStr_TOT = sum(XM_macro_Unoccu_bi, na.rm=T) + sum(XM_macro_Unk_bi, na.rm=T) + sum(XM_macro_DomPlat_bi, na.rm=T) + sum(XM_macro_OB_bi, na.rm=T),
      
      NonCCDStr_Area = (sum(XM_Area_Unoccu, na.rm=T) + sum(XM_Area_Unk, na.rm=T) + sum(XM_Area_OB, na.rm=T) + sum(XM_Area_DomPlat, na.rm=T)) * 0.0001,
      
      OB = sum(XM_grp_OB_bi, na.rm=T),
      OB_Area = sum(XM_Area_OB, na.rm=T) * 0.0001,
      Unoccu = sum(XM_macro_Unoccu_bi, na.rm=T),
      Unoccu_Area = sum(XM_Area_Unoccu, na.rm=T) * 0.0001,
      DomPlat = sum(XM_macro_DomPlat_bi, na.rm=T),
      DomPlat_Area = sum(XM_Area_DomPlat, na.rm=T) * 0.0001,
      Unk = sum(XM_macro_Unk_bi, na.rm=T),
      Unk_Area = sum(XM_Area_Unk, na.rm=T) * 0.0001,
      CCStr = sum((XM_grp_CCFeat_bi), na.rm=T),
      CCStrArea = sum(XM_Area_CCFeat, na.rm=T) * 0.0001,
      AoD = sum(XM_grp_AoD_bi, na.rm=T),
      AoD_Area = sum((XM_grp_AoD_bi * Area_m2), na.rm=T) * 0.0001,
      Plazas = sum(XM_Plaza_bi, na.rm=T),
      Plaza_Area = sum((XM_Plaza_bi * Area_m2), na.rm=T) * 0.0001,
      PlazAoD_O = sum(XM_Plaza_bi, na.rm=T) + sum(XM_grp_AoD_bi, na.rm=T),
      PlazAoD_O_Area = (sum((XM_Plaza_bi * Area_m2), na.rm=T) + sum((XM_grp_AoD_bi * Area_m2), na.rm=T)) * 0.0001,
      CCFeat_AoD_Area = (sum((XM_grp_AoD_bi * Area_m2), na.rm=T) + sum(XM_Area_CCFeat, na.rm=T))* 0.0001,
      CCFeat_O_Area = sum(XM_Area_CCFeatO, na.rm=T) * 0.0001,
      
      UnOccuFeat = sum(NonCCDStr_TOT, na.rm=T),
      UnOccuFeat_Area = sum(NonCCDStr_Area, na.rm=T) * 0.0001,
      
      Elite_DStr = sum(XM_grp_High_bi, na.rm=T),
      Elite_Pop = sum(XM_Pop_High, na.rm=T),
      Elite_DStrArea = sum(XM_Area_High, na.rm=T) * 0.0001,
      Avg_Elite_DStrArea = my_mean(XM_Area_High) * 0.0001,
      Med_Elite_DStrArea = my_median(XM_Area_High) * 0.0001,
      Avg_Elite_DStrPop = my_mean(XM_Pop_High),
      IM_DStr = sum(XM_grp_IM_bi, na.rm=T),
      IM_Pop = sum(XM_Pop_IM, na.rm=T),
      IM_DStrArea = sum(XM_Area_IM, na.rm=T) * 0.0001,
      Avg_IM_DStrArea = my_mean(XM_Area_IM) * 0.0001,
      Med_IM_DStrArea = my_median(XM_Area_IM) * 0.0001,
      Avg_IM_DStrPop = my_mean(XM_Pop_IM),
      Low_DStr = sum(XM_grp_Low_bi, na.rm=T),
      Low_Pop = sum(XM_Pop_Low, na.rm=T),
      Low_DStrArea = sum(XM_Area_Low, na.rm=T) * 0.0001,
      Avg_Low_DStrArea = my_mean(XM_Area_Low) * 0.0001,
      Med_Low_DStrArea = my_median(XM_Area_Low) * 0.0001,
      Avg_Low_DStrPop = my_mean(XM_Pop_Low),
      UncerStat_DStr = sum(XM_grp_UncertStat_bi, na.rm=T),
      UncerStat_Pop = sum(XM_Pop_UncertStat, na.rm=T),
      UncerStat_Area = sum(XM_Area_UncertStat, na.rm=T) * 0.0001,
      Avg_UncerStat_DStrArea = my_mean(XM_Area_UncertStat) * 0.0001,
      Med_UncerStat_DStrArea = my_median(XM_Area_UncertStat) * 0.0001,
      Avg_UncerStat_DStrPop = my_mean(XM_Pop_UncertStat))
  
  ## Replace NaN and NA with zero (0)
  
  #Teo_Rings = Teo_Rings[-c(1,12),]
  Teo_Rings$Avg_Elite_DStrArea[is.nan(Teo_Rings$Avg_Elite_DStrArea)]<-0
  Teo_Rings$Med_Elite_DStrArea[is.nan(Teo_Rings$Med_Elite_DStrArea)]<-0
  Teo_Rings$Avg_Elite_DStrPop[is.nan(Teo_Rings$Avg_Elite_DStrPop)]<-0
  Teo_Rings$Avg_IM_DStrArea[is.nan(Teo_Rings$Avg_IM_DStrArea)]<-0
  Teo_Rings$Med_IM_DStrArea[is.nan(Teo_Rings$Med_IM_DStrArea)]<-0
  Teo_Rings$Avg_IM_DStrPop[is.nan(Teo_Rings$Avg_IM_DStrPop)]<-0
  Teo_Rings$Avg_Low_DStrArea[is.nan(Teo_Rings$Avg_Low_DStrArea)]<-0
  Teo_Rings$Med_Low_DStrArea[is.nan(Teo_Rings$Med_Low_DStrArea)]<-0
  Teo_Rings$Avg_UncerStat_DStrArea[is.nan(Teo_Rings$Avg_UncerStat_DStrArea)]<-0
  Teo_Rings$Med_UncerStat_DStrArea[is.nan(Teo_Rings$Med_UncerStat_DStrArea)]<-0
  Teo_Rings$Avg_UncerStat_DStrPop[is.nan(Teo_Rings$Avg_UncerStat_DStrPop)]<-0    
  Teo_Rings[is.na(Teo_Rings)] <- 0
  
  
  ## Construct Ring Dataset
  
  
  Ring <- RingsPoly@data$Ring   
  MinRingDist <- RingsPoly@data$Dist - 500
  MedRingDist <- RingsPoly@data$Dist - 250
  MaxRingDist <- RingsPoly@data$Dist 
  Ring_Area <- RingsPoly@data$Area_ha
  Popdens_TOT = Teo_Rings$Pop_TOT / Ring_Area
  Pop_TOT = Teo_Rings$Pop_TOT
  DStr_TOT = Teo_Rings$DStr_TOT
  Area_DStr = Teo_Rings$Area_DStr
  Ring_Data <- data.frame(Ring,MinRingDist,MedRingDist,MaxRingDist,Ring_Area,Pop_TOT,Popdens_TOT,DStr_TOT,Area_DStr)
  Next <- Teo_Rings[,c(5:28)]
  Elite <- Teo_Rings[,c(29:34)]
  IM <- Teo_Rings[,c(35:40)]
  Low <- Teo_Rings[,c(41:46)]
  UncertStat <- Teo_Rings[,c(47:52)]
  
  Ring_Data <-cbind(Ring_Data, Next)
  
  
  Ring_Data <- Ring_Data %>%
    mutate(Popdens_DStrArea = Pop_TOT / Area_DStr,
           Area_NonDStr = Ring_Area - Area_DStr,
           Area_NonCCZone = Ring_Area - CCStrArea,
           Popdens_NonCCZone = Pop_TOT / Area_NonCCZone,
           Area_NonCCZone_AoD = Ring_Area - CCFeat_AoD_Area,
           Popdens_NonCCZone_AoD = Pop_TOT / Area_NonCCZone_AoD,
           Area_NonCCZone_O = Ring_Area - CCFeat_O_Area,
           Popdens_NonCCZone_O = Pop_TOT / Area_NonCCZone_O,
           Area_Open = Ring_Area - (NonCC_Feat_Area + CCFeat_O_Area),
           Area_OpenUnoccu = Ring_Area - ((NonCC_Feat_Area - Unoccu_Area) + CCFeat_O_Area),
           PctRow_Area_DStr = Area_DStr / Ring_Area,
           PctRow_Area_NonDStr = (Ring_Area - Area_DStr) / Ring_Area,
           PctRow_Area_CCFeat_AoD = CCFeat_AoD_Area / Ring_Area,
           PctRow_Area_CCFeat_O = CCFeat_O_Area / Ring_Area,
           PctRow_Area_CCStr = CCStrArea / Ring_Area,
           PctRow_Area_Open = Area_Open / Ring_Area,
           PctRow_Area_OpenUnoccu = Area_OpenUnoccu / Ring_Area,
           PctRow_Area_NonCCZone = Area_NonCCZone / Ring_Area,
           PctRow_Area_NonCCZone_AoD = Area_NonCCZone_AoD / Ring_Area,
           PctRow_Area_NonCCZone_O = Area_NonCCZone_O / Ring_Area,
           PctRow_Area_UnOccuFeat = UnOccuFeat_Area / Ring_Area,
           PctRow_Area_Unk = Unk_Area / Ring_Area,
           PctRow_Area_OB = OB_Area / Ring_Area,
           PctRow_Area_AoD = AoD_Area / Ring_Area,
           PctRow_Area_Plazas = Plaza_Area / Ring_Area,
           PctRow_Area_DomPlat = DomPlat_Area / Ring_Area,
           PctCol_Area_DStr = Area_DStr / sum(Area_DStr),
           PctCol_Area_NonDStr = Area_NonDStr / sum(Area_NonDStr),
           
           PctCol_Area_CCFeat_AoD = CCFeat_AoD_Area / sum(CCFeat_AoD_Area),
           PctCol_Area_CCFeat_O = CCFeat_O_Area / sum(CCFeat_O_Area),
           
           PctCol_Area_CCStr = CCStrArea / sum(CCStrArea),
           PctCol_Area_Open = Area_Open / sum(Area_Open),
           PctCol_Area_OpenFields = Area_OpenUnoccu / sum(Area_OpenUnoccu),
           PctCol_Area_NonCCZone = Area_NonCCZone / sum(Area_NonCCZone),
           PctCol_Area_NonCCZone_AoD = Area_NonCCZone_AoD / sum(Area_NonCCZone_AoD),
           PctCol_Area_NonCCZone_O = Area_NonCCZone_O / sum(Area_NonCCZone_O),
           PctCol_Area_UnOccuFeat = UnOccuFeat_Area / sum(UnOccuFeat_Area),
           PctCol_Area_Unk = Unk_Area / sum(Unk_Area),
           PctCol_Area_OB = OB_Area / sum(OB_Area),
           PctCol_Area_AoD = AoD_Area / sum(AoD_Area),
           PctCol_Area_Plazas = Plaza_Area / sum(Plaza_Area),
           PctCol_Area_DomPlat = DomPlat_Area / sum(DomPlat_Area))
  
  Ring_Data <-cbind(Ring_Data, Elite)
  
  Ring_Data <- Ring_Data %>%
    mutate(PctRow_DStr_Elite = Elite_DStr / DStr_TOT,
           PctRow_Pop_Elite = Elite_Pop / Pop_TOT,
           PctRow_DStrArea_Elite = Elite_DStrArea / Area_DStr,
           PctCol_DStr_Elite = Elite_DStr / sum(Elite_DStr),
           PctCol_Pop_Elite = Elite_Pop / sum(Elite_Pop),
           PctCol_DStrArea_Elite = Elite_DStrArea / sum(Elite_DStrArea),
           Elite_Popdens_NonCCZone = Elite_Pop / Area_NonCCZone,
           Elite_Popdens_NonCCZone_AoD = Elite_Pop / Area_NonCCZone_AoD,
           Elite_Popdens_NonCCZone_O = Elite_Pop / Area_NonCCZone_O,
           Elite_Popdens_EliteDStrArea = Elite_Pop / Elite_DStrArea,
           Elite_Popdens_TOTArea = Elite_Pop / Ring_Area)
  
  Ring_Data$PctCol_DStrArea_Elite[is.nan(Ring_Data$PctCol_DStrArea_Elite)]<-0
  Ring_Data$Elite_Popdens_EliteDStrArea[is.nan(Ring_Data$Elite_Popdens_EliteDStrArea)]<-0
  
  
  Ring_Data <-cbind(Ring_Data, IM)
  Ring_Data <- Ring_Data %>%
    mutate(PctRow_DStr_IM = IM_DStr / DStr_TOT,
           PctRow_Pop_IM = IM_Pop / Pop_TOT,
           PctRow_DStrArea_IM = IM_DStrArea / Area_DStr,
           PctCol_DStr_IM = IM_DStr / sum(IM_DStr),
           PctCol_Pop_IM = IM_Pop / sum(IM_Pop),
           PctCol_DStrArea_IM = IM_DStrArea / sum(IM_DStrArea),
           IM_Popdens_NonCCZone = IM_Pop / Area_NonCCZone,
           IM_Popdens_NonCCZone_AoD = IM_Pop / Area_NonCCZone_AoD,
           IM_Popdens_NonCCZone_O = IM_Pop / Area_NonCCZone_O,
           IM_Popdens_IMDStrArea = IM_Pop / IM_DStrArea,
           IM_Popdens_TOTArea = IM_Pop / Ring_Area)
  
  
  
  Ring_Data <-cbind(Ring_Data, Low)
  
  Ring_Data <- Ring_Data %>%
    mutate(PctRow_Ring_DStr_Low = Low_DStr / DStr_TOT,
           PctRow_Ring_Pop_Low = Low_Pop / Pop_TOT,
           PctRow_Ring_DStrArea_Low = Low_DStrArea / Area_DStr,
           PctCol_Tot_LowDStr = Low_DStr / sum(Low_DStr),
           PctCol_Tot_LowPop = Low_Pop / sum(Low_Pop),
           PctCol_Tot_LowDStrArea = Low_DStrArea / sum(Low_DStrArea),
           Low_Popdens_NonCCZone = Low_Pop / Area_NonCCZone,
           Low_Popdens_NonCCZone_AoD = Low_Pop / Area_NonCCZone_AoD,
           Low_Popdens_NonCCZone_O = Low_Pop / Area_NonCCZone_O,
           Low_Popdens_TOTArea = Low_Pop / Ring_Area,
           Low_Popdens_OpenArea = Low_Pop / Area_Open,
           Low_Popdens_LowDStrArea = Low_Pop / Low_DStrArea)
  
  
  Ring_Data <-cbind(Ring_Data, UncertStat)
  
  Ring_Data <- Ring_Data %>%
    mutate(PctRow_DStr_UncerStat = UncerStat_DStr / DStr_TOT,
           PctRow_Pop_UncerStat = UncerStat_Pop / Pop_TOT,
           PctRow_DStrArea_UncerStat = UncerStat_Area / Area_DStr,
           PctCol_DStr_UncerStat = UncerStat_DStr / sum(UncerStat_DStr),
           PctCol_Pop_UncerStat = UncerStat_Pop / sum(UncerStat_Pop),
           PctCol_DStrArea_UncerStat = UncerStat_Area / sum(UncerStat_Area),
           UncerStat_Popdens_NonCCZone = UncerStat_Pop / Area_NonCCZone,
           UncerStat_Popdens_NonCCZone_AoD = UncerStat_Pop / Area_NonCCZone_AoD,
           UncerStat_Popdens_NonCCZone_O = UncerStat_Pop / Area_NonCCZone_O,
           UncerStat_Popdens_TOTArea = UncerStat_Pop / Ring_Area,
           UncetStat_Popdens_OpenArea = UncerStat_Pop / Area_Open,
           UncerStat_Popdens_UncerStatDStrArea = UncerStat_Pop / UncerStat_Area)
  
  
  ## Outputs
  
  if (output_arch_data == F & output_ring_data == T){
    outputs <- Ring_Data
  }
  
  if (output_arch_data == T & output_ring_data == F){
    outputs <- PolyData
  }
  
  if (output_arch_data == T & output_ring_data == T){
    outputs <- list(Ring_Data, PolyData)
  }
  
  return(outputs)
  
}













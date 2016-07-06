##########################################################
#### HERBACEOUS BIOMASS ESTIMATION - NSERP STUDY AREA ####
################## KJB  July 2016  #######################
##########################################################

#### NOTE: MUST USE 32 BIT R TO CONNECT TO ACCESS ####

## WD

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Biomass"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Biomass"
	if (file.exists(wd_workcomp)) {
	  setwd(wd_workcomp)
	} else {
	  if(file.exists(wd_laptop)) {
		setwd(wd_laptop)
	  } else {
		  cat("Are you SURE you got that file path right?\n")
		  }
	  }

## PACKAGES

library(RODBC)
library(dplyr)
library(tidyr)
	
## DATA

# Connect to Access phenology database (work computer or laptop)

if (file.exists(wd_workcomp)) {
  channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                             dbq=C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/Sapphire_Veg_Phenology.accdb")
} else {
  if(file.exists(wd_laptop)) {
    channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                             dbq=C:/Users/kjbark3r/Documents/NSERP/Databases/Sapphire_Veg_Phenology.accdb")
  } else {
    cat("Are you SURE you got that file path right?\n")
  }
}
  rm(wd_workcomp, wd_laptop)

# Pull life form info from NSERP_SP_list table 

spp <- sqlQuery(channel, paste("select PlantCode, LifeForm
                                 from NSERP_SP_list"))
spp <- rename(spp, Species = PlantCode)
  
# Pull Classfication table; add quadrat id, quadrat-visit ID, life form

classn <- sqlQuery(channel, paste("select * from Classification"))
  colnames(classn) <- c("VisitDate", "PlotID", "PlotM", "Species", "Total", "Live", "Senesced")
  classn$Species <- trimws(classn$Species) #remove leading/trailing whitespace
 
 classn <- classn %>%
    mutate(Quadrat = paste(PlotID,"-",PlotM, sep="")) %>%
	mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep="")) %>%
    left_join(spp, by = "Species")

	for(i in 1:nrow(classn)) {
	  classn$LifeForm[i] <- ifelse(grepl('UNK ', classn$Species[i]), "forb", next)
	}

# Pull Cover table; add unique quadrat ID and quadrat-visit ID

cover <- sqlQuery(channel, paste("select * from Cover"))
  colnames(cover) <- c("VisitDate", "PlotID", "PlotM", "Grass", "Shrub",
                       "SubShrub", "Forb", "MossLichen", "NonVeg", "SmTree")
  cover <- mutate(cover, Quadrat = paste(PlotID,"-",PlotM, sep="")) %>%
		   mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep=""))

# Rescale species % cover 

temp <- select(cover, -c(VisitDate, PlotID, PlotM, Quadrat))
bigdf <- full_join(classn, temp, by = "QuadratVisit")
  
bigdf$RescaledCover <- ifelse(bigdf$LifeForm == "forb", bigdf$Total/bigdf$Forb,
                                      ifelse(bigdf$LifeForm == "graminoid", bigdf$Total/bigdf$Grass,
                                             NA))
     
# Pull ClipPlots table; add unique quadrat ID and quadrat-visit ID

clip <- sqlQuery(channel, paste("select * from ClipPlots"))
  colnames(clip) <- c("VisitDate", "PlotID", "PlotM", "LifeForm", "EmptyBag",
                       "Total", "Live", "Senesced", "WetWt", "DryWt")
clip <- clip %>%
    mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep=""))
#    select(LifeForm, Total, DryWt, QuadratVisit) %>%
#    rename(TotalCov = Total)
#kristin - you may not need the above
    
# Rescale species %cover by life form type

drywt <- clip %>%
  select(QuadratVisit, LifeForm, DryWt) %>%
  spread(LifeForm, DryWt) %>%
  rename(ForbWt = Forb, GrassWt = Grass) %>%
  full_join(bigdf, by = "QuadratVisit") %>%
  select(QuadratVisit, Species, RescaledCover, LifeForm, ForbWt, GrassWt)

drywt$grams <- ifelse(drywt$LifeForm == "forb", drywt$RescaledCover*drywt$ForbWt,
                      ifelse(drywt$LifeForm == "graminoid", drywt$RescaledCover*drywt$GrassWt,
                              ifelse(NA)))

  
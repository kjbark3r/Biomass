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
	
#########
## DATA - READ IN AND SET UP

#Connect to Access phenology database (work computer or laptop)
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

#Pull life form info from NSERP_SP_list table 
spp <- sqlQuery(channel, paste("select PlantCode, LifeForm
                                 from NSERP_SP_list"))
spp <- rename(spp, Species = PlantCode)
  
#Pull Cover info; add  quadrat ID, quadrat-visit ID, plot-visit ID
cover <- sqlQuery(channel, paste("select * from Cover"))
colnames(cover) <- c("VisitDate", "PlotID", "PlotM", "Grass", "Shrub",
                     "SubShrub", "Forb", "MossLichen", "NonVeg", "SmTree")
cover <- mutate(cover, Quadrat = paste(PlotID,"-",PlotM, sep="")) %>%
  mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep="")) %>%
#  mutate(PlotVisit = paste(PlotID, ".", VisitDate, sep="")) %>%
  select(-c(VisitDate, PlotID, PlotM, Quadrat))

# Pull Classfication info; add quadrat id, quadrat-visit ID, plot-visit ID, life form
classn <- sqlQuery(channel, paste("select * from Classification"))
  colnames(classn) <- c("VisitDate", "PlotID", "PlotM", "Species", "Total", "Live", "Senesced")
  classn$Species <- trimws(classn$Species) #remove leading/trailing whitespace
classn <- classn %>%
  mutate(Quadrat = paste(PlotID,"-",PlotM, sep="")) %>%
	mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep="")) %>%
  mutate(PlotVisit = paste(PlotID, ".", VisitDate, sep="")) %>%
  left_join(spp, by = "Species")
    	for(i in 1:nrow(classn)) {  #all "unknown" species are forbs
    	  classn$LifeForm[i] <- ifelse(grepl('UNK ', classn$Species[i]), "forb", next)
    	}

#Pull ClipPlots table; add unique quadrat ID and quadrat-visit ID
clip <- sqlQuery(channel, paste("select * from ClipPlots"))
colnames(clip) <- c("VisitDate", "PlotID", "PlotM", "LifeForm", "EmptyBag",
                    "Total", "Live", "Senesced", "WetWt", "DryWt")
clip <- clip %>%
  mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep="")) %>%
  mutate(PlotVisit = paste(PlotID, ".", VisitDate, sep=""))

#########
## DATA - MANIPULATIONS/CALCULATIONS

#Rescale species % cover 
cover <- full_join(cover, classn, by = "QuadratVisit")
cover$RescaledCover <- ifelse(cover$LifeForm == "forb", cover$Total/cover$Forb,
                                      ifelse(cover$LifeForm == "graminoid", 
                                             cover$Total/cover$Grass, 
                                             ifelse(NA)))

#####################
## KRISTIN SOMETHING BELOW THIS IS EFFED UP.
## FIX IT.
     
#Apportion species-specific biomass based on % cover
drywt <- clip %>%
  select(QuadratVisit, LifeForm, DryWt) %>%
  spread(LifeForm, DryWt) %>%
  rename(ForbWt = Forb, GrassWt = Grass) %>%
  full_join(cover, by = "QuadratVisit") %>%
  select(PlotVisit, QuadratVisit, Species, RescaledCover, LifeForm, ForbWt, GrassWt)
drywt$grams <- ifelse(drywt$LifeForm == "forb", drywt$RescaledCover*drywt$ForbWt,
                      ifelse(drywt$LifeForm == "graminoid", drywt$RescaledCover*drywt$GrassWt,
                              ifelse(NA)))

#Scale up to plot level - all herbaceous biomass
biomass <- drywt
biomass$grams[is.na(biomass$grams)] <- 0
biomass <- summarise(group_by(biomass, PlotVisit, Species), g0.75m = sum(grams))
biomass <- biomass[-1767,] #remove NA row caused by above line for some reason
biomass$g1m <- biomass$g0.75m*1.33333333333333
biomass <- left_join(biomass, spp, by = "Species")


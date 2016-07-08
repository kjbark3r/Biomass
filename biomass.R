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
colnames(cover) <- c("VisitDate", "PlotID", "PlotM", "GrassCov", "ShrubCov",
                     "SubShrubCov", "ForbCov", "MossLichenCov", "NonVegCov", "SmTreeCov")
cover <- mutate(cover, Quadrat = paste(PlotID,"-",PlotM, sep="")) %>%
  mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep="")) %>%
  select(c(GrassCov, ForbCov, QuadratVisit))

# Pull Classfication info; add quadrat id, quadrat-visit ID, plot-visit ID, life form
classn <- sqlQuery(channel, paste("select * from Classification"))
  colnames(classn) <- c("VisitDate", "PlotID", "PlotM", "Species", "Total", "Live", "Senesced")
  classn$Species <- trimws(classn$Species) #remove leading/trailing whitespace
classn <- classn %>%
  mutate(Quadrat = paste(PlotID,"-",PlotM, sep="")) %>%
	mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep="")) %>%
  mutate(PlotVisit = paste(PlotID, ".", VisitDate, sep="")) %>%
  left_join(spp, by = "Species") %>%
  subset(LifeForm == "forb" | LifeForm == "graminoid")
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
sppcover <- left_join(cover, classn, by = "QuadratVisit")
sppcover$RescaledCover <- ifelse(sppcover$LifeForm == "forb", sppcover$Total/sppcover$ForbCov,
                                      ifelse(sppcover$LifeForm == "graminoid", 
                                             sppcover$Total/sppcover$GrassCov, 
                                             ifelse(NA)))
sppcover <- subset(sppcover, select = c(PlotVisit, QuadratVisit, Species, 
                                        RescaledCover, LifeForm, ForbCov, GrassCov))

#biomass - plot level
drywt <- clip %>%
  select(QuadratVisit, LifeForm, DryWt) %>%
  spread(LifeForm, DryWt) %>%
  rename(ForbWt = Forb, GrassWt = Grass) 
drywt$ForbWt[is.na(drywt$ForbWt)] <- 0 #replace NA with 0 
drywt$GrassWt[is.na(drywt$GrassWt)] <- 0  #bc technically weighed it

#all herbaceous biomass (g/m^2)
biomass.plot <- sppcover %>%
  select(PlotVisit, QuadratVisit, ForbCov, GrassCov) %>%
  inner_join(drywt, by = "QuadratVisit") 
biomass.plot <- biomass.plot[!duplicated(biomass.plot),] #remove duplicate rows
biomass.plot <- summarise(group_by(biomass.plot, PlotVisit), gForbs = sum(ForbWt)*1.33333,
                          gGrass = sum(GrassWt)*1.33333)
biomass.plot$gHerb <- biomass.plot$gForbs+biomass.plot$gGrass
biomass.plot$PlotID <- substr(biomass.plot$PlotVisit, 1, 3)
biomass.plot$Date <- substr(biomass.plot$PlotVisit, 5, 14)

write.csv(biomass.plot, file = "biomass_allherbaceous.csv", row.names = FALSE)

#Species-specific biomass (g/m^2) PER QUADRAT
biomass.spp <- left_join(sppcover, drywt, by = "QuadratVisit")
biomass.spp$ClipGrams <- ifelse(biomass.spp$LifeForm == "forb", biomass.spp$RescaledCover*biomass.spp$ForbWt,
                      ifelse(biomass.spp$LifeForm == "graminoid", biomass.spp$RescaledCover*biomass.spp$GrassWt,
                             ifelse(NA)))
biomass.spp <- biomass.spp[!is.na(biomass.spp$ClipGrams),] #remove quadrats without clip plots

####################################
### FAIR WARNING
### BELOW CODE MAY NOT WORK

#forage biomass per plot-visit
forage <- read.csv("forageplants.csv")
biomass.forage <- left_join(forage, biomass.spp, by = "Species") 
biomass.forage <- summarise(group_by(biomass.forage, PlotVisit), grams = sum(ClipGrams)*1.33333)

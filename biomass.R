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

# LIFE FORM 
spp <- sqlQuery(channel, paste("select PlantCode, LifeForm, NameScientific
                                 from NSERP_SP_list"))
spp <- rename(spp, Species = PlantCode)
  
# COVER - plus quadrat ID, quadrat-visit ID, plot-visit ID
cover <- sqlQuery(channel, paste("select * from Cover"))
colnames(cover) <- c("VisitDate", "PlotID", "PlotM", "GrassCov", "ShrubCov",
                     "SubShrubCov", "ForbCov", "MossLichenCov", "NonVegCov", "SmTreeCov")
cover <- mutate(cover, Quadrat = paste(PlotID,"-",PlotM, sep="")) %>%
  mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep="")) %>%
  select(c(GrassCov, ForbCov, QuadratVisit))

# CLASSIFICATION - plus quadrat id, quadrat-visit ID, plot-visit ID, life form, genus
classn <- sqlQuery(channel, paste("select * from Classification"))
  colnames(classn) <- c("VisitDate", "PlotID", "PlotM", "Species", "Total", "Live", "Senesced")
  classn$Species <- trimws(classn$Species) #remove leading/trailing whitespace
classn <- classn %>%
  mutate(Quadrat = paste(PlotID,"-",PlotM, sep="")) %>%
	mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep="")) %>%
  mutate(PlotVisit = paste(PlotID, ".", VisitDate, sep="")) %>%
  left_join(spp, by = "Species") %>%
  subset(LifeForm == "forb" | LifeForm == "graminoid")
  classn$Genus <- sapply(strsplit(forage$Species, " "), "[", 1)
for(i in 1:nrow(classn)) {  #all "unknown" species are forbs
  classn$LifeForm[i] <- ifelse(grepl('UNK ', classn$Species[i]), "forb", next)
}

# CLIP PLOTS - plus quadrat ID, quadrat-visit ID
clip <- sqlQuery(channel, paste("select * from ClipPlots"))
colnames(clip) <- c("VisitDate", "PlotID", "PlotM", "LifeForm", "EmptyBag",
                    "Total", "Live", "Senesced", "WetWt", "DryWt")
clip <- clip %>%
  mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep="")) %>%
  mutate(PlotVisit = paste(PlotID, ".", VisitDate, sep=""))

#########
## DATA - MANIPULATIONS/CALCULATIONS

#rescale species % cover 
sppcover <- left_join(cover, classn, by = "QuadratVisit")
sppcover$RescaledCover <- ifelse(sppcover$LifeForm == "forb", sppcover$Total/sppcover$ForbCov,
                                      ifelse(sppcover$LifeForm == "graminoid", 
                                             sppcover$Total/sppcover$GrassCov, 
                                             ifelse(NA)))
sppcover <- subset(sppcover, select = c(PlotVisit, QuadratVisit, Species, Genus,
                                        RescaledCover, LifeForm, ForbCov, GrassCov))

#dry weight per quadrat - all herbaceous by life form
drywt <- clip %>%
  select(QuadratVisit, LifeForm, DryWt) %>%
  spread(LifeForm, DryWt) %>%
  rename(ForbWt = Forb, GrassWt = Grass) 
drywt$ForbWt[is.na(drywt$ForbWt)] <- 0 #replace NA with 0 
drywt$GrassWt[is.na(drywt$GrassWt)] <- 0  

#biomass per quadrat - all herbaceous by species
biomass.spp <- left_join(sppcover, drywt, by = "QuadratVisit")
biomass.spp$ClipGrams <- ifelse(biomass.spp$LifeForm == "forb", biomass.spp$RescaledCover*biomass.spp$ForbWt,
                                ifelse(biomass.spp$LifeForm == "graminoid", biomass.spp$RescaledCover*biomass.spp$GrassWt,
                                       ifelse(NA)))
biomass.spp <- biomass.spp[!is.na(biomass.spp$ClipGrams),] #remove quadrats without clip plots
biomass.spp <- subset(biomass.spp, select = c(QuadratVisit, PlotVisit, PlotID, PlotM, Species, LifeForm, 
                                              NameScientific, RescaledCover, ClipGrams))


#biomass per plot - all herbaceous
biomass.plot <- sppcover %>%
  select(PlotVisit, QuadratVisit, ForbCov, GrassCov) %>%
  inner_join(drywt, by = "QuadratVisit") 
biomass.plot <- biomass.plot[!duplicated(biomass.plot),] #remove rows duplicated by join
biomass.plot <- summarise(group_by(biomass.plot, PlotVisit), gForbs = sum(ForbWt)*1.33333,
                          gGrass = sum(GrassWt)*1.33333) #biomass to plot-level, g/m^2
biomass.plot$gHerb <- biomass.plot$gForbs+biomass.plot$gGrass
biomass.plot$PlotID <- substr(biomass.plot$PlotVisit, 1, 3)
biomass.plot$Date <- substr(biomass.plot$PlotVisit, 5, 14)


#biomass per plot - forage only
  forage <- read.csv("foragespecies.csv")
biomass.forage <- biomass.spp 
biomass.forage$NameScientific <- as.character(biomass.forage$NameScientific) #add genus
biomass.forage$Genus <- sapply(strsplit(biomass.forage$NameScientific, " "), "[", 1)
biomass.forage <- semi_join(biomass.forage, forage, by = "Genus") #forage plants only
biomass.forage <- summarise(group_by(biomass.forage, PlotVisit), grams = sum(ClipGrams)*1.33333)

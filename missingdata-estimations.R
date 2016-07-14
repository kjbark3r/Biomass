#################################################
##### ESTIMATING DRY WEIGHT FROM WET WEIGHT #####
#AND FORAGE SPECIES BASED ON OTHER QUADRAT COVER#
############# NSERP  KJB July 2016  #############
#################################################


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

#read in data & remove 9999s
wts <- sqlQuery(channel, paste("select * from ClipPlots"))
colnames(wts) <- c("VisitDate", "PlotID", "PlotM", "LifeForm", "EmptyBag",
                    "Total", "Live", "Senesced", "WetWt", "DryWt")
wts.rec <- wts[!wts$DryWt == 9999.00,]

#regress dry ~ wet + b0
reg <- lm(DryWt ~ WetWt, data = wts.rec); summary(reg)
reg.intrxn <- lm(DryWt ~ WetWt * LifeForm, data = wts.rec); summary(reg.intrxn)

#for funzies - grass and forb separately to see if estimations differ much 
  #(maybe forbs lose more water weight?)
wts.forb <- wts.rec[wts.rec$LifeForm == "Forb",]          
wts.grass <- wts.rec[wts.rec$LifeForm == "Grass",] 
reg.forb <- lm(DryWt ~ WetWt, data = wts.forb); summary(reg.forb)
reg.grass <- lm(DryWt ~ WetWt, data = wts.grass); summary(reg.grass)
  #graphs
par(mfrow = c(3,1))
scatter.smooth(wts.rec$DryWt ~ wts.rec$WetWt, main = "All Data")
scatter.smooth(wts.forb$DryWt ~ wts.forb$WetWt, main = "Forbs",
               ylim = c(0, 80))
scatter.smooth(wts.grass$DryWt ~ wts.grass$WetWt, main = "Graminoids",
               ylim = c(0, 80), xlim = c(0,350))
  
#predict 9999s
to.est <- which(wts$DryWt == 9999.00)
  #see which are forbs and which are graminoids
  wts[to.est[1], "LifeForm"] #forb
  wts[to.est[2], "LifeForm"] #forb
  wts[to.est[3], "LifeForm"] #forb
  wts[to.est[4], "LifeForm"] #grass

wts[to.est[1], "DryWt"] <- reg.forb$coefficients[2]*wts[to.est[1], "WetWt"] + reg.forb$coefficients[1]
wts[to.est[2], "DryWt"] <- reg.forb$coefficients[2]*wts[to.est[2], "WetWt"] + reg.forb$coefficients[1]
wts[to.est[3], "DryWt"] <- reg.forb$coefficients[2]*wts[to.est[3], "WetWt"] + reg.forb$coefficients[1]
wts[to.est[4], "DryWt"] <- reg.grass$coefficients[2]*wts[to.est[4], "WetWt"] + reg.grass$coefficients[1]

#print estimates to paste into access
#to avoid weirdness with column names/importing back from R
wts[to.est[1],]
wts[to.est[2],]
wts[to.est[3],]
wts[to.est[4],]

############################
#####forage forb missing info
###2 clip plots with forb weight but no forb species recorded in plot
###########################

#calculate forb cover in each plot other than the one missing data
#calculate forage forb cover in each plot other than the one missing data
#calculate what % of forbs are forage forbs
#calculate forage forb biomass based on that percentage

###
#RAW DATA

# COVER - INCLUDING NON-CLIP PLOTS
allcover <- classn %>%
  group_by(QuadratVisit, LifeForm) %>%
  summarise(Wt = sum(Total)) %>%
  spread(LifeForm, Wt) %>%
  rename(ForbCov = forb, GrassCov = graminoid) 
allcover$ForbCov[is.na(allcover$ForbCov)] <- 0; allcover$GrassCov[is.na(allcover$GrassCov)] <- 0


#ALL QUADRAT SPP - INCLUDING NON-CLIP PLOTS
allquadrat.spp <- left_join(allcover, classn, by = "QuadratVisit") %>%
  select(-PlotVisit) #avoid duplicated column name after next join
  #rescale species % cover
allquadrat.spp$RescaledCover <- ifelse(allquadrat.spp$LifeForm == "forb", allquadrat.spp$Total/allquadrat.spp$ForbCov,
                                      ifelse(allquadrat.spp$LifeForm == "graminoid", 
                                             allquadrat.spp$Total/allquadrat.spp$GrassCov, 
                                             ifelse(NA)))
allquadrat.spp <- left_join(allquadrat.spp, quadrat, by = "QuadratVisit")
allquadrat.spp <- subset(allquadrat.spp, select = c(PlotVisit, QuadratVisit, Species, Genus, 
                                              RescaledCover, LifeForm, ForbCov, GrassCov, 
                                              ForbWt, GrassWt, AllHerbWt))
  #estimate species weight based on adjusted %cover
allquadrat.spp$ClipGrams <- ifelse(allquadrat.spp$LifeForm == "forb", allquadrat.spp$RescaledCover*allquadrat.spp$ForbWt,
                                ifelse(allquadrat.spp$LifeForm == "graminoid", allquadrat.spp$RescaledCover*allquadrat.spp$GrassWt,
                                       ifelse(NA)))

###
#ESTIMATIONS

#323.2014-06-30.20
forage323 <- allquadrat.spp %>%
  filter(QuadratVisit %in% "323.2014-06-30.0" | QuadratVisit %in% "323.2014-06-30.10" | 
         QuadratVisit %in% "323.2014-06-30.20" | QuadratVisit %in% "323.2014-06-30.30" | 
         QuadratVisit %in% "323.2014-06-30.40") %>%
  semi_join(foragespp, by = "Genus") %>%
  subset(LifeForm == "forb")

for323 <- allcover %>%
  filter(QuadratVisit %in% "323.2014-06-30.0" | QuadratVisit %in% "323.2014-06-30.10" | 
         QuadratVisit %in% "323.2014-06-30.20" | QuadratVisit %in% "323.2014-06-30.30" | 
         QuadratVisit %in% "323.2014-06-30.40") %>%
  select(QuadratVisit, ForbCov)  %>%
  group_by(QuadratVisit) %>%
  full_join(forage323, by = "QuadratVisit") %>%
  summarise(ForageForbCov = sum(RescaledCover))
for323$ForageForbCov[is.na(for323$ForageForbCov)] <- 0
ff323 <- mean(for323$ForageForbCov)

#344.2014-06-16.20
forage344 <- allquadrat.spp %>%
  filter(QuadratVisit %in% "344.2014-06-16.0" | QuadratVisit %in% "344.2014-06-16.10" | 
         QuadratVisit %in% "344.2014-06-16.20" | QuadratVisit %in% "344.2014-06-16.30" | 
         QuadratVisit %in% "344.2014-06-16.40") %>%
  semi_join(foragespp, by = "Genus") %>%
  subset(LifeForm == "forb")

for344 <- allcover %>%
  filter(QuadratVisit %in% "344.2014-06-16.0" | QuadratVisit %in% "344.2014-06-16.10" | 
         QuadratVisit %in% "344.2014-06-16.20" | QuadratVisit %in% "344.2014-06-16.30" | 
         QuadratVisit %in% "344.2014-06-16.40") %>%
  select(QuadratVisit, ForbCov)  %>%
  group_by(QuadratVisit) %>%
  full_join(forage344, by = "QuadratVisit") %>%
  summarise(ForageForbCov = sum(RescaledCover))
for344$ForageForbCov[is.na(for344$ForageForbCov)] <- 0
ff344 <- mean(for344$ForageForbCov)

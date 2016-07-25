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
## DATA - BIOMASS PLOTS
if (file.exists(wd_workcomp)) {
  channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                             dbq=C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/Sapphire_Veg_Database.accdb")
  } else {
    if(file.exists(wd_laptop)) {
      channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                               dbq=C:/Users/kjbark3r/Documents/NSERP/Databases/Sapphire_Veg_Database.accdb")
    } else {
      cat("Are you SURE you got that file path right?\n")
  }
}
rm(wd_workcomp, wd_laptop)

#read in data & remove 9999s
wts <- sqlQuery(channel, paste("select * from ClipPlots"))
colnames(wts) <- c("VisitDate", "PlotID", "PlotM", "LifeForm", "EmptyBag",
                    "Total", "Live", "Senesced", "WetWt", "DryWt")
wts.rec <- wts[!wts$DryWt == 9999,]

#regress dry ~ wet + b0
reg <- lm(DryWt ~ WetWt, data = wts.rec); summary(reg)
reg.intrxn <- lm(DryWt ~ WetWt * LifeForm, data = wts.rec); summary(reg.intrxn)

#for funzies - grass and forb separately to see if estimations differ much 
  #(maybe forbs lose more water weight?)
wts.forb <- wts.rec[wts.rec$LifeForm == "Forb",]          
wts.grass <- wts.rec[wts.rec$LifeForm == "Grass",] 
reg.forb <- lm(DryWt ~ WetWt, data = wts.forb); summary(reg.forb)
reg.grass <- lm(DryWt ~ WetWt, data = wts.grass); summary(reg.grass)

##AIC##
#separate grass/forb models?
Cand.set <- list( )
Cand.set[[1]] <- glm(DryWt ~ WetWt, data = wts.rec)
Cand.set[[2]] <- glm(DryWt ~ WetWt + LifeForm, data = wts.rec)
names(Cand.set) <- c("Lumped", "Diff LifeForm")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)

Cand.set <- list( )
Cand.set[[1]] <- glm(DryWt ~ WetWt + LifeForm, data = wts.rec)
Cand.set[[2]] <- glm(DryWt ~ WetWt, data = wts.forb)
Cand.set[[3]] <- glm(DryWt ~ WetWt, data = wts.grass)
names(Cand.set) <- c("Diff LifeForm", "Forb Only", "Grass Only")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)

  #graphs
#par(mfrow = c(3,1))
#scatter.smooth(wts.rec$DryWt ~ wts.rec$WetWt, main = "All Data")
#scatter.smooth(wts.forb$DryWt ~ wts.forb$WetWt, main = "Forbs",
#               ylim = c(0, 80))
#scatter.smooth(wts.grass$DryWt ~ wts.grass$WetWt, main = "Graminoids",
#               ylim = c(0, 80), xlim = c(0,350))
  
###########
#ESTIMATE 9999s
#pathetically

(to.est <- which(wts$DryWt == 9999.00))
  #see which are forbs and which are graminoids
  wts[to.est[1], "LifeForm"] #G
  wts[to.est[2], "LifeForm"] #G
  wts[to.est[3], "LifeForm"] #F
  wts[to.est[4], "LifeForm"] #G
  wts[to.est[5], "LifeForm"] #G
  wts[to.est[6], "LifeForm"] #G
  wts[to.est[7], "LifeForm"] #F
  wts[to.est[8], "LifeForm"] #F
  wts[to.est[9], "LifeForm"] #F
  wts[to.est[10], "LifeForm"] #F
  wts[to.est[11], "LifeForm"] #G
  wts[to.est[12], "LifeForm"] #F
  wts[to.est[13], "LifeForm"] #F
  wts[to.est[14], "LifeForm"] #G
  wts[to.est[15], "LifeForm"] #F
  wts[to.est[16], "LifeForm"] #F
  wts[to.est[17], "LifeForm"] #G
  wts[to.est[18], "LifeForm"] #G
  wts[to.est[19], "LifeForm"] #G
  
wts[to.est[1], "DryWt"] <- reg.grass$coefficients[2]*wts[to.est[1], "WetWt"] + reg.grass$coefficients[1]
wts[to.est[2], "DryWt"] <- reg.grass$coefficients[2]*wts[to.est[2], "WetWt"] + reg.grass$coefficients[1]
wts[to.est[3], "DryWt"] <- reg.forb$coefficients[2]*wts[to.est[3], "WetWt"] + reg.forb$coefficients[1]
wts[to.est[4], "DryWt"] <- reg.grass$coefficients[2]*wts[to.est[4], "WetWt"] + reg.grass$coefficients[1]
wts[to.est[5], "DryWt"] <- reg.grass$coefficients[2]*wts[to.est[5], "WetWt"] + reg.grass$coefficients[1]
wts[to.est[6], "DryWt"] <- reg.grass$coefficients[2]*wts[to.est[6], "WetWt"] + reg.grass$coefficients[1]
wts[to.est[7], "DryWt"] <- reg.forb$coefficients[2]*wts[to.est[7], "WetWt"] + reg.forb$coefficients[1]
wts[to.est[8], "DryWt"] <- reg.forb$coefficients[2]*wts[to.est[8], "WetWt"] + reg.forb$coefficients[1]
wts[to.est[9], "DryWt"] <- reg.forb$coefficients[2]*wts[to.est[9], "WetWt"] + reg.forb$coefficients[1]
wts[to.est[10], "DryWt"] <- reg.forb$coefficients[2]*wts[to.est[10], "WetWt"] + reg.forb$coefficients[1]
wts[to.est[11], "DryWt"] <- reg.grass$coefficients[2]*wts[to.est[11], "WetWt"] + reg.grass$coefficients[1]
wts[to.est[12], "DryWt"] <- reg.forb$coefficients[2]*wts[to.est[12], "WetWt"] + reg.forb$coefficients[1]
wts[to.est[13], "DryWt"] <- reg.grass$coefficients[2]*wts[to.est[13], "WetWt"] + reg.grass$coefficients[1]
wts[to.est[14], "DryWt"] <- reg.grass$coefficients[2]*wts[to.est[14], "WetWt"] + reg.grass$coefficients[1]
wts[to.est[15], "DryWt"] <- reg.forb$coefficients[2]*wts[to.est[15], "WetWt"] + reg.forb$coefficients[1]
wts[to.est[16], "DryWt"] <- reg.forb$coefficients[2]*wts[to.est[16], "WetWt"] + reg.forb$coefficients[1]
wts[to.est[17], "DryWt"] <- reg.grass$coefficients[2]*wts[to.est[17], "WetWt"] + reg.grass$coefficients[1]
wts[to.est[18], "DryWt"] <- reg.grass$coefficients[2]*wts[to.est[18], "WetWt"] + reg.grass$coefficients[1]
wts[to.est[19], "DryWt"] <- reg.grass$coefficients[2]*wts[to.est[19], "WetWt"] + reg.grass$coefficients[1]

#print estimates to paste into access
#to avoid weirdness with column names/importing back from R
wts[to.est[1],]
wts[to.est[2],]
wts[to.est[3],]
wts[to.est[4],]
wts[to.est[5],]
wts[to.est[6],]
wts[to.est[7],]
wts[to.est[8],]
wts[to.est[9],]
wts[to.est[10],]
wts[to.est[11],]
wts[to.est[12],]
wts[to.est[13],]
wts[to.est[14],]
wts[to.est[15],]
wts[to.est[16],]
wts[to.est[17],]
wts[to.est[18],]
wts[to.est[19],]



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

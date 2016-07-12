###############################################
#### ESTIMATING DRY WEIGHT FROM WET WEIGHT ####
############ NSERP  KJB July 2016  ############
###############################################


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

##########################################################
#######  MISC CODE AND TROUBLESHOOTING RELATED TO  #######
#### HERBACEOUS BIOMASS ESTIMATION - NSERP STUDY AREA ####
################## KJB  July 2016  #######################
##########################################################

## WD
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\Biomass"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\Biomass"
	if (file.exists(wd_workcomp)) {
	  setwd(wd_workcomp)
	} else {
	  if(file.exists(wd_laptop)) {
		setwd(wd_laptop)
	  } else {
		if(file.exists(wd_external)) {
		  setwd(wd_external)
		} else {
		  cat("Are you SURE you got that file path right?\n")
		}
	  }
	}
	rm(wd_workcomp, wd_laptop)


## FIGURING OUT ACCESS CONNECTION STUFF

library(RODBC)
library(dplyr)

############################################
## RUN CODE BELOW THIS LINE PIECEMEAL
############################################

####################	
# goal 1: pull classification info

channel <- odbcConnectAccess("C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/Sapphire_Veg_Phenology_2015-10-16Kelly")
  #eff, need 32-bit Windows or R, or try different function. Diff fcn first

	#diff fcn in 64 bit R
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};dbq=C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/Sapphire_Veg_Phenology_2015-10-16Kelly.accdb")
  #nope. switching to 32 bit R
  #EEEEEE it worked!

class <- sqlQuery(channel, paste("select * from Classification"))

# goal 2: create unique quadrat IDs

class %>%
  rename(PlotID = Plot ID)
  #ugh, it's confused by spaces. Rename instead.

class <- make.names(class)
# only works for vectors; would have to do manually either way

colnames(class) <- c("VisitDate", "PlotID", "PlotM", "Species", "Total", "Live", "Senesced")

class <- class %>%
  mutate(Quadrat = paste(PlotID,"-",PlotM))
#works but has space between "-" and each number

class <- class %>%
  mutate(Quadrat = paste(PlotID,"-",PlotM, sep=""))
#perf


# goal 3: add plant type
## can pull this from NSERP_SP_list table - PlantCode and LifeForm

# figuring out whay just some of them didn't work
class[class$LifeForm %in% NA,]
#i think they have spaces in the names

class$PlantCode <- make.names(class$PlantCode)
#nope, makes spaces in to periods.

class$PlantCode <- gsub(" ", "", class$PlantCode, fixed = TRUE)
#this does work, but you didn't actually want it to remove spaces
  #from SP entries. need to write out exceptions.

#what this needs to do:
# remove typo'd spaces
  #but not the spaces from
    # XXXX SP
    # UNK X
# then pull LifeForm based on PlantCode
  #but not for
    # XXXX SP
      #for these base it on the first 4 letters
    # UNK X  
      #for these base it on the last letters (after UNK)

# doing it without changing anything first to see what names are left
class <- left_join(class, spp, by = "PlantCode")
class[class$LifeForm %in% NA,]
  #write exceptions based on NAs

#1. identify values that include "UNK"
  #does dplyr select work?
  a <- select(class$PlantCode, contains("UNK"))
  #not so much

  # %in% ?
  class[class$PlantCode %in% "UNK"*,]
  #tried a few variations and can't get it to work

  #how bout the grep thing?
  grep('UNK ', class$PlantCode, value = TRUE)
  #nice
  
#2. label LifeForm for UNKs
  if(grep('UNK ', class$PlantCode, value = TRUE)){
    class$LifeForm <- "forb"
  }
  #this doesn't work - need a y/n answer to if
  #try grepl - returns "logical vector"

  if(grepl('UNK ', class$PlantCode)){
    class$LifeForm <- "forb"
  }
  #nope - need ifelse if using grepl 
  #bc ifelse is vectorized
  
  ifelse(grepl('UNK ', class$PlantCode), "forb", )
  #close. have to have "no" argument; can't leave it blank
  
  class$LifeForm <- ifelse(grepl('UNK ', class$PlantCode), "forb", 
                           class$LifeForm)
  #worked for the UNKs but screwed up the others
  
  for (i in 1:nrow(class)){
    class$LifeForm[i] <- ifelse(grepl('UNK ', class$PlantCode), "forb", 
                           class$LifeForm[i])
  }
  #effs them all up (makes all NA)
  
    for (i in 1:nrow(class)){
    class$LifeForm[i,] <- ifelse(grepl('UNK ', class$PlantCode[i,]), "forb", 
                           class$LifeForm[i,])
  }
  #error
  
  for (i in 1:nrow(class)){
  class$LifeForm[i] <- ifelse(grepl('UNK ', class$PlantCode[i]), "forb", 
                         class$LifeForm[i])
  }
  #warning, i made all your data into NAs, hope that's cool. love, r

  for (i in 1:nrow(class)){
  class$LifeForm[i] <- ifelse(grepl('UNK ', class$PlantCode), "forb", 
                         class$LifeForm[i])
  }
  #ditto above

  class$LifeForm[1:length(class)] <- ifelse(grepl('UNK ', class$PlantCode), "forb", 
                         class$LifeForm)  
  #2 warnings, but didn't make all NAs (just did nothing)
  
  class$LifeForm[1:nrow(class)] <- ifelse(grepl('UNK ', class$PlantCode), "forb", 
                         class$LifeForm)  
  #warning, invalid factor level, made all NAs

    class$LifeForm <- ifelse(grepl('UNK ', class$PlantCode), "forb", 
                           next)
    #error, you didn't actually make this a for loop, duh
    
  for(i in 1:nrow(class)) {
    class$LifeForm[i] <- ifelse(grepl('UNK ', class$PlantCode[i]), "forb", 
                           next)
  }
    #geez, took ya long enough

## REMOVING LEADING/TRAILING SPACES FROM PLANT CODES (TYPOS)

    class$PlantCode <- gsub(" ", "", class$PlantCode, fixed = TRUE)
    #removes all spaces (incl SP or UNK entries), not just leading/trailing
  
    #trying to use Trim function from Access
    #Access code would be::: TrimmedName: Trim([CategoryName])
    
    class$PlantCode <- sqlQuery(channel, paste("Trim([PlantCode])"))
    #whoa, makes PlantCode into super weird stuff
    
    class$PlantCode <- sqlQuery(channel, paste("PlantCode: Trim([PlantCode])"))
    #ditto    
    
    class$PlantCode <- sqlQuery(channel, paste("PlantCode2: Trim([PlantCode])"))
    #samesies
    
    class$PlantCode <- sqlQuery(channel, paste("Expr1: Trim([PlantCode])"))
    #yup...
    
    class$PlantCode <- sqlQuery(channel, paste("Trim([PlantCode])"))
    #stillllll
    
    #oh my god this is a thing?
    trimws(class$PlantCode)

    
### RESCALE SPECIES % COVER (MAKE % OF LIFE FORM IN QUADRAT)
    
    # % total forb cover in quadrat: from Cover table
    cover <- sqlQuery(channel, paste("select * from Cover"))
    
      #remove spaces from column names
    cover <- make.names(cover[,])
      #eh screw it, there are also dashes and slashes and ugh. GOing manual.
  
#make unique quadrat-visit identifier
    test <- class %>%
      mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep=""))
    
    testcov <- cover %>%
      mutate(QuadratVisit = paste(PlotID,".",PlotM, ".", VisitDate, sep=""))
    
    visit <- unique(test$QuadratVisit)
    
    bignasty <- left_join(test, testcov, by = "QuadratVisit")
    
    bignasty$RescaledCover <- ifelse(bignasty$LifeForm == "forb", bignasty$Total/bignasty$Forb,
                                      ifelse(bignasty$LifeForm == "graminoid", bignasty$Total/bignasty$Grass,
                                             NA)
                               )
    

## ARE SUB-SHRUBS INCLUDED IN PHENOLOGY PLOTS?
    ## no
spp[spp$LifeForm %in% "sub-shrub",]
cover[cover$LifeForm %in% "subshrub",]
class[class$LifeForm %in% "subshrub",]
class[class$Species %in% "ARCUVA",]
class[class$Species %in% "BALSAG",]  

## need for loop here? (code cleanup)

hm <- select(class, -LifeForm)

	for(i in 1:nrow(hm)) {
	  hm$LifeForm[i] <- ifelse(grepl('UNK ', hm$Species[i]), "forb", 
								ifelse(hm$LifeForm == "sub-shrub", "forb", next))
	}

  #OR

bignasty$RescaledCover <- ifelse(bignasty$LifeForm == "forb", bignasty$Total/bignasty$Forb,
                                      ifelse(bignasty$LifeForm == "graminoid", bignasty$Total/bignasty$Grass,
                                             NA)
                               )
#############
## ACCESS CONNECTION ON LAPTOP

if(file.exists("C:/Users/kjbark3r/Documents/NSERP/Databases/Sapphire_Veg_Phenology.accdb")){
  cat("Yay!")} else {
    cat("BOO")}
#ok, no problem finding the file. something about the driver/connection.

channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                             dbq=C:/Users/kjbark3r/Documents/NSERP/Databases/Sapphire_Veg_Phenology.accdb")

#############
## make separate columns for grass dry weight and forb dry wt

drywt <- clip %>%
  select(QuadratVisit, LifeForm, DryWt) %>%
  spread(LifeForm, DryWt) 

drywt <- rename(drywt, ForbWt = Forb, GrassWt = Grass)

#############
## scale spp-specific biomass to plot level

biomass <- drywt %>%
  group_by(PlotVisit, Species) %>%
  mutate(g0.75m = sum(grams))
##only pulled some values, lots of NAs, not sure what happened

biomass <- drywt %>%
  group_by(PlotVisit, Species) %>%
  summarise(g0.75m = sum(grams))
##cleaner result but still not working for everything

biomass <- drywt %>%
  group_by(Species, PlotVisit) %>%
  summarise(g0.75m = sum(grams))
##similar to above - not sure why some work and others not
##setup is good; numbers are wrong

sum(drywt$grams[1:3])
#works as expected

test <- drywt %>%
  group_by(PlotVisit) %>%
  summarise(totalg = sum(grams))
##gives all NAs

test <- group_by(drywt, PlotVisit)
summarise(test, sum(grams))
##whyyyyy?
##grams are numeric...

test <- group_by(drywt, PlotVisit)
test$totalg <- mean(test$grams)
##same problem. so not an issue with summarise or sum
##maybe bc NAs? 

test <- drywt
test$grams[is.na(test$grams)] <- 0
biomass <- test %>%
  group_by(PlotVisit, Species) %>%
  mutate(g0.75m = sum(grams))
##i think this worked but didn't collapse data by plot visit
##bc somebody forgot to summarise, not mutate...

test <- drywt
test$grams[is.na(test$grams)] <- 0
test <- test %>%
  group_by(PlotVisit, Species) %>%
  summarise(g0.75m = sum(grams))
##hooray
rm(test)

#now same as above but keep LifeForm data  

test <- drywt
test$grams[is.na(test$grams)] <- 0
test <- test %>%
  group_by(PlotVisit, Species) %>%
  filter(g0.75m == sum(grams))
#Rsplosion

test <- drywt
test$grams[is.na(test$grams)] <- 0
test <- test %>%
  group_by(PlotVisit) %>%
  filter(g0.75m == sum(grams))
#ditto above
#filter prob doesn't make new columns

test <- drywt
test$grams[is.na(test$grams)] <- 0
test <- test %>%
  group_by(PlotVisit, Species) %>%
  filter(grams == sum(grams))
#well... it functionally did something...
#(something totally wrong)

test <- drywt
test$grams[is.na(test$grams)] <- 0
test <- test %>%
  group_by(PlotVisit) %>%
  filter(grams == sum(grams))
#this one's even worse
#screw it, i'll just re-join lifeform
rm(test)

#####
#sum biomass per plot visit; add PlotID and Date (for ease of use later); export
all.herb.biomass <- biomass %>%
  group_by(PlotVisit) %>%
  summarise(Biomass = sum(g1m)) %>%
  mutate(PlotID = substr(PlotVisit, 1, 3)) %>%
  mutate(Date = substr(PlotVisit, 5, 14)) %>%
  select(PlotID, Date, PlotVisit, Biomass)
#wrong numbers - not sure why
write.csv(all.herb.biomass, file = "herbaceousbiomass.csv", row.names = FALSE)

##next steps:
#look for NAs in base data
  #found some rows all NA except QuadratVisit in drywt df
spp[spp$Species %in% NA,]
cover[cover$Species %in% NA,]
classn[classn$Species %in% NA,]
clip[clip$PlotVisit %in% NA,]
drywt[drywt$Species %in% NA,]
  #only NAs are in here - all are plot 325, visit 2014-06-18
  #also missing some forb and grass weights in here
  
#investigating drywt step by step
drywt <- clip %>%
  select(QuadratVisit, LifeForm, DryWt)
  #issues already... 4 drywts are 9999.00

#determining whether those 9999s are the only thing screwing up my #s
biomass[biomass$PlotVisit == "681.2014-09-04" & biomass$LifeForm == "forb",]
biomass[biomass$PlotVisit == "344.2014-09-03" & biomass$LifeForm == "forb",]
biomass[biomass$PlotVisit == "340.2015-05-27" & biomass$LifeForm == "forb",]
biomass[biomass$PlotVisit == "344.2015-07-06" & biomass$LifeForm == "graminoid",]

#checking for incorrect NAs at every step
#cuz they sure as shit show up at some point
any(is.na(cover$ForbCov)); any(is.na(cover$GrassCov))
  #nope, cover df is good
any(is.na(drywt$DryWt))
  #none here until spread()
  #those NAs are valid - but should prob be 0s
    #for the purposes of this analysis
any(is.na(drywt$ForbWt)); any(is.na(drywt$GrassWt))
drywt[is.na(drywt$ForbWt),]
  #NAs show back up when add cover to df
any(is.na(cover$RescaledCover))
  #ah ha
a <- cover[is.na(cover$RescaledCover),]
unique(a$LifeForm)
  #makes sense; happens for shrubs
any(is.na(drywt$grams))
  #whyyyy

########
## making sure cover df and drywt df have same quadratvisits
(wtf <- setdiff(cover$QuadratVisit, drywt$QuadratVisit))
  #nope. . .

########
## check ALL the NAs!

#sppcover
any(is.na(sppcover[,1])); any(is.na(sppcover[,2])); any(is.na(sppcover[,3]))
any(is.na(sppcover[,4])); any(is.na(sppcover[,5])); any(is.na(sppcover[,6]))
any(is.na(sppcover[,7]))

#drywt
any(is.na(drywt[,1])); any(is.na(drywt[,2])); any(is.na(drywt[,3]))
any(is.na(drywt[,4])); any(is.na(drywt[,5])); any(is.na(drywt[,6]))
any(is.na(drywt[,7])); any(is.na(drywt[,8])); any(is.na(drywt[,9]))

########
## comparing plot-level herbaceous vs forage-only biomass

hm <- biomass.plot %>%
  full_join(biomass.forage, by = "PlotVisit") %>%
  rename(gForage = grams) %>%
  mutate(diff = gHerb-gForage)
#hm... 4 plots have higher forage biomass than herbaceous
#and those aren't plots we have weird data for (afaik)
#which means i effed something up

#things to check:
## calculate herbaceous biomass by summing species data
    ## instead of using the general forb/graminoid data
test.biomass.plot <- biomass.spp %>%
  inner_join(drywt, by = "QuadratVisit") 
test.biomass.plot <- summarise(group_by(biomass.plot, PlotVisit), gForbs = sum(ForbWt)*1.33333,
                          gGrass = sum(GrassWt)*1.33333) #biomass to plot-level, g/m^2

biomass.forage <- biomass.spp 
biomass.forage$NameScientific <- as.character(biomass.forage$NameScientific) #add genus
biomass.forage$Genus <- sapply(strsplit(biomass.forage$NameScientific, " "), "[", 1)
biomass.forage <- semi_join(biomass.forage, forage, by = "Genus") #forage plants only
biomass.forage <- summarise(group_by(biomass.forage, PlotVisit), grams = sum(ClipGrams)*1.33333)


#per plot - biomass, all herbaceous (by life form)
a.lifeform <- summarise(group_by(quadrat, PlotVisit), sum(AllHerbWt)*1.33333)
colnames(plot) <- c("PlotVisit", "SumHerb")
  #SAME THING BUT USING SPECIES WEIGHTS (to check numbers)
a.spp <- summarise(group_by(quadrat.spp, PlotVisit), sum(ClipGrams)*1.3333)
colnames(plot.test) <- c("PlotVisit", "SumSpp")
  #EFF.
hm <- full_join(plot, plot.test, by = "PlotVisit")
  hm$diff <- hm[,2]-hm[,3]

  #####ugh ok let's start at the beginning here
#base data
  # number of plots
length(unique(classn$PlotID))
length(unique(clip$PlotID))
length(unique(cover$PlotID))
  #all = 12
length(unique(classn$PlotVisit)); length(unique(clip$PlotVisit))
  #both 109 (PlotVisit not recorded in cover)
length(unique(classn$QuadratVisit)); length(unique(clip$QuadratVisit)); length(unique(cover$QuadratVisit))
  #545 quadrat visits; 325 clip plot visits
#worked up data
length(unique(quadrat$PlotID)); length(unique(quadrat$PlotVisit)); length(unique(quadrat$QuadratVisit))
length(unique(quadrat.spp$PlotID)); length(unique(quadrat.spp$PlotVisit)); length(unique(quadrat.spp$QuadratVisit))

#try without converting first
#per plot - biomass, all herbaceous (by life form)
a.lifeform <- summarise(group_by(quadrat, PlotVisit), sum(AllHerbWt))
colnames(a.lifeform) <- c("PlotVisit", "SumHerb")
  #SAME THING BUT USING SPECIES WEIGHTS (to check numbers)
a.spp <- summarise(group_by(quadrat.spp, PlotVisit), sum(ClipGrams))
colnames(a.spp) <- c("PlotVisit", "SumSpp")
a <- full_join(a.lifeform, a.spp, by = "PlotVisit")
a$diff <- round(a[,2]-a[,3])

#ok after drilling in to the database and investigating manually in excel
#i think some of the forb/grass cover sums are recorded incorrectly
#e.g. 322.2014-06-30.20 recorded grass cover is 6, but 
    #indiv spp data in classification data adds to 8
  #i am full of hatred for whoever has made these eggregious errors

#########################
## DELETED CODE
##########################


#Scale up to plot level - all herbaceous biomass
biomass <- drywt
biomass <- summarise(group_by(biomass, PlotVisit, Species), g0.75m = sum(grams))
#biomass <- biomass[-1767,] #remove NA row caused by above line for some reason
biomass$g1m <- biomass$g0.75m*1.33333333333333
biomass <- left_join(biomass, spp, by = "Species")

# COVER - plus quadrat ID, quadrat-visit ID, plot-visit ID
cover <- sqlQuery(channel, paste("select * from Cover"))
colnames(cover) <- c("VisitDate", "PlotID", "PlotM", "GrassCov", "ShrubCov",
                     "SubShrubCov", "ForbCov", "MossLichenCov", "NonVegCov", "SmTreeCov")
cover <- mutate(cover, Quadrat = paste(PlotID,"-",PlotM, sep="")) %>%
  mutate(QuadratVisit = paste(PlotID,".", VisitDate,".",PlotM, sep="")) %>%
  select(c(GrassCov, ForbCov, QuadratVisit))


#####################################
########## (PREVIOUS CALCULATIONS)
#biomass per plot - all herbaceous
biomass.plot <- quadrat.spp %>%
  select(PlotVisit, QuadratVisit, ForbCov, GrassCov) %>%
  inner_join(quadrat, by = "QuadratVisit") 
biomass.plot <- biomass.plot[!duplicated(biomass.plot),] #remove rows duplicated by join
biomass.plot <- summarise(group_by(biomass.plot, PlotVisit), gForbs = sum(ForbWt)*1.33333,
                          gGrass = sum(GrassWt)*1.33333) #biomass to plot-level, g/m^2
biomass.plot$gHerb <- biomass.plot$gForbs+biomass.plot$gGrass
biomass.plot$PlotID <- substr(biomass.plot$PlotVisit, 1, 3)
biomass.plot$Date <- substr(biomass.plot$PlotVisit, 5, 14)


#biomass per plot - forage only
biomass.forage <- quadrat.spp 
biomass.forage$NameScientific <- as.character(biomass.forage$NameScientific) #add genus
biomass.forage$Genus <- sapply(strsplit(biomass.forage$NameScientific, " "), "[", 1)
biomass.forage <- semi_join(biomass.forage, forage, by = "Genus") #forage plants only
biomass.forage <- summarise(group_by(biomass.forage, PlotVisit), grams = sum(ClipGrams)*1.33333)

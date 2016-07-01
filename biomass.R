##########################################################
#######  MISC CODE AND TROUBLESHOOTING RELATED TO  #######
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
		if(file.exists(wd_external)) {
		  setwd(wd_external)
		} else {
		  cat("Are you SURE you got that file path right?\n")
		}
	  }
	}
	rm(wd_workcomp, wd_laptop)


## PACKAGES

library(RODBC)
library(dplyr)
	
## DATA

	# connect to phenology database
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                             dbq=C:/Users/kristin.barker/Documents/NSERP/Databases and Mort Reports/Sapphire_Veg_Phenology_2015-10-16Kelly.accdb")

  #pull Classfication table and remove spaces from column names
class <- sqlQuery(channel, paste("select * from Classification"))
  colnames(class) <- c("VisitDate", "PlotID", "PlotM", "Species", "Total", "Live", "Senesced")

  #pull plant type data from NSERP_SP_list table 
spp <- sqlQuery(channel, paste("select PlantCode, LifeForm
                                 from NSERP_SP_list"))

  #add unique quadrat id and plant type
class <- class %>%
  mutate(Quadrat = paste(PlotID,"-",PlotM, sep="")) %>%
  rename(PlantCode = Species) %>%
class <- left_join(class, spp, by = "PlantCode")

class$PlantCode <- gsub(" ", "", class$PlantCode, fixed = TRUE)
#this does work, but you didn't actually want it to...
  #shouldn't remove spaces from SP or UNK entries. need to write out exceptions.


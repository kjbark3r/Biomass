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

#goal 4: rescale species %cover


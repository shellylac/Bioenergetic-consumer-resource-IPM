##############################################################
# CODE TO RUN THE BASELINE MODEL AND STORE OUTPUT  
##############################################################
rm(list=ls())
set.seed(321)

#Make the directory to hold the results/outputs
resultPath<-"specify output path here"
dir.create(resultPath, showWarnings = FALSE, recursive = T)

#set monthstorun (number of iterations)
monthstorun<-3000 # 250 years

#source the model set up code and set baseline seasons
#=================
source("ModelSetUpCode_part1.R")
source("BaselineSeasonsCode.R") # create new set of veg growth rates and snow months
stoch_veg_growth_rate<-baseline_veg_growth_rate         
snow_indicator<-baseline_snow_indicator                
#saveRDS(baseline_snow_indicator, "BL_snowindicator.rds")   # save for use in sensitivity analysis
#saveRDS(baseline_veg_growth_rate, "BL_veggrowthrate.rds")  # save for use in sensitivity analysis
#stoch_veg_growth_rate<-readRDS(BL_veggrowthrate)  # use stored growth rate
#snow_indicator<-readRDS(BL_snowindicator)         # use stored growth rate

#==================================================
#Create Arrays to hold desired output
#==================================================
TrackVeg_withHerbs<- CountBreed<- array(NA, monthstorun)
TrackHerbPopSize<-array(NA, c(2500, monthstorun))
TrackMeanSizeZ<-TrackMeanSizeZs<-TrackMeanSizeZr<-array(NA, monthstorun)

#====================================
#Set initial conditions
#====================================
# for resources
TrackVeg_withHerbs[1]<- params["Veg_initial"] 

#for consumers (10000 individuals)
source("MakeInitialPopVector.R")
TrackHerbPopSize[,1]<-pop_seed


#Initial conditions - other required output
TrackMeanSizeZ[1]<-sum(TrackHerbPopSize[,1]*Z_phenotypes)/sum(TrackHerbPopSize[,1])
TrackMeanSizeZs[1]<-sum(TrackHerbPopSize[,1]*zs_values)/sum(TrackHerbPopSize[,1])
TrackMeanSizeZr[1]<-sum(TrackHerbPopSize[,1]*zr_values)/sum(TrackHerbPopSize[,1])

#======================================================
#source the iteration code to run model simulation
source("ModelIterationsCode_part2.R")
#======================================================

#SAVE REQUIRED OUTPUT
HerbivorePopSize<-colSums(TrackHerbPopSize) 
HerbivoreBiomass<-colSums(TrackHerbPopSize*Z_phenotypes)

saveRDS(TrackHerbPopSize, paste(resultPath,file="RawHerbivoreNumbers.rds", sep="/")) 
saveRDS(HerbivorePopSize, paste(resultPath,file="HerbivorePopSize_BL.rds", sep="/")) 
saveRDS(HerbivoreBiomass, paste(resultPath,file="HerbivoreBiomass_BL.rds", sep="/"))
saveRDS(TrackVeg_withHerbs, paste(resultPath,file="VegBiomass_Herbivores_BL.rds", sep="/"))
saveRDS(CountBreed, paste(resultPath,file="CountBreed_BL.rds", sep="/")) 
saveRDS(TrackMeanSizeZ, paste(resultPath,file="MeanSizeZ_BL.rds", sep="/"))
saveRDS(TrackMeanSizeZs, paste(resultPath,file="MeanSizeZs_BL.rds", sep="/"))
saveRDS(TrackMeanSizeZr, paste(resultPath,file="MeanSizeZr_BL.rds", sep="/"))


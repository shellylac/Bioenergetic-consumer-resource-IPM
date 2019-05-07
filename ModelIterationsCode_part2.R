#############################################
#RUN ITERATIONS OF THE BASELINE MODEL
############################################

#Initialise breed time
breed_time<-2 #time when breeding occurs

#ITERATION LOOP
for (iteration in 2:monthstorun){ #monthstobreed is given in the analysis script - part 3
  print(c(iteration,breed_time))
  #
  lastMonthPopVector<-TrackHerbPopSize[,iteration-1]
  lastVegGrowthRate<-stoch_veg_growth_rate[iteration-1]
  lastMonthVegHerbs<-TrackVeg_withHerbs[iteration-1]

  #Work out the max consumption for whole consumer population - given distribution of phenotypes and available veg
  Eat_per_Zs<-cmax.z(zr_values, zs_values, params["cmax_slope"], params["half_sat_val"],  params["steepness"], params["fat_threshold"], lastMonthVegHerbs)
  maxHerbsCanEat<-sum(Eat_per_Zs*lastMonthPopVector)
  
  #-----------Calculate the veg scaling factor (p) ---------------------
  if(lastMonthVegHerbs<=params["Veg_min"]) {
    p<-0
  } else if (lastMonthVegHerbs-maxHerbsCanEat>params["Veg_min"]){
    p<-1
  } else {
    p<-(lastMonthVegHerbs-params["Veg_min"])/maxHerbsCanEat
  }
  
  #Add in snow effect
  vegScalingFactor<-ifelse(snow_indicator[iteration]==0, p, params["snow_effect"]*p)
  
  #-------------------------------------------------------------------------
    #Calculate available resources 
  currentAvailVegHerbs<-Veg.fun(lastVegGrowthRate, params["Veg_max"], lastMonthVegHerbs, vegScalingFactor, maxHerbsCanEat)
  TrackVeg_withHerbs[iteration]<-currentAvailVegHerbs
  
  #-----------------------
  #Calculate IPM matrix components
  M <- bigmatrix(zr_values,zs_values, params, lastMonthVegHerbs, vegScalingFactor)
  
  #---------------------------------------
  #Obtain IPM Kernal for breeding month
  if(iteration==breed_time) {
    currentPopVector  <- M$G%*%(M$S*lastMonthPopVector) + M$D%*%(M$R*lastMonthPopVector)
    whichbreed<-ifelse(M$R==0, 0, 1) #identify which individuals bred
    countbreed<-sum(whichbreed*lastMonthPopVector)
    CountBreed[iteration]<-countbreed
    breed_time<-breed_time+12 # increment breed time 
  }
  #Obtain IPM Kernal for non-breeding months
  else {
    currentPopVector  <- M$G%*%(M$S*lastMonthPopVector)
    CountBreed[iteration]<-0
  }
  #Store relevant output
  TrackHerbPopSize[,iteration]<- as.vector(currentPopVector)
  TrackMeanSizeZ[iteration]<-sum(currentPopVector*Z_phenotypes)/sum(currentPopVector)
  TrackMeanSizeZs[iteration]<-sum(currentPopVector*zs_values)/sum(currentPopVector)
  TrackMeanSizeZr[iteration]<-sum(currentPopVector*zr_values)/sum(currentPopVector)
}



############################################################
#BIOENERGETIC CR-IPM MODEL SETUP
############################################################

#Install libraries
library(Matrix)
library(expm)
library(rlist)

#PART A - DEFINE CONSTANTS AND BASELINE PARAMETERS


#Define Global Constants
BL_snowindicator<-"Inputs/BL_snowindicator.rds"
BL_veggrowthrate<-"Inputs/BL_veggrowthrate.rds"
resultPathPrefix_Sensitivity <- "insert folder name here"
resultPathPrefix_Seasonality <- "insert folder name here"

#Iterations - 
monthstorun<-monthstorun #uses number of iterations provided in analysis script - part 3.

#MODEL PARAMETERS - Specify baseline model parameters and values
params <- c(
  #vegetation parameters 
  Veg_max=14000000, Veg_min=420000, Veg_initial=1000000, veg_energy=10,
  #survival parameters 
  surv_int= 3.0, surv_slope=0.02, starvation_ratio=0.03,
  #consumption parameters
  cmax_slope=6.0, steepness=15, fat_threshold=0.3, half_sat_val=10000, 
  #metabolism and growth dynamics
  burn_slope=0.45, m_anabolism=54.6, m_catabolism=39.3, ZR_to_ZS=0.5, convert_coef_1=0.95, 
  #reproduction parameters
  zs_threshold_k=140, breeding_zr_threshold=0.10, neonate_mortality=0.10, ZR_to_ReproZ=0.6, 
  #develepmont parameters
  ReproZ_to_OffspringZS=0.9, convert_coef_2=0.95, convert_coef_3=0.95,
  #snow effect
  snow_effect=0.5
)

#--------------------------------------------------------------------------
#Define the consumer (herbivore) size ranges 
zs_50 <- seq(0.1,240,length.out=50) # needed with G & D matrices
zr_50 <- seq(0.1,120,length.out=50) # needed with G & D matrices
zs_values <- rep(zs_50,each=50)     # needed with S & R matrices
zr_values <- rep(zr_50,times=50)    # needed with S & R matrices
#Add these up to get our Z phenotypes 
Z_phenotypes<-zs_values+zr_values # Total size phenotypes (Z)
#--------------------------------------------------------------------

#################################################
#PART B - DEFINE ALL THE FUNCTIONS 
#################################################

#Resource dynamics 
#------------------------------
Veg.fun  <- function(veg.growth.rate, Veg.max, Veg.withHerbs, veg.scaling.factor, Eaten.per.month){
  veg<-((1-veg.growth.rate)*Veg.max) + (veg.growth.rate*Veg.withHerbs) - (veg.scaling.factor*Eaten.per.month)
  return(veg)
} 

#Maximum consumption
#------------------------------
cmax.z <- function(zr.values,zs.values, cmax.slope, half.sat.val, steepness, fat.threshold, veg.biomass){
  tot.mass<-zr.values+zs.values
  u<-(cmax.slope*zs.values^(2/3))*(veg.biomass/(half.sat.val + veg.biomass))*(1/(1 + exp(-steepness*((fat.threshold*tot.mass)-zr.values))))  
  return(u)
}

#Metabolism
#------------------------------
burn.it <- function(z,burn.slope){
  30*(burn.slope*z^(3/4))   #z = total size (ZS + ZR)
} 

#Survival 
#------------------------------
Surv.fun <- function(zr.values,zs.values,surv.int,surv.slope, starvation.ratio) {
  z.tot<-zr.values+zs.values
  u<-ifelse (zr.values/z.tot < starvation.ratio, 0, 
             exp(surv.int + surv.slope*zr.values)) 
  return(u/(1+u))
}

#Reprorudction
#------------------------------
Repro.fun <- function(zr.values,zs.values,zs.threshold.k, cmax.slope, veg.energy, half.sat.val,steepness,fat.threshold,
                      burn.slope, m.anabolism, m.catabolism, breeding.zr.threshold, neonate.mortality, veg.biomass, veg.scaling.factor) {
  z.tot<-zr.values+zs.values
  max.consumption <- cmax.z(zr.values,zs.values, cmax.slope, half.sat.val, steepness, fat.threshold, veg.biomass) #
  Delta.E <- veg.energy*veg.scaling.factor*max.consumption - burn.it(z.tot,burn.slope)  #
  #(Calculate Zr_o = post consumption value of Zr)
  Zr_o <- ifelse(Delta.E>0, zr.values + (1/m.anabolism)*Delta.E, zr.values + (1/m.catabolism)*Delta.E)
  u<-ifelse (zs.values<=zs.threshold.k, 0, ifelse (Zr_o/z.tot<=breeding.zr.threshold, 0, 1))
  u<-(1-neonate.mortality)*u 
  return(u)
}

#Growth dynamics
#------------------------------
G.fun<-function(zr.50, zs.50, cmax.slope,  half.sat.val, steepness,fat.threshold,veg.energy,burn.slope, m.anabolism, m.catabolism, 
                zs.threshold.k, breeding.zr.threshold, ZR.to.ReproZ, ZR.to.ZS, convert.coef.1, veg.biomass, veg.scaling.factor){
  G <- matrix(0,2500,2500) # The Growth Kernal
  for (i in 1:50){ #zs.vals
    for (j in 1:50){ #zr.vals
      # calc maximum consumption
      max.consumption <- cmax.z(zr.50[j], zs.50[i], cmax.slope,  half.sat.val, steepness,fat.threshold, veg.biomass)
      # energy available after are metabolism
      Delta.E <- veg.energy*veg.scaling.factor*max.consumption - burn.it(zr.50[j]+zs.50[i], burn.slope) 
      #Calculate Zr_o = post consumption value of ZR
      Zr.o <- if (Delta.E>0) (zr.50[j] + (1/m.anabolism)*Delta.E) else (zr.50[j] + (1/m.catabolism)*Delta.E)
      #Determine change in ZS and ZR for growers and potential breeders
      #potential breeders @ breeding time
      if (breed_time==iteration & zs.50[i]>=zs.threshold.k){ 
        mu.zs <- zs.50[i]
        mu.zr <- if (Zr.o/(Zr.o+zs.50[i]) <= breeding.zr.threshold) Zr.o else ((1-ZR.to.ReproZ)*Zr.o)
      }
      #potential breeders not @ breeding time
      if (breed_time!=iteration & zs.50[i]>=zs.threshold.k){ 
        mu.zs <- zs.50[i]
        mu.zr <- Zr.o
      }
      #growers @ breeding time 
      if (breed_time==iteration & zs.50[i] < zs.threshold.k){  
        mu.zs <- zs.50[i] + (ZR.to.ZS*convert.coef.1*Zr.o) 
        mu.zr <- (1-ZR.to.ZS)*Zr.o
      }
      #growers not @ breeding time 
      if (breed_time!=iteration & zs.50[i] < zs.threshold.k){     
        mu.zs <- zs.50[i] + (ZR.to.ZS*convert.coef.1*Zr.o) 
        mu.zr <- (1-ZR.to.ZS)*Zr.o
      }
      sigma.zs <- sqrt(5 - exp(-0.05*zs.50[i])) # variance in growth (ZS)
      sigma.zr <- sqrt(5 - exp(-0.05*zr.50[j])) # variance in reserve change (ZR)
      temp.zs <- dnorm(zs.50,mu.zs,sigma.zs)
      temp.zr <- dnorm(zr.50,mu.zr,sigma.zr)   
      temp.zs <- temp.zs/sum(temp.zs)
      temp.zr <- temp.zr/sum(temp.zr)
      temp.zs <- rep(temp.zs,each=50)
      temp.zr <- rep(temp.zr,times=50)
      temp.z <- temp.zr*temp.zs
      G[,(i-1)*50+j] <- temp.z
    }
  }
  G <- ifelse(is.nan(G),0,G)
  return(G)
}

#Development 
#------------------------------
D.fun<-function(zr.50, zs.50, cmax.slope,  half.sat.val, steepness,fat.threshold,veg.energy,burn.slope, m.anabolism, m.catabolism, 
                ReproZ.to.OffspringZS, convert.coef.2, ZR.to.ReproZ, convert.coef.3, veg.biomass, veg.scaling.factor){
  D <- matrix(0,2500,2500)
  if (iteration==breed_time){ # Only do this during breeding times
    for (i in 1:50){#zs.vals
      for (j in 1:50){#zr.vals
        # calculate maximum consumption
        max.consumption <- cmax.z(zr.50[j], zs.50[i], cmax.slope,  half.sat.val, steepness,fat.threshold, veg.biomass)
        # energy available after are metabolism
        Delta.E <- veg.energy*veg.scaling.factor*max.consumption - burn.it(zr.50[j]+zs.50[i],burn.slope) 
        #Calculate Zr_o = post consumption value of ZR
        Zr.o <- if (Delta.E>0) zr.50[j] + (1/m.anabolism)*Delta.E else zr.50[j] + (1/m.catabolism)*Delta.E
        #Calculate mean offspring structural mass
        mu.zs <- ReproZ.to.OffspringZS*convert.coef.2*ZR.to.ReproZ*Zr.o 
        #Calculate mean offspring reserve mass
        mu.zr <- (1-ReproZ.to.OffspringZS)*convert.coef.3*ZR.to.ReproZ*Zr.o 
        sigma.zs <- sqrt(5 - exp(-0.05*zs.50[i])) # variance in growth of offspring (ZS)
        sigma.zr <- sqrt(5 - exp(-0.05*zr.50[j])) # variance in reserve size of offspring (ZR)
        temp.zs <- dnorm(zs.50,mu.zs,sigma.zs)
        temp.zr <- dnorm(zr.50,mu.zr,sigma.zr)   
        temp.zs <- temp.zs/sum(temp.zs)
        temp.zr <- temp.zr/sum(temp.zr)
        temp.zs <- rep(temp.zs,each=50)
        temp.zr <- rep(temp.zr,times=50)
        temp.z <- temp.zr*temp.zs
        D[,(i-1)*50+j] <- temp.z
      }
    }
    D <- ifelse(is.nan(D),0,D)
    return(D)
  }
}

#################################################################
#PART C - DEFINE FUNCTION TO CONSTRUCT COMPONENTS OF THE IPM KERNAL
################################################################

bigmatrix<-function(zr.values,zs.values, bigmatrix.params, veg.biomass, veg.scaling.factor) {
  S <- ((Surv.fun(zr.values,zs.values, bigmatrix.params["surv_int"], bigmatrix.params["surv_slope"], bigmatrix.params["starvation_ratio"])))
  
  R <- ((Repro.fun(zr.values,zs.values, bigmatrix.params["zs_threshold_k"], bigmatrix.params["cmax_slope"],
                   bigmatrix.params["veg_energy"],bigmatrix.params["half_sat_val"], bigmatrix.params["steepness"], bigmatrix.params["fat_threshold"],
                   bigmatrix.params["burn_slope"], bigmatrix.params["m_anabolism"],bigmatrix.params["m_catabolism"],
                   bigmatrix.params["breeding_zr_threshold"], bigmatrix.params["neonate_mortality"], veg.biomass, veg.scaling.factor)))
  
  G <- G.fun(zr_50, zs_50, bigmatrix.params["cmax_slope"], bigmatrix.params["half_sat_val"],  bigmatrix.params["steepness"],
             bigmatrix.params["fat_threshold"], bigmatrix.params["veg_energy"], bigmatrix.params["burn_slope"], 
             bigmatrix.params["m_anabolism"], bigmatrix.params["m_catabolism"], bigmatrix.params["zs_threshold_k"], 
             bigmatrix.params["breeding_zr_threshold"], bigmatrix.params["ZR_to_ReproZ"], bigmatrix.params["ZR_to_ZS"], bigmatrix.params["convert_coef_1"],
             veg.biomass, veg.scaling.factor)
  
  D <- D.fun (zr_50, zs_50, bigmatrix.params["cmax_slope"],  bigmatrix.params["half_sat_val"], bigmatrix.params["steepness"],bigmatrix.params["fat_threshold"],
              bigmatrix.params["veg_energy"],bigmatrix.params["burn_slope"], bigmatrix.params["m_anabolism"], bigmatrix.params["m_catabolism"], 
              bigmatrix.params["ReproZ_to_OffspringZS"], bigmatrix.params["convert_coef_2"], bigmatrix.params["ZR_to_ReproZ"], bigmatrix.params["convert_coef_3"], 
              veg.biomass, veg.scaling.factor)
  
  return(list(S=S,R=R,G=G,D=D))
}

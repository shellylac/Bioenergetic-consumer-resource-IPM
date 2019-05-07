
#====================================================================================
#a#-----------LONGER GROWING SEASON---------
longer_grow_season<-rep(c(rep(0,8),rep(1,4)),length.out=monthstorun)
b <- runif(length(longer_grow_season), min=0.1, max=0.6)
longer_grow_season <- ifelse(longer_grow_season==0,b,longer_grow_season)

#b#-----------LOWER PRODUCTIVITY (NORMAL GROWING SEASON)-----
low_prod_growth_rate <- rep(c(rep(0,6),rep(1,6)),length.out=monthstorun)
b <- runif(length(low_prod_growth_rate), min=0.5, max=0.9)
low_prod_growth_rate <- ifelse(low_prod_growth_rate==0,b,low_prod_growth_rate)

#================================================
#c#-------- FEWER SNOW FALL MONTHS -----------
snow_options_123<-list("one"=1, "two"= c(1,1), "three"=c(1,1,1))
probsnowfall<-c(0.2, 0.6, 0.2)
snow_indicator_123<-mysnow(snow_options_123)#this makes one random snow year

for(i in 1:(monthstorun/12)){
  k<-mysnow(snow_options_123)
  snow_indicator_123<-c(snow_indicator_123,k)
}
snow_indicator_123<-snow_indicator_123[1:monthstorun]

#d#-------- LESS SNOW FALL -----------
normal_snoweffect<-0.50
lower_snoweffect<-0.80

#==================================================
#Create "seasons" for use in iteration loop - based on arguments passed in to R Script
#args are one of the following: "LongerSeason", "LowerProd", "FewerSnowMonths", "LessSnowEffect")

if (season_types=="LongerSeason"){
  stoch_veg_growth_rate<-longer_grow_season
  snow_indicator<-snow_indicator
  params["snow_effect"]<-normal_snoweffect
} else if (season_types=="LowerProd"){
  stoch_veg_growth_rate<-low_prod_growth_rate
  snow_indicator<-snow_indicator
  params["snow_effect"]<-normal_snoweffect
} else if (season_types=="FewerSnowMonths"){
  stoch_veg_growth_rate<-stoch_veg_growth_rate
  snow_indicator<-snow_indicator_12
  params["snow_effect"]<-normal_snoweffect
} else if (season_types=="LessSnowEffect"){
  stoch_veg_growth_rate<-stoch_veg_growth_rate
  snow_indicator<-snow_indicator
  params["snow_effect"]<-lower_snoweffect
} else { 
  stoch_veg_growth_rate<-stoch_veg_growth_rate
  snow_indicator<-snow_indicator
  params["snow_effect"]<-normal_snoweffect
}

#==================================================
#Set BASELINE stochastic growth/nongrowth months 
baseline_veg_growth_rate <- rep(c(rep(0,6),rep(1,6)),length.out=monthstorun)
b <- runif(length(baseline_veg_growth_rate), min=0.1, max=0.6)  
baseline_veg_growth_rate <- ifelse(baseline_veg_growth_rate==0,b,baseline_veg_growth_rate)

#==================================================
#Set BASELINE snow months indicator
probsnowfall<-c(0.2,0.4,0.4)

mysnow<-function(list){
  no_snow<-rep(0,8) # six growing months + two pre_snow months
  snow_months<-as.vector(list.sample(list,1, prob=probsnowfall))[[1]]#randomly draw number of snow months
  end_no_snow<-rep(0, 12-(length(no_snow)+length(snow_months)))#calculate remaining months in year
  snow_year<-c(no_snow, snow_months, end_no_snow)#compile the "snow" year
  return(snow_year)
}

#set the possible number of snow fall months
baseline_snow_options<-list("two"=c(1,1), "three"= c(1,1,1), "four"=c(1,1,1,1))

#make one random snow year
baseline_snow_indicator<-mysnow(baseline_snow_options)

#Make the desired number of random snow fall years, based on number of iterations.
for(i in 1:(monthstorun/12)){
  k<-mysnow(baseline_snow_options)
  baseline_snow_indicator<-c(baseline_snow_indicator,k)
}

baseline_snow_indicator<-baseline_snow_indicator[1:monthstorun]

#=============================================================

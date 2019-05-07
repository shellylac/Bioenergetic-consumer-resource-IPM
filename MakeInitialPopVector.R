#######################################################
#Make initial population size vector for consumers
#######################################################
#randomly generate pop vector of 2500 phenotypes
pop_vec<-rnorm(2500, mean = 10, sd=5)
#Save this for use in sensitivity and seasonality analyses
saveRDS(pop_vec, "RandomInitialPopVector.rds")

#-----------------------------------------------------------------------------
#Define the consumer (herbivore) size ranges 
zs_50 <- seq(0.1,240,length.out=50); zs_values <- rep(zs_50,each=50)
zr_50 <- seq(0.1,120,length.out=50); zr_values <- rep(zr_50,times=50) 
#-----------------------------------------------------------------------------

#remove phenotypes below and above observed ZS/ZR phenotypes
NZs_zeros<-ifelse(zs_values<10 | zs_values>180, 0, 1) 
NZr_zeros<-ifelse(zr_values<3 | zr_values>81, 0, 1)    
Zeros<-ifelse(NZs_zeros==0, NZs_zeros, NZr_zeros) 
pop_seed<-ifelse(Zeros==0, 0, pop_vec)

#Make initial pop vector of 10000 animals
pop_seed<-pop_seed*(10000/sum(pop_seed))
sum(pop_seed)#10000

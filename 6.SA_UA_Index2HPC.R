rm(list=ls())

path.data  <-  paste(getwd(),"/",sep="")


##############################
#Chunk: library
#############################

library(COINr6)

###########################
#Chunk:  loading data
#############################

load(paste(path.data,'HIBOs.Rdata', sep=''))


#############################
#Chunk:  defining specs normalization method and   weights
#############################

HIBO$Method$normalise$npara <- list(zscore=c(100,10), minmax=c(1,100))


#############################
#Chunk:   number of MCMC
#############################

## N=10000
## Nboot=400

#############################
#Chunk:  defining specs normalization method and   weights
#############################



SAspecs1 <- list(
  treat=list(winmax=c(5,10,2)),
  normalise = list(ntype = c( "zscore", "minmax")), 
  aggregate=list(agweights=c('PCA_NormalisedL1', 'Original','OptimsedLev3')))
  
 
SAresultsIndex_agg <- sensitivity(HIBO, v_targ = c('Index'),
                                                 SA_specs = SAspecs1,
                                                  N = N,Nboot=Nboot,
                                                SA_type = "SA")
       



SAresultsPl_agg<- sensitivity(HIBO, v_targ = c('Pl'),
                           SA_specs = SAspecs1,
                          N = N,Nboot=Nboot,
                          SA_type = "SA")



SAresultsPe_agg <- sensitivity(HIBO, v_targ = c('Pe'),
                           SA_specs = SAspecs1,
                           N = N,Nboot=Nboot,
                           SA_type = "SA")

                  



SAresultsLi_agg<- sensitivity(HIBO, v_targ = c('Li'),
                             SA_specs = SAspecs1,
                             N = N,Nboot=Nboot,
                             SA_type = "SA")

                  
 

#############################
#Chunk:  saving
#############################

## save( SAresultsLi_agg,SAresultsPe_agg,SAresultsPl_agg,
##      SAresultsIndex_agg,
##   file="SaResIndex.Rdata")

##############################
#Chunk: source 
##############################
## This code is computing the correlation ratio
## from the paper 'Weights and importance in composite indicators: Closing the gap
## Author links open overlay panel' Becker et al. 2017

## https://www.sciencedirect.com/science/article/pii/S1470160X17301759?via%3Dihub



##############################
#Chunk:   Setting working directory
##############################


rm(list=ls())

path.data <-  paste(getwd(),"/",sep="")

##############################
#Chunk: library
##############################

library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(gridExtra)
library(data.table)
library(mgcv)
library(COINr6)
#############################
#Chunk:  loading  HIBO data
#############################

load(paste(path.data,'HIBOs.Rdata', sep=''))


#############################
#Chunk:  1. estimating Si
#############################
mydf=HIBO$Data$Aggregated

#############################
#Chunk:  Indicators positions
#############################

exp_start=9
exp_end=66
exp_nvar=exp_end-exp_start+1

######################################
#  Chunk code:  linear and non linear R^2
######################################

df=list()
gm=list()
for (j in exp_start:exp_end){
  exposure = colnames(mydf)[j]
  outcome ='Index'
    print(outcome)
    print(exposure)
    fit= lm(get(outcome) ~ get(exposure), data=mydf)
    print(summary(fit))
    df[[j]]=fit
    fit1=gam(get(outcome) ~ s(get(exposure)), data=mydf)
    gm[[j]]=fit1
}


  df1=df[-which(sapply(df, is.null))]
  gm1=gm[-which(sapply(gm, is.null))]
  
   Sl=lapply(df1, function(x) summary(x)$r.squared)
   Slpmine=lapply(gm1, function(x)  var(fitted.values(x))/var(mydf$Index))
   Slp=lapply(gm1, function(x) summary(x)$r.sq)
   Slp=do.call(c, Slp)
  #dfdt=do.call(cbind, df)

  names(gm1)=names(mydf[,9:66])



######################################
#  Chunk code:   regression on each 
######################################
   mydfsub=mydf[,c(9:66)]
   
xreg=list()
xres=list()

   for (j in 1:ncol(mydfsub)){
     exposure = colnames(mydfsub)[j]
     predictors =colnames(mydfsub)[-j]
     print(length(predictors))
     print(exposure)
     fit1=lm(get(exposure) ~.,mydfsub)
     print(summary(fit1))
     xreg[[j]]=fitted(fit1)
     xres[[j]]=resid(fit1)   
   }


    

 names(xres)=names(mydfsub)
 xres=as.data.frame(do.call(cbind, xres))
 xres$Index=mydf$Index



######################################
#  Chunk code: function for  regression on each domain
######################################


myformlm=function(df){
    lf=list()
    for( i in 1:ncol(df)){
       lf[[i]]=as.formula(paste(colnames(df)[i],
                          paste(colnames(df)[-i],
sep = "", collapse = " + "), sep = " ~ "))
    }
    return(lf)
    }

  


myformgam=function(df){
    lf=list()
    for( i in 1:ncol(df)){     
    formulaRight <- paste('s(',colnames(df)[-i] , ')', sep = '', collapse = ' + ')
    lf[[i]]= as.formula(paste(colnames(df)[i], formulaRight ,
                           sep = " ~ "))
    }
    return(lf)
    }

  



  
######################################
#  Chunk code:   non linear reg of y on residuals to estimates Si^u 
######################################

zi=list()
zin=list() 
 
 for (j in 1:ncol(xres)){
   exposure = colnames(xres)[j]
   outcome ='Index'
   print(outcome)
   print(exposure)
   fit= lm(get(outcome) ~ get(exposure), data=xres)
   print(summary(fit))
   zi[[j]]=fit
   fit1=gam(get(outcome) ~ s(get(exposure)), data=xres) 
   zin[[j]]=fit1
 }
  
Su=lapply(zin, function(x) summary(x)$r.sq)
Su=do.call(c, Su)[1:58]

dfS=as.data.frame(cbind(Slp, Su))
dfS$Sc=dfS$Slp-dfS$Su


######################################
#  Chunk code: re-ordering the correlation ratio
######################################

out <- getIn(HIBO, dset = 'Aggregated', aglev = 3)
outInd<- getIn(HIBO, dset = 'Aggregated', aglev = 4)
     dat4ss=cbind( out$ind_data_only, outInd$ind_data_only)


predIdx <- c(1:(ncol(dat4ss)-1))

# lm(y ~ x), for x being a single predictor
Slin=sapply(predIdx, function(x)
    summary(lm(dat4ss[, ncol(dat4ss)] ~ dat4ss[, x]))$r.squared)

Snlin=sapply(predIdx, function(x)
    summary(gam(dat4ss[, ncol(dat4ss)] ~ s(dat4ss[, x])))$r.sq)

lf=myformlm(out$ind_data_only)
resd=as.data.frame(sapply(lf, function(x) resid(lm(x,out$ind_data_only))))
names(resd)=names(out$ind_data_only) 
resd$Index=outInd$ind_data_only$Index


predIdx <- c(1:(ncol(resd)-1))
Sulin=sapply(predIdx, function(x)
    summary(lm(resd[, ncol(resd)] ~ resd[, x]))$r.squared)

Sunlin=sapply(predIdx, function(x)
    summary(gam(resd[, ncol(resd)] ~ s(resd[, x])))$r.sq)
names(Sunlin)=names(dat4ss)[1:(ncol(dat4ss)-1)]

ss2df=data.frame(Slin, Snlin, Sulin, Sunlin)
ss2df$Sclin=ss2df$Slin-ss2df$Sulin
ss2df$Scnlin=ss2df$Snlin-ss2df$Sunlin


######################################
#  Chunk code: checking the results
######################################
ss2df


ss2df$dom=rownames(ss2df)



mylong= gather(ss2df, condition, measurement, Slin:Scnlin, factor_key=TRUE)
mylong$group=c(rep('Linear',3), rep('Non-Linear',3),rep('Linear',3),
               rep('Non-Linear',3),rep('Linear',3),rep('Non-Linear',3))
mylong$group2=c(rep(NA,6),rep('Uncorrelated',6),rep('Correlated',6))
mylong$dom=plyr::mapvalues(as.factor(mylong$dom), from=c('Li','Pe', 'Pl'), to=c('Lives', 'People', 'Places')) 

mylong=na.omit(mylong)
mylong$group3=paste(mylong$group, mylong$group2, sep=' ')


######################################
#  Chunk code:  plot 
######################################

p <- ggplot(data = mylong, aes (x =group , y = measurement, fill =  group2)) + 
    geom_col() +
    facet_grid(.~dom)+
    xlab('')+ylab('') +theme_light()+
    theme( legend.title = element_blank(), legend.position='bottom',
          panel.background = element_rect(colour = "white"),
       strip.text.x = element_text(
        size = 9, color = "black", face = "bold.italic" )
       ) +scale_fill_manual( values = c("black", "grey"),
                 labels=c(as.expression(bquote('Correleated'~S[d]^c)),
                                       as.expression(bquote('Uncorrelated' ~S[d]^u))))

          

p

#ggsave(file='Results/weightplot.jpeg', plot=p, bg='white', dpi=350)

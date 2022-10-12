rm(list=ls())

path.data  <-  paste(getwd(),"/",sep="")



##############################
#Chunk: library
##############################

library(ggplot2)
library(tidyr)
library(gridExtra)
library(COINr6)
library(DeLuciatoR)

#############################
#Chunk:  loading data
#############################

load('HIBOs.Rdata')


#############################
#Chunk:  loading  Sensitivity and Uncert. Results generated in file 6
#############################

#load("SaResIndex.Rdata")

#############################
#Chunk: checking names
#############################

stopifnot(all.equal(HIBO$Input$IndData$UnitCode, SAresults$RankStats$UnitCode))
stopifnot(all.equal(HIBO$Input$IndData$UnitCode, SAresults$Nominal$UnitCode))
SAresults$RankStats$UnitName= HIBO$Input$IndData$UnitName
SAresults$Nominal$UnitName=HIBO$Input$IndData$UnitName

#############################
#Chunk:  Modified function that plots ranks distributions with names and not area codes
#############################

source(paste(path.data,'MymodifiedFoo.R', sep='')) 
less

#############################
#Chunk:  plot SA 
#############################
p1=plotSARanksName(SAresults)

p2=plotSARanksName2(SAresults, order_by = "original")


#############################
#Chunk:   rank shift 
#############################            

ranksim=SAresults$Ranks
ranksim$UnitCode 


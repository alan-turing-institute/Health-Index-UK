rm(list=ls())
 
path.data  <-  paste(getwd(),"/",sep="")

##############################
#Chunk: library
##############################
library(Amelia)
library(gtsummary)
library(ggplot2)
library(dplyr)
library(psych)
library(FactoMineR)
library(tidyverse)
library(GGally)
library(rjson)
library(moments)
library(reshape)
library(skimr)
library(rlang)
library(ggcorrplot)
library(gridExtra)
library(ggraph)
library(COINr6) 
library(xtable)
library(readr)
library(data.table)
library(reshape)
library(paletteer)
library(patchwork)
library("hisafer")

################################
#Chunk:  loading  ONS results
################################

load(paste(path.data, 'OutputsOrig.Rdata', sep=''))


load(paste(path.data,'Df_indicators.Rdata', sep=''))

mydata=mydatanotTrans  # just the imputed dataset 


#############################
#Chunk:   var labels
#############################

mylab <- read.csv(paste( path.data,'input_files/mymetadata.csv',sep=''))


#############################
#Chunk:  creating inputMetadata # data dictionary  
#############################

mydico<- read.csv(paste(path.data, 'input_files/Dico.csv', sep='' ))
                  
myord <- mydico$Var

mydico<- merge(mydico, mylab, by.x='key', by.y='code')

#############################
#Chunk:  creating  direction 
#############################

pr1 <- mydico$key%in%variables_reverse

mydico$Direction  <- ifelse(pr1, -1, 1)


mydico <- mydico[match(myord,mydico$key),]
#############################
#Chunk:  creating  IndMeta 
#############################


HiboIndMeta <- as_tibble( data.frame(IndName=mydico$label,  IndCode= mydico$key,Direction=mydico$Direction,  IndWeight=rep(1, nrow(mydico)),  
Denominator=NA,  IndUnit=NA,  Target=NA,  Agg1=mydico$Subdomains,   Agg2=mydico$Domains, Agg3=rep('Index', nrow(mydico))))


#############################
#Chunk:  creating  AggMeta
#############################
mysb <- read.csv(paste(path.data,'input_files/sbdlab.csv', sep=''))


AgLevel <- c(rep(2, length(names(table(HiboIndMeta$Agg1)))),
             rep(3,  length(names(table(HiboIndMeta$Agg2)))),
             4)

HiboAggMeta <- as_tibble(data.frame(AgLevel= AgLevel,
                          Code= mysb$sbd,  
                              Name= mysb$sbdlab,
                      Weight=rep(1,length(AgLevel))))
#############################
#Chunk:  mydata is the data only imputed ( no variables reversed nor transfomed)
#############################

apply(mydata[,c(3:61)],2, function(x) sum(is.na(x)))


#############################
#Chunk:  mydata  merged with geography information and population by year at UTLA level 
#############################
mydatageo  <- merge(mydata, geopop, by.x=c('Area.code', 'Year'), by.y=c('UTLA20CD', 'Year'))

mydatageo15=mydatageo[mydatageo$Year==2015,]
mydatageo16=mydatageo[mydatageo$Year==2016,]
mydatageo17=mydatageo[mydatageo$Year==2017,]
mydatageo18=mydatageo[mydatageo$Year==2018,]


#############################
#Chunk:  mydata  merged with geography information and population by year at UTLA level 
#############################

myInputData15b <- data.frame(UnitName = mydatageo15$Area.name,
                         UnitCode= mydatageo15$Area.code,
                         Group_GeoCode=  mydatageo15$RGN20CD,
                         Group_GeoName=  mydatageo15$RGN20NM,
                         Group_NatPop= mydatageo15$National_population,
                         Group_UTLAPop=mydatageo15$UTLA_population,
                         Group_RegPop= mydatageo15$Region_population,
                         Year=mydatageo15$Year,
                         mydatageo15[, c(4:61)])
write.csv(myInputData15b, file='myInputData15.csv')

myInputData15=read_csv('myInputData15.csv', col_select=2:67)


myInputData16b<- data.frame(UnitName = mydatageo16$Area.name,
                         UnitCode= mydatageo16$Area.code,
                         Group_GeoCode=  mydatageo16$RGN20CD,
                         Group_GeoName=  mydatageo16$RGN20NM,
                         Group_NatPop= mydatageo16$National_population,
                         Group_UTLAPop=mydatageo16$UTLA_population,
                         Group_RegPop= mydatageo16$Region_population, Year=mydatageo16$Year,
                         mydatageo16[, c(4:61)])



write.csv(myInputData16b, file='myInputData16.csv')

myInputData16=read_csv('myInputData16.csv', col_select=2:67)


myInputData17b<- data.frame(UnitName = mydatageo17$Area.name,
                         UnitCode= mydatageo17$Area.code,
                         Group_GeoCode=  mydatageo17$RGN20CD,
                         Group_GeoName=  mydatageo17$RGN20NM,
                         Group_NatPop= mydatageo17$National_population,
                         Group_UTLAPop=mydatageo17$UTLA_population,
                         Group_RegPop= mydatageo17$Region_population,Year=mydatageo17$Year,
                         mydatageo17[, c(4:61)])
 



write.csv(myInputData17b, file='myInputData17.csv')

myInputData17=read_csv('myInputData17.csv', col_select=2:67)



myInputData18b <- data.frame(UnitName = mydatageo18$Area.name,
                         UnitCode= mydatageo18$Area.code,
                         Group_GeoCode=  mydatageo18$RGN20CD,
                         Group_GeoName=  mydatageo18$RGN20NM,
                         Group_NatPop= mydatageo18$National_population,
                         Group_UTLAPop=mydatageo18$UTLA_population,
                         Group_RegPop= mydatageo18$Region_population, Year=mydatageo18$Year,
                         mydatageo18[, c(4:61)])                    



write.csv(myInputData18b, file='myInputData18.csv')

myInputData18=read_csv('myInputData18.csv', col_select=2:67)


#############################
#Chunk:  reordering checking data correspondence
##########################
all.equal(names(myInputData18)[c(9:66)], HiboIndMeta$IndCode)
all.equal(names(myInputData17)[c(9:66)], HiboIndMeta$IndCode)
all.equal(names(myInputData16)[c(9:66)], HiboIndMeta$IndCode)
all.equal(names(myInputData15)[c(9:66)], HiboIndMeta$IndCode)

#############################
#Chunk:  Assembling all 
#############################

HIBO<- assemble(IndData = myInputData15, IndMeta =HiboIndMeta, AggMeta = HiboAggMeta)

HIBO16<- assemble(IndData = myInputData16, IndMeta =HiboIndMeta, AggMeta = HiboAggMeta)

HIBO17<- assemble(IndData = myInputData17, IndMeta =HiboIndMeta, AggMeta = HiboAggMeta)

HIBO18<- assemble(IndData = myInputData18, IndMeta =HiboIndMeta, AggMeta = HiboAggMeta)

############################
#Chunk:  plots
############################


iplotBar(HIBO, dset = "Raw", isel = "hle", usel = "SGP", aglev = 1)
iplotBar(HIBO, dset = "Raw", isel = "env_noise", usel = "SGP", aglev = 1)
iplotBar(HIBO, dset = "Raw", isel = "pub_greenspace", usel = "SGP", aglev = 1)
iplotBar(HIBO, dset = "Raw", isel = "priv_greenspace", usel = "SGP", aglev = 1)
iplotBar(HIBO, dset = "Raw", isel = "road_safety", usel = "SGP", aglev = 1)
iplotBar(HIBO, dset = "Raw", isel = "sti_screen", usel = "SGP", aglev = 1)

getIn(HIBO, dset = "Raw", icodes = "Pe.1", aglev = 1)


#############################
#Chunk:  box plot raw
#############################
BoxRaw=ggplot(stack(HIBO$Data$Raw[,9:66]), aes(x = ind, y = values)) +
  labs(x="", y="") +
  geom_boxplot(fill = "white", colour = "#3366FF")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ggtitle('Raw')


#############################
#Chunk:  plots fig. 1 
#############################
route=plotframework(HIBO)


#############################
#Chunk:   stats
#############################

HIBO <- getStats(HIBO, dset = "Raw", out2 = "COIN")
HIBO$Analysis$Raw$StatTable %>% roundDF() %>%
reactable::reactable(resizable = TRUE, bordered = TRUE, highlight = TRUE,defaultPageSize =100)
#defaultPageSize = 10)


HIBO16 <- getStats(HIBO16, dset = "Raw", out2 = "COIN")
HIBO16$Analysis$Raw$StatTable %>% roundDF() %>%
  reactable::reactable(resizable = TRUE, bordered = TRUE, highlight = TRUE,defaultPageSize = 100)


HIBO17 <- getStats(HIBO17, dset = "Raw", out2 = "COIN")
HIBO17$Analysis$Raw$StatTable %>% roundDF() %>%
  reactable::reactable(resizable = TRUE, bordered = TRUE, highlight = TRUE,defaultPageSize = 100)


HIBO18 <- getStats(HIBO18, dset = "Raw", out2 = "COIN")
HIBO18$Analysis$Raw$StatTable %>% roundDF() %>%
  reactable::reactable(resizable = TRUE, bordered = TRUE, highlight = TRUE,defaultPageSize = 100)


### correlation in 2018
HIBO18$Data$Raw%>%select(hyp_ten, resp, cvd, road_safety, sti_screen, GP_dist, pharm_dist)%>%cor()

#############################
#Chunk: Checkings
#############################
HIBO <- checkData(HIBO, dset = "Raw")
head(HIBO$Analysis$Raw$MissDatSummary)

#############################
#Chunk:   statistics 
#############################

statlist <- getStats(HIBO, dset = "Raw", out2 = "list")
statlist$StatTable
statlist$Outliers
statlist$Correlations


plotCorr(HIBO, dset = "Raw", aglevs = 1, grouplev = 0,withparent= 'none', showvals = T, box_level=2)


c1=plotCorr(HIBO, dset = "Raw", aglevs = 1, showvals = T,text_colour= 'red')
c2=plotCorr(HIBO16, dset = "Raw", aglevs = 1, showvals = T)
c3=plotCorr(HIBO17, dset = "Raw", aglevs = 1, showvals = T)
c4=plotCorr(HIBO18, dset = "Raw", aglevs = 1, showvals = T)

#############################
# Chunk code: saving correlation into pdf
#############################


## ggsave(filename="Results/Corr2015.pdf",
##        plot = c1,
##        device = cairo_pdf, 
##        width = 20, 
##        height = 20, 
##        units = "in")




## ggsave(filename="Results/Corr2015.jpeg",
##        plot = c1,
##        width = 20, 
##        height = 20, 
##        units = "in")

############################
#Chunk:    Normalizations and list of transformed variables SI table 4 
#############################


# treat at defaults
HIBO<- treat(HIBO, dset = "Raw", winmax = 5)
tab1=HIBO$Analysis$Treated$TreatSummary %>% filter(Treatment != "None")
########################
HIBO16<- treat(HIBO16, dset = "Raw", winmax = 5)
tab2=HIBO16$Analysis$Treated$TreatSummary %>% filter(Treatment != "None")
########################
HIBO17<- treat(HIBO17, dset = "Raw", winmax = 5)
tab3=HIBO17$Analysis$Treated$TreatSummary %>% filter(Treatment != "None")
########################
HIBO18<- treat(HIBO18, dset = "Raw", winmax = 5)
tab4=HIBO18$Analysis$Treated$TreatSummary %>% filter(Treatment != "None")


#############################
#Chunk:  box plot Treated
#############################
ggplot(stack(HIBO$Data$Treated[,9:66]), aes(x = ind, y = values)) +
  labs(x="Indicators", y="Frequency") +
  geom_boxplot(fill = "white", colour = "#3366FF")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


#############################
#Chunk:    all normalization  to zscores around average 100
#############################
HIBO<- normalise(HIBO, dset = "Treated",ntype='zscore', direction=HIBO$Input$IndMeta$Direction,
                 npara=list(zscore=c(100,10)))


HIBO16<- normalise(HIBO16 ,dset = "Treated" ,ntype='zscore', direction=HIBO$Input$IndMeta$Direction,
                    npara = list(zscore = c(100,10)))

HIBO17<- normalise(HIBO17, dset = "Treated",ntype='zscore', direction=HIBO$Input$IndMeta$Direction,
                   npara = list(zscore = c(100,10)))
HIBO18<- normalise(HIBO18, dset = "Treated",ntype='zscore',direction=HIBO$Input$IndMeta$Direction, 
                   npara = list(zscore = c(100,10)))


#############################
#Chunk:  box plot z score norm SI fig.2 
#############################
apply(HIBO$Data$Normalised[,9:66], 2, summary)



boxZscAT=ggplot(stack(HIBO$Data$Normalised[,9:66]), aes(x = ind, y = values)) +
  labs(x="", y="") +
  geom_boxplot(fill = "white", colour = "#3366FF")+ylim(50,150)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ggtitle('Zscore Modified')

#############################
#Chunk:  ONS normalized data with transformations
#############################
load(file='input_files/ONS_NORM.Rdata')



boxZscONS=ggplot(stack(bxdf15), aes(x = ind, y = values)) +
  labs(x="", y="") +
  geom_boxplot(fill = "white", colour = "#3366FF")+ylim(50,150)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ggtitle('Zscore ONS')

 ############################

boxZscAT+boxZscONS+ plot_layout(ncol = 1)
 box2sa= boxZscAT+boxZscONS+ plot_layout(ncol = 1)
 

 gt <- patchwork::patchworkGrob(box2sa)
 gt=gridExtra::grid.arrange(gt, left = "Frequency", bottom = "Indicators")
 
 
#ggsave(file='Results/BoxplotComp.jpeg', plot=gt, width=18, height = 11)


###########################
#Chunk:   a modified getPCA function from the COINr6 pkg, returns normalized weights
###########################
source(paste(path.data, 'input_files/MymodifiedFoo.R', sep=''))

###########################
#Chunk:    my pca 
###########################


HIBO<- getPCA2(HIBO, dset = "Normalised", aglev = 1)
HIBO16<- getPCA2(HIBO16, dset = "Normalised", aglev = 1)

HIBO17<- getPCA2(HIBO17, dset = "Normalised", aglev = 1)
HIBO18<- getPCA2(HIBO18, dset = "Normalised", aglev = 1)


###########################
#Chunk:    variance explained  SI table 6
###########################

HIBOb<- getPCA3(HIBO, dset = "Normalised", aglev = 1,out2='list')


HIBO16b<- getPCA3(HIBO16, dset = "Normalised", aglev = 1,out2='list')
HIBO17b<- getPCA3(HIBO17,dset = "Normalised", aglev = 1,out2='list')
HIBO18b<- getPCA3(HIBO18, dset = "Normalised", aglev = 1,out2='list')

myvar=lapply(HIBOb$PCAresults, function(x)  x$PCAsum$importance[2,1]*100)
myvar16=lapply(HIBO16b$PCAresults, function(x)  x$PCAsum$importance[2,1]*100)
myvar17=lapply(HIBO17b$PCAresults, function(x)  x$PCAsum$importance[2,1]*100)
myvar18=lapply(HIBO18b$PCAresults, function(x)  x$PCAsum$importance[2,1]*100)
mydfvar=cbind(myvar, myvar16, myvar17, myvar18)
###########################
#Chunk:    over time weights
###########################

head(HIBO$Parameters$Weights$PCA_NormalisedL1)
head(HIBO16$Parameters$Weights$PCA_NormalisedL1)
head(HIBO17$Parameters$Weights$PCA_NormalisedL1)
head(HIBO18$Parameters$Weights$PCA_NormalisedL1)


Weightsovertime=data.frame(id= HIBO$Parameters$Weights$PCA_NormalisedL1[,2], 
                           we2015=HIBO$Parameters$Weights$PCA_NormalisedL1[,3],
                           we2016=HIBO16$Parameters$Weights$PCA_NormalisedL1[,3],
                           we2017=HIBO17$Parameters$Weights$PCA_NormalisedL1[,3],
                           we2018=HIBO18$Parameters$Weights$PCA_NormalisedL1[,3])



Weightsovertime=Weightsovertime[1:58,]
weightslong=gather( Weightsovertime, value, measurement, we2015:we2018)

load("FA_Exp_Weights_ONS.Rdata")

weightscomaprisons= merge(Weightsovertime,indicator_weights_df,by.x='id', by.y='Indicator')

library(scatterPlotMatrix)
psca=scatterPlotMatrix(weightscomaprisons[,2:6])


weightslong= gather(weightscomaprisons,condition, value, we2015:Weight)
weightslong$year=c(rep(2015:2019, each=58))
weightslong$year=as.factor(weightslong$year)
weightslong$year=plyr::mapvalues(weightslong$year, from=c(2015:2019), to=c('2015', '2016', '2017', '2018', 'FA'))

###########################
#Chunk: comaprisons   over time weights fig. 7
# ###########################



comp1=ggplot(weightslong, aes(fill=year, y=value, x=id)) + 
  geom_bar(position="dodge", stat="identity")+ 
  theme_bw() +scale_fill_paletteer_d("nord::aurora")+
  #scale_fill_brewer(palette="Greens")+
theme(axis.text.x=element_text(angle=90),  plot.background = element_blank(),plot.title = element_text(hjust = 0.5),
      legend.title=element_blank())+
  ylab('weight')+xlab('')+ggtitle('Weight comparison between PCA weight  per each year and FA weights')



#ggsave_fitmax(comp1, file='Results/PCA_FA_weights.pdf',bg='white')

############################
#Chunk:    aggregate
###########################
HIBO <- aggregate(HIBO, dset = "Normalised",agweights = "Original")
HIBO2 <- aggregate(HIBO, dset = "Normalised",agweights = "Original")
HIBO3 <- aggregate(HIBO, dset = "Normalised",agweights = "PCA_NormalisedL1")



HIBO16 <- aggregate(HIBO16, dset = "Normalised",agweights = "Original")
HIBO17 <- aggregate(HIBO17, dset = "Normalised",agweights = "Original")
HIBO18 <- aggregate(HIBO18, dset = "Normalised",agweights = "Original")

HIBO$Data$Aggregated

getResults(HIBO, tab_type = "Summary", out2 = "COIN")$Results$Index
getResults(HIBO16, tab_type = "Summary", out2 = "COIN")$Results$Index
getResults(HIBO17, tab_type = "Summary", out2 = "COIN")$Results$Index
getResults(HIBO18, tab_type = "Summary", out2 = "COIN")$Results$Index

###########################
#Chunk:    Optimized weights
###########################


HIBO<- weightOpt(HIBO, itarg = "equal", aglev = 3, out2 = "COIN") ## domains
HIBO$Analysis$Weights$OptimsedLev3$CorrResults


#HIBO<- weightOpt(HIBO, itarg = "equal", aglev = 2, out2 = "COIN")  ## subdmains
HIBO16<- weightOpt(HIBO16, itarg = "equal", aglev = 3, out2 = "COIN") ## domains
HIBO17<- weightOpt(HIBO17, itarg = "equal", aglev = 3, out2 = "COIN") ## domains
HIBO18<- weightOpt(HIBO18, itarg = "equal", aglev = 3, out2 = "COIN") ## domains

HIBO16$Analysis$Weights$OptimsedLev3$CorrResults
HIBO17$Analysis$Weights$OptimsedLev3$CorrResults
HIBO18$Analysis$Weights$OptimsedLev3$CorrResults

############################
#Chunk:  HIBO with pca and optmized weights
#############################
HIBO$Parameters$Weights$mypersonal=data.frame(AgLevel= HIBO$Parameters$Weights$OptimsedLev3$AgLevel,
                                              Code=  HIBO$Parameters$Weights$OptimsedLev3$Code,
                                              Weight=c(HIBO$Parameters$Weights$PCA_NormalisedL1$Weight[1:75],
                                                       HIBO$Parameters$Weights$OptimsedLev3$Weight[76:79]))

HIBO$Parameters$Weights$mypersonal2=data.frame(AgLevel= HIBO$Parameters$Weights$OptimsedLev3$AgLevel,
                                              Code=  HIBO$Parameters$Weights$OptimsedLev3$Code,
                                              Weight=c(HIBO$Parameters$Weights$PCA_NormalisedL1$Weight[1:75],
                                                       HIBO$Parameters$Weights$Original$Weight[76:79]))

HIBOb <- aggregate(HIBO, dset = "Normalised", agtype="arith_mean", agweights='mypersonal')
HIBOc <- aggregate(HIBO, dset = "Normalised", agtype="arith_mean", agweights='mypersonal2')

           
rank15b= getResults(HIBOb, tab_type = "Summary", out2 = "COIN")$Results$SummaryScore

rank15c= getResults(HIBOc, tab_type = "Summary", out2 = "COIN")$Results$SummaryScore


# ############################
#Chunk:  rank from HIBO
#############################

rank15= getResults(HIBO, tab_type = "Summary", out2 = "COIN")$Results$SummaryScore
rank16= getResults(HIBO16, tab_type = "Summary", out2 = "COIN")$Results$SummaryScore
rank17= getResults(HIBO17, tab_type = "Summary", out2 = "COIN")$Results$SummaryScore
rank18= getResults(HIBO18, tab_type = "Summary", out2 = "COIN")$Results$SummaryScore


############################
#Chunk:  difference in ranking
############################

rank15b$rankb=rank15b$Rank
rank15c$rankc=rank15c$Rank
mydfcomp=merge(rank15c, rank15b[,c(2,5)], by='UnitName')



mydfcomp%>%arrange(desc(Rank))%>%
  mutate(UnitName=fct_reorder(UnitName,Rank))%>%
  ggplot(aes(x=UnitName, y=Rank))+
  geom_point(aes(x=UnitName, y=rankb), colour="blue")



mydfcomp=mydfcomp%>%arrange(desc(Rank))%>%
  mutate(UnitName=fct_reorder(UnitName,Rank))
mydfcomp$RankDiff=mydfcomp$rankc-mydfcomp$rankb


p1=mydfcomp%>%arrange(desc(Rank))%>%
  mutate(UnitName=fct_reorder(UnitName,Rank))%>%
  ggplot(aes(x=UnitName, y=RankDiff))+
  geom_bar(stat = "identity", aes(fill = RankDiff), legend = FALSE) +
  theme(axis.text.x = element_text(angle=90))

# ggsave(file='Results/RankDiff.jpeg', plot=p1, bg='white', height = 11, width=18) 
  
 
############################
#Chunk:  rank from ONS orig
############################

myrank=function(x, year){
  y=x%>% filter(Year==year)
  y$rank= data.table::frank(-y$Overall.index)
  y=y[order(y$rank),] 
  y
}

df_overall15=myrank(df_overall, 2015)
df_overall16=myrank(df_overall, 2016)
df_overall17=myrank(df_overall, 2017)
df_overall18=myrank(df_overall, 2018)



##############################
#Chunk: COIN vs ONS 
#############################

head(df_overall15)
df_overall15$rankONS=df_overall15$rank
rank15$rankCoin=rank15$Rank
df_overall15=as.data.frame(df_overall15)
names(df_overall15)[2]='UnitName'

df_overall15=merge(df_overall15, rank15, by='UnitName')

plot(df_overall15$rankONS, df_overall15$rankCoin, xlab='ONS', ylab='COIN')
plot(df_overall15$rankONS, df_overall15$rankCoin, xlab='ONS', ylab='COIN')


##############################
#Chunk: checkings  
#############################

names(HIBO$Parameters$Weights)

##############################
#Chunk: saving for comparisons 
#############################

save(HIBO, HIBO16, HIBO17,HIBO18, 
     df_overall15, 
     df_overall16,
     df_overall17,
     df_overall18,
     file='HIBOs.Rdata')






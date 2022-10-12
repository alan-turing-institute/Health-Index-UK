rm(list=ls())

path.data <- paste(getwd(),"/",sep="")

##############################
#Chunk: library
##############################

library(gtsummary)
library(maptools)
library(ggplot2)
library(dplyr)
library(psych)
library(FactoMineR)
library(tidyverse)
library(GGally)
library(rjson)
library(rlist)
library(imputeTS)
library(moments)
library(reshape)
library(skimr)
library(rlang)
library(tidyr)
library(SDMTools)

##############################
#Chunk: data reading
##############################



data15 <- read.csv(paste(path.data,'data/England_all_geog_aggregated_2015.csv',sep=''))
data15$Year=2015

data16 <- read.csv(paste(path.data,'data/England_all_geog_aggregated_2016.csv',sep=''))
data16$Year=2016

data17 <- read.csv(paste(path.data,'data/England_all_geog_aggregated_2017.csv',sep=''))
data17$Year=2017

data18 <- read.csv(paste(path.data,'data/England_all_geog_aggregated_2018.csv',sep=''))
data18$Year=2018

##############################
#Chunk: geographies 
##############################


geopop<- read.csv(paste(path.data,'lookups/geog_pop_lookup.csv', sep=''))


##############################
#Chunk: import json files ( return lists) 
##############################

index_structure <- fromJSON(file=paste(path.data,'input_files/index_structure.json',
                                      sep=''))

## variables that need to be read in reverse 
variables_reverse <- fromJSON(file=paste(path.data,'/input_files/variables_reverse.json',sep=''))


 variable_transformations <-  fromJSON(file=paste(path.data,'/input_files/variable_transformations.json',sep=''))



##############################
#Chunk: data checkings
##############################


lldata <- list(data15=data15, data16=data16,data17=data17, data18=data18)

lapply( lldata, dim)
colid <- lapply(lldata, names)

sapply(lldata, function(x) apply(x, 2 ,function(y) sum(is.na(y))))

colid <- do.call(c, colid)
 table(colid)

lapply(lldata, function(x) table(names(x)))


sapply(lldata, function(x) table(x$Area.code))
ids <- sapply(lldata, function(x) names(table(x$Area.name)))

thenas <- lapply(lldata, function(x) apply(x, 2 ,function(y) sum(is.na(y))))
### missed values
lapply(thenas, function(x) x[x>0])

##############################
#Chunk: allvar variables
##############################

allvar=read.csv('input_files/Allvar.csv', sep=',')

##############################
#Chunk: missed  variables
##############################

missvar <- lapply( lldata, function(x) setdiff(allvar$Var, names(x)))
missvar


##############################
#Chunk: missed  variables    added to dataframes                                     
##############################

lldata1  <- list()
for (i in 1:4) {
            df1=data.frame(matrix(NA, nrow=149, ncol=length(missvar[[i]])))
            names(df1) <-missvar[[i]]
            lldata1[[i]] <- cbind(df1,lldata[[i]]) 
            }



##############################
#Chunk:  lldata with all variables
##############################
     


#checkings
sapply(lldata1, function(x) apply(x, 2 ,function(y) sum(is.na(y))))

##############################
#Chunk:  lldata selecting only variables ( allvar ) for FA  the imputables
##############################

lldata2 <-  lapply(lldata1, function(x) x[, c("Area.code", "Area.name" ,allvar$Var, 'Year')])


lapply( lldata2, dim)
lapply(lldata2, names)

thenas <- lapply(lldata2, function(x) apply(x, 2 ,function(y) sum(is.na(y))))
lapply(thenas, function(x) x[x>0])

##############################
#Chunk:   from list to dataframe
##############################

mydata <-  do.call(rbind, lldata2)
apply(mydata, 2,function(x) sum(is.na(x)))


dim(mydata)

##############################
#Chunk:  #Imputation for BCP and Dorset for Pl.env.tnoise
# List of all UTLAs in South West, excluding BCP and Dorset
##############################

sw_UTLA = c("E06000022", "E06000023", "E06000024", "E06000025", "E06000026", "E06000027", "E06000030", "E06000052", "E06000054", "E10000008", "E10000013", "E10000027")


df_sw <- mydata[mydata$Area.code %in%sw_UTLA,]
meantransNoise <-  mean(df_sw$trans_noise,na.rm=TRUE)
 
 dim(mydata[mydata$Area.code %in% sw_UTLA,])

mydata <- mydata %>% mutate(trans_noise=ifelse(Year==2016 & Area.code%in%c("E06000058", "E06000059"),  meantransNoise, trans_noise))



##############################
#Chunk:  Identifying variables with only one year of obeser and NA otherwise ( see trans_noise which is only avialb. for 2015)
##############################
ind=apply(mydata,2, function(x) sum(is.na(x)))
ind1=names(ind[ind>400])
ind2=setdiff(names(ind[ind>0]), names(ind[ind>400]))


##############################
#Chunk: imputing  the var with full years missed
##############################


mydata<- mydata %>% 
  group_by(Area.code) %>%
  mutate_at(vars(ind1), funs(na_locf(.))) %>%
    ungroup

##############################
#Chunk:  imputing vars with  less than 400 obs missed
##############################

mydata<- mydata %>% 
  group_by(Area.code) %>%
  mutate_at(all_of(ind2), funs(na_interpolation(.))) %>%
    ungroup


##############################
#Chunk:  saving raw data, just with imputed values 
##############################
mydatanotTrans=mydata


##############################
#Chunk: data transformation functions
##############################


square <- function(x) x^2
cbrt <- function(x) x^(1/3) 
cube <- function(x) x^3
reciprocal <- function(x) 1/x
dreverse <- function(x) x*(-1)


##############################
#Chunk: data transformation application to df 
##############################

mytransf<-  function(x,y) {
x <- x%>%
  as_tibble() %>%
  mutate(across(variable_transformations$log, log))

x <- x%>%
  as_tibble() %>%
  mutate(across(y$sqrt, sqrt))

x <- x%>%
  as_tibble() %>%
  mutate(across(y$cbrt, cbrt))


x <- x%>%
  as_tibble() %>%
  mutate(across(y$square, square))


x <- x%>%
  as_tibble() %>%
  mutate(across(y$cube, cube))

x <- x%>%
  as_tibble() %>%
  mutate(across(y$reciprocal ,reciprocal))	

x <- x%>%
  as_tibble() %>%
  mutate(across(y$reciprocal,
 dreverse))


x <-  x%>%
    as_tibble() %>%
     mutate(across(variables_reverse, dreverse))
 
       
return(x)
}

#############################
#Chunk:  tansformed and reversed  back into data frame
##############################

df_indicators <- mytransf(x=mydata, y=variable_transformations)



#############################
#Chunk: saving 
#############################


save(mydatanotTrans,df_indicators,variables_reverse, geopop,index_structure,variable_transformations,
     file='Df_indicators.Rdata') 

#############################
#Chunk: boxplot 
#############################

BoxRaw=ggplot(stack(df_indicators[,3:61]), aes(x = ind, y = values)) +
  labs(x="Indicators", y="Frequency") +
  geom_boxplot(fill = "white", colour = "#3366FF")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ggtitle('Raw')



##############################
#Chunk: checkings on normality 
############################## 
## this function creates qqplots if needed it


mypri <- function(x, year){
    x <- x %>%select(allvar$Var)%>%
        as.data.frame()
    par(mfrow=c(4,7))
    pdf(paste(year, '_1', '.pdf', sep=''), paper="USr") 
        for( i in 1:28){
           qqnorm(x[,i], main=names(x)[i]); qqline(x[,i])
        }
    dev.off()
    
    pdf(paste(year, '_2', '.pdf', sep=''), paper="USr") 
      for( j in 29:56){
         qqnorm(x[,j], main=names(x)[j]); qqline(x[,j])
       }
      dev.off()   

}


############################
#Chunk: time normalization Part 1: creating national weights for 2015 
##############################

geopop$national.weights= geopop$UTLA_population/geopop$National_population

data.base=df_indicators[df_indicators$Year==2015,]
stopifnot(nrow(data.base)==149)
base_area="UTLA" 

national_weights = geopop[, c("UTLA20CD", "Year", "national.weights")]
names(national_weights)[1]="Area.code"
##### remove duplicates
national_weights2015=national_weights[national_weights$Year==2015,]


data.basemelt=gather(data.base, Indicator, Indicator.Index, hle:crime_pers)


### as in py 
data.basemelt= merge(data.basemelt, national_weights2015, by=c('Area.code','Year'),  all.x=TRUE)
table(data.basemelt$Area.code)



## ##############################
## #Chunk: time normalization    short cuts
## ##############################



data.basemelt <- data.basemelt%>%
            group_by(Indicator) %>%
    mutate(Indicator.index.wm=wt.mean(Indicator.Index,national.weights),
           Indicator.index.wsd=wt.sd(Indicator.Index,national.weights))%>%
     summarise_at(c('Indicator.index.wm','Indicator.index.wsd'), mean)%>%
    ungroup%>%
    as.data.frame()


##############################
#Chunk: time normalization 
##############################

df_norm= gather(df_indicators, Indicator, Indicator.Index, hle:crime_pers)
df_norm=merge(df_norm, data.basemelt, by='Indicator', all.x=TRUE)


  
df_norm$Indicator.Index=(df_norm$Indicator.Index-df_norm$Indicator.index.wm)/df_norm$Indicator.index.wsd



df_norm$Indicator.Index1 =(df_norm$Indicator.Index * 10) + 100

apply(df_norm, 2, function(x) sum(is.na(x)))
summary(df_norm$Indicator.Index1)


##############################
#Chunk: recasting in wide  format 
##############################
df_indicators <- reshape2::dcast(df_norm, Area.code+ Area.name+Year ~ Indicator, value.var="Indicator.Index1")

##############################
#Chunk: weights from factor analysis
##############################
indexll <- do.call(c, index_structure)
ind <- sapply(indexll, length)
ind1 <-  which(ind==1)
ind2 <-  which(ind!=1)

## the unique indicator 

weightsll1 <- list()
for( i in 1: length(ind1)) {
    varb=do.call(c,indexll[ind1])
    names(varb)=NULL 
    weightsll1[[i]]=1
    names(weightsll1[[i]])=varb[i]
    }
    

## the mutiple  indicators  
myfa <- list()
weightsll2 <-list() 
for( i in 1: length(ind2)) {
    varb <- indexll[[ind2[i]]]
    myfa[[i]] <-  fa(scale(df_indicators[,varb]), factors=1, fm='ml')
    loadings <- abs(myfa[[i]]$loadings[1:length(varb),])
    weightsll2[[i]] <-  loadings/sum(loadings)
      }



indicator_weights<- c(unlist( weightsll2), unlist( weightsll1))

indicator_weights_df = data.frame(Indicator=names( indicator_weights), Weight= indicator_weights)

save(indicator_weights_df, file="FA_Exp_Weights_ONS.Rdata")
##############################
#Chunk:  domains and sub domains  weights
##############################

getSubdomaninweigths <-  function(x, y){
       ind=lapply(x, length)
  subdomain_weights <- sapply(ind, function(x)  rep(1/x, x))
       for ( i in 1:length(subdomain_weights)){
           names(subdomain_weights[[i]])=names(x[[i]])
           }
       names(subdomain_weights)=NULL
        subdomain_weights=do.call(c, subdomain_weights) 
       return(subdomain_weights)
}


subdomain_weights= getSubdomaninweigths(index_structure)
subdomain_weights=as.data.frame( subdomain_weights)
subdomain_weights$Subdomains=rownames(subdomain_weights)

 getDomainweights <- function(x){
     domain_weights= rep(1/length(x), length(x))
     names(domain_weights)=names(x) 
     return( domain_weights)
    }


    
 domain_weights=getDomainweights(index_structure)


##############################
#Chunk: reading in hierach str for domains/subdomains and overall
##############################


domains2subdom <- read.csv(paste(path.data,'input_files/Domains_Subdomains.csv' ,sep=''))
names(domains2subdom)[3]='Indicator'
subdoman2domain <- read.csv(paste(path.data, 'input_files/subdomains2domains.csv', sep=''))
names(subdoman2domain)[2]="Indicator"


##############################
#Chunk:  Aggregate Index df_subdomains
##############################


df = gather(df_indicators, Indicator, Indicator.Index,activity:wp_safety)

df_subdomains <- merge(df, domains2subdom, by='Indicator', all.x=TRUE)
df_subdomains <- merge(df_subdomains, indicator_weights_df , by='Indicator', all.x=TRUE)
df_subdomains$Subdomain.index=df_subdomains$Indicator.Index*df_subdomains$Weight

df_subdomains<-  df_subdomains%>%
     group_by( Area.code,Area.name,Year, Subdomains)%>%
  summarize_at("Subdomain.index", sum,na.rm=T)

##############################
#Chunk:  Aggregate Index df_domains
##############################

df_domains <- merge(df_subdomains, subdomain_weights ,   by='Subdomains', all.x=TRUE)
df_domains$Domain= substr(df_domains$Subdomains,1,2)
df_domains$Domain.index <- df_domains$subdomain_weights*df_domains$Subdomain.index


df_domains <- df_domains%>%
           group_by( Area.code,     Area.name,Year, Domain)%>%
    summarize_at("Domain.index", sum)

##############################
#Chunk:  Aggregate Index df_overall
##############################


df_overall <- df_domains
df_overall$Domains.weights <- rep(0.3333, nrow(df_overall))
df_overall$Overall.index <- df_overall$Domain.index*df_overall$Domains.weights

df_overall <- df_overall%>%
           group_by( Area.code, Area.name,Year)%>%
    summarize_at("Overall.index", sum)


#############################
# Chunk: recast 
#############################

df_domains <- reshape2:::dcast(df_domains, Area.code+Area.name+Year~Domain,  value.var='Domain.index')

df_subdomains <- reshape2:::dcast(df_subdomains, Area.code+Area.name+Year~Subdomains,  value.var='Subdomain.index')

##############################
#Chunk:  Aggregate geography  
##############################

 higher_weights <- geopop 
higher_weights$Weight <- higher_weights$UTLA_population / higher_weights$Region_population


##############################
#Chunk:  Aggregate geography  Regional : indicators
##############################


df <-gather(df_indicators, Variable ,Value,activity:wp_safety)



df <-merge(df, higher_weights, by.x=c('Area.code', 'Year'), by.y=c('UTLA20CD', 'Year'), all.x=TRUE)

df$Value <- df$Value*df$Weight


df_higher <- df%>%
    group_by( RGN20CD,RGN20NM,Year, Variable)%>%
   summarize_at("Value", sum,na.rm=TRUE)

df_ind2reg= reshape2::dcast(df_higher, RGN20CD+RGN20NM+Year~Variable,  value.var='Value')


rm(df, df_higher)


##############################
#Chunk:  Aggregate geography  Regional: subdomains
##############################

df <-gather(df_subdomains, Variable ,Value, Li.1:Pl.5)


df <-merge(df, higher_weights, by.x=c('Area.code', 'Year'), by.y=c('UTLA20CD', 'Year'), all.x=TRUE)

df$Value <- df$Value*df$Weight



df_higher <- df%>%
    group_by( RGN20CD,RGN20NM,Year, Variable)%>%
   summarize_at("Value", sum,na.rm=TRUE)

df_sub2reg= reshape2::dcast(df_higher, RGN20CD+RGN20NM+Year~Variable,  value.var='Value')

rm(df, df_higher)

##############################
#Chunk:  Aggregate geography  Regional: domains
##############################



df <-gather(df_domains, Variable ,Value, Li:Pl)


df <-merge(df, higher_weights, by.x=c('Area.code', 'Year'), by.y=c('UTLA20CD', 'Year'), all.x=TRUE)

df$Value <- df$Value*df$Weight



df_higher <- df%>%
    group_by( RGN20CD,RGN20NM,Year, Variable)%>%
   summarize_at("Value", sum,na.rm=TRUE)

df_dom2reg= reshape2::dcast(df_higher, RGN20CD+RGN20NM+Year~Variable,  value.var='Value')





##############################
#Chunk:  Aggregate geography  Regional: overall 
##############################


df <-merge(df_overall, higher_weights, by.x=c('Area.code', 'Year'), by.y=c('UTLA20CD', 'Year'), all.x=TRUE)

df$Value <- df$Overall.index*df$Weight



df_over2reg <- df%>%
    group_by(RGN20NM, RGN20CD, Year )%>%
   summarize_at("Value", sum)

rm(df)




##############################
#Chunk:  Aggregate geography  National : indicators/subdom/domains/overall
##############################



df <-gather(df_indicators, Variable ,Value,activity:wp_safety)



df <-merge(df, higher_weights, by.x=c('Area.code', 'Year'), by.y=c('UTLA20CD', 'Year'), all.x=TRUE)

df$Value <- df$Value*df$national.weights



####  summing and reshaping 


df_higher <- df%>%
    group_by( Year, Variable)%>%
   summarize_at("Value", sum,na.rm=TRUE)

df_ind2nat= reshape2::dcast(df_higher,Year~Variable,  value.var='Value')



#### removing 
rm(df, df_higher)



### index_level= #### subdomains / geo_higher=Nationall 
 
df <-gather(df_subdomains, Variable ,Value,Li.1:Pl.5)

df <-merge(df, higher_weights, by.x=c('Area.code', 'Year'), by.y=c('UTLA20CD', 'Year'), all.x=TRUE)

df$Value <- df$Value*df$national.weights


df_higher <- df%>%
    group_by(Year, Variable )%>%
   summarize_at("Value", sum,na.rm=TRUE)

df_subdom2nat= reshape2::dcast(df_higher,Year~Variable,  value.var='Value')


#### removing 
rm(df, df_higher)


### index_level= #### domains / geo_higher= nat



df <-gather(df_domains, Variable ,Value,Li:Pl)

df <-merge(df, higher_weights, by.x=c('Area.code', 'Year'), by.y=c('UTLA20CD', 'Year'), all.x=TRUE)

df$Value <- df$Value*df$national.weights




df_higher <- df%>%
    group_by( Year, Variable )%>%
   summarize_at("Value", sum,na.rm=TRUE)

df_dom2nat= reshape2::dcast(df_higher,Year~Variable,  value.var='Value')

#### removing 
rm(df, df_higher)



### index_level= overall  / geo_higher= nat

df <-merge(df_overall, higher_weights, by.x=c('Area.code', 'Year'), by.y=c('UTLA20CD', 'Year'), all.x=TRUE)

df$Value <- df$Overall.index*df$national.weights


df_over2nat <- df%>%
    group_by(Year )%>%
   summarize_at("Value", sum,na.rm=TRUE)


############################
#Chunk:  Savings
############################

save(df_over2nat,df_dom2nat,df_subdom2nat,df_ind2nat, indicator_weights_df,
df_over2reg,df_dom2reg,df_sub2reg,df_ind2reg,df_domains,df_overall, df_subdomains, file="OutputsOrig.Rdata")


rm(list=ls())

path.data  <-  paste(getwd(),"/",sep="")

##############################
#Chunk: library
##############################
library(Amelia)
library(gtsummary)
library(maptools)
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
library(tidyr)
library(ggcorrplot)
library(gridExtra)
library(ggraph)
library(COINr6)
library(xtable)
library(ggplot2)
library(ggrepel)
library(ggbump)
library(cowplot)
library(wesanderson)
require(gtools)
library(RankAggreg)
library("hisafer")
library(DeLuciatoR)
#############################
#Chunk:  loading  ONS rankings and all df 
#############################

load(paste(path.data, 'OutputsOrig.Rdata', sep=''))
load(paste(path.data,'Df_indicators.Rdata', sep=''))


#############################
#Chunk:  Plots fig. 5
#############################


plines1=df_domains%>%filter(Year==2015)%>%
 ggplot( aes(Pl,Pe, label = Area.name))+
  geom_point()+xlab('Places')+ ylab('People')+
  geom_text_repel(aes(label = Area.name),   size = 3.5) +
    geom_abline()+ geom_smooth(method='lm', formula= y~x)+
    theme_light()




plines2=df_domains%>%filter(Year==2015)%>%
  ggplot( aes(Pl,Li, label = Area.name))+
  geom_point()+xlab('Places')+ ylab('Lives')+
  geom_text_repel(aes(label = Area.name),   size = 3.5) +
  geom_abline()+ geom_smooth(method='lm', formula= y~x)+
    theme_light()




plines3=df_domains%>%filter(Year==2015)%>%
  ggplot( aes(Pe,Li, label = Area.name))+
  geom_point()+xlab('People')+ ylab('Lives')+
  geom_text_repel(aes(label = Area.name),   size = 3.5) +
  geom_abline()+ geom_smooth(method='lm', formula= y~x)+
    theme_light()



p1=plot_grid(plines1, plines2, plines3,nrow=1, labels=c('A', 'B', 'C')) 

#ggsave_fitmax(p1, file='Results/plinesall.jpeg', maxheight=12, maxwidth=20)
#ggsave_fitmax(p1, file='Results/plinesall.pdf', maxheight=12, maxwidth=20)



#############################
#Chunk: data function  dom 
#############################
dtformdom <-  function(x, year=year, title=title){
    
                      y=x[x$Year==year,]
     maxind=apply(y[,c(4:6)], 1, max)
     minind=apply(y[,c(4:6)], 1, min)
    dfranges=data.frame(maxin=maxind, minin=minind)
    rownames(dfranges)=y$Area.name

dfpoints=gather(y, Indicators, Value, Li:Pl)
dfpoints$group=substr(dfpoints$Indicators, start=1, stop=2)
dfpoints$group1=plyr::mapvalues(dfpoints$group, from=c('Li', 'Pe', 'Pl'), to=c('Lives', 'People',  'Places'))
    z=as.data.frame(df_overall[df_overall$Year==year,] )
   z=z[order(z$Overall.index),]  
    names(z)[4]='Value'
     
   dfranges=dfranges[match(z$Area.name, rownames(dfranges)),]
    dfranges$level_order <- factor(rownames(dfranges),level=c(rownames(dfranges)))
 
    
p1=ggplot() +
  geom_linerange(data = dfranges,
                 mapping=aes(x = level_order, ymin = minin, ymax = maxin, 
                             lwd = 0.5), color = 'lightgreen', alpha = 0.5,
                 show.legend=FALSE,width=0.3, position = position_dodge(width=0.8))+
    #scale_y_continuous(c(78, 116)) +
    labs(x=NULL, y=NULL) +theme_classic()+
    geom_point(dfpoints, mapping=aes(x=Area.name, y=Value, shape=group1))+
    geom_point(z, mapping=aes(x=Area.name, y=Value),shape=4)+
    #coord_flip()+
    theme(axis.text.x = element_text(angle = 90, size=12),
          legend.position = "right",
          legend.title = element_blank(),
          legend.text = element_text(size=14),
          legend.key.size = unit(2,"line"),
          plot.title = element_text(hjust = 0.5))+
    ggtitle(title)

         
   return(p1)


}                       


#############################
#Chunk: plot domainsfig 2 and SI fig. 5-7
#############################

d1=dtformdom(df_domains, year=2015,title='Health Index 2015');d1

d2=dtformdom(df_domains, year=2016,title='Health Index 2016');d2
d3=dtformdom(df_domains, year=2017,title='Health Index 2017');d3
d4=dtformdom(df_domains, year=2018,title='Health Index 2018');d4

## saving


## ggsave("Results/d1.pdf", plot = d1, dpi=300,width=18, height=11)
## ggsave("Results/d2.pdf", plot = d2,  dpi=300,width=18, height=11)
## ggsave("Results/d3.pdf", plot = d3,  dpi=300,width=18, height=11)
## ggsave("Results/d4.pdf", plot = d4,  dpi=300,width=18, height=11)




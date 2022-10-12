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
library(readr)
library(data.table)
library(reshape)
library(paletteer)
library(goeveg)
library(ggrepel)
library(DeLuciatoR )
#############################
#Chunk:  loading  ONS rankings 
#############################

load(paste(path.data, 'OutputsOrig.Rdata', sep=''))

load(paste(path.data,'Df_indicators.Rdata', sep=''))

load(paste(path.data,'HIBOs.Rdata', sep=''))

figlab=read.csv(paste(path.data, 'input_files/ParentsLabels.csv', sep='') )




#############################
#Chunk:    Normalized correlation 
#############################

outW <- effectiveWeight(HIBO)
outW$LabelsParents$Labels[76:78]=c("Lives", "People","Places")
outW$LabelsParents$Parents[59:65]="Lives"
outW$LabelsParents$Parents[66:70]="People"
outW$LabelsParents$Parents[71:75]="Places"


figlab$EffectiveWeights=outW$EffectiveWeights


#############################
#Chunk:plotting fig 1
#############################


fig1<- plotly::plot_ly(labels = figlab$Labels3, 
                       parents = figlab$Parents, 
                       values = figlab$EffectiveWeights, 
                       type = "sunburst", branchvalues = "total")
fig1


#############################
#Chunk:    Normalized correlation 
#############################

plotCorr(HIBO, dset = "Normalised", aglevs = 1, grouplev = 0,withparent= 'none', 
         showvals = T, box_level=2 ,flagcolours = TRUE, text_colour='black',
         discrete_colours=c("#80d67b", "#b8e8b5", "#e2e6e1", 
                            "#9cc4de"))


plotCorr(HIBO, dset = "Normalised", aglevs = 1, icodes='Li.2', 
         showvals = T, box_level=2 ,flagcolours = TRUE, text_colour='black',
         discrete_colours=c("#80d67b", "#b8e8b5", "#e2e6e1", 
                            "#9cc4de"))



#############################
#Chunk:  loading  ONS rankings rm london  
#############################


CheckPillars <- removeElements(HIBO, 1, "Index")
stopifnot(all.equal(figlab$Labels[1:58],names(CheckPillars$MeanAbsDiff[-1])))

names(CheckPillars$MeanAbsDiff[-1]) <-c(figlab$Labels3[1:58])

pildf=data.frame(Pillar = names(CheckPillars$MeanAbsDiff[-1]),
           Impact = CheckPillars$MeanAbsDiff[-1], 
           Pillar2=figlab$Labels3[1:58])



Pind1= ggplot(pildf,
             aes(x=Pillar2, y=Impact)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
  )+ ylab('Absolute Rank mean shift')+
  xlab('Indicators')

Pind1


## ggsave_fitmax(Pind1, file='Results/IndicatorsShift.pdf', maxheight=12, maxwidth=20)




Pind= ggplot(data.frame(Pillar = names(CheckPillars$MeanAbsDiff[-1]),
                        Impact = CheckPillars$MeanAbsDiff[-1]),
             aes(x=Pillar, y=Impact)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
  )+ ylab('Absolute Rank mean shift')+
  xlab('Indicators')






#############################
#Chunk: compare value difference between original and at version, SI table 5 
#############################

dfindex2015=df_overall%>%filter(Year==2015)
dfindex2015$UnitName= dfindex2015$Area.name 
dfindex2015$UnitName= dfindex2015$Area.name 
dfindex2015$UnitName= dfindex2015$Area.name 
dfindex2015$rankOns=frank(dfindex2015$Overall.index)


dfat2015=HIBO$Data$Aggregated%>%select(UnitCode, UnitName, Index)
dfat2015$rankAT= frank(dfat2015$Index)


df2plot=merge(dfat2015, dfindex2015, by='UnitName')

df2plot$diff=df2plot$Index-df2plot$Overall.index
plot(Index~Overall.index, df2plot)

df2plot$rankdiff=df2plot$rankAT-df2plot$rankOns

p1=df2plot%>%arrange(desc(rankOns))%>%
  mutate(UnitName=fct_reorder(UnitName,rankOns))%>%
  ggplot(aes(x=UnitName, y=rankdiff))+
  geom_bar(stat = "identity", aes(fill = rankdiff), legend = FALSE) +
  theme(axis.text.x = element_text(angle=90))

#ggsave(file='Results/RankDiffATONS.jpeg', plot=p1, bg='white', height = 11, width=18) 
head(df2plot)
df2plot=df2plot[order(df2plot$rankOns),]


#write.csv(df2plot, file='Results/AtOnsRanks.csv')
#tab2SM=read.csv(file='Results/AtOnsRanks.csv')


#############################
#Chunk:    plot corr modified for labels
#############################

plotCorr2=function (COIN, dset = "Raw", icodes = NULL, aglevs = 1, cortype = "pearson", 
                    withparent = "parent", grouplev = NULL, box_level = NULL, 
                    showvals = TRUE, flagcolours = FALSE, flagthresh = c(-0.4, 
                                                                         0.3, 0.9), pval = 0.05, insig_colour = "#F0F0F0", text_colour = NULL, 
                    discrete_colours = NULL, box_colour = NULL, out2 = "fig") 
{
  if (length(icodes) == 1) {
    icodes = rep(icodes, 2)
  }
  if (length(aglevs) == 1) {
    aglevs = rep(aglevs, 2)
  }
  if (aglevs[2] > aglevs[1]) {
    aglevs <- rev(aglevs)
    icodes <- rev(icodes)
  }
  if (withparent == "none" | withparent == "parent") {
    if (withparent == "none") 
      withparent <- FALSE
    if (withparent == "parent") 
      withparent <- TRUE
    crtable <- getCorr(COIN, dset = dset, icodes = icodes, 
                       aglevs = aglevs, cortype = cortype, pval = pval, 
                       withparent = withparent, grouplev = grouplev)
  }
  else if (withparent == "family") {
    aglev1 <- min(aglevs)
    agcols <- dplyr::select(COIN$Input$IndMeta, .data$IndCode, 
                            dplyr::starts_with("Agg"))
    crtable <- data.frame(Var1 = NA, Var2 = NA, Correlation = NA)
    for (ii in (aglev1 + 1):ncol(agcols)) {
      crtableii <- getCorr(COIN, dset = dset, icodes = icodes, 
                           aglevs = c(ii, aglev1), cortype = cortype, pval = pval, 
                           withparent = TRUE, grouplev = grouplev)
      crtableii$Var1 <- colnames(agcols)[ii]
      crtable <- rbind(crtable, crtableii)
    }
    crtable <- crtable[-1, ]
  }
  else {
    stop("withfamily should be either 'none', 'parent' or 'family'.")
  }
  crtable <- crtable[as.character(crtable$Var1) != as.character(crtable$Var2), 
  ]
  if (out2 == "fig") {
    ord1 <- unique(crtable$Var1)
    ord2 <- unique(crtable$Var2)
    aggcols <- dplyr::select(COIN$Input$IndMeta, .data$IndCode, 
                             dplyr::starts_with("Agg"))
    if (withparent != "family") {
      c1 <- unlist(aggcols[aglevs[1]])
      ord1 <- unique(c1[c1 %in% ord1])
    }
    c2 <- unlist(aggcols[aglevs[2]])
    ord2 <- unique(c2[c2 %in% ord2])
    if (withparent == "family") {
      ord2 <- rev(ord2)
    }
    if (setequal(ord1, ord2)) {
      ord2 <- rev(ord1)
    }
    hithresh <- 0.9
    weakthresh <- 0.3
    negthresh <- -0.4
    if (flagcolours) {
      crtable$Flag <- ifelse(crtable$Correlation >= hithresh, 
                             yes = "High", no = "Moderate")
      crtable$Flag[(crtable$Correlation <= weakthresh)] <- "Weak"
      crtable$Flag[(crtable$Correlation <= negthresh)] <- "Negative"
      plt <- ggplot2::ggplot(data = crtable, ggplot2::aes(x = factor(.data$Var1, 
                                                                     levels = ord1), y = factor(.data$Var2, levels = ord2), 
                                                          fill = .data$Flag, label = .data$Correlation)) + 
        ggplot2::geom_tile(colour = "white") + ggplot2::labs(x = NULL, 
                                                             y = NULL, fill = "Correlation") + ggplot2::theme_classic() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                           hjust = 1)) + ggplot2::scale_x_discrete(expand = c(0, 
                                                                                                              0)) + ggplot2::scale_y_discrete(expand = c(0, 
                                                                                                                                                         0))
      if (is.null(discrete_colours)) {
        discrete_colours <- c("#80d67b", "#b8e8b5", "#e2e6e1", 
                              "#b25491")
      }
      else {
        stopifnot(is.character(discrete_colours), length(discrete_colours) == 
                    4)
      }
      plt <- plt + ggplot2::scale_fill_manual(breaks = c("High", 
                                                         "Moderate", "Weak", "Negative"), values = discrete_colours, 
                                              na.value = insig_colour)
    }
    else {
      plt <- ggplot2::ggplot(data = crtable, ggplot2::aes(x = factor(.data$Var1, 
                                                                     levels = ord1), y = factor(.data$Var2, levels = ord2), 
                                                          fill = .data$Correlation, label = .data$Correlation)) + 
        ggplot2::geom_tile(colour = "white") + ggplot2::labs(x = NULL, 
                                                             y = NULL, fill = "Correlation") + ggplot2::theme_classic() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                           hjust = 1)) + ggplot2::scale_x_discrete(expand = c(0, 
                                                                                                              0)) + ggplot2::scale_y_discrete(expand = c(0, 
                                                                                                                                                         0))
      plt <- plt + ggplot2::scale_fill_gradient2(mid = "#FBFEF9", 
                                                 low = "#A63446", high = "#0C6291", limits = c(-1, 
                                                                                               1), na.value = insig_colour)
    }
    if (showvals) {
      if (flagcolours) {
        text_colour <- "#6a6a6a"
      }
      else {
        text_colour <- "white"
      }
      plt <- plt + ggplot2::geom_text(colour = text_colour, 
                                      size = 3, na.rm = TRUE)
    }
    if (is.null(box_colour)) {
      box_colour <- "#505050"
    }
    if (withparent == "family") {
      acls <- unique(aggcols[min(aglevs):ncol(aggcols)])
      acls <- acls[unlist(acls[1]) %in% unlist(unique(crtable[2])), 
      ]
      for (icol in 2:ncol(acls)) {
        parents <- unlist(acls[icol])
        yends <- match(unique(parents), parents)
        yends <- length(ord2) - yends + 1.5
        ystarts <- c(yends, 0.5)
        ystarts <- ystarts[-1]
        xstarts <- rep(icol - 1.5, length(ystarts))
        xends <- xstarts + 1
        plt <- plt + ggplot2::annotate("rect", xmin = xstarts, 
                                       xmax = xends, ymin = ystarts, ymax = yends, 
                                       fill = NA, color = box_colour)
      }
    }
    else if (!is.null(box_level)) {
      if (box_level < aglevs[2] + 1) {
        stop("box_level must be at least the aggregation level above aglevs.")
      }
      acls <- unique(aggcols[c(aglevs[1], box_level)])
      acls <- acls[unlist(acls[1]) %in% unlist(unique(crtable[1])), 
      ]
      parents <- unlist(acls[2])
      starts <- match(unique(parents), parents)
      ends <- c(starts, length(ord1) + 1)
      ends <- ends[-1]
      xstarts <- starts - 0.5
      xends <- ends - 0.5
      if (aglevs[1] == aglevs[2]) {
        yends <- length(ord1) - xends + 1
        ystarts <- length(ord1) - xstarts + 1
      }
      else {
        acls <- unique(aggcols[c(aglevs[2], box_level)])
        acls <- acls[unlist(acls[1]) %in% unlist(unique(crtable[2])), 
        ]
        parents <- unlist(acls[2])
        ystarts <- match(unique(parents), parents) - 
          0.5
        yends <- c(ystarts, length(ord2) + 0.5)
        yends <- yends[-1]
      }
      plt <- plt + ggplot2::annotate("rect", xmin = xstarts, 
                                     xmax = xends, ymin = ystarts, ymax = yends, fill = NA, 
                                     color = box_colour)
    }
    return(plt)
  }
  else if (out2 == "dflong") {
    return(crtable)
  }
  else if (out2 == "dfwide") {
    crtable <- tidyr::pivot_wider(crtable, values_from = .data$Correlation, 
                                  names_from = .data$Var1)
    colnames(crtable)[1] <- "Indicator"
    return(crtable)
  }
}






pIndcorr= plotCorr2(HIBO, dset = "Aggregated", aglevs = 1, grouplev = 0,withparent= 'none', 
         showvals = T, box_level=2 ,flagcolours = TRUE, text_colour='black',
          discrete_colours=c("#80d67b", "#b8e8b5", "#e2e6e1", 
            "#9cc4de"))
 

######  Indicators versus Subdomains 
PIndSubcorr=plotCorr2(HIBO, dset = "Aggregated", aglevs = c(1,2), withparent= 'none',
         showvals = T, box_level=2 ,flagcolours = TRUE, text_colour='black',
         discrete_colours=c("#80d67b", "#b8e8b5", "#e2e6e1", 
                            "#9cc4de"))



PIndSubcorrDF=plotCorr2(HIBO, dset = "Aggregated", aglevs = c(1,2), 
                     showvals = T, box_level=2 ,flagcolours = TRUE, text_colour='black',
                     discrete_colours=c("#80d67b", "#b8e8b5", "#e2e6e1", 
                                        "#9cc4de"), out2='dfwide')



PSubDom=plotCorr2(HIBO, dset = "Aggregated", aglevs = c(2,3),withparent= 'none',
                       showvals = T, box_level=3 ,flagcolours = TRUE, text_colour='black',
                       discrete_colours=c("#80d67b", "#b8e8b5", "#e2e6e1",  "#9cc4de"))
     
                                                                          
PSubDom=plotCorr2(HIBO, dset = "Aggregated", aglevs = c(2,3),withparent= 'none',
                  showvals = T, box_level=3 ,flagcolours = TRUE, text_colour='black',
                  discrete_colours=c("#80d67b", "#b8e8b5", "#e2e6e1",  "#9cc4de"))

#############################
#Chunk: save plots
#############################


## ggsave('Results/pIndcorr.pdf', plot = pIndcorr,  dpi=300,width=16, height=12)

## ggsave('Results/PIndSubcorrr.pdf', plot =PIndSubcorr ,  dpi=300,width=16, height=12)

## ggsave('Results/PSubDom.pdf', plot =PSubDom ,  dpi=300,width=10, height=10)







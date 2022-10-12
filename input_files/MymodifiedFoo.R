###############################
# Chunk code: computes PCA weights normalized
###############################

getPCA2=
function (COIN, dset = "Raw", icodes = NULL, aglev = NULL, nowarnings = FALSE, 
          out2 = "COIN") 
{
  PCAwts <- function(icodes1) {
    out <- getIn(COIN, dset = dset, icodes = icodes1, aglev = aglev)
    nNA <- sum(is.na(out$ind_data_only))
    if (nNA > 0) {
      dat4PCA <- stats::na.omit(out$ind_data_only)
      if (!nowarnings) {
        warning(paste0(nNA, " missing values found. Removing ", 
                       nrow(out$ind_data_only) - nrow(dat4PCA), " rows with missing values in order to perform\nPCA. You can also try imputing data first to avoid this."))
      }
    }
    else {
      dat4PCA <- out$ind_data_only
    }
    print(names(dat4PCA))
    PCAres <- stats::prcomp(dat4PCA, center = TRUE, scale = TRUE)
    print(summary(PCAres))
    print(summary(PCAres)[[6]][2,1])
    if (is.null(aglev)) {
      aglev <- 1
    }
    wts <- abs(PCAres$rotation[, 1]) %>% as.numeric()
    wts <- wts/sum(wts)
    print(paste('wts are:', wts, sep=' '))
    return(list(wts = wts, PCAres = PCAres,  IndCodes = out$IndCodes))
  }
  agcols <- dplyr::select(COIN$Input$IndMeta, .data$IndCode, 
                          dplyr::starts_with("Agg"))
  agcols <- agcols[c(aglev, aglev + 1)]
  out3 <- getIn(COIN, dset = dset, icodes = icodes, aglev = aglev)
  IndCodes <- out3$IndCodes
  parents <- unique(agcols[(agcols[[1]] %in% IndCodes), 2])
  parents <- parents[[1]]
  PCAlist <- vector(mode = "list", length = length(parents))
  wlist <- COIN$Parameters$Weights$Original
  for (ii in 1:length(parents)) {
    outPCA <- PCAwts(parents[ii])
    wlist$Weight[match(outPCA$IndCodes, wlist$Code)] <- outPCA$wts
    PCAlist[[ii]] <- outPCA
  }
  names(PCAlist) <- parents
  if ((out3$otype == "COINobj") & (out2 == "COIN")) {
    eval(parse(text = paste0("COIN$Parameters$Weights$PCA_", 
                             dset, "L", aglev, "<-wlist")))
    eval(parse(text = paste0("COIN$Analysis$", dset, "$PCA$L", 
                             aglev, "<- PCAlist")))
    return(COIN)
  }
  else {
    output <- list(Weights = wlist, PCAresults = PCAlist)
    return(output)
  }
}

###############################
# Chunk code: computes PCA weights normalized
###############################


getPCA3=
  function (COIN, dset = "Raw", icodes = NULL, aglev = NULL, nowarnings = FALSE, 
            out2 = "COIN") 
  {
    PCAwts <- function(icodes1) {
      out <- getIn(COIN, dset = dset, icodes = icodes1, aglev = aglev)
      nNA <- sum(is.na(out$ind_data_only))
      if (nNA > 0) {
        dat4PCA <- stats::na.omit(out$ind_data_only)
        if (!nowarnings) {
          warning(paste0(nNA, " missing values found. Removing ", 
                         nrow(out$ind_data_only) - nrow(dat4PCA), " rows with missing values in order to perform\nPCA. You can also try imputing data first to avoid this."))
        }
      }
      else {
        dat4PCA <- out$ind_data_only
      }
      print(names(dat4PCA))
      PCAres <- stats::prcomp(dat4PCA, center = TRUE, scale = TRUE)
      PCAsum=summary(PCAres)
      print(summary(PCAres))
      print(summary(PCAres)[[6]][2,1])
      if (is.null(aglev)) {
        aglev <- 1
      }
      wts <- abs(PCAres$rotation[, 1]) %>% as.numeric()
      wts <- wts/sum(wts)
      print(paste('wts are:', wts, sep=' '))
      return(list(wts = wts, PCAres = PCAres, PCAsum=PCAsum, IndCodes = out$IndCodes))
    }
    agcols <- dplyr::select(COIN$Input$IndMeta, .data$IndCode, 
                            dplyr::starts_with("Agg"))
    agcols <- agcols[c(aglev, aglev + 1)]
    out3 <- getIn(COIN, dset = dset, icodes = icodes, aglev = aglev)
    IndCodes <- out3$IndCodes
    parents <- unique(agcols[(agcols[[1]] %in% IndCodes), 2])
    parents <- parents[[1]]
    PCAlist <- vector(mode = "list", length = length(parents))
    wlist <- COIN$Parameters$Weights$Original
    for (ii in 1:length(parents)) {
      outPCA <- PCAwts(parents[ii])
      wlist$Weight[match(outPCA$IndCodes, wlist$Code)] <- outPCA$wts
      PCAlist[[ii]] <- outPCA
    }
    names(PCAlist) <- parents
    if ((out3$otype == "COINobj") & (out2 == "COIN")) {
      eval(parse(text = paste0("COIN$Parameters$Weights$PCA_", 
                               dset, "L", aglev, "<-wlist")))
      eval(parse(text = paste0("COIN$Analysis$", dset, "$PCA$L", 
                               aglev, "<- PCAlist")))
      return(COIN)
    }
    else {
      output <- list(Weights = wlist, PCAresults = PCAlist)
      return(output)
    }
  }











###############################
# Chunk code: plots SA results using unit name and not codes
###############################


plotSARanksName= function (SAresults, plot_units = NULL, order_by = "nominal",
    dot_colour = NULL, line_colour = NULL) 
{
    rnks <- SAresults$RankStats
    if (!is.null(plot_units)) {
        if (length(plot_units == 1)) {
            if (plot_units == "top10") {
                unit_include <- SAresults$Nominal$UnitName[SAresults$Nominal$Rank <= 
                  10]
            }
            else if (plot_units == "bottom10") {
                unit_include <- SAresults$Nominal$UnitName[SAresults$Nominal$Rank >= 
                  (max(SAresults$Nominal$Rank, na.rm = TRUE) - 
                    10)]
            }
            else {
                stop("plot_units not recognised: should be either a character vector of unit codes or else\n      \"top10\" or \"bottom10\" ")
            }
        }
        else {
            unit_include <- plot_units
        }
        rnks <- rnks[rnks$UnitName %in% unit_include, ]
        SAresults$Nominal <- SAresults$Nominal[SAresults$Nominal$UnitName %in% 
            unit_include, ]
    }
    if (order_by == "nominal") {
        plot_order <- SAresults$Nominal$UnitName[order(SAresults$Nominal$Score, 
            decreasing = FALSE)]
    }
    else if (order_by == "median") {
        plot_order <- SAresults$Nominal$UnitName[order(rnks$Median, 
            decreasing = TRUE)]
    }
    
    stats_long <- tidyr::pivot_longer(rnks, cols = c("Median", 
        "Q5", "Q95"), names_to = "Statistic", values_to = "Rank")
    if (is.null(dot_colour)) {
        dot_colour <- "#83af70"
    }
    if (is.null(line_colour)) {
        line_colour <- "grey"
    }
    stats_long %>% ggplot2::ggplot(aes(x = .data$Rank, y = .data$UnitName)) + 
        ggplot2::geom_line(aes(group = .data$UnitName), color = line_colour) + 
        ggplot2::geom_point(aes(color = .data$Statistic, shape = .data$Statistic, 
            size = .data$Statistic)) + ggplot2::scale_shape_manual(values = c(16, 
        15, 15)) + ggplot2::scale_size_manual(values = c(2, 0, 
        0)) + ggplot2::labs(y = "", color = "") + ggplot2::guides(shape = FALSE, 
        size = FALSE, color = FALSE) + ggplot2::theme_classic() + 
        ggplot2::theme(legend.position = "top") + ggplot2::scale_color_manual(values = c(dot_colour, 
        "#ffffff", "#ffffff")) + ggplot2::scale_y_discrete(limits = plot_order) + 
        ggplot2::scale_x_reverse() + ggplot2::coord_flip() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, 
            hjust = 1))
}



###############################
# Chunk code: plots SA results using unit name and (not codes)
###############################



plotSARanksName2= function (SAresults, plot_units = NULL, order_by = "nominal", myorder=NULL, 
    dot_colour = NULL, line_colour = NULL) 
{
    rnks <- SAresults$RankStats
    if (!is.null(plot_units)) {
        if (length(plot_units == 1)) {
            if (plot_units == "top10") {
                unit_include <- SAresults$Nominal$UnitName[SAresults$Nominal$Rank <= 
                  10]
            }
            else if (plot_units == "bottom10") {
                unit_include <- SAresults$Nominal$UnitName[SAresults$Nominal$Rank >= 
                  (max(SAresults$Nominal$Rank, na.rm = TRUE) - 
                    10)]
            }
            else {
                stop("plot_units not recognised: should be either a character vector of unit codes or else\n      \"top10\" or \"bottom10\" ")
            }
        }
        else {
            unit_include <- plot_units
        }
        rnks <- rnks[rnks$UnitName %in% unit_include, ]
        SAresults$Nominal <- SAresults$Nominal[SAresults$Nominal$UnitName %in% 
            unit_include, ]
    }
    if (order_by == "nominal") {
        plot_order <- SAresults$Nominal$UnitName[order(SAresults$Nominal$Score, 
            decreasing = FALSE)]
    }
    else if (order_by == "median") {
        plot_order <- SAresults$Nominal$UnitName[order(rnks$Median, 
            decreasing = TRUE)]
    }
 
     else if (order_by == "original") {
     plot_order <- SAresults$Nominal$UnitName[order(match(SAresults$Nominal$UnitName, myorder))]   
    }
    
    stats_long <- tidyr::pivot_longer(rnks, cols = c("Median", 
        "Q5", "Q95"), names_to = "Statistic", values_to = "Rank")
    if (is.null(dot_colour)) {
        dot_colour <- "#83af70"
    }
    if (is.null(line_colour)) {
        line_colour <- "grey"
    }
    stats_long %>% ggplot2::ggplot(aes(x = .data$Rank, y = .data$UnitName)) + 
        ggplot2::geom_line(aes(group = .data$UnitName), color = line_colour) + 
        ggplot2::geom_point(aes(color = .data$Statistic, shape = .data$Statistic, 
            size = .data$Statistic)) + ggplot2::scale_shape_manual(values = c(16, 
        15, 15)) + ggplot2::scale_size_manual(values = c(2, 0, 
        0)) + ggplot2::labs(y = "", color = "") + ggplot2::guides(shape = FALSE, 
        size = FALSE, color = FALSE) + ggplot2::theme_classic() + 
        ggplot2::theme(legend.position = "top") + ggplot2::scale_color_manual(values = c(dot_colour, 
        "#ffffff", "#ffffff")) + ggplot2::scale_y_discrete(limits = plot_order) + 
        ggplot2::scale_x_reverse() + ggplot2::coord_flip() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, 
            hjust = 1))
        }


    
##############################
# Chunk: plots according the order 
##############################


plotSARanksSimOrig= function (SAresults, plot_units = NULL, order_by = "nominal", myorder=NULL, 
    dot_colour = NULL, line_colour = NULL) 
{
    rnks <- SAresults$RankStats
    if (!is.null(plot_units)) {
        if (length(plot_units == 1)) {
            if (plot_units == "top10") {
                unit_include <- SAresults$Nominal$UnitName[SAresults$Nominal$Rank <= 
                  10]
            }
            else if (plot_units == "bottom10") {
                unit_include <- SAresults$Nominal$UnitName[SAresults$Nominal$Rank >= 
                  (max(SAresults$Nominal$Rank, na.rm = TRUE) - 
                    10)]
            }
            else {
                stop("plot_units not recognised: should be either a character vector of unit codes or else\n      \"top10\" or \"bottom10\" ")
            }
        }
        else {
            unit_include <- plot_units
        }
        rnks <- rnks[rnks$UnitName %in% unit_include, ]
        SAresults$Nominal <- SAresults$Nominal[SAresults$Nominal$UnitName %in% 
            unit_include, ]
    }
    if (order_by == "nominal") {
        plot_order <- SAresults$Nominal$UnitName[order(SAresults$Nominal$Score, 
            decreasing = FALSE)]
    }
    else if (order_by == "median") {
        plot_order <- SAresults$Nominal$UnitName[order(rnks$Median, 
            decreasing = TRUE)]
    }
 
     else if (order_by == "original") {
     plot_order <- SAresults$Nominal$UnitName[order(match(SAresults$Nominal$UnitName, myorder))]   
    }
    
    stats_long <- tidyr::pivot_longer(rnks, cols = c("Median", 
        "Q5", "Q95"), names_to = "Statistic", values_to = "Rank")
    if (is.null(dot_colour)) {
        dot_colour <- "#83af70"
    }
    if (is.null(line_colour)) {
        line_colour <- "grey"
    }
    stats_long %>% ggplot2::ggplot(aes(x = .data$Rank, y = .data$UnitName)) + 
        ggplot2::geom_line(aes(group = .data$UnitName), color = line_colour) + 
        ggplot2::geom_point(aes(color = .data$Statistic, shape = .data$Statistic, 
            size = .data$Statistic)) + ggplot2::scale_shape_manual(values = c(16, 
        15, 15)) + ggplot2::scale_size_manual(values = c(2, 0, 
        0)) + ggplot2::labs(y = "", color = "") + ggplot2::guides(shape = FALSE, 
        size = FALSE, color = FALSE) + ggplot2::theme_classic() + 
        ggplot2::theme(legend.position = "top") + ggplot2::scale_color_manual(values = c(dot_colour, 
        "#ffffff", "#ffffff")) + ggplot2::scale_y_discrete(limits = plot_order) + 
        ggplot2::scale_x_reverse() + ggplot2::coord_flip() + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, 
            hjust = 1))
        }
##############################
# Chunk: functions from Permanyier,2012
##############################

rankmob <-  function(rank1, rank2){
    n=length(rank1)
    d1=sum((rank1-rank2)^2)
    d2=1/((n^3-n)/3)
    d=d1*d2
    return(d)
}


rankmobsubset <- function(rank1, rank2, mstart, mstop){
    
    n=length(rank1)
    rank1m=rank1[mstart,mstop]
    rank2m=rank2[mstart,mstop]
    m=mstop-mstart+1
    if (m<=n/2){
      phi=2*m*(((m^2-1)/3)+(n-m)^2 )
        }
    else phi=(n^3-n)/3
   df=sum((rank1m-rank2m)^2)/phi
return(df)
}


# 
# 
# distancefunction <- function(){
# 
# 
# 
#     }
##############################
# Chunk  code:   myplot provides black correaltion in the heatmap, instead of white.
##############################



myplotCorr=function (COIN, dset = "Raw", icodes = NULL, aglevs = 1, cortype = "pearson", 
                     withparent = "parent", grouplev = NULL, box_level = NULL, 
                     showvals = TRUE, flagcolours = FALSE, flagthresh = c(-0.4, 
                                                                          0.3, 0.9), 
                     pval = 0.05, insig_colour = "#F0F0F0", text_colour = NULL, 
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
                             yes = "High", no = "OK")
      crtable$Flag[(crtable$Correlation <= weakthresh)] <- "Weak"
      crtable$Flag[(crtable$Correlation <= negthresh)] <- "Negative"
      plt <- ggplot2::ggplot(data = crtable, ggplot2::aes(x = factor(.data$Var1, 
                                                                     levels = ord1), y = factor(.data$Var2, levels = ord2), 
                                                          fill = .data$Flag, label = .data$Correlation)) + 
        ggplot2::geom_tile(colour = "black") + ggplot2::labs(x = NULL, 
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
                                                         "OK", "Weak", "Negative"), values = discrete_colours, 
                                              na.value = insig_colour)
    }
    else {
      plt <- ggplot2::ggplot(data = crtable, ggplot2::aes(x = factor(.data$Var1, 
                                                                     levels = ord1), y = factor(.data$Var2, levels = ord2), 
                                                          fill = .data$Correlation, label = .data$Correlation)) + 
        ggplot2::geom_tile(colour = "black") + ggplot2::labs(x = NULL, 
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
        text_colour <- "black"
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


######################################

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
                             yes = "High", no = "OK")
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
                                                         "OK", "Weak", "Negative"), values = discrete_colours, 
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
                                      size = 5, na.rm = TRUE)
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








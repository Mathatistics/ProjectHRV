library(readxl)
library(data.table)
library(reshape2)
library(pls)
library(plyr)
library(ggplot2)

## Function
getSpectrogram <- function(rr, n = 64, Fs = 4, overlap = 16) {
  require(reshape2)
  require(signal)
  require(data.table)
  require(ggplot2)
  
  spmat <- specgram(rr, n = n, Fs = Fs, overlap = overlap)
  sp <- spmat$S
  rownames(sp) <- spmat$f
  colnames(sp) <- spmat$t
  dt <- data.table(melt(log10(abs(sp)), varnames = c('Frequency', 'Time')))
  plt <- ggplot(dt, aes(Time, Frequency, fill = value)) + 
    geom_tile() + 
    theme_bw() +
    theme(legend.position = 'none')
  return(invisible(list(sp = spmat, plot = plt)))
}

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = "bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position = "none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

getScorePlot <- function(mvr.model, comps = c(1,2), cat.var = bigD[, Events]){
  scrs.df.name <- paste('Comp', comps, sep = '.')
  scores <- data.table(mvr.model$scores[, comps])[, Events := cat.var]
  setnames(scores, names(scores)[seq_along(comps)], scrs.df.name)
  expl.var <- paste(names(explvar(mvr.model)[comps]), '(', 
                    round(explvar(mvr.model)[comps], 2), '%)', 
                    sep = '')
  scr.plt <- ggplot(scores, aes_string(scrs.df.name[1], 
                                       scrs.df.name[2], 
                                       fill = 'Events')) + 
    geom_point(shape = 21, size = 3, alpha = 0.8) +
    theme_bw() +
    theme(legend.position = 'top', legend.title = element_blank()) +
    geom_vline(xintercept = 0, color = 'red', linetype = 2) +
    geom_hline(yintercept = 0, color = 'red', linetype = 2) +
    labs(x = expl.var[1], y = expl.var[2]) +
    guides(fill = guide_legend(nrow = 2))
  return(scr.plt)
}

rmsep.plot <- function(model){
  rmsep <- data.table(melt(RMSEP(model)$val))[, comp := as.numeric(model) - 1]
  min.rmsep <- rmsep[which.min(value)]
  ## Plotting
  plt <- ggplot(rmsep, aes(comp, value, linetype = estimate, color = estimate)) + 
    geom_line() + theme_bw() + 
    theme(legend.title = element_blank(), 
          legend.position = 'top') + 
    geom_point(color = 'black') + 
    geom_point(data = min.rmsep, color = 'red', size = 2) + 
    geom_vline(xintercept = min.rmsep[, comp], 
               color = 'gray', linetype = 2) + 
    geom_hline(yintercept = min.rmsep[, value], 
               color = 'gray', linetype = 2) + 
    geom_text(data = min.rmsep, aes(
      label = paste('RMSEP:', round(value, 4))),
      color = 'black', 
      size = 4,
      angle = 90,
      vjust = 0, 
      hjust = -0.5) +
    labs(x = 'Components', y = 'RMSEP')
  return(list(rmsep = rmsep, min.rmsep = min.rmsep, rmsep.plot = plt))
}

## Loading Data
data <- read_excel('Report/Data/FullDataset.xlsx', sheet = 1)
label <- data.table(read_excel('Report/Data/FullDataset.xlsx', sheet = 3), key = 'Series')
which.series <- label[, Series]
event <- label[, Event]
data <- as.matrix(data[, car::which.names(which.series, names(data))])
dt <- data.table(melt(data, varnames = c('n', 'Series'), value.name = 'rr'), key = 'Series')[!is.na(rr)]
## Removing Series with very few observations (less than 128)
which.selected <- as.character(dt[, length(n) > 128, by = Series][which(V1), Series])
dt <- dt[Series %in% which.selected]

## Attach Events
dt <- dt[label[Series %in% which.selected, Event, by = Series]]

sp.mat <- llply(dt[, as.character(unique(Series))], function(x){
  getSpectrogram(as.numeric(dt[Series == x, rr]))
})
names(sp.mat) <- dt[, unique(Series), by = Event][, Event]

bigD <- data.table(ldply(sp.mat, function(x){log(abs(t(x$sp$S)))}))
setnames(bigD, names(bigD)[1], 'Events')
bigD[, Events := as.factor(Events)]
bigD[, resp := as.numeric(Events)]
setkey(bigD, Events)

pcr.mdl <- pcr(resp ~ ., data = bigD[, -1, with = F], 
               validation = 'LOO')
pls.mdl <- plsr(
  resp ~ ., data = bigD[, -1, with = F], 
                validation = 'LOO')

pcr.rmsep <- data.table(melt(RMSEP(pcr.mdl)$val))[, comp := as.numeric(model) - 1]
pls.rmsep <- data.table(melt(RMSEP(pls.mdl)$val))[, comp := as.numeric(model) - 1]

pcr.min.comp <- pcr.rmsep[which.min(value)]
pls.min.comp <- pls.rmsep[which.min(value)]

## RMSEP plot (PCR)

rmsep.plt <- llply(list(pcr.mdl, pls.mdl), function(x){
  rmsep.plot(x)$rmsep.plot + 
    ggtitle(paste('RMSEP plot for', toupper(x$call[[1]]), 'model'))
})
do.call(grid_arrange_shared_legend, rmsep.plt)

## Scoreplot

pcr.scr.plts <- llply(list(c(1,2), c(1,3), c(2,3), c(2,4)), function(x){
  getScorePlot(pcr.mdl, x, bigD[, Events])
})
pls.scr.plts <- llply(list(c(1,2), c(1,3), c(2,3), c(2,4)), function(x){
  getScorePlot(pls.mdl, x, bigD[, Events])
})
do.call(grid_arrange_shared_legend, pcr.scr.plts)
do.call(grid_arrange_shared_legend, pls.scr.plts)
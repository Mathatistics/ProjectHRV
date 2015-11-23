## ----FunctionPackage, echo = FALSE, results='hide'-----------------------
## Set Working Directory
require(knitr)

## Setting up Chunks Option
opts_chunk$set(comment = NA)
opts_knit$set(root.dir = "/Users/rajurim/Dropbox (NMBU)/ProjectHVR")

rm(list = ls())
source('functions.R')
file.copy('functions.R', '../functions.R', overwrite = TRUE)
## Load Packages
req.pkgs <- c('plyr', 'data.table', 'ggplot2', 'readr', 'reshape2', 'pls', 'knitr')
invisible(lapply(req.pkgs, require, character.only = TRUE, 
                 warn.conflicts = FALSE,
                 quietly = TRUE))

## ----gettingData, echo = FALSE-------------------------------------------
allFiles <- dir('Data')
evntFiles <- allFiles[grep('^[0-9]', allFiles)]

evntName <- laply(evntFiles, function(x) {
  substr(x, 
         regexpr('[0-9]_[a-zA-Z]', x) + 2, 
         regexpr('_RR.csv', x) - 1)
})
evntName <- gsub('Raju_', '', evntName)

## Reading Files
rr <- llply(evntFiles, function(x){
  fread(file.path('Data', x, fsep = '/'), drop = 1:3)/1000
})
names(rr) <- evntName

## Removing Outliers
rr <- llply(rr, function(x) {
  x <- x[, .(Time = cumsum(x[, ` rr`]), RR = x[, ` rr`])]
  x <- x[RR > .5 & RR < 1.5, ]
  return(x)
})

rrdt <- data.table(melt(rr, 1:2))
setnames(rrdt, names(rrdt), c('Time', 'RR', 'Events'))


## Smoothing
rrs <- llply(rr, function(x){
  splnFn <- splinefun(x = x[, Time], y = x[, RR])
  Time_seq <- seq(floor(min(x[, Time])), 
                  ceiling(max(x[, Time])), 
                  by = 1/4)
  x[, .(Time = Time_seq, RR = splnFn(Time_seq))]
})

## ----removeOutlier, echo = FALSE-----------------------------------------
  # Treaming Series to remove Outliers -------------------------------------------
usefulInfo <- data.table(from = c(150, 1000, 100, 1000, 2000), 
                         to = c(nrow(rrs$walking_in_stairs), 
                                nrow(rrs$Cycling), 2500, 23000, 
                                nrow(rrs$light_Jogging)))
setattr(usefulInfo, 'row.names', evntName)

# Subsetting DataTables ---------------------------------
rrs <- llply(seq_along(rrs), function(x){
  rrs[[x]][Time > usefulInfo[x, from] & Time < usefulInfo[x, to], ]
})
names(rrs) <- evntName

rrsdt <- data.table(melt(rrs, 1))[, !('variable'), with = FALSE]
setnames(rrsdt, names(rrsdt), c('Time', 'RR', 'Events'))

## ----rrplot, echo=FALSE, fig.height=8.5----------------------------------
  plt.rr <- ggplot(rrdt, aes(Time, RR)) + 
  geom_line(aes(color = Events), size = 0.25) + 
  facet_wrap(~Events, scale = 'free', ncol = 1) + 
  theme_bw() +
  theme(legend.title = element_blank(), 
        legend.position = 'top') + 
  ylab('RR-value')

## ----smoothplot, echo = FALSE, fig.height=8------------------------------
  plt.smooth <- ggplot(rrsdt[!Events %like% 'stairs'], aes(Time, RR)) + 
  geom_line(aes(color = Events), size = 0.25) + 
  facet_wrap(~Events, ncol = 2, scale = 'free') + 
  theme_bw() + 
  theme(legend.position = 'top', 
        legend.title = element_blank())

## ----splitTable, echo = FALSE--------------------------------------------
# rrsplit <- as.data.table(melt(llply(rrs, function(x){
#   fctr <- getSplitFactor(x, init = 1, 
#                          n.obs = nrow(x), 
#                          n.fctr = 256, 
#                          n.ovrlap = 16)
#   dt <- as.data.table(melt(llply(fctr, function(y){
#     x[y, ][, Split := .I]
#   }), 1:3))
#   setnames(dt, names(dt)[4], 'nobs')
# }), 1:4, value.factor = TRUE))
# setnames(rrsplit, names(rrsplit)[5], 'Events')
# rrsplit <- rrsplit[, Freq := Re(fft(RR)), by = .(nobs, Events)]
load('rrsplit.Rdata')

## ----splitTable.print, echo = FALSE--------------------------------------
dataSumry <- rrsplit[, .(split.nobs = length(unique(Split)), 
                           nsplit = length(unique(nobs)), 
                           nobs = length(RR)), by = Events]
dataSumry <- dataSumry[, nobs.bs := rrsdt[, length(RR), by = Events][, V1]]

## ----fftdata, echo=FALSE-------------------------------------------------
rrval <- dcast.data.table(rrsplit, nobs + Events ~ Split, 
                            fun.aggregate = mean, value.var = 'RR')
rrFreq <- dcast.data.table(rrsplit, 
                           nobs + Events ~ Split,
                           fun.aggregate = mean, value.var = 'Freq')

## ----fftplotm, echo = FALSE, warning=FALSE, eval = FALSE-----------------
## rr.subset <- rrsplit[Time %between% c(min(Time), mean(range(Time))),
##                      .(Time, RR, Split, Freq),
##                      by = c('Events', 'nobs')]
## rr.subset <- rr.subset[, log.Freq := log(Freq)]
## freq.plt <- ggplot(rr.subset, aes(Split, Freq, color = Events)) +
##   geom_line() +
##   facet_wrap(~Events, ncol = 1, scale = 'free') +
##   theme_bw() +
##   theme(legend.position = 'none') +
##   scale_y_log10()

## ----pca, echo=FALSE-----------------------------------------------------
pc.a.rr <- prcomp(rrval[, !c('nobs', 'Events'), with = FALSE], center = TRUE, scale. = TRUE)
pca.var <- data.table(comp = 1:ncol(pc.a.rr$x), var = round(explvar(pc.a.rr), 2))
pc.scores <- data.table(Events = rrval[, Events], pc.a.rr$x)
pc.load <- data.table(pc.a.rr$rotation)

## ----longSeries, echo=FALSE----------------------------------------------
longSeries <- data.table(readxl::read_excel('Report/Data/FullDataset.xlsx')[, 1:3])
longSeries <- melt(data.table(n = longSeries[, .I], longSeries), 1)
longSeries <- longSeries[, na.omit(value), by = variable]
invisible(longSeries[, n := 1:length(V1), by = variable])
setnames(longSeries, names(longSeries), c('p', 'rr', 'n'))
invisible(longSeries[, time := cumsum(rr), by = p])
setkey(longSeries, p)
longSeries <- longSeries[rr < 2]

ls.plt <- ggplot(longSeries, aes(time, rr, color = p)) + 
  geom_line(size = 0.5) + 
  facet_wrap(~p, ncol = 1, scale = 'free') +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x = 'Time (in Second)', y = 'RR-value')

## ----longSeriesPlot, echo = FALSE, fig.cap='Long Term (< 12 hours) rr-measurement for 3 differnt persons', fig.height=5.5, fig.pos='H'----
plot(ls.plt)

## ----st.rrPlot, echo=FALSE, fig.height=4, fig.cap='Short term RR-series for different activities', fig.pos='H'----
print(plt.smooth)

## ----pca.plot, echo = FALSE, fig.cap='Variance explained in Principal Component Analysis', fig.pos='H', fig.height=3----
ggplot(pca.var[1:15], aes(comp, var)) + 
  geom_line(size = 0.5) + 
  geom_point(shape = 21, size = 3, fill = 'gray') + 
  theme_bw() + 
  labs(x = 'Principal Components', y = 'Variance Explained') +
  scale_x_continuous(breaks = seq(15))

## ----pca.score.plot, echo=FALSE, fig.cap='Score plot for Principal component analysis colored with various activities', fig.pos = 'H', fig.height=4, fig.width = '0.8\\textwidth'----
lbl <- paste(paste('PC', 1:2, sep = ''), '(', pca.var[1:2, var], '%)', sep = '')
scrPlot.pca <- ggplot(pc.scores, aes(PC1, PC2, fill = Events)) + 
  geom_point(shape = 21) +
  theme_bw() + 
  theme(legend.title = element_blank(), 
        legend.position = 'top') +
  labs(x = lbl[1], y = lbl[2])
print(scrPlot.pca)


# Set workspace ----------------------------------------------------------------
setwd('~/Dropbox (NMBU)/ProjectHVR/')

# Data Preparation --------------------------------------------------------

# Loading Packages ———————————————————————————————————————————————————————#
req.pkgs <-
  c('readr', 'plyr', 'ggplot2', 'reshape2', 'car', 'pls', 'dplyr', 'data.table')
invisible(lapply(
  req.pkgs,
  require,
  quietly = TRUE,
  warn.conflicts = FALSE,
  character.only = TRUE
))

# File Preperation ———————————————————————————————————————————————————————#
rm(list = ls())
allFiles <- dir('Data')
evntFiles <- allFiles[-grep('TwentyFour', allFiles)]

evntName <- laply(evntFiles, function(x) {
  substr(x,
         regexpr('[0-9]_[a-zA-Z]', x) + 2,
         regexpr('_RR.csv', x) - 1)
})
evntName <- gsub('Raju_', '', evntName)

rr <- llply(evntFiles, function(x) {
  rr <-
    read.csv(paste('Heart Rate Variability Logger/', x, sep = ''))[, 'rr']
  t <- cumsum(rr)
  data.frame(t = t / 1000, rr = rr)
})
names(rr) <- evntName

rr.wo <- llply(rr, function(x) {
  x[which(x[, 2] < 1500 & x[, 2] > 500),]
})

rr.smooth <- llply(rr.wo, function(x, freq = 4) {
  splnFn <- splinefun(x = x$t,
                      y = x$rr,
                      method = 'fmm')
  
  time.from <- ceiling(min(x$t))
  time.to <- floor(max(x$t))
  time.seq <- seq(time.from, time.to, 1 / freq)
  return(data.frame(time = time.seq, rr = splnFn(time.seq)))
})

# Treaming Series to remove Outliers —————————————————————————————————————————#
usefulInfo <- data.frame(
  from = c(150, 1000, 100, 1000, 2000),
  to = c(
    nrow(rr.smooth$walking_in_stairs),
    nrow(rr.smooth$Cycling),
    2500,
    23000,
    nrow(rr.smooth$light_Jogging)
  )
)
rownames(usefulInfo) <- evntName

# Attaching Attributes to the dataframes ——————————————————————————————————————#
for (x in seq_along(rr.smooth)) {
  attr(rr.smooth[[x]], 'from') <- usefulInfo[x, 1]
  attr(rr.smooth[[x]], 'to') <- usefulInfo[x, 2]
}

# Subsetting dataframes ———————————————————————————————————————————————————————#
subset.rr <- llply(rr.smooth, function(x) {
  filter(x, time > attr(x, 'from') & time < attr(x, 'to'))
})

# Old Code (Collapse it) ——————————————————————————————————————————————————————#
#Splitting and accumulating the dataframe ------------------------------------#
n.obs <- 2 ^ 8
split.rr <- ldply(subset.rr, function(x){
  fctr <- as.factor(c(rep(1:n.obs, nrow(x) %/% n.obs),
                      rep(0, nrow(x) %% n.obs))
                    )
  x.spltd <- split(x, fctr)
  x.spltd <- x.spltd[-1]
  avgd <- ldply(x.spltd, function(x){
    mean(Re(fft(x$rr)))
  })
  avgd[, -1]
})
colnames(split.rr) <- c('event', 1:(ncol(split.rr) - 1))

#A linear plot ---------------------------------------------------------------#
stkData <- melt(split.rr, id.vars = 'event',
                value.name = 'rr',
                variable.name = 'Freq')
stkData$Freq <- as.numeric(stkData$Freq)
#
plt <- ggplot(stkData, aes(Freq, rr, color = event)) +
  geom_line() +
  geom_point(size = 1.5) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'top')
#
print(plt)

# Performing PCA --------------------------------------------------------------#
# pc.a <- prcomp(split.rr[, -1], center = T, scale. = T)

# Extra code -------------------------------------------------------------------
getSplitFactor <- function(x, init, n.obs = nrow(x), n.fctr, n.ovrlap){
  getFactor <- list()
  i <- 1
  while (init + n.fctr < n.obs) {
    getFactor[[i]] <- seq(init, length.out = n.fctr)
    init <- init + n.ovrlap
    i <- i + 1
  }
  return(getFactor)
}

getSplited <- function(rrEventList, start.from = 1, n.factor = 128, n.obs.overlap = 16, rr.var = 'rr', Freq = FALSE){
    llply(rrEventList, function(rrList){
      splitFactors <- getSplitFactor(init = start.from, n.obs = nrow(rrList), n.fctr = n.factor, n.ovrlap = n.obs.overlap)
      ldply(splitFactors, function(fctrList){
        time <- rrList[fctrList, 1]
        if (Freq) rrVal <- Re(fft(rrList[fctrList, 2])) else rrVal <- rrList[fctrList, 2]
        df <- data.frame(time, rrVal)
        if (Freq) names(df)[2] <- 'rrFreq'
        return(df)
      })
    })
}

Splited.rrVal <- getSplited(subset.rr, start.from = 1, n.factor = 128, n.obs.overlap = 16, Freq = FALSE)
Splited.rrFreq <- getSplited(subset.rr, start.from = 1, n.factor = 128, n.obs.overlap = 16, Freq = TRUE)

rrValStacked <- ldply(Splited.rrVal, rbind, .id = 'event')
rrFreqStacked <- ldply(Splited.rrFreq, rbind, .id = 'event')

stkdf <- melt(big.df, variable.name = 'n', value.name = 'freq', id.vars = 'event')
stkdf$nn <- as.numeric(stkdf$n)

pc.a <- prcomp(t(big.df[,-1]), center = T, scale. = T)

big.df$resp <- as.numeric(big.df$event)
pls.mdl <- plsr(resp ~ ., data = big.df[, -1], scale = T)

scrs <- data.frame(pls.mdl$scores[,])
print(ggplot(scrs, aes(Comp.1, Comp.3)) + 
        geom_point(aes(color = big.df$event)))

# Extra content --------------------------------------------------
max.length <- max(ldply(rrValue.wo, length)$V1)
rrdf <- ldply(rrValue.wo, function(x) c(x, rep(0, max.length - length(x))))
rownames(rrdf) <- rrdf[, 1]
rrdf <- rrdf[, -1]
rrfft <- adply(rrdf, 1, function(x) {
  x <- as.numeric(x)
  return(sqrt((Re(fft(x)) ^ 2 + Im(fft(x)) ^ 2)))
})
rownames(rrfft) <- rownames(rrdf)

pc.a <- princomp(t(rrfft), cor = T)

# Event: Walking_in_stairs
## Changing into time domain (second)
get.evnt.df <- function(evnt, freq) {
  wlkStrs <- cumsum(rrValue.wo[[evnt]])/1000
  start.t <- head(wlkStrs, 1)
  end.t <- tail(wlkStrs, 1)
  time.seq <- seq(round(start.t), round(end.t), by = 1/freq)
  wlkdf <- data.frame(
    time = time.seq,
    rr.val = approx(x = wlkStrs,
                    y = rrValue.wo[[evnt]],
                    xout = time.seq)$y
  )
  return(wlkdf)
}
get.evnt.df(evnt = eventName[1], freq = 4)

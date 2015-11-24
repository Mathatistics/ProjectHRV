## Loading models and data
if (file.exists('Exports/pcr.Rdata'))
  load('Exports/pcr.Rdata')
if (file.exists('Exports/pls.Rdata'))
  load('Exports/pls.Rdata')

## Loading Dataset
Series <- DT(melt(as.matrix(read_excel('Data/FullDataset.xlsx', sheet = 1)), 
                  varnames = c('n', 'ID'), value.name = 'rr'), key = 'ID')
Series <- na.omit(Series)
Labels <- DT(read_excel('Data/FullDataset.xlsx', sheet = 2), key = 'ID')

## Creating Short and Long Series
ShortSeries <- Series[Labels[as.logical(ShortSelection), .(ID, events)]]
LongSeries <- Series[Labels[as.logical(LongSelection), .(ID, events)]]
setkeyv(ShortSeries, 'ID'); setkeyv(LongSeries, 'ID')

## Filterout very short Series
ShortSeries <- ShortSeries[ShortSeries[, .(rr.length = length(rr)), 
                                       by = ID][rr.length > 128, .(ID)]]
invisible(ShortSeries[, time := cumsum(rr), by = ID])

## Creating Smoothing Spline and smoothing rr values w.r.t. time
splnFn <- ShortSeries[, .(fn = list(splinefun(time, rr)),
                          rrlst = list(rr),
                          time = list(time),
                          events = unique(events)), by = ID]
invisible(splnFn[, maxTime := unlist(llply(time, function(x) ceiling(max(x))))])
invisible(splnFn[, time.seq := llply(maxTime, function(x) seq(0, x, 0.25))])
invisible(splnFn[, rrs := llply(splnFn[, as.character(ID)], function(x){
  fn <- splnFn[x, fn][[1]]
  time.sq <- splnFn[x, time.seq][[1]]
  return(rrs = fn(time.sq))
})])

## Extracting Smooth rr Series with Events and PersonID
rrsmooth <- splnFn[, .(events, time = unlist(time.seq), 
                       rrs = unlist(rrs)), 
                   by = ID]

## Plotting the RR-Series removing negative rr values and values greater than 2
ggplot(rrsmooth[rrs < 2 & rrs > 0], aes(time, rrs, color = ID)) + 
  geom_line() + facet_wrap(~events, scale = 'free') + 
  theme_bw() + theme(legend.position = 'none')

## Converting into Frequency windows with following properties
windows.size <- 128; Frequency <- 4; Overlap <- 16

spcgrm <- rrsmooth[, .(events = unique(events), 
                       sg = list(specgram(rrs))), 
                   by = ID]

## Getting log10 of absolute value of rr frequency for each Person
freqMat <- ldply(spcgrm[, as.character(ID)], function(x){
  spcgrm[x, data.table(ID = ID, event = events, t(log10(abs(sg[[1]]$S))))]
})

## Dummy Categorical Variable
resp.dummy <- model.matrix(~event - 1, data = freqMat)
colnames(resp.dummy) <- gsub('event', '', colnames(resp.dummy))

myData <- data.frame(y = I(resp.dummy), 
                     x = I(as.matrix(freqMat[, -c(1:2)])))
ldaData <- data.frame(y = freqMat[, "event"], 
                      x = I(as.matrix(freqMat[, -c(1:2)])))
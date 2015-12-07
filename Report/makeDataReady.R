## Loading models and data ----------
if (file.exists('Exports/pcr.Rdata'))
  load('Exports/pcr.Rdata')
if (file.exists('Exports/pls.Rdata'))
  load('Exports/pls.Rdata')

## Loading Dataset -------------
Series <- data.table(melt(as.matrix(read_excel('Data/CompleteDataset.xlsx', sheet = 1)), 
                  varnames = c('n', 'ID'), value.name = 'rr'), key = 'ID')
Series <- na.omit(Series)
Labels <- data.table(read_excel('Data/CompleteDataset.xlsx', sheet = 2), key = 'ID')

## Creating Short and Long Series ---------
Series <- Series[Labels[, .(ID, Activity, Gender, Person, Test)]]
setkeyv(Series, 'ID');

invisible(Series[, time := cumsum(rr), by = ID])

## Creating Smoothing Spline and smoothing rr values w.r.t. time --------
splnFn <- Series[, .(fn = list(splinefun(time, rr)),
                          rrlst = list(rr),
                          time = list(time),
                          Activity = unique(Activity),
                     Person = unique(Person),
                     Test = as.logical(unique(Test))), by = ID]
invisible(splnFn[, maxTime := unlist(llply(time, function(x) ceiling(max(x))))])
invisible(splnFn[, time.seq := llply(maxTime, function(x) seq(0, x, 0.125))])
invisible(splnFn[, rrs := llply(splnFn[, as.character(ID)], function(x){
  fn <- splnFn[x, fn][[1]]
  time.sq <- splnFn[x, time.seq][[1]]
  return(rrs = fn(time.sq))
})])

## Extracting Smooth rr Series with Activity and PersonID ---------
rrsmooth <- splnFn[, .(Test,
                       Person, 
                       Activity, 
                       time = unlist(time.seq), 
                       rrs = unlist(rrs)), 
                   by = ID]

## Plotting the RR-Series removing negative rr values and values greater than 2 -----
ggplot(rrsmooth[rrs < 2 & rrs > 0 & !Test], aes(time, rrs, color = Person)) + 
  geom_line() + facet_wrap(~Activity, scale = 'free', ncol = 5) + 
  theme_bw() + theme(legend.position = 'none')

## Converting into Frequency windows with following properties -------
windows.size <- 128; Frequency <- 4; Overlap <- 16

spcgrm <- rrsmooth[, .(Test = unique(Test),
                       Activity = unique(Activity),
                       Person = unique(Person),
                       sg = list(specgram(rrs))), 
                   by = ID]

## Getting log10 of absolute value of rr frequency for each Person ------
freqMat <- data.table(ldply(spcgrm[, as.character(ID)], function(x){
  spcgrm[x, data.table(Test = Test,
                       Person = Person, 
                       ID = ID,
                       event = Activity, t(log10(abs(sg[[1]]$S))))]
}))
freq.cols <- ls(freqMat, pattern = '^V')

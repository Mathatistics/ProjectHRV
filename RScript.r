# Loading Packages
req.pkgs <- c('readr', 'plyr', 'ggplot2', 'reshape2', 'car', 'pls')
invisible(lapply(req.pkgs, require, 
                 quietly = TRUE, 
                 warn.conflicts = FALSE, 
                 character.only = TRUE)
)

# File Preperation
rm(list = ls())
files <- dir('Heart Rate Variability Logger/', pattern = '[a-zA-Z]_RR')

eventName <- laply(files, function(x) {
  substr(x, 
         regexpr('[0-9]_[a-zA-Z]', x) + 2, 
         regexpr('_RR.csv', x) - 1)
})
eventName <- gsub('Raju_', '', eventName)

rrValue <- llply(files, function(x) {
  read.csv(paste('Heart Rate Variability Logger/', x, sep = ''))[, 'rr']
})
names(rrValue) <- eventName

rrValue.wo <- llply(rrValue, function(x){
  x[x < 2000]
})

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

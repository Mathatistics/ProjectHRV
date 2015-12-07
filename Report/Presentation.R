## ----initialStuffs, echo = FALSE, message=FALSE, warning=FALSE, error=FALSE, results='hide'----
## Setting up working directory
wd <- "/Users/rajurim/Dropbox (NMBU)/ProjectHRV/Report"
if (getwd() != wd)
  setwd(wd)

source('initialStuffs.R')
source('functions.R')
source('makeDataReady.R')
knitr::opts_chunk$set(comment = NA)
# knitr::opts_chunk$set(eval = F)
req.rdata <- c('pcr1', 'pls1', 'cpls1',
               'pcr2', 'pls2', 'cpls2',
               'pcr3', 'pls3', 'cpls3')
fp <- file.path('Exports', paste(req.rdata, 'Rdata', sep = '.'))
fp <- fp[file.exists(fp)]
llply(fp, load, envir = .GlobalEnv)
mdl.list <- paste(gsub('[[:digit:]]', replacement = '', req.rdata), 
                  gsub('[[:alpha:]]', replacement = '', req.rdata), 
                  sep = '.')

## ----firstSet, child="Child1.Rnw", eval = T------------------------------

## ----Set1Setup, echo=FALSE-----------------------------------------------
## Getting Data
set.1 <- freqMat[, .SD, .SDcols = freq.cols, by = .(ID, event, Test)]
train.1 <- list(x = I(as.matrix(set.1[which(!Test), freq.cols, with = F])),
              y = set.1[which(!Test), event])
test.1 <- list(x = I(as.matrix(set.1[which(Test), freq.cols, with = F])),
              y = set.1[which(Test), event])

## ----Set1ModelBuilding, echo=FALSE, eval = T-----------------------------
## Model Fitting
## PCR Model
if (!exists('pcr.1')) {
  pcr.1 <- plda(x = train.1$x, cat.resp = train.1$y, fn = 'pcr', split = 10)
  save(pcr.1, file = 'Exports/pcr1.Rdata')
}
## PLS Model
if (!exists('pls.1')) {
  pls.1 <- plda(x = train.1$x, cat.resp = train.1$y, fn = 'plsr', split = 10)
  save(pls.1, file = 'Exports/pls1.Rdata')
}
## CPLS Model
if (!exists('cpls.1')) {
  cpls.1 <- plda(x = train.1$x, cat.resp = train.1$y, fn = 'cppls', split = 10)
  save(cpls.1, file = 'Exports/cpls1.Rdata')
}

## ----Set1ErrorSetup, echo=FALSE, fig.width='\\textwidth', fig.height=2.5----
err.dt <- getError(pls.1, type = 'both')$err.dt[
  getError(pcr.1, type = 'both')$err.dt][
    getError(cpls.1, type = 'both')$err.dt]
setnames(err.dt, names(err.dt), c('Type', 'Comp', 'PLS', 'PCR', 'CPLS'))
err.dt <- melt(err.dt, 1:2)
err.min.dt <- err.dt[, .(Comp = which.min(value), value = min(value)), 
                     by = .(Type, variable)]
setkeyv(err.dt, c('Type', 'Comp', 'variable'))
setkeyv(err.min.dt, c('Type', 'Comp', 'variable'))

## ----Set1ErrorPlot, errPlot, echo = F, fig.width='\\linewidth', fig.height=4.5, eval = TRUE----
ggplot(err.dt, aes(Comp, value)) + geom_line() +
  facet_grid(Type~variable, scale = 'free_y', as.table = TRUE) +
  theme_bw() +
  geom_text(data = err.min.dt, aes( x = Inf, y = Inf,
           label = paste('Comp:', Comp, '\nError:', round(value, 2))
  ), hjust = 1.25, vjust = 1.25, size = 4) +
  labs(x = 'Components', y = 'Misclassification Error')

## ----Set1MscSetup, echo=FALSE, fig.width='\\textwidth', fig.height=2.5----
msc.pls.1 <- msc(getPredicted(pls.1, ncomp = 4, newdata = test.1$x, 
                            newY = test.1$y))
msc.pcr.1 <- msc(getPredicted(pcr.1, ncomp = 56, newdata = test.1$x, 
                            newY = test.1$y))
msc.cpls.1 <- msc(getPredicted(cpls.1, ncomp = 29, newdata = test.1$x, 
                            newY = test.1$y))
msc.1 <- msc.pls.1$conf.dt[msc.pcr.1$conf.dt][msc.cpls.1$conf.dt]
setnames(msc.1, names(msc.1), c('Original', 'Correct', 'Type', 'PLS', 'PCR', 'CPLS'))
msc.1 <- melt(msc.1, 1:3)
setkeyv(msc.1, c('Original', 'Correct', 'Type', 'variable'))

## ----Set1MscPlot.1, echo = F, eval=TRUE, fig.height=4.8------------------
mscPlot(msc.1, 'train')

## ----Set1MscPlot.2, echo = F, eval=TRUE, fig.height=4.8------------------
mscPlot(msc.1, 'test')

## ----Set1ScorePlot1, echo = F, eval=TRUE, fig.height=5-------------------
print(plotScore(pcr.1, 'Model:PCR'))

## ----Set1ScorePlot2, echo = F, eval=TRUE, fig.height=5-------------------
print(plotScore(pls.1, 'Model:PLS'))

## ----Set1ScorePlot3, echo = F, eval=TRUE, fig.height=5-------------------
print(plotScore(cpls.1, 'Model:CPPLS'))


## ----secondSet, child="Child2.Rnw", eval = T-----------------------------

## ----Set2Setup, echo=FALSE-----------------------------------------------
## Getting Data
set.2 <- freqMat[, llply(.SD, mean), .SDcols = freq.cols, by = .(ID, event, Test)]
train.2 <- list(x = I(as.matrix(set.2[which(!Test), freq.cols, with = F])),
              y = set.2[which(!Test), event])
test.2 <- list(x = I(as.matrix(set.2[which(Test), freq.cols, with = F])),
              y = set.2[which(Test), event])

## ----Set2ModelBuilding, echo=FALSE---------------------------------------
## Model Fitting
## PCR Model
if (!exists('pcr.2')) {
  pcr.2 <- plda(x = train.2$x, cat.resp = train.2$y, fn = 'pcr', split = 10)
  save(pcr.2, file = 'Exports/pcr2.Rdata')
}
## PLS Model
if (!exists('pls.2')) {
  pls.2 <- plda(x = train.2$x, cat.resp = train.2$y, fn = 'plsr', split = 10)
  save(pls.2, file = 'Exports/pls2.Rdata')
}
## CPLS Model
if (!exists('cpls.2')) {
  cpls.2 <- plda(x = train.2$x, cat.resp = train.2$y, fn = 'cppls', split = 10)
  save(cpls.2, file = 'Exports/cpls2.Rdata')
}

## ----Set2ErrorSetup, echo=FALSE, fig.width='\\textwidth', fig.height=2.5----
err.dt <- getError(pls.2, type = 'both')$err.dt[
  getError(pcr.2, type = 'both')$err.dt][
    getError(cpls.2, type = 'both')$err.dt]
setnames(err.dt, names(err.dt), c('Type', 'Comp', 'PLS', 'PCR', 'CPLS'))
err.dt <- melt(err.dt, 1:2)
err.min.dt <- err.dt[, .(Comp = which.min(value), value = min(value)), 
                     by = .(Type, variable)]
setkeyv(err.dt, c('Type', 'Comp', 'variable'))
setkeyv(err.min.dt, c('Type', 'Comp', 'variable'))

## ----Set2ErrorPlot, errPlot, echo = F, fig.width='\\linewidth', fig.height=4.5, eval = TRUE----
ggplot(err.dt, aes(Comp, value)) + geom_line() +
  facet_grid(Type~variable, scale = 'free_y', as.table = TRUE) +
  theme_bw() +
  geom_text(data = err.min.dt, aes( x = Inf, y = Inf,
           label = paste('Comp:', Comp, '\nError:', round(value, 2))
  ), hjust = 1.25, vjust = 1.25, size = 4) +
  labs(x = 'Components', y = 'Misclassification Error')

## ----Set2MscSetup, echo=FALSE, fig.width='\\textwidth', fig.height=2.5----
msc.pls.2 <- msc(getPredicted(pls.2, ncomp = 15, newdata = test.2$x, 
                            newY = test.2$y))
msc.pcr.2 <- msc(getPredicted(pcr.2, ncomp = 128, newdata = test.2$x, 
                            newY = test.2$y))
msc.cpls.2 <- msc(getPredicted(cpls.2, ncomp = 68, newdata = test.2$x, 
                            newY = test.2$y))
msc.2 <- msc.pls.2$conf.dt[msc.pcr.2$conf.dt][msc.cpls.2$conf.dt]
setnames(msc.2, names(msc.2), c('Original', 'Correct', 'Type', 'PLS', 'PCR', 'CPLS'))
msc.2 <- melt(msc.2, 1:3)
setkeyv(msc.2, c('Original', 'Correct', 'Type', 'variable'))

## ----Set2MscPlot.1, echo = F, eval=TRUE, fig.height=5--------------------
mscPlot(msc.2, 'train')

## ----Set2MscPlot.2, echo = F, eval=TRUE, fig.height=5--------------------
mscPlot(msc.2, 'test')

## ----Set2ScorePlot1, echo = F, eval=TRUE, fig.height=5-------------------
print(plotScore(pcr.2, 'Model:PCR', cat.var = train.2$y, point.size = 2))

## ----Set2ScorePlot2, echo = F, eval=TRUE, fig.height=5-------------------
print(plotScore(pls.2, 'Model:PLS', cat.var = train.2$y, point.size = 2))

## ----Set2ScorePlot3, echo = F, eval=TRUE, fig.height=5-------------------
print(plotScore(cpls.2, 'Model:CPPLS', cat.var = train.2$y, point.size = 2))


## ----thirdSet, child="Child3.Rnw", eval = T------------------------------

## ----Set3Setup, echo=FALSE-----------------------------------------------
## Getting Data
set.3 <- freqMat[, llply(.SD, mean), .SDcols = freq.cols, by = .(Person, event, Test)]
train.3 <- list(x = I(as.matrix(set.3[which(!Test), freq.cols, with = F])),
              y = set.3[which(!Test), event])
test.3 <- list(x = I(as.matrix(set.3[which(Test), freq.cols, with = F])),
              y = set.3[which(Test), event])

## ----Set3ModelBuilding, echo=FALSE---------------------------------------
## Model Fitting
## PCR Model
if (!exists('pcr.3')) {
  pcr.3 <- plda(x = train.3$x, cat.resp = train.3$y, fn = 'pcr', split = 10, fitComp = 5)
  save(pcr.3, file = 'Exports/pcr3.Rdata')
}
## PLS Model
if (!exists('pls.3')) {
  pls.3 <- plda(x = train.3$x, cat.resp = train.3$y, fn = 'plsr', split = 10, fitComp = 5)
  save(pls.3, file = 'Exports/pls3.Rdata')
}
## CPLS Model
if (!exists('cpls.3')) {
  cpls.3 <- plda(x = train.3$x, cat.resp = train.3$y, fn = 'cppls', split = 10, fitComp = 5, ldafn = 'rda')
  save(cpls.3, file = 'Exports/cpls3.Rdata')
}

## ----Set3ErrorSetup, echo=FALSE, fig.width='\\textwidth', fig.height=2.5----
err.dt <- getError(pls.3, type = 'both')$err.dt[
  getError(pcr.3, type = 'both')$err.dt][
    getError(cpls.3, type = 'both')$err.dt]
setnames(err.dt, names(err.dt), c('Type', 'Comp', 'PLS', 'PCR', 'CPLS'))
err.dt <- melt(err.dt, 1:2)
err.min.dt <- err.dt[, .(Comp = which.min(value), value = min(value)), 
                     by = .(Type, variable)]
setkeyv(err.dt, c('Type', 'Comp', 'variable'))
setkeyv(err.min.dt, c('Type', 'Comp', 'variable'))

## ----Set3ErrorPlot, errPlot, echo = F, fig.width='\\linewidth', fig.height=4.5, eval = TRUE----
ggplot(err.dt, aes(Comp, value)) + geom_line() +
  facet_grid(Type~variable, scale = 'free_y', as.table = TRUE) +
  theme_bw() +
  geom_text(data = err.min.dt, aes( x = Inf, y = Inf,
           label = paste('Comp:', Comp, '\nError:', round(value, 2))
  ), hjust = 1.35, vjust = 1.35, size = 4) +
  labs(x = 'Components', y = 'Misclassification Error')

## ----Set3MscSetup, echo=FALSE, fig.width='\\textwidth', fig.height=2.5----
msc.pls.3 <- msc(getPredicted(pls.3, ncomp = 1, newdata = test.3$x, 
                            newY = test.3$y))
msc.pcr.3 <- msc(getPredicted(pcr.3, ncomp = 1, newdata = test.3$x, 
                            newY = test.3$y))
msc.cpls.3 <- msc(getPredicted(cpls.3, ncomp = 1, newdata = test.3$x, 
                            newY = test.3$y))
msc.3 <- msc.pls.3$conf.dt[msc.pcr.3$conf.dt][msc.cpls.3$conf.dt]
setnames(msc.3, names(msc.3), c('Original', 'Correct', 'Type', 'PLS', 'PCR', 'CPLS'))
msc.3 <- melt(msc.3, 1:3)
setkeyv(msc.3, c('Original', 'Correct', 'Type', 'variable'))

## ----Set3MscPlot.1, echo = F, eval=TRUE, fig.height=5--------------------
mscPlot(msc.3, 'train')

## ----Set3MscPlot.2, echo = F, eval=TRUE, fig.height=5--------------------
mscPlot(msc.3, 'test')

## ----Set3ScorePlot1, echo = F, eval=TRUE, fig.height=5-------------------
print(plotScore(pcr.3, 'Model:PCR', cat.var = train.3$y, point.size = 2))

## ----Set3ScorePlot2, echo = F, eval=TRUE, fig.height=5-------------------
print(plotScore(pls.3, 'Model:PLS', cat.var = train.3$y, point.size = 2))

## ----Set3ScorePlot3, echo = F, eval=TRUE, fig.height=5-------------------
print(plotScore(cpls.3, 'Model:CPPLS', cat.var = train.3$y, point.size = 2))


## ----mscErrorSetup, echo = F---------------------------------------------
evl <- function(string) eval(parse(text = string))
mscError <- data.table(
  ldply(ls(environment(), pattern = 'msc.[a-z]+.[0-9]'), function(x){
    cbind(Model = x, 
          TrainMsc = round(evl(x)$train.msc, 4), 
          TestMsc = round(evl(x)$test.msc, 4),
          ncomp = evl(x)$ncomp)
})
)
invisible(mscError[, c('Model', 'Set') := 
                     tstrsplit(Model, '.', fixed = T)[-1]])
mscError <- melt(mscError, id.vars = c('Model', 'Set', 'ncomp'))
invisible(mscError[, c('variable', 'Model', 'Set', 'ncomp') := 
                       .(gsub('Msc', '', mscError[, variable]),
                         toupper(Model),
                         paste('Set', Set),
                         ncomp)])

## ----mscErrorPlot, echo = F, fig.height=4, fig.cap='Training and Test Misclassification Error for all the three models. The LDA models were fitted with the scores obtained from three models with components (number above each points) needed to get minimum cross-validation error.'----
ggplot(mscError, aes(Model, value, color = variable)) + 
  geom_point() + 
  geom_text(aes(label = ncomp), vjust = -1, color = 'black', size = rel(4)) +
  facet_grid(. ~ Set, as.table = TRUE) + 
  geom_line(aes(group = variable)) + 
  theme_bw() +
  theme(legend.position = 'top',
        legend.title = element_blank()) +
  labs(y = 'Misclassification Error')


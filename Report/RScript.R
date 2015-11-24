scriptFiles <- c('initialStuffs.R', 'functions.R', 'initialStuffs.R',
                 'makeDataReady.R')
invisible(llply(scriptFiles, source))

## Setting up crossvalidation
split <- 10
train.err.mat <- matrix(ncol = split, nrow = ncol(myData$x), 
                        dimnames = list(Comp = 1:ncol(myData$x),
                                        Split = 1:split))
test.err.mat <- matrix(ncol = split, nrow = ncol(myData$x), 
                       dimnames = list(Comp = 1:ncol(myData$x),
                                       Split = 1:split))

cvSplits <- cvsegments(nrow(myData), k = split, type = 'random')

for (cmp in 1:ncol(myData$x)) {
  ## PLS Model fit on whole dataset
  mdl <- plsr(y ~ x, data = myData, ncomp = cmp)
  x.scrs <- mdl$scores ## Getting Scores
  
  ## Start cv-loop
  for (k in seq_along(cvSplits)) {
    splt <- cvSplits[[k]] 
    ## Predict X-Scores to use as predictor in LDA
    ldaData <- data.frame(y = ldaData[, 'y', drop = F], 
                           x = I(x.scrs))
    
    ## LDA fit
    lda.fit <- lda(y ~ x, data = ldaData[-splt, ])
    
    ## LDA Prediction
    lda.trn.pred <- predict(lda.fit)
    lda.tst.pred <- predict(lda.fit, newdata = ldaData[splt, ])
    
    ## Confusion Table
    trn.conf.tbl <- table(lda.trn.pred$class, ldaData[-splt, 'y'])
    tst.conf.tbl <- table(lda.tst.pred$class, ldaData[splt, 'y'])
    
    ## Prediction Error
    tst.pred.err <- 1 - sum(diag(tst.conf.tbl)) / sum(tst.conf.tbl)
    trn.pred.err <- 1 - sum(diag(trn.conf.tbl)) / sum(trn.conf.tbl)
    
    ## Saving Results
    test.err.mat[cmp, k] <- tst.pred.err
    train.err.mat[cmp, k] <- trn.pred.err
  }
}

err.dt <- DT(melt(data.frame(ncomp = 1:ncol(myData$x), 
                    test = apply(test.err.mat, 1, mean), 
                    train = apply(train.err.mat, 1, mean)), 1))

min.err.dt <- err.dt[, .(value = min(value), 
                         ncomp = which.min(value)), 
                     by = variable]

## Prediction Plot
predErrorPlot <- ggplot(err.dt, aes(ncomp, value)) + geom_line() +
  facet_wrap(~variable, scale = 'free') + 
  theme_bw() + 
  labs(x = 'Components', y = 'Prediction Error') +
  geom_hline(aes(yintercept = value), data = min.err.dt, 
             color = 'gray', linetype = 2) +
  geom_vline(aes(xintercept = ncomp), data = min.err.dt,
             color = 'gray', linetype = 2 ) +
  geom_point(color = 'red', data = min.err.dt) +
  geom_text(data = min.err.dt, aes(
    ncomp, value, 
    label = paste('PredError:', round(value, 2), ',',
                  'Comps:', ncomp),
    size = 3, angle = 90,
    hjust = -1, vjust = -0.5)) +
  theme(legend.position = 'none')

print(predErrorPlot)

ldaData <- data.frame(y = freqMat[, "event"], 
                      x = I(mdl$scores[, 1:min.err.dt[, min(ncomp)]]))
plda.Mdl <- lda(y ~ x, data = ldaData)
plda.confMat <- table(predict(plda.Mdl)$class, freqMat[, "event"])
plda.errRate <- 1 - sum(diag(plda.confMat))/sum(plda.confMat)

ldaMdl <- lda(event ~ ., data = freqMat[, -1])
lda.conf <- table(predict(ldaMdl)$class, freqMat[, "event"])
lda.errRate <- 1 - sum(diag(lda.conf))/sum(lda.conf)

plda.cfmat.dt <- DT(melt(plda.confMat, 
                         varnames = c('Correct', 'Predicted'), 
                         value.name = 'count'),
                    key = c('Correct', 'Predicted'))
correct.pred <- plda.cfmat.dt[Correct == Predicted]
wrong.pred <- plda.cfmat.dt[Correct != Predicted, 
                            .(wrongCount = sum(count)),
                            by = Correct]
conf.dt <- melt(correct.pred[wrong.pred], 1:2)

## Confusion Matrix Plot
conf.plot <- ggplot(conf.dt, aes(Correct, value, fill = variable)) +
  geom_bar(stat = 'identity', position = 'fill') + coord_flip() +
  theme_bw() + scale_y_continuous(label = scales::percent) + 
  theme(legend.position = 'top', legend.title = element_blank(),
        axis.title = element_blank()) +
  geom_text(aes(ymax = value/sum(value), label = value), 
            position = 'fill', 
            hjust = 1.5, 
            size = 3.5)
print(conf.plot)











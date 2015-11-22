longSeries <- data.table(readxl::read_excel('Report/Data/FullDataset.xlsx')[, 1:3])
longSeries <- melt(data.table(n = longSeries[, .I], longSeries), 1)
longSeries <- longSeries[, na.omit(value), by = variable]
longSeries[, n := 1:length(V1), by = variable]
setnames(longSeries, names(longSeries), c('p', 'rr', 'n'))
longSeries[, time := cumsum(rr), by = p]
setkey(longSeries, p)

longSeries <- longSeries <- fullData[p %in% c('P1', 'P2', 'P3')]

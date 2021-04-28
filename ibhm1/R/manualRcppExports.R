weightedMean <- function(x, w) {
    .Call('IBHM1_weightedMean', PACKAGE = 'IBHM1', x, w)
}

weightedVar <- function(x, w) {
    .Call('IBHM1_weightedVar', PACKAGE = 'IBHM1', x, w)
}

weightedCov <- function(x, z, w) {
    .Call('IBHM1_weightedCov', PACKAGE = 'IBHM1', x, z, w)
}

weightedR <- function(y1, y2, w) {
    .Call('IBHM1_weightedR', PACKAGE = 'IBHM1', y1, y2, w)
}

tiedRanks <- function(x) {
    .Call('IBHM1_tiedRanks', PACKAGE = 'IBHM1', x)
}

weightedRho <- function(y1, y2, w) {
    .Call('IBHM1_weightedRho', PACKAGE = 'IBHM1', y1, y2, w)
}


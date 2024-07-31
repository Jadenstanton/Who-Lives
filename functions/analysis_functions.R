############################################
# # ANALYSIS # #
############################################

## this function is used in "wholivesdatapull" to correctly order rows: Orl,Jeff,St.T/071,051,103
## input: dataframe (from censusapi pull)
## output: dataframe with rows 1 & 2 switched
switch <- function(dataframe) {
    dataframe2 <- dataframe[2, ] # extract row 2 (Orleans)
    dataframe <- rbind(dataframe2, dataframe[-2, ]) # move row 2 to be row 1
}

## calculates MOE for 2000 STF3 files.
## Formula on pg954 of documentation, table A "Unadjusted Standard Error for Estimated Totals"
## This is only for estimate totals.  Median and pcts will have to be done differently!!!***
## N = population
## Design factor table not found, so until we multiply by design factor, it's unadjusted std error.
## critical value for ACS is z = 1.645 (they use 90% CI)
moe2000 <- function(est, n, designfac = 1) {
    se_unadj <- sqrt(5 * est * (1 - (est / n)))
    se <- se_unadj * designfac # when designfac = 1 it produces the unadjusted standard errors
    MOE <- se * 1.645
    return(MOE)
}


## calculates MOE for aggregated estimates
## moe = sqrt(sum(estimateMOE^2))
## input: dataframe of estimates' MOEs (i.e. use cbind)
## output: column of MOEs
moeagg <- function(estimateMOE) {
    squares <- matrix(0, dim(estimateMOE)[1], dim(estimateMOE)[2])
    for (i in 1:dim(estimateMOE)[2]) {
        squares[, i] <- t(estimateMOE[, i] * estimateMOE[, i])
    }
    sumsquares <- apply(squares, 1, sum)
    return(sqrt(sumsquares))
}

## calculates MOE for proportions
## p = x/y
## moe = sqrt(moex^2 - p^2 * moey^2)/y
## input: columns of measures y, moex, moey, p
## output: column of MOEs
moeprop <- function(y, moex, moey, p) {
    mp <- matrix(0, length(y))
    for (i in 1:length(y)) {
        if ((moex[i] * moex[i] - p[i] * p[i] * moey[i] * moey[i]) < 0) {
            mp[i] <- (sqrt(moex[i] * moex[i] + p[i] * p[i] * moey[i] * moey[i])) / y[i]
        } else {
            mp[i] <- (sqrt(moex[i] * moex[i] - p[i] * p[i] * moey[i] * moey[i])) / y[i]
        }
    }
    return(mp)
}

## stat testing for 2000 vs 201* data
## input: columns of estimates and their MOEs (zeros for Census 2000)
## output: column of yes or no if significant
stattest <- function(x, moex = matrix(0, length(x)), y, moey, zscore = 1.96) {
    significant <- matrix(0, length(x))
    v <- abs((x - y) / sqrt((moex / zscore)^2 + (moey / zscore)^2))
    significant <- ifelse(v > zscore, "yes", "no")
    return(as.list(significant))
}

library(Hmisc)
calc_edge_quantile <- function(nobs, weights=rep(1, nobs), ruler, quantile) {
  out <- wtd.quantile(x=ruler, weights=weights, probs=quantile, normwt=FALSE)
  return(out)
}

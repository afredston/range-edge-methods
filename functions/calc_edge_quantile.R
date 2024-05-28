library(Hmisc)
calc_edge_quantile <- function(nobs, presabs, weights=rep(1, nobs), ruler) {
  out <- wtd.quantile(x=ruler, weights=weights, probs=quantile, normwt=FALSE)
  return(out)
}
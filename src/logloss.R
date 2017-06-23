
# Metric: log-loss

logLoss <- function(actual, prediction, eps = 1e-16){
  prediction <- plyr::colwise(pmin)(plyr::colwise(pmax)(prediction, eps), 1 - eps)
  - colMeans( actual * log(prediction) + (1 - actual) * log(1 - prediction) )
}

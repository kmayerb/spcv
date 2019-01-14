#' rmse
#'
#' calculate root mean squared error from two vectors
#'
#' @param observations a vector object of numeric values (NAs will be ignored)
#' @param predictions  a vector object of numeric values (NAs will be ignored)
#'
#' @return the root mean squared error for the two vectors
#' @export
rmse <- function(observations, predictions){
  if(! (is.numeric(observations) & is.numeric(predictions)) ){
    stop("The observations and preduction inputs must be numeric vectors.")
  }
  if(length(observations) != length(predictions)){
    stop("The observations and predictions vectors must be of equal length.")
  }
  return( sqrt( mean( (observations - predictions)^2, na.rm = T) ))
}

# test.that statements for rmse
# identical(mean( (c(5,5,5)-c(4,3,1))^2), 7)
# identical(sqrt(mean((c(1,5,7)-c(0,0,0))^2)),5)
# identical(rmse(c(1,5,7),c(0,0,0)), 5)
# identical(rmse(c(NA,NA,1), c(NA,NA,2)), 1)

#' spcv
#'
#' @param df.sp SpatialPointsDataFrame (see sp package)  
#' @param my_idp numeric bandwidth parameter
#' @param var_name variable name of numeric
#'
#' @return list cv.input SpatialPointsDataFrame, cv.pred vector, cv.error vector, cv.rmse numeric
#' @export
spcv <- function(df.sp, my_idp = 2, var_name = "conc", ...){
  # check inputs are of the correct class
  input_checks = 0
  ifelse(class(df.sp)[1] == "SpatialPointsDataFrame",
         input_checks <- input_checks + 1,
         warning("spatial_cross_validation requires df.sp input to be a SpatialPointsDataFrame\n" ) )
  ifelse(is.numeric(my_idp),
         input_checks <- input_checks + 1,
         warning("my_idp bandwidth parameter must be numeric\n" ) )
  ifelse(var_name %in% names(df.sp),
         input_checks <-  input_checks + 1,
         warning("var_name must be a column name in df.sp\n"))
  ifelse(is.numeric(df.sp[[var_name]]),
         input_checks <-  input_checks + 1,
         warning("df.sp[[var_name]] must be a numeric vector\n"))
  if (input_checks < 4){
    stop("See warning(s) and correct spatial_cross_validation input types")
  }
  
  # lapply in this context creates a list of data.frames, each missing one observation,
  # which will late be exploited for applying hold-one-out cross-validation
  cv1 <- lapply ( 1:nrow(df.sp), function(i) {
    list(cv.df = df.sp[-c(i),], holdout.df = df.sp[i,])
  })
  # calculate the hold-out error by perform interpolation using all data but one hold-out
  cv.predictions <- lapply(cv1, function(x){
    estimate = gstat::idw(as.formula(paste0(var_name, "~1")), x$cv.df, x$holdout.df, idp= my_idp, debug.level = 0, ...)
    estimate$var1.pred
  })
  
  # convert list to vector format
  cv.pred = unlist(cv.predictions)
  # calculate the difference between the interpolated estimate and the hold-out test point
  cv.error = df.sp[[var_name]] - cv.pred
  # calculate the rmse - root mean squared error (function defined externally)
  cv.rmse = rmse(cv.pred,df.sp$conc )
  
  return(list( cv.input   = df.sp,
               cv.pred    = cv.pred,
               cv.error   = cv.error,
               cv.rmse    = cv.rmse))
  
}
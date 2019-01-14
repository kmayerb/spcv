#' spcv
#'
#' @param df.sp a SpatialPointsDataFrame object See the sp package for how to create one.
#' @param my_idp a numeric object, specifying the inverse distance weighting bandwidth parameter
#' @param var_name a string object, specifying the variable name
#' for the data frame column containing numeric values to be interpolated.
#' For example, concentrations of a chemical found in spatial samples.
#'
#' @return list containing four objects: cv.input (SpatialPointsDataFrame),
#' cv.pred (vector of predictions at the hold out locations),
#' cv.error (vector of errors based on difference of hold out test points and interpolated prediction),
#' cv.rmse (numeric root mean squared error, which is vased on cv.error vector)
#'
#' @export
#' @import gstat
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
    estimate = gstat::idw(as.formula(paste0(var_name, "~1")),
                          x$cv.df,
                          x$holdout.df,
                          idp = my_idp,
                          debug.level = 0, ...)
    estimate$var1.pred
  })

  # convert list to vector format
  cv.pred = unlist(cv.predictions)
  # calculate the difference between the interpolated estimate and the hold-out test point
  cv.error = df.sp[[var_name]] - cv.pred
  # calculate the rmse - root mean squared error (function defined externally)
  cv.rmse = spcv::rmse(cv.pred,df.sp$conc )

  return(list( cv.input   = df.sp,
               cv.pred    = cv.pred,
               cv.error   = cv.error,
               cv.rmse    = cv.rmse))

}

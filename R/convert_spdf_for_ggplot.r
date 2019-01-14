#' convert_spdf_for_ggplot2
#'
#' A helper function that converts the SpatialPointDataFrame to a long-form output format
#' plottable in ggplot2. In the context of spcv it is useful for plotting the output of
#' gstat::idw(). The var_name argument defaults to "var1.pred" the column of interpolation predictions
#' produced by gstat::idw(). This variable is renamed to "z" in the output data.frame.
#'
#' @param df.sp a SpatialPointsDataFrame object (for more info on this class of object see the sp package)
#' @param var_name a string object specifying the column in the SpatialPointsDataFrame containing the variable to be
#' defined as z in the ggplot ready output data.frame
#'
#' @return df a data.frame object in long from for compatability with ggplot2; the var_name of interest is renamed z
#' @export
convert_spdf_for_ggplot2 <- function(df.sp, var_name = "var1.pred") {
  df = as.data.frame(cbind(coordinates(df.sp), z = df.sp[[var_name]]))
  return(df)
}

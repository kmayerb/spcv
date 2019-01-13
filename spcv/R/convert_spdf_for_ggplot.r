# helper function to convert the output of gstat::idw output to a format for ggplot

#' convert_spdf_for_ggplot2
#'
#' @param df.sp 
#' @param var_name 
#'
#' @return
#' @export
convert_spdf_for_ggplot2 <- function(df.sp, var_name = "var1.pred") {
  df = as.data.frame(cbind(coordinates(df.sp), z = df.sp[[var_name]]))
  return(df)
}

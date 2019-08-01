interpoldata <- function(x,overlap) {
  x <- drop(as.array(time_interpolate(as.magpie(x), interpolated_year = overlap, integrate_interpolated_years = FALSE)))
  names(x) <- overlap
  return(x)
}
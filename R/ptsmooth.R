#' Smoothed Depth by Time Plot
#'
#' Interpolates from a series of depth profiles, taken at different dates,
#' times, or locations, and generates a smoothed bivariate time/location  (x
#' axis) by depth (y axis, reversed) plot using linear interpolation. Values of
#' the variable being plotted are symbolized by color.
#'
#' @param .dt   Data frame containing data to plot. Can be NULL, if all data
#'     vectors are found in the enclosing environment.
#' @param .x    Value that defines the x (time) coordinate of the plot.  Usually
#'     a date or other time coordinate. Can be either a  vector or the name
#'     of a vector in source data frame, not an expression.
#' @param .y    Value that defines the (reversed) y coordinate of the plot.
#'     Usually the depth.  Can be either a numeric vector or the name
#'     of a numeric vector in source data frame, not an expression.
#' @param .val  Value to be symbolized via the color of the dots.  Usually a
#'     measured environmental variable that varies with water depth. Can be
#'     either a numeric vector or the name of a numeric vector in source data
#'     frame, not an expression.
#' @param .res_x  Resolution for the plot, in the x, or time dimension.
#' @param .res_y  Resolution for the plot in the y or depth dimension.
#' @param y_grow_grid  TRUE/FALSE should the grid be expanded to integer depths?
#' @param y_with_zero  TRUE/FALSE should the grid include depth = 0?
#'
#' @return  A `ggplot` object (S3 class defined by `ggplot2`)
#' @import ggplot2
#' @importFrom magrittr %>%
#' @export
#' @examples
#' data(dep_sonde)
#' ptsmooth(dep_sonde, sample_date, depth, temp,
#'          .res_x = 0.5, .res_y = 5,
#'          y_grow_grid = TRUE,
#'          y_with_zero = TRUE)
ptsmooth <- function(.dt, .x, .y, .val,
                     .res_x = 0.5, .res_y = 2,
                     y_grow_grid = TRUE,
                     y_with_zero = TRUE) {

  #todo: Figure out how to make function output work with facets
  #todo: Make it so plot does not show values to ofar outside of
  #      range of observations

  # These are ugly argument checks, since they don't provide nice error messages.
  stopifnot(is.data.frame(.dt))

  # Check type and length of singleton parameters
  stopifnot(is.numeric(.res_x))
  stopifnot(length(.res_x) == 1)
  stopifnot(is.numeric(.res_y))
  stopifnot(length(.res_y) == 1)

  #browser()
  ddate  <- rlang::ensym(.x)
  ddepth <- rlang::ensym(.y)
  vvalue <- rlang::ensym(.val)

  # Create internal dataframe
  # This may be wasteful of memory for large data sets but simplifies coding.
  df <- tibble::tibble(xx = rlang::eval_tidy(ddate, .dt),
               yy = rlang::eval_tidy(ddepth, .dt),
               zz = rlang::eval_tidy(vvalue, .dt))

  # build grid for dates
  # What is maximum / minimum date / .x?
  max_x = max(df$xx, na.rm = TRUE)
  min_x = min(df$xx, na.rm = TRUE)

  # Generate the grid
  xgrid = seq(min_x, max_x, by = .res_x)

  # build the grid for depths
  # What is maximum depth / .y?
  max_y = max(df$yy, na.rm = TRUE)
  if(y_grow_grid)
    max_y <- ceiling(max_y)

  # What is the minimum?
  if(y_with_zero)
    min_y = 0
  else{
    min_y = min(df$yy, na.rm = TRUE)
    if(y_grow_grid)
      min_y <- floor(min_y)
  }

  # Generate the grid
  ygrid = seq(min_y, max_y, by = .res_y)


  # Calculate depths below which we should replace interpolated values by NAs...
  # We can use interpol for this again....
  #browser()
  # First we determine minimum y  for each x value.
  max_y <- df %>%
    dplyr::group_by(xx) %>%
    dplyr::summarize(max_y = max(yy))
  # and then interpolate
  max_grid_y <- interpol(max_y$xx, max_y$max_y, xgrid, grow_grid = FALSE) %>%
    dplyr::rename(grid_x = ind, max_y = dep) %>%
    dplyr::select(-id)


  # We work through each date in a tidyr::nested tibble.
  # This returns a dataframe for each date.
  # note that we use the `.name` parameter to the interpol function to remember
  # the date for each data set.

  profs <- df %>%
    dplyr::group_by(xx) %>%
    tidyr::nest() %>%
    dplyr::mutate(prof = purrr::map(data, function(dat) interpol(dat$yy, dat$zz,
                                                   .name = xx,
                                                   .grid = ygrid)))
  # And we use `reduce()` and `bind_rows()` to combine them to one data frame
  profs <- purrr::reduce(profs$prof, dplyr::bind_rows) %>%
    dplyr::rename(.xx = id, .yy = ind, .zz = dep)
  rm(df)

  # Now, we work through each depth
  full_grid <- profs %>%
    dplyr::group_by(.yy) %>%
    tidyr::nest() %>%
    dplyr::mutate(full_grid = purrr::map(data, function(dat)
      interpol(dat$.xx,  dat$.zz,
               .grid = xgrid,
               .name = .yy,
               grow_grid = FALSE,
               na.rm = TRUE)))

  rr <-  purrr::reduce(full_grid$full_grid, dplyr::bind_rows)
  rr <- rr %>%
    dplyr::rename(x = ind, y = id, val = dep)

  # Finally, we need to convert extra grid cells "below" our data to NA
  # note this code assumes .val is a real.....
  #browser()
  rr <- rr %>%
    dplyr::mutate(limit = max_grid_y$max_y[match(x, max_grid_y$grid_x)])

  rr <- rr %>%
    dplyr::mutate( val = dplyr::if_else(y > limit,
                                 NA_real_, val))


  plt <- ggplot(rr, aes(x, y)) +
    geom_tile(aes(fill = val)) +
    scale_y_reverse() +
    labs(x = rlang::as_string(ddate),
         y = rlang::as_string(ddepth),
         color = rlang::as_string(vvalue))
  return(plt)
}


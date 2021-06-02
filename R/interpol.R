#' Interpolate values to a grid using linear interpolation
#'
#' These functions are both a thin wrappers around the `approx()` function from
#' Base R's `stats` package. The primary difference is that these functions
#' output a tibble instead of list with `x` and `y` components. The
#' tibble / data frame output simplifies use in some work flows.
#'
#' The functions uses simple linear interpolation to estimate values at
#' (generally unmeasured) locations between observed points.
#'
#' The parameter names `.y`, `.x`, are mnemonics, to remind you that
#' the function will estimate the dependent variable `.y` at evenly spaced
#' intervals values along the .x direction, based on the (`.x`, `.y`) pairs
#' provided.
#'
#' Ideas for this function and its use in profile graphics were inspired by
#' Dewey Dunnington's
#' fishandwhistle.net [blog](https://fishandwhistle.net/post/2019/depth-time-heatmaps/).
#'
#' @param .x         Vector of (observed) x, or independent values
#' @param .y         Vector of (observed) y, or dependent values
#' @param .grid      Vector of locations for calculated interpolated values.
#' @param .name      An identifier added to the output dataframe, often useful
#'     when this function is used in loops or with `map()` , `lapply()` or
#'     their cousins.
#' @param .res       Single numeric value representing output resolution in
#'     units compatible with the `.x` variable.
#' @param grow_grid Should the output grid be extended to whole integer values
#'     just above and just below the observed data (`interpol_res`)? Should
#'     interpolated values outside the source data be NA, or estimated as the
#'     value at the closest data extreme (`interpol()`)?
#'     Useful for aligning data with whole number dates, depths, etc, which can
#'     make interpolated graphics look a bit better.
#' @param with_zero Should the grid be expanded to include a value of zero?
#'     Useful for interpolation along depths, in the common situation where
#'     the shallowest observation is slightly below the surface, but a nice
#'     graphic should start at depth == 0.
#' @param ...       Further parameters to send to `approx()`. `na.rm = TRUE` is
#'     perhaps the most likely.
#' @return A data frame with components `id` (arbitrary identifier, often useful
#' when output from these functions are fed directly into loops or used in
#' functional programming.), `ind` (independent variable), and `dep` (dependent
#' variable).
#' @describeIn interpol  Interpolate to values of a grid provided nu the user.
#' `interpol()` does nothing but check arguments, set useful defaults and
#' convert the output of `approx()` to a tibble.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' data(dep_sonde)
#' dat <- dep_sonde[dep_sonde$month == 'Jul',]
#' grid <- seq(0, 14, 1)
#' interp <- interpol(dat$depth, dat$temp, .grid = grid)
#'
#' ggplot(dat, aes(temp, depth)) +
#'   geom_point(size = 3, col = 'red') +
#'   geom_point(mapping = aes(dep, ind), data = interp, shape = 3) +
#'   scale_y_reverse()
interpol <- function(.x, .y, .grid,
                     .name = 'ID',
                     grow_grid = TRUE,
                     ...
                     )   # end grid on integer values?
{

  # Check parameters.  Both data and time  coordinates are numeric values
  # under the hood, but they do not test as numeric with `as.numeric()`
  # Is this better handled with "duck typing"?
  stopifnot(is.numeric(.x) | inherits(.x, 'Date') | inherits(.x, 'POSIXt'))
  stopifnot( is.numeric(.y) | inherits(.y, 'Date') | inherits(.y, 'POSIXt'))
  if (missing(.grid )) {
    stop("interpol: Must provide a .grid of new positions at which to ",
         "interpolate.")
  }

  if((inherits(.x, 'Date') | inherits(.x, 'POSIXt')) & grow_grid)
    stop("interpol: Set grow_grid = FALSE with date or time coordinates.")

  # Check type and length of singleton parameters
  stopifnot(is.logical(grow_grid))
  stopifnot(length(grow_grid) == 1)

  # Check for missing values in the X (`.x`) value -- these are not allowed
  # with `approx()`.  Missing values in the Y values will be handled according
  # to the setting of `na.rm =`.
  if(any(is.na(.x))) {
    nm = names(.x)
    stop('interpol: NAs not permitted in the independent variable. ',
         'Remove NAs from ',
         dplyr::if_else(nchar(nm) > 0, nm, 'the independent variable'),
         ' and try again.' )
  }

  # We pass a custom .grid to the `xout` parameter of `approx()`
  # so passing `xout` or `n` to this function introduces ambiguity, is an error.
  extra_args = rlang::list2(...)
  if ('xout' %in% names(extra_args) | 'n' %in% names(extra_args)) {
    stop("interpol: An `xout` or `n` parameter was found. Specify location f ",
         "interpolated values with `.grid`.")
  }

  # And we have linked grow_grid to expand out to nearest whole value values
  # so we want to prevent the user from passing a `rule` parameter.  Since we
  # don't use `yleft` an `yright`, the user can still override selectively.
  if ('rule' %in% names(extra_args)) {
    stop("interpol: A `rule` parameter was found.  Specify whether to accept ",
         "interpolation outside extreme values with `grow_grid`.",
         "Use `yleft` or `yright` parameters to control directly.")
  }


  # We tell `approx()` to expand the grid by carrying forward the
  # extreme values if grow_grid is true.
  # rule = 1 replaces values beyond extremes with NA, rule = 2 carries extreme
  # values.  Different behavior for left and right margins can be specified
  # by rule = c(1,2), etc. but that will not be convenient inside other functions.
  if (isTRUE(grow_grid)) {
    the_rule = 2
  }
  else {
    the_rule = 1
  }

  # Graceful fail if all or all but one of the data passed is NA
  # `approx()` throws an error.  We catch it here instead.
  # otherwise this crashes `approx()`
  if(sum(is.na(.y)) >= length(.y) - 1) {
    vals <- tibble::tibble(ind = .grid, dep = NA, id = .name) %>%
      dplyr::relocate(id)
    #cat('\nNAs\n')
    #print(vals$ind)
    return(vals)
  }
  else {

    vals <- tibble::as_tibble(stats::approx(x = .x, y = .y, xout = .grid, rule = the_rule, ...)) %>%
      dplyr::rename(ind = x, dep = y) %>%
      dplyr::mutate(id = .name) %>%
      dplyr::relocate(id)
    return(vals)
  }
}


#' @describeIn interpol Interpolation based on resolution.
#'   `interpol_res()` is specialized for creating interpolated values at
#'   regularly spaced intervals. It includes some optional parameters that
#'   simplify its use in the context of creating smoothed depth-time plots from
#'   repeated "profile" data, as often collected in limnology and oceanography.
#'   The function is not directly used by other functions in this package, which
#'   al lrely on `interpol()` instead.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' data(dep_sonde)
#' dat <- dep_sonde[dep_sonde$month == 'Jul',]
#' interp <- interpol_res(dat$depth, dat$temp, .res = 0.5)
#'
#' ggplot(dat, aes(temp, depth)) +
#'   geom_point(size = 3, col = 'red') +
#'   geom_point(mapping = aes(dep, ind), data = interp, shape = 3) +
#'   scale_y_reverse()
interpol_res <- function(.x, .y, .res,
                         .name = 'ID',
                         grow_grid = TRUE,   # end grid on integer values?
                         with_zero = TRUE,   # Is zero the logical minimum?
                         ...)
{

  # Check parameters.  Both data and time  coordinates are numeric values
  # under the hood, but they do not test as numeric with `as.numeric()`
  # Is this better handled with "duck typing"?
  stopifnot(is.numeric(.x) | inherits(.x, 'Date') | inherits(.x, 'POSIXt'))
  stopifnot( is.numeric(.y) | inherits(.y, 'Date') | inherits(.y, 'POSIXt'))
  if (missing(.res))
    stop("interpol_res: Must provide a .res parameter specifying resolution.")

  if((inherits(.x, 'Date') | inherits(.x, 'POSIXt')) & with_zero)
    stop("interpol_res: Set with_zero = FALSE with date or time coordinates.")
  if((inherits(.x, 'Date') | inherits(.x, 'POSIXt')) & with_zero)
    stop("interpol_res: Set grow_grid = FALSE with date or time coordinates.")

  # Check type and length of singleton parameters
  stopifnot(is.logical(with_zero))
  stopifnot(length(with_zero) == 1)
  stopifnot(is.logical(grow_grid))
  stopifnot(length(grow_grid) == 1)

  # Check for missing values in the X (`.x`) value -- these are not allowed
  # with `approx()`.  Missing values in the Y values will be handled according
  # to the setting of `na.rm =`.
  if(any(is.na(.x))) {
    nm = names(.x)
    stop('interpol_res: NAs not permitted in the independent variable. ',
         'Remove NAs from ',
         dplyr::if_else(nchar(nm) > 0, nm, 'the independent variable'),
         ' and try again.' )
  }

  # We pass a custom ggrid to the `xout` parameter of `approx()`
  # so passing `xout` or `n` to this function introduces ambiguity, is an error.
  extra_args = rlang::list2(...)
  if ('xout' %in% names(extra_args) | 'n' %in% names(extra_args)) {
    stop("interpol_res: An `xout` or `n` parameter was found. Specify ",
         "resolution of interpolated values with `.res`.")
  }

  # And we have linked grow_grid to expand out to nearest whole value values
  # so we want to prevent the user from passing a `rule` parameter.  Since we
  # don't use `yleft` an `yright`, the user can still override selectively.
  if ('rule' %in% names(extra_args)) {
    stop("interpol_res: A `rule` parameter was found.  Specify whether to expand ",
         "interpolation to nearest whole number values with `grow_grid`.",
         "Use `yleft` or `yright` parameters to control directly.")
  }

  # We tell `approx()` to expand the grid by carrying forward the
  # extreme values if grow_grid is true.
  # rule = 1 replaces values beyond extremes with NA, rule = 2 carries extreme
  # values.  Different behavior for left and right margins can be specified
  # by rule = c(1,2), etc. but that will not be convenient inside other functions.
  if (isTRUE(grow_grid)) {
    the_rule = 2
  }
  else {
    the_rule = 1
  }

  # We develop a grid based on the data and the `.res`, `with_zero`
  # and `expand_grid` parameters.
  # If desired, we also round max and min depths to a whole number.
  # Rounding is complicated because `ceiling()` and `floor()` don't
  # work with date and time objects.

  # What is maximum depth / .x?
  max_x = max(.x, na.rm = TRUE)
  if(grow_grid) {
    if(max_x >= 0) {
      max_x <- trunc(max_x + 0.5)
    }
    else {
      max_x <- trunc(max_x)
    }
  }

  # What is the minimum?
  if (with_zero) {
    min_x = 0
  }
  else {
    min_x = min(.x, na.rm = TRUE)
    if(grow_grid) {
      if(min_x >= 0) {
        trunc(min_x)
      }
      else{
        min_x <- trunc(min_x - 0.5)
      }
    }
  }

    # Generate the grid
    ggrid = seq(min_x, max_x, by = .res)

    # Graceful fail if all or all but one of the data passed is NA
    # `approx()` throws an error.  We catch it here instead.
    # otherwise this crashes `approx()`
    if(sum(is.na(.y)) >= length(.y) - 1) {
      vals <- tibble::tibble(ind = ggrid, dep = NA, id = .name) %>%
        dplyr::relocate(id)
      return(vals)
    }
    else {

      vals <- tibble::as_tibble(stats::approx(x = .x, y = .y, xout = ggrid, rule = the_rule, ...)) %>%
        dplyr::rename(ind = x, dep = y) %>%
        dplyr::mutate(id = .name) %>%
        dplyr::relocate(id)
      #cat('\nRegular\n')
      #print(vals$ind)
      return(vals)
    }
  }

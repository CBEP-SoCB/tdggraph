
#' Draw depth profiles as lines
#'
#' Build a conventional "depth down" `ggplot2` line graphic for repeated depth
#' profile data.
#'
#' Depth profiles are drawn as lines, with depth on a reversed vertical axis,
#' so that zero is at the top and depth increases downward.  Values of a
#' measured variable increase along the X axis.  Multiple profiles (often dates
#' or times) are drawn as separate lines, coded by color.  The argument that
#' defines those groups should be a factor or character vector.
#'
#' The function's first parameter is a data frame, and it produces a `ggplot2`
#' plot object, so can be integrated into `ggplot2` and`tidyverse` work flows.
#'
#' @param .dt   Data frame containing data to plot. Can be NULL, if all data
#'     vectors are found in the enclosing environment.
#' @param .val  Value that defines the x coordinate of the plot.  Usually a
#'     measured quantity.  Either a numeric vector or name of a
#'     numeric vector from the source data frame (unquoted or quoted)
#' @param .y    Value that defines the (reversed) y coordinate of the plot.
#'     Usually the depth coordinate.  Can be either a numeric vector or the name
#'     of a numeric vector in source data frame (unquoted or quoted)
#' @param .grp  Factor or character vector, name of a factor or character vector
#'     in the source data frame, or an expression that evaluates (with data
#'     masking) to a character or factor vector.  Usually a date.
#' @param .sort TRUE / FALSE.  Should data be sorted by .y variable before
#'     plotting?  This is almost always what you want to generate clean
#'     traces for each profile.
#' @param ... Other arguments to pass to `geom_path()`.  Likely to include
#'     additional aesthetics.  To pass additional aesthetic mappings, use
#'     `mapping = aes()`.
#'
#' @return  A `ggplot` object (S3 class defined by `ggplot2`)
#' @import ggplot2
#' @export
#'
#' @examples
#' df <- tibble::tibble(depth = rep(1:10,3),
#'                 month = factor(rep(c(6,7,8),each = 10),
#'                                  labels = c('Jun', 'Jul', 'Aug')),
#'                 val = (depth+50)/5 - 5 + rnorm(30, 0, 0.25))
#' ptlines(df, val, depth, month) +
#'   theme_minimal()
ptlines <- function(.dt, .val, .y, .grp, .sort = TRUE, ...) {

  # Ugly argument check, since it doesn't provide nice error message.
  stopifnot(is.data.frame(.dt) | is.null(.dt))

  #todo: Add further data conformity error checks to facilitate testing
  #todo: add optional .grp grouping variable that allows data masking
  #todo: Figure out how to make function output work with facets
  #todo add code to allow specification of y axis limits -- currently not
  #     possible by modifying the plot output as usual.

  # We want to be able to accept arguments as unquoted names or quoted names.
  # `ensym()`  captures only names, not expressions.  We are testing the use
  # of `enexpr()` for the color factor to allow use of structures like
  # `factor(.date)`.  That feels like a natural way to call this function.
  dep <- rlang::ensym(.val)
  ind <- rlang::ensym(.y)
  col <- rlang::enexpr(.grp)

  # Check if the grouping variable is a factor or at least character vector
  # Non-factor variables could be handled inside the function
  # by adding a grouping variable, but it is better to be explicit.
  grp <-  rlang::eval_tidy(col, .dt)
  if (! methods::is(grp, 'factor') & ! is.character(grp))
    stop('Grouping variable must be a factor or character vector.')
  #browser()
  # We construct a local data frame so we can reorder if .sort == TRUE
  df <- tibble::tibble(ind = rlang::eval_tidy(ind, .dt),
                       dep = rlang::eval_tidy(dep, .dt),
                       grp = grp)
  if (.sort) {
    df <- df %>%
      dplyr::arrange(ind)
  }

  # Should we check that these are data names from the data frame?
  # The check is not strictly necessary, as other errors will be triggered if we
  # don't catch this here.  The main value of an explicit check
  # may be to prevent pulling in data from the calling environment
  # rather than the data.  But a check may block useage styles where
  # the user is passing data assembled outside of the dataframe for one or more
  # of the parameters.  Should we be checking lengths and types instead?

  ggplot(data = df, aes(dep, ind, color = grp)) +
    geom_path(...) +
    labs(x = rlang::as_string(ind), y = rlang::as_string(dep),
                  color = rlang::as_string(col)) +
    scale_y_reverse()
}



#' Draw repeated profile data as dots
#'
#' Build a "depth down" `ggplot` point graphic for repeated depth
#' profile data.
#'
#' Create a graphic that draws vertical profile data across multiple dates or
#' times. Depth is drawn reversed so that zero is at
#' the top and depth increases downward.  Values are drawn as dots on an x
#' (usually date or time) by y (usually depth, reversed) layout.  By default,
#' dots are symbolized by color to depict measured values.
#'
#' The function's first parameter is a data frame, and it produces a `ggplot`
#' plot object, so can be integrated into `ggplot2` and`tidyverse` work flows.
#'
#' Default colors from `ggplot2` are seldom optimal.  Users will usually want to
#' adjust colors using `scale_color_viridis_c()`, `scale_color_distiller()`, or
#' `scale_color_continuous()`.
#'
#' Passing the name of a vector in the data frame into a supplementary
#' `mapping = aes()` call does yet not work.  Any supplementary aesthetics
#' defined in `mapping = aes()` must refer to names from the enclosing
#' environment, either via an explicit reference to the data frame, or
#' by referring to a vector defined in the enclosing environment.
#'
#' As a result, this function does not yet work well with filled point symbols.
#' (R point shapes 21 through 25).  That takes several additional steps:
#'
#' 1. Add `mapping = aes(fill = ?)` to the function call, pointing explicitly
#'    to the data in the enclosing environment.
#'
#' 2. Pass a `shape` argument selecting the filled symbol shape
#'
#' 3. Usually, pass a `color` argument to define the fixed dot outline color.
#'    (`color = 'gray50'` is often a good starting point).  If you don't,
#'    `ggplot2` will vary the outline color according to the value of `.val`,
#'    which is probably not what you want.
#'
#' 4. Usually, you will want to supplement the call to this function with a
#'    cal lto `scale_fill_viridis_c()`, `scale_fill_distiller()`, or
#'    `scale_fill_continuous()` to define the fill color scale.
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
#' @param ... Further arguments to pass to `geom_point()`.  Likely to include
#'     additional aesthetic specifications.  To pass additional aesthetic
#'     mappings, use `mapping = aes()` referring to names from the enclosing
#'     environment (not a data column name).
#'
#' @return  A `ggplot` object (S3 class defined by `ggplot2`)
#' @import ggplot2
#' @export
#'
#' @examples
#' # Dummy data
#' df <- tibble::tibble(depth = rep(1:10,3),
#'                 month = factor(rep(c(6,7,8),each = 10),
#'                                  labels = c('Jun', 'Jul', 'Aug')),
#'                 val = (depth+50)/5 - 5 + rnorm(30, 0, 0.25))
#' # Normal use
#' ptdots(df, month, depth, val) +
#'   theme_minimal()
#'
#' # Filled Symbols
#' ptdots(dep_sonde, month, depth, do, size = 5,
#'       shape = 21, color = 'gray25',
#'      mapping = aes(fill = dep_sonde$do)) +
#'  theme_minimal()
#'
ptdots <- function(.dt, .x, .y, .val, ...) {

  # Ugly argument check, since it doesn't provide nice error message.
  stopifnot(is.data.frame(.dt) | is.null(.dt))

  # We want to be able to accept arguments as unquoted names or quoted names.
  # `ensym()`  captures only names, not expressions.
  ind <- rlang::ensym(.x)
  dep <- rlang::ensym(.y)
  col <- rlang::ensym(.val)

  ggplot(
    data = NULL,
    aes(
      x = rlang::eval_tidy(ind, .dt),
      y = rlang::eval_tidy(dep, .dt),
      color = rlang::eval_tidy(col, .dt)
    )
  ) +
    geom_point(...) +
    labs(x = rlang::as_string(ind),
         y = rlang::as_string(dep),
         color = rlang::as_string(col)) +
    scale_y_reverse()
}

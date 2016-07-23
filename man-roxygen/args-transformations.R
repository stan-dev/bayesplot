#' @param transformations An optional named list specifying transformations to
#'   apply to parameters. The name of each list element should be a parameter
#'   name and the content of each list element should be a function (or any item
#'   to match as a function via \code{\link{match.fun}}, e.g. a string naming a
#'   function). If a function in the list of transformations is specified by its
#'   name as a string (e.g. \code{"log"}), then it can be used to construct a
#'   new parameter label for the appropriate parameter (e.g.
#'   \code{"log(sigma)"}). If a function itself is specified (e.g. \code{log} or
#'   \code{function(x) log(x)}) then \code{"t"} is used in the new parameter
#'   label to indicate that the parameter is transformed (e.g.
#'   \code{"t(sigma)"}).

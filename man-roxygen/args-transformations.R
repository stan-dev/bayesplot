#' @param transformations Optionally, transformations to apply to parameters
#'   before plotting. If `transformations` is a function or a single string
#'   naming a function then that function will be used to transform all
#'   parameters. To apply transformations to particular parameters, the
#'   `transformations` argument can be a named list with length equal to
#'   the number of parameters to be transformed. Currently only univariate
#'   transformations of scalar parameters can be specified (multivariate
#'   transformations will be implemented in a future release). If
#'   `transformations` is a list, the name of each list element should be a
#'   parameter name and the content of each list element should be a function
#'   (or any item to match as a function via [match.fun()], e.g. a
#'   string naming a function). If a function is specified by its name as a
#'   string (e.g. `"log"`), then it can be used to construct a new
#'   parameter label for the appropriate parameter (e.g. `"log(sigma)"`).
#'   If a function itself is specified
#'   (e.g. `log` or `function(x) log(x)`)
#'   then `"t"` is used in the new parameter label to indicate that the
#'   parameter is transformed (e.g. `"t(sigma)"`).
#'
#'   Note: due to partial argument matching `transformations` can be
#'   abbreviated for convenience in interactive use (e.g., `transform`).

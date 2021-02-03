#' @param regex_pars An optional [regular expression][base::grep] to use for
#'   parameter selection. Can be specified instead of `pars` or in addition to
#'   `pars`. When using `pars` for tidy parameter selection, the `regex_pars`
#'   argument is ignored since [select helpers][tidyselect::select_helpers]
#'   perform a similar function.

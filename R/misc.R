# Convert yrep matrix into a molten data frame
#
# @param yrep A matrix, already validated using validate_yrep().
# @return A data frame with three columns:
# \itemize{
#  \item 'value': the numeric values.
#  \item 'y_id': integer indicating from which yrep column each values comes.
# \item 'rep_id': factor with S levels, where S is nrow(yrep), i.e. the number
#   of simulations included in yrep.
# }
#
melt_yrep <- function(yrep, label = TRUE) {
  out <- reshape2::melt(
    data = yrep,
    varnames = c("rep_id", "y_id")
  )
  id <- if (label) create_yrep_ids(out$rep_id) else out$rep_id
  out$rep_id <- factor(id, levels = unique(id))
  out
}

# Prepare data for use in PPCs by group
#
# @param y,yrep,group Validated y, yrep, and group objects from the user.
# @param stat Either NULL or a string naming a function.
# @value If \code{stat} is NULL, a molten data frame grouped by group and
#   variable. If \code{stat} specifies a function then a summary table created
#   by dplyr::summarise.
#
ppc_group_data <- function(y, yrep, group, stat = NULL) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Please install the dplyr package.")

  d <- data.frame(
    group = factor(group),
    y = y,
    yrep = t(yrep)
  )
  colnames(d) <- gsub(".", "_", colnames(d), fixed = TRUE)
  molten_d <- reshape2::melt(d, id.vars = "group")
  molten_d <- dplyr::group_by_(molten_d, .dots = list(~group, ~variable))
  if (is.null(stat))
    return(molten_d)

  stat <- match.fun(stat)
  dplyr::summarise_(molten_d, value = ~stat(value))
}

create_yrep_ids <- function(ids) paste('italic(y)[rep] (', ids, ")")
yrep_label <- function() expression(italic(y)^rep)
yrep_avg_label <- function() expression(paste("Average ", italic(y)^rep))
y_label <- function() expression(italic(y))
Ty_label <- function() expression(italic(T(y)))
Tyrep_label <- function() expression(italic(T)(italic(y)^rep))

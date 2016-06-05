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

# Stack y below melted yrep data
#
# @param y Validated y input.
# @param yrep Validated yrep input.
# @return A data frame with the all the columns as the one returned by
#   melt_yrep(), plus a column "is_y" indicating whether the values pertain
#   to y (or yrep).
#
melt_and_stack <- function(y, yrep) {
  molten_yrep <- melt_yrep(yrep)
  yobs_lab <- "italic(y)"
  levels(molten_yrep$rep_id) <- c(levels(molten_yrep$rep_id), yobs_lab)
  ydat <- data.frame(
    rep_id = yobs_lab,
    y_id = seq_along(y),
    value = y
  )
  within(data = rbind(molten_yrep, ydat), {
    rep_id <- relevel(rep_id, ref = yobs_lab)
    is_y <- rep_id == yobs_lab
  })
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


# labels ----------------------------------------------------------------
create_yrep_ids <- function(ids) paste('italic(y)[rep] (', ids, ")")
yrep_label <- function() expression(italic(y)^rep)
yrep_avg_label <- function() expression(paste("Average ", italic(y)^rep))
y_label <- function() expression(italic(y))
Ty_label <- function() expression(italic(T(y)))
Tyrep_label <- function() expression(italic(T)(italic(y)^rep))


# ggplot helpers ----------------------------------------------------------
facet_wrap_parsed <- function(...) {
  facet_wrap(..., labeller = label_parsed)
}
dont_expand_y_axis <- function() {
  scale_y_continuous(expand = c(0,0))
}
dont_expand_x_axis <- function() {
  scale_x_continuous(expand = c(0,0))
}
dont_expand_axes <- function() {
  coord_cartesian(expand = FALSE)
}

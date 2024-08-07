% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppc-test-statistics.R
\name{PPC-test-statistics}
\alias{PPC-test-statistics}
\alias{PPC-statistics}
\alias{ppc_stat}
\alias{ppc_stat_grouped}
\alias{ppc_stat_freqpoly}
\alias{ppc_stat_freqpoly_grouped}
\alias{ppc_stat_2d}
\alias{ppc_stat_data}
\title{PPC test statistics}
\usage{
ppc_stat(
  y,
  yrep,
  stat = "mean",
  ...,
  binwidth = NULL,
  bins = NULL,
  breaks = NULL,
  freq = TRUE
)

ppc_stat_grouped(
  y,
  yrep,
  group,
  stat = "mean",
  ...,
  facet_args = list(),
  binwidth = NULL,
  bins = NULL,
  breaks = NULL,
  freq = TRUE
)

ppc_stat_freqpoly(
  y,
  yrep,
  stat = "mean",
  ...,
  facet_args = list(),
  binwidth = NULL,
  bins = NULL,
  freq = TRUE
)

ppc_stat_freqpoly_grouped(
  y,
  yrep,
  group,
  stat = "mean",
  ...,
  facet_args = list(),
  binwidth = NULL,
  bins = NULL,
  freq = TRUE
)

ppc_stat_2d(y, yrep, stat = c("mean", "sd"), ..., size = 2.5, alpha = 0.7)

ppc_stat_data(y, yrep, group = NULL, stat)
}
\arguments{
\item{y}{A vector of observations. See \strong{Details}.}

\item{yrep}{An \code{S} by \code{N} matrix of draws from the posterior (or prior)
predictive distribution. The number of rows, \code{S}, is the size of the
posterior (or prior) sample used to generate \code{yrep}. The number of columns,
\code{N} is the number of predicted observations (\code{length(y)}). The columns of
\code{yrep} should be in the same order as the data points in \code{y} for the plots
to make sense. See the \strong{Details} and \strong{Plot Descriptions} sections for
additional advice specific to particular plots.}

\item{stat}{A single function or a string naming a function, except for the
2D plot which requires a vector of exactly two names or functions. In all
cases the function(s) should take a vector input and return a scalar
statistic. If specified as a string (or strings) then the legend will
display the function name(s). If specified as a function (or functions)
then generic naming is used in the legend.}

\item{...}{Currently unused.}

\item{binwidth}{Passed to \code{\link[ggplot2:geom_histogram]{ggplot2::geom_histogram()}} to override
the default binwidth.}

\item{bins}{Passed to \code{\link[ggplot2:geom_histogram]{ggplot2::geom_histogram()}} to override
the default binwidth.}

\item{breaks}{Passed to \code{\link[ggplot2:geom_histogram]{ggplot2::geom_histogram()}} as an
alternative to \code{binwidth}.}

\item{freq}{For histograms, \code{freq=TRUE} (the default) puts count on the
y-axis. Setting \code{freq=FALSE} puts density on the y-axis. (For many
plots the y-axis text is off by default. To view the count or density
labels on the y-axis see the \code{\link[=yaxis_text]{yaxis_text()}} convenience
function.)}

\item{group}{A grouping variable of the same length as \code{y}.
Will be coerced to \link[base:factor]{factor} if not already a factor.
Each value in \code{group} is interpreted as the group level pertaining
to the corresponding observation.}

\item{facet_args}{A named list of arguments (other than \code{facets}) passed
to \code{\link[ggplot2:facet_wrap]{ggplot2::facet_wrap()}} or \code{\link[ggplot2:facet_grid]{ggplot2::facet_grid()}}
to control faceting. Note: if \code{scales} is not included in \code{facet_args}
then \strong{bayesplot} may use \code{scales="free"} as the default (depending
on the plot) instead of the \strong{ggplot2} default of \code{scales="fixed"}.}

\item{size, alpha}{For the 2D plot only, arguments passed to
\code{\link[ggplot2:geom_point]{ggplot2::geom_point()}} to control the appearance of scatterplot points.}
}
\value{
The plotting functions return a ggplot object that can be further
customized using the \strong{ggplot2} package. The functions with suffix
\verb{_data()} return the data that would have been drawn by the plotting
function.
}
\description{
The distribution of a (test) statistic \code{T(yrep)}, or a pair of
(test) statistics, over the simulated datasets in \code{yrep}, compared to the
observed value \code{T(y)} computed from the data \code{y}. See the
\strong{Plot Descriptions} and \strong{Details} sections, below, as
well as Gabry et al. (2019).

\strong{NOTE:} Although the default test statistic
is the mean, this is unlikely to detect anything interesting in most cases.
In general we recommend using some other test statistic as discussed in
Section 5 of Gabry et al. (2019).
}
\details{
For Binomial data, the plots may be more useful if
the input contains the "success" \emph{proportions} (not discrete
"success" or "failure" counts).
}
\section{Plot Descriptions}{

\describe{
\item{\code{ppc_stat()}, \code{ppc_stat_freqpoly()}}{
A histogram or frequency polygon of the distribution of a statistic
computed by applying \code{stat} to each dataset (row) in \code{yrep}. The value of
the statistic in the observed data, \code{stat(y)}, is overlaid as a vertical
line. More details and example usage of \code{ppc_stat()} can be found in Gabry
et al. (2019).
}
\item{\code{ppc_stat_grouped()},\code{ppc_stat_freqpoly_grouped()}}{
The same as \code{ppc_stat()} and \code{ppc_stat_freqpoly()}, but a separate plot is
generated for each level of a grouping variable. More details and example
usage of \code{ppc_stat_grouped()} can be found in Gabry et al. (2019).
}
\item{\code{ppc_stat_2d()}}{
A scatterplot showing the joint distribution of two statistics
computed over the datasets (rows) in \code{yrep}. The value of the
statistics in the observed data is overlaid as large point.
}
}
}

\examples{
y <- example_y_data()
yrep <- example_yrep_draws()
ppc_stat(y, yrep, stat = "median")
ppc_stat(y, yrep, stat = "sd") + legend_none()

# use your own function for the 'stat' argument
color_scheme_set("brightblue")
q25 <- function(y) quantile(y, 0.25)
ppc_stat(y, yrep, stat = "q25") # legend includes function name

# can define the function in the 'stat' argument instead of
# using its name but then the legend doesn't include the function name
ppc_stat(y, yrep, stat = function(y) quantile(y, 0.25))

# plots by group
color_scheme_set("teal")
group <- example_group_data()
ppc_stat_grouped(y, yrep, group, stat = "median")
ppc_stat_grouped(y, yrep, group, stat = "mad") + yaxis_text()

# force y-axes to have same scales, allow x axis to vary
ppc_stat_grouped(y, yrep, group, facet_args = list(scales = "free_x")) + yaxis_text()

# the freqpoly plots use frequency polygons instead of histograms
ppc_stat_freqpoly(y, yrep, stat = "median")
ppc_stat_freqpoly_grouped(y, yrep, group, stat = "median", facet_args = list(nrow = 2))

# ppc_stat_2d allows 2 statistics and makes a scatterplot
bayesplot_theme_set(ggplot2::theme_linedraw())
color_scheme_set("viridisE")
ppc_stat_2d(y, yrep, stat = c("mean", "sd"))

bayesplot_theme_set(ggplot2::theme_grey())
color_scheme_set("brewer-Paired")
ppc_stat_2d(y, yrep, stat = c("median", "mad"))

# reset aesthetics
color_scheme_set()
bayesplot_theme_set()

}
\references{
Gabry, J. , Simpson, D. , Vehtari, A. , Betancourt, M. and
Gelman, A. (2019), Visualization in Bayesian workflow.
\emph{J. R. Stat. Soc. A}, 182: 389-402. doi:10.1111/rssa.12378.
(\href{https://rss.onlinelibrary.wiley.com/doi/full/10.1111/rssa.12378}{journal version},
\href{https://arxiv.org/abs/1709.01449}{arXiv preprint},
\href{https://github.com/jgabry/bayes-vis-paper}{code on GitHub})

Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari,
A., and Rubin, D. B. (2013). \emph{Bayesian Data Analysis.} Chapman & Hall/CRC
Press, London, third edition. (Ch. 6)
}
\seealso{
Other PPCs: 
\code{\link{PPC-censoring}},
\code{\link{PPC-discrete}},
\code{\link{PPC-distributions}},
\code{\link{PPC-errors}},
\code{\link{PPC-intervals}},
\code{\link{PPC-loo}},
\code{\link{PPC-overview}},
\code{\link{PPC-scatterplots}}
}
\concept{PPCs}

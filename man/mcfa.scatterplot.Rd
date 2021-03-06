% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mcfa_scatterplot.R
\name{mcfa.scatterplot}
\alias{mcfa.scatterplot}
\title{Plot Interval Scatterplot}
\usage{
mcfa.scatterplot(x, y, sym.data, pos.var)
}
\arguments{
\item{x}{symbolic table with only one column.}

\item{y}{symbolic table with only one column.}

\item{sym.data}{original symbolic table.}

\item{pos.var}{column number of the variables to be plotted.}
}
\description{
Plot Interval Scatterplot
}
\examples{
data("ex_mcfa1")
sym.table <- classic.to.sym(ex_mcfa1,
  concept = suspect,
  hair = sym.set(hair),
  eyes = sym.set(eyes),
  region = sym.set(region)
)

res <- sym.mcfa(sym.table, c(1, 2))
mcfa.scatterplot(res[, 2], res[, 3], sym.data = sym.table, pos.var = c(1, 2))
}

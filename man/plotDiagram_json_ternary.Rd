% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Figaro_parser.R
\name{plotDiagram_json_ternary}
\alias{plotDiagram_json_ternary}
\title{Inner function, for binary plots}
\usage{
plotDiagram_json_ternary(
  graphDef,
  wrdata,
  lbl,
  new,
  template_options,
  color_options,
  transform_options
)
}
\arguments{
\item{graphDef}{The template, loaded into a list from json}

\item{wrdata}{The plotting dataset}

\item{lbl}{The labels (GCDkit style)}

\item{new}{Open in a new window?}

\item{template_options}{See plotDiagram_json}

\item{color_options}{See plotDiagram_json}

\item{transform_options}{See plotDiagram_json}
}
\description{
Inner function, for binary plots
}
\details{
Internal function, that does the actual plotting
in the case of a ternary plot
}

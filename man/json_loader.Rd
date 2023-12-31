% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generalParsing_fct.R
\name{json_loader}
\alias{json_loader}
\title{This function loads a json template and returns a list}
\usage{
json_loader(json, path = NULL)
}
\arguments{
\item{json}{Name of the template file}

\item{path}{Path to json file}
}
\description{
This function loads a json template and returns a list
}
\details{
If no path is supplied, the function will look in the json_template
directory of the package folder, and its subdirectories. It will return
the first match, starting by the top-level folder and exploring
sub-folders aphabetically.
}

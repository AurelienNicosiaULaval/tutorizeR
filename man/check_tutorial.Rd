% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_to_tutorial.R
\name{check_tutorial}
\alias{check_tutorial}
\title{Render a generated tutorial to ensure it compiles}
\usage{
check_tutorial(file)
}
\arguments{
\item{file}{Path to the tutorial `.Rmd` you want to test.}
}
\value{
Invisibly, the path to the rendered HTML **or** the error object.
}
\description{
The file is rendered in a temporary directory; warnings and errors are
captured.  If rendering fails, a short message is printed.
}

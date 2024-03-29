% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Metafunctions.R
\name{statsALL_NumAttrs}
\alias{statsALL_NumAttrs}
\title{8-12. Eval all the following metrics on Numeric Attributes
min, max, mean, sd, var, hist}
\usage{
statsALL_NumAttrs(ds, tgt, numCores = 1)
}
\arguments{
\item{ds}{A data set}

\item{tgt}{The index of the target variable}

\item{numCores}{number of cores for parallel computing}
}
\value{
A vector with the minimum, maximum, mean, standard deviation and variance of numerical attributes' MIC score,
and a vector with the cardinality of values after binning (10 bins)
}
\description{
8-12. Eval all the following metrics on Numeric Attributes
min, max, mean, sd, var, hist
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparison_utils.R
\name{complete_for_comparison}
\alias{complete_for_comparison}
\title{Get the ID of patient with sufficiently complete records}
\usage{
complete_for_comparison(df)
}
\arguments{
\item{df}{The data frame to search for complete records}
}
\value{
a vector of ids
}
\description{
Required features:
\itemize{
\item Gender
\item Age
\item Smoking status
\item BMI
\item Race
\item GERD
\item Medication
}
}

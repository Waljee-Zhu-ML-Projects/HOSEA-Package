% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sysdata.R
\name{feature_groups}
\alias{feature_groups}
\title{Feature groups and categories}
\usage{
feature_groups(model = "xgb_srs_any.meta")
}
\arguments{
\item{model}{The model path (relative to \verb{<HOSEA package path>/extdata}) from which to get feature names}
}
\value{
A data frame with three columns (name, group, category)
}
\description{
This is completely hard-coded to fit with the current model, so be careful when using this.
}

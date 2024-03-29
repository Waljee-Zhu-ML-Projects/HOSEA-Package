% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_data.R
\name{create_charlson_data}
\alias{create_charlson_data}
\title{Process ICD files to construct patient records}
\usage{
create_charlson_data(
  dir = "./unzipped_data/",
  files = c("alldxs.sas7bdat"),
  which = charlson_vars,
  master = NULL,
  verbose = T
)
}
\arguments{
\item{dir}{Directory}

\item{files}{List of files to search through}

\item{which}{List of comorbidities to consider}

\item{master}{Data frame containing (id, start, end, ...)}

\item{verbose}{Whether to print things out or not}
}
\value{
A list of (df, master) where:
\itemize{
\item df has column (id, <all comorbidities in \code{which}>)
\item master is the input master
}
}
\description{
If the patient has at least one ICD 9/10 hit for a particular comorbidity
between \code{start} and \code{end} in \code{master}, then the corresponding variables in the
output will contain a 1.
}
\details{
If the patient has no ICD codes at all, then Nas will be implicitly added.
Note that \code{visitin4yrs} is later used to impute 0 for some of these cases.
}

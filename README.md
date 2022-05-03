# HOSEA Package

## Changelog

0.0.0.9003:

- Added prediction for the three models (any, eac, egjac)
- Models are included in the package and automatically loaded

## Installation

### Using `devtools`

- Create a PAT on Gitlab with sufficient access (I think "read_repository" should be sufficient for use.)
- Install using `devtools`:

```r
devtools::install_git(url="https://gitlab.umich.edu/waljee-zhu-ml-projects/hosea-package.git")
library(HOSEA)
```

### Alternative

I had trouble installing it directly through `devtools` on the UM cluster becuase of access rights so I did it manually:

- Clone the repository `https://gitlab.umich.edu/waljee-zhu-ml-projects/hosea-package.git`
- Move to the new directory
- Install the package `R CMD INSTALL .`

## Data processing

The first function to use is `load_process_data` which will load raw data and proces it into
the appropriate format for prediction

```r
out = load_process_data(
  dir="unzipped_data/",
  files_sample=c("sample.sas7bdat"),
  files_charlson=c("alldxs.sas7bdat"),
  files_labs=c("alllabs.sas7bdat"),
  files_meds=c("allmeds.sas7bdat"),
  start=-4, end=0, 
  verbose=3
)
```

A few remarks:

- `dir` is the path to the directory containing all relevant files
- `files_X` contains list of files for the specific type of information
- `start` and `end` specifies the window during which to compute summaries for labs, medications and comorbidities
- `verbose` specifies the amount of logging pushed to the console (0-3)
- `out` is a list with entries `df` which is the dataframe for prediction and `master` which contains some metadata

## Risk prediction

The second important function of this package is `predict.HOSEA` which can perform multiple imputation and average the predicted risk
across the imputations. The predicted risk for all three models (any, EAC only and EGJAC only) is returned averaged over the multiple imputations.

```r
pred = predict.HOSEA(out$df)
head(pred)
```

If only the combined model is desired, specify

```r
pred = predict.HOSEA(out$df, xgb_fits=list(ANY=XGB_ANY))
head(pred)
```

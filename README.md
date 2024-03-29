# HOSEA Package: K-ECAN Prediction Tool & Analyses Utilities

## Changelog

1.0.0.0000

- Removed MICE and other unnecessary files and functions
- Minor clean-up here and there

0.0.0.9010

- Added Z transform to MICE
- Fixed MICE binary imputation
- New MICE imputer and new models
- Now, the package contains all the necessary for MICE or SRS imputation + prediction

0.0.0.9009

- A few new analysis tools

0.0.0.9008:

- New imputation and a few helper functions implemented
- Updated models with MICE imputation
- Some major changes in API (see updated example below)
- Quality control should not work anymore, to be updated soon ...

0.0.0.9007:

- Updated models with updated EGJAC cases

0.0.0.9006:

- Updated models to new ICD10 GERD coding

0.0.0.9005:

- Updated models to full 10M controls and new GERD coding

0.0.0.9004:

- Added quality control

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

I had trouble installing it directly through `devtools` on the UM cluster because of access rights so I did it manually:

- Clone the repository `https://gitlab.umich.edu/waljee-zhu-ml-projects/hosea-package.git`
- Move to the new directory
- Install the package `R CMD INSTALL .`

### Alternative 2

- Download the repository `https://gitlab.umich.edu/waljee-zhu-ml-projects/hosea-package/-/archive/main/hosea-package-main.zip`
- Unzip
- Use `devtools::install()` to install, passing the appropriate path as argument

## Data processing

The first function to use is `load_process_data` which will load raw data and process it into
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
- `start` and `end` specifies the window during which to compute summaries for labs, medications and comorbidities. The default, `-4` to `0` means we take data up to 4 years before the index date.
- `verbose` specifies the amount of logging pushed to the console (0-3)
- `out` is a list with entries `df` which is the dataframe for prediction and `master` which contains some metadata

## Risk prediction

The second important function of this package is `predict.HOSEA` which can perform multiple imputation and average the predicted risk
across the imputations. The predicted risk for all three models (any, EAC only and EGJAC only) is returned averaged over the multiple imputations.

```r
pred = predict.HOSEA(out$df)
head(pred)
```

If multiple calls are to be performed, speed-ups can be obtained by pre-loading the models:

```r
models=load_models()
imputer=load_imputer()
pred = predict.HOSEA(out$df, models=models, imputer=imputer)
```

This also allows to select just one model at the time, e.g.,

```r
models = load_models(files_meta=c(ANY="xgb_mice_any.meta"), files_models=c(ANY="xgb_mice_any.model"))
pred = predict.HOSEA(out$df, models=models)
```

You can also use multiprocessing to speed-up the imputation (I am not convinced this is faster
because of communication time):

```r
n_cores = 10
cluster = parallel::makeCluster(n_cores)
pred = predict.HOSEA(out$df, cluster=cluster)
parallel::stopCluster(cluster)
```

## Misc notes

Gitlab:

- There is a recent bug in the interaction between GreatLakes and Gitlab. To fix it, run the following:

```
export no_proxy=localhost,127.0.0.1,.localdomain
export NO_PROXY=$no_proxy
```

- Apparently, ITS is looking to move from Gitlab to GitHub Enterprise soon.

## Notes on updating a model

 The current workflow is to simply replace the 
appropriate files in `inst/extdata/`. Make sure fine names match with `load_imputer` and `load_models`.

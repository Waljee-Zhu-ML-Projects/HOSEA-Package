# HOSEA Package

## Installation

### Using `devtools`

- Create a PAT on Gitlab with sufficient access (I think "read_repository" should be sufficient for use.)
- Install using `devtools`:

```r
devtools::install_git(url="https://gitlab.umich.edu/waljee-zhu-ml-projects/hosea-package.git")
library(HOSEA)
```

### Alternative

I had trouble installing it directly through `devtools` becuase of access rights so I did it manually:

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

To obtain risk prediction, we first need an XGBoost model from which we extract the imputation data and the model itself:

```r
results = readRDS(model_path)
xgb_fit = results$xgb_fit
quantiles = results$quantiles
```

The second important function of this package is `predict` which can perform multiple imputation and average the predicted risk
across the imputations

```r
pred = predict(xgb_fit, out$df, quantiles, n_imputations=10)
```

There may be discrepancies between the column names of `out$df` and those in `xgb_fit$feature_anmes` due to updates
to the data processing function and using an model fitted using older data. I attempted to write some rules to catch them all,
but it's simpler to just do it manually. This should be fixed in the future when the model will be refitted using all data
under the new data processing function. Things to look for: lower/upper cases, age, gerd.

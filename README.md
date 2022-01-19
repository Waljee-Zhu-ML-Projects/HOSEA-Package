# HOSEA Package

Installation:

- Create a PAT on Gitlab with sufficient access (I put "api, read_user, read_api, read_repository, write_repository")
- Install using `devtools`:

```r
devtools::install_git(url="https://gitlab.umich.edu/waljee-zhu-ml-projects/hosea-package.git")
library(HOSEA)
```

Import and process data 

```r
df = load_process_data(dir="dirname/", start=-5, end=-1)
```

which requires `dirname` to contain the following `.sas7bdat` files

- `alldxs???` containing ICD codes (may have multiple files as long as they all start by `alldxs`)
- `allmeds` containing relevant prescriptions
- `colonoscopy` containing  colonoscopy events
- `labs_???` containing lab results for `???` being all of `a1c`, `bmp`, `cbc`, `crp`, `fobt`, `lft`, `lipid`
- `sample` containing demographic information as well as diagnosis at index

and where `start`, `end` define the window of data used for prediction where 0 correspond to index (so `start` and `end` should be negative with default `start=-5` and `end=-1`.)

The resulting data frame has the following structure

- `ID`: unique subject identifier
- `CaseControl`: the diagnosis at index
- 11 Demographic variables (age, sex, race:4, smoking status:2, BMI, weight, agent orange exposure)
- 16 Charlson and GERD indicators (chf, ctd, dem, diab_c, gerd, hiv, mld, msld, para, rd, cd, copd, diab_nc, mi, pud, pvd)
- 2x2=4 event variables (types: colonoscopy, fobt; summaries: n, maxdiff)
- 2x5=10 medication variables (types: H2R, PPI; summaries: total, mean, max, maxdiff, tv)
- 33x6=198 lab data (types: A1c bun calc chlor co2 creat gluc k na baso eos hct hgb lymph mch mchc mcv mono mpv neut platelet rbc rdw wbc CRP alkphos alt ast totprot chol hdl ldl trig; summaries: mean, min, max, mindiff, maxdiff, tv)

for a total of 241 columns and 239 predictors.
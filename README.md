# HOSEA Package

Installation:

- Create a PAT on Gitlab with sufficient access (I put "api, read_user, read_api, read_repository, write_repository")
- Install using `devtools`:

```r
devtools::install_gitlab(
  repo="waljee-zhu-ml-projects/hosea-package", 
  host="gitlab.umich.edu",
  auth_token="<your-personal-access-token>"
)
```

Usage

```r
library(HOSEA)
```
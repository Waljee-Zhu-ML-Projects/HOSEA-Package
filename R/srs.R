#' Simple Random Sampling Imputation
#'
#' @param df The data frame to impute
#' @param vars_to_impute The list of variables to perform imputation on
#' @param n_quantiles The number of quantiles used to reduce memory
#' @param n_samples Number of imputes requested
#' @param return_models Whether to return the quantiles or not 
#'
#' @return If `return_model==T`, then this returns a list (imputed_df, models). See [HOSEA.srs.fit] for details.
#' Otherwise, only `imputed_df` is returned which is either a single data frame (`n_samples==1`)
#' or a list of data frames (`n_samples>1`).
#' @export HOSEA.srs
#' @seealso [HOSEA.srs.fit] [impute.HOSEA.srs]
HOSEA.srs = function(
  df,
  vars_to_impute,
  n_quantiles=10000,
  n_samples=1,
  return_models=T
){
  obj = HOSEA.srs.fit(df, vars_to_impute, n_quantiles)
  imputed = impute.HOSEA.srs(obj, df, n_samples)
  if(return_models) return(list(imputed_df = imputed, models=obj)) else return(imputed)
}

#' Fitting Simple Random Sampling Imputation
#'
#' @param df The data frame to impute
#' @param vars_to_impute The list of variables to perform imputation on
#' @param n_quantiles The number of quantiles used to reduce memory
#'
#' @return A list
#' @export HOSEA.srs.fit
#' @importFrom stats quantile
#' @seealso [HOSEA.srs] [impute.HOSEA.srs]
HOSEA.srs.fit = function(
  df, 
  vars_to_impute,
  n_quantiles=10000
){
  probs = seq(0, n_quantiles) / n_quantiles
  models = lapply(vars_to_impute, function(v){
    stats::quantile(df %>% pull(v), probs=probs, na.rm=T, type=1)
  })
  names(models) = vars_to_impute
  out = list(models=models)
  class(out) = "HOSEA.srs"
  return(out)
}

#' Impute using Simple Random Sampling from a fitted SRS imputer
#'
#' @param obj The SRS imputer
#' @param df The data frame to impute
#' @param n_samples the number of imputes to output
#' @param ... For compatibility with [impute]
#'
#' @return a df or a list of dfs
#' @export impute.HOSEA.srs
#' @exportS3Method impute HOSEA.srs
#' @seealso [HOSEA.srs] [HOSEA.srs.fit] [impute]
impute.HOSEA.srs = function(
  obj, 
  df, 
  n_samples=1, 
  ...
){
  df %<>% arrange(id)
  # check that we have enough columns
  df_cols = colnames(df)
  obj_cols = names(obj$model)
  stopifnot(sapply(obj_cols, function(x) x %in% df_cols) %>% all())  
  
  # cycle
  imputed_dfs = lapply(seq(n_samples), function(i) {
    wdf = df
    for(v in obj_cols){
      tf = wdf %>% pull(!!v) %>% is.na()
      to_predict = wdf %>% dplyr::filter(tf) %>% pull(id)
      n = length(to_predict)
      if(n>0){
        new_values = sample(obj$models[[v]], size=n, replace=T)
        wdf[wdf %>% pull(id) %in% to_predict, v] = new_values
      }
    }
    return(wdf)
  })
  
  # return
  return(imputed_dfs %>% bind_rows())
}
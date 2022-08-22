#' Title
#'
#' @param df 
#' @param vars_to_impute 
#' @param n_quantiles 
#' @param n_samples 
#' @param return_models 
#'
#' @return either the imputed df or a list of the df and the models
#' @export HOSEA.srs
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

#' Title
#'
#' @param df 
#' @param vars_to_impute 
#' @param n_quantiles 
#'
#' @return the list of models
#' @export HOSEA.srs.fit
HOSEA.srs.fit = function(
  df, 
  vars_to_impute,
  n_quantiles=10000
){
  probs = seq(0, n_quantiles) / n_quantiles
  models = lapply(vars_to_impute, function(v){
    quantile(df %>% pull(v), probs=probs, na.rm=T, type=1)
  })
  names(models) = vars_to_impute
  out = list(models=models)
  class(out) = "HOSEA.srs"
  return(out)
}

#' Title
#'
#' @param obj 
#' @param df 
#' @param n_samples 
#' @param ... 
#'
#' @return a df or a list of dfs
#' @export impute.HOSEA.srs
#' @exportS3Method impute HOSEA.srs
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
  if(n_samples==1) return(imputed_dfs[[1]]) else return(imputed_dfs)
}
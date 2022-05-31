#' Obtain prediction from XGBoost model(s)
#'
#' @param df data frame to predict
#' @param n_imputations number of imputation
#' @param xgb_fits a list of list(xgb_fit, quantiles, ...)
#'
#' @return predicted risk averaged over the n imputations. A data frame with columns (id, ANY, EAC, EGJAC).
#' @export predict.HOSEA
#' @import dplyr magrittr xgboost purrr
predict.HOSEA = function(df, 
                         n_imputations=10,
                         xgb_meta=list(ANY=XGB_ANY, EAC=XGB_EAC, EGJAC=XGB_EGJAC),
                         xgb_models=list(ANY="", EAC="", EGJAC="")
                         ){
  pred_dfs = lapply(names(xgb_fits), function(name){
    xgb_fit = xgb_fits[[name]]$xgb_fit
    quantiles = xgb_fits[[name]]$quantiles
    # imputations
    imputed = lapply(seq(n_imputations), function(i){
      set.seed(i)
      return(impute_srs(df, quantiles))
    })
    imputed %<>% bind_rows()
    # prediction
    xgb_df = xgboost::xgb.DMatrix(as.matrix(df %>% select(xgb_fit$feature_names)))
    proba = predict(xgb_fit, newdata=xgb_df)
    pred_df = data.frame(id=df$id, proba=proba)
    pred_df %>% group_by(id) %>% summarise(risk=mean(proba))
    colnames(pred_df) = c("id", name)
    return(pred_df)
  })
  out = pred_dfs %>% purrr::reduce(full_join, by="id")
  return(out)
}


impute_srs = function(df, quantiles){
  n_quantiles = nrow(quantiles)
  for(col in colnames(quantiles)){
    ids = which(is.na(df[[col]]))
    n = length(ids)
    if(n>0){
      is = sample.int(n=n_quantiles, size=n, replace=T)
      values = quantiles[is, col]
      df[ids, col] = values
    }
  }
  return(df)
}
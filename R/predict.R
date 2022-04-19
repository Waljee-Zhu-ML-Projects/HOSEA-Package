#' Obtain prediction from XGBoost model
#'
#' @param df data frame to predict
#' @param xgb_fit the xgboost object
#' @param quantiles quantiles for imputation
#' @param n_imputations number of imputation
#'
#' @return predicted risk averaged over the n imputations. A data frame with columns (id, risk)
#' @export
predict = function(xgb_fit, df, quantiles, n_imputations=10){
  # imputations
  imputed = lapply(seq(n_imputations), function(i){
    set.seed(i)
    return(impute_srs(df, quantiles))
  })
  imputed %<>% bind_rows()
  # prediction
  xgb_df = xgb.DMatrix(as.matrix(df %>% select(xgb_fit$feature_names)))
  proba = predict(xgb_fit, newdata=xgb_df)
  pred_df = data.frame(id=df$ID, proba=proba)
  pred_df %>% group_by(id) %>% summarise(risk=mean(proba))
  return(pred_df)
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
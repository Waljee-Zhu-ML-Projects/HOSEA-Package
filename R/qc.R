#' Performs some simple checks on processed data 
#'
#' @param df the dataframe to check
#' @param model which model to use for quantiles and missing proportions
#'
#' @return nothing, but will raise warnings when it finds potential issues.
#' @export
#' @import dplyr magrittr
quality_control = function(df, model=XGB_ANY){
  xgb_fit=model$xgb_fit
  quantiles=model$quantiles %>% select(xgb_fit$feature_names)
  missing_prop=model$missing_prop
  
  cat("Checking columns ...\n")
  model_cols = xgb_fit$feature_names
  df_cols = colnames(df)
  if(!all(model_cols %in% df_cols)){
    which = !(model_cols %in% df_cols)
    warning(paste0(
      "Could not find the following features in the dataframe:\n\n",
      paste(model_cols[which], collapse=", "),
      "\n"
    ))
  }
  cat("...done!\n")
  
  cat("Checking for outliers ...\n")
  n_quantiles = nrow(quantiles)
  qs = seq(n_quantiles)/(n_quantiles + 1)
  ql = which.max(qs > 0.005)
  qu = which.min(qs < 0.995)
  prop_out = sapply(xgb_fit$feature_names, function(varname){
    x = df %>% pull(varname)
    l = (quantiles %>% pull(varname))[ql]
    u = (quantiles %>% pull(varname))[qu]
    mean((x>u) | (x<l), na.rm=T)
  }) # we expect this to be ~1%
  which = prop_out > 0.02
  if(sum(which)>0){
    warning(paste0(
      "The following features appears to have excess outliers compared to the training set:\n\n",
      paste(model_cols[which], collapse=", "),
      "\n(Identified as having >2% of values outside the [0.5%, 99.5%] quantiles. 1% is expected.)"
    ))
  }
  cat("...done!\n")
  
  cat("Checking missing value proportions ...\n")
  df_missing_prop = df %>% mutate(across(missing_prop%>%names(), is.na)) %>% colMeans()
  n = nrow(df)
  which = missing_prop > 0.
  p0 = missing_prop[which]
  p1= df_missing_prop[which]
  z = abs(p1-p0) / sqrt(p0*(1-p0)/n)
  pval = pnorm(z, lower.tail=F)*2*length(which)
  which = names(pval[pval<0.05])
  if(length(which)>0){
    warning(paste0(
      "The following features have different missing rates compared to the training set:\n\n",
      paste(which, collapse=", ")
    ))
  }
  cat("...done!\n")
}
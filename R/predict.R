



#' Title
#'
#' @param file 
#'
#' @return
#' @export
#'
#' @examples
load_imputer = function(
  file="mice.imputer"
){
  filename = paste0(system.file('extdata', package = 'HOSEA'), "/", file)
  imputer = readRDS(filename)
  return(imputer)
}


#' Title
#'
#' @param files_meta 
#' @param files_models 
#'
#' @return
#' @export
#'
#' @examples
load_models = function(
  files_meta=list(ANY="xgb_mice_any.meta", EAC="xgb_mice_eac.meta", EGJAC="xgb_mice_egjac.meta"),
  files_models=list(ANY="xgb_mice_any.model", EAC="xgb_mice_eac.model", EGJAC="xgb_mice_egjac.model")
){
  models = intersect(names(files_meta), names(files_models)) # only models with both will be used
  out = lapply(models, function(model){
    filename_model = paste0(system.file('extdata', package = 'HOSEA'), "/", files_models[[model]])
    filename_meta  = paste0(system.file('extdata', package = 'HOSEA'), "/", files_meta[[model]])
    xgb_fit = xgboost::xgb.load(filename_model)
    xgb_meta = readRDS(filename_meta)
    xgb_fit$feature_names = xgb_meta[[model]]$xgb_fit$feature_names
    return(xgb_fit)
  })
  names(out) = models
  return(out)
}


#' Title
#'
#' @param models a list of models to obtain predictions
#' @param imputer an imputation object from HOSEA, NULL to perform no imputation
#' @param newdata data frame to predict, which may contain missing data to be imputed
#' @param n_samples 
#'
#' @return predicted risk averaged over the n imputations. A data frame with columns (id, ANY, EAC, EGJAC).
#' @export predict.HOSEA
#' @import dplyr magrittr xgboost purrr
predict.HOSEA = function(
  newdata,
  models=load_models(), 
  imputer=load_imputer(), 
  n_samples=10
){
  if(!is.null(imputer)){
    newdata = impute(imputer, newdata, n_samples=n_samples)
    newdata %<>% bind_rows()
  }
  
  pred_dfs = lapply(models, function(name){
    # prediction
    xgb_df = xgboost::xgb.DMatrix(as.matrix(newdata %>% select(models[[name]]$feature_names)))
    proba = predict(models[[name]], newdata=xgb_df)
    pred_df = data.frame(id=newdata$id, proba=proba)
    pred_df %>% group_by(id) %>% summarise(risk=mean(proba))
    colnames(pred_df) = c("id", name)
    return(pred_df)
  })
  
  out = pred_dfs %>% purrr::reduce(full_join, by="id")
  return(out)
}


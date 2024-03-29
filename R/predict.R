#' Title
#'
#' @param file 
#'
#' @return
#' @export
#'
#' @examples
load_imputer = function(
  file="srs.imputer"
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
#' @importFrom xgboost xgb.load
load_models = function(
  files_meta=list(ANY="xgb_srs_any.meta", EAC="xgb_srs_eac.meta", GCA="xgb_srs_egjac.meta"),
  files_models=list(ANY="xgb_srs_any.model", EAC="xgb_srs_eac.model", GCA="xgb_srs_egjac.model")
){
  models = intersect(names(files_meta), names(files_models)) # only models with both will be used
  out = lapply(models, function(name){
    filename_model = paste0(system.file('extdata', package = 'HOSEA'), "/", files_models[[name]])
    filename_meta  = paste0(system.file('extdata', package = 'HOSEA'), "/", files_meta[[name]])
    xgb_fit = xgboost::xgb.load(filename_model)
    xgb_meta = readRDS(filename_meta)
    xgb_fit$feature_names = xgb_meta$xgb_fit$feature_names
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
#' @param n_samples number of imputes to aggregate over
#' @param cluster a cluster object for parallel processing
#'
#' @return predicted risk averaged over the n imputations. A data frame with columns (id, ANY, EAC, EGJAC).
#' @evalNamespace "export(predict.HOSEA)"
#' @import dplyr 
#' @importFrom purrr reduce
#' @importFrom magrittr %<>%
#' @importFrom xgboost xgb.load xgb.DMatrix
#' @importFrom stats predict
predict.HOSEA = function(
  newdata,
  models=load_models(), 
  imputer=load_imputer(), 
  n_samples=10,
  cluster=NULL
){
  if(!is.null(imputer)){
    newdata = impute(imputer, newdata, n_samples=n_samples, cluster=cluster)
  }
  
  pred_dfs = lapply(names(models), function(name){
    # prediction
    xgb_df = xgboost::xgb.DMatrix(as.matrix(newdata %>% select(models[[name]]$feature_names)))
    proba = predict(models[[name]], newdata=xgb_df)
    pred_df = data.frame(id=newdata$id, proba=proba)
    pred_df %<>% group_by(id) %>% summarise(risk=mean(proba))
    colnames(pred_df) = c("id", name)
    return(pred_df)
  })
  
  out = pred_dfs %>% purrr::reduce(full_join, by="id")
  return(out)
}


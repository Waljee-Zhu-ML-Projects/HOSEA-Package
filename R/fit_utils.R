#' Title
#'
#' @param df 
#' @param master 
#' @param outcome 
#'
#' @return
#' @export
#' @import dplyr magrittr
patch_outcome = function(df, master, outcome="ANY"){
  outcomes = master %>% select(id, casecontrol, cancertype)
  outcomes %<>% mutate(
    ANY=casecontrol,
    EAC=as.integer(cancertype=="EAC"),
    EGJAC=as.integer(cancertype=="EGJAC")
  )
  outcomes_ = outcomes%>%select(id, !!outcome)
  df %<>%
    left_join(outcomes_, by="id") %>%
    select(-casecontrol) %>% 
    rename(casecontrol=!!outcome)
}


xgboost_options = function(
  max_depth = 5,
  subsample = 0.1,
  eta = 2,
  objective = 'binary:logistic',
  eval_metric = 'auc',
  nthread=-1,
  ...
){
  return(list(
    max_depth = max_depth,
    subsample = subsample,
    eta = eta,
    objective = objective,
    eval_metric = eval_metric,
    nthread=nthread,
    ...
  ))
}


stratified_split = function(
  df,
  proportions=c(train=2, valid=1, test=1)
){
  cases = df %>% filter(casecontrol==1)
  controls = df %>% filter(casecontrol==0)
  df_names = names(proportions)
  cases_split = sample(df_names, nrow(cases), prob=proportions, replace=T)
  controls_split = sample(df_names, nrow(controls), prob=proportions, replace=T)
  cases_df = lapply(df_names, function(nm) cases %>% filter(cases_split==nm))
  names(cases_df) = df_names
  controls_df = lapply(df_names, function(nm) controls %>% filter(controls_split==nm))
  names(controls_df) = df_names
  out = lapply(df_names, function(nm) bind_rows(cases_df[[nm]], controls_df[[nm]]))
  names(out) = df_names
  return(out)
}


balanced_resample = function(df){
  cases = df %>% filter(casecontrol==1)
  controls = df %>% filter(casecontrol==0)
  n_controls = nrow(controls)
  cases %<>% sample_n(n_controls, replace=T)
  return(bind_rows(cases, controls))
}


#' Title
#'
#' @param df 
#' @param vars 
#'
#' @return
#' @export
#' @import xgboost dplyr
to_xgb = function(df, vars=NULL){
  if(is.null(vars)){
    out = xgboost::xgb.DMatrix(as.matrix(df%>%select(-c(id,casecontrol))),
                      label=df$casecontrol)
  }else{
    out = xgboost::xgb.DMatrix(as.matrix(df%>%select(vars)),
                               label=df$casecontrol)
  }
  return(out)
}


prepare_watchlist = function(df_list, vars=NULL){
  out = lapply(df_list, function(df) to_xgb(df, vars))
  # make sure "valid" is the last one
  if("valid" %in% names(out)){
    tmp = out[["valid"]]
    out[["valid"]] = NULL
    out = append(out, list(valid=tmp))
  }
  return(out)
}


xgb_fit = function(
  watchlist, 
  xgb_options,
  nrounds=2000,
  verbose=1,
  print_every_n=10,
  early_stopping_rounds=100,
  ...
){
  out = xgboost::xgb.train(
    params=xgb_options,
    data=watchlist$train,
    nrounds=nrounds,
    watchlist=watchlist,
    verbose=verbose,
    print_every_n=print_every_n,
    early_stopping_rounds=early_stopping_rounds,
    ...
  )
  return(out)
}


get_feature_names = function(df, to_drop=NULL){
  cols = colnames(df)
  cols = cols[!(cols %in% c("id", "casecontrol"))]
  for(str in to_drop) cols = cols[!stringr::str_starts(cols, str)]
  return(cols)
}


summary_df_list = function(df_list){
  tab = sapply(df_list, function(df) df$casecontrol %>% table())
  rownames(tab) = c("Case", "Control")
  return(tab)
}


summarize_missingness = function(df){
  missing_prop = df %>% mutate(across(everything(), is.na)) %>% colMeans()
  missing_rate = df %>% mutate(across(everything(), is.na)) %>% rowSums() %>% table()
}

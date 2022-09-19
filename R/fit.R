#' Title
#'
#' @param df 
#' @param master 
#' @param outcome 
#' @param imputation 
#' @param to_drop 
#' @param nrounds 
#' @param print_every_n 
#' @param early_stopping_rounds 
#' @param ... 
#'
#' @return
#' @export
#' @import magrittr dplyr
HOSEA.fit = function(
  df, 
  master,
  outcome="ANY",
  imputation="srs",
  to_drop=c("chol", "rbc", "hgb"),
  nrounds=2000,
  print_every_n=10,
  early_stopping_rounds=100,
  ...
){
  df %<>% patch_outcome(master, outcome)
  cat("[HOSEA] Selected outcome ", outcome, "\n")
  features = get_feature_names(df, to_drop)
  n0all = (df$casecontrol==0)%>%sum
  n1all = (df$casecontrol==1)%>%sum
  df_list = stratified_split(df, c(train=2, test=1, valid=1))
  cat("[HOSEA] Split into train/valid/test\n")
  summary_df_list(df_list) %>% print()
  cat("[HOSEA] Building imputation models ...\n")
  imputer = switch(
    imputation,
    "mice"=HOSEA.mice.fit(df_list$train, features, n_rounds=20),
    "srs"=HOSEA.srs.fit(df_list$train, features, n_quantiles=10000)
  )
  missingness = summarize_missingness(df_list$train)
  cat("[HOSEA] Imputation models built\n")
  df_list$train %<>% balanced_resample()
  ids = lapply(df_list, function(df) df$id %>% unique())
  cat("[HOSEA] Resample cases in training set\n")
  summary_df_list(df_list) %>% print()
  df_list %<>% lapply(function(df) impute(imputer, df, 1))
  watchlist = prepare_watchlist(df_list, features)
  params_xgb = do.call(xgboost_options, list(...))
  params_xgb$scale_pos_weight = n1all/n0all
  cat("[HOSEA] Starting XGBoost fitting\n")
  cat("[HOSEA]", timestamp::timestamp(prefix="", suffix="", quiet=T), "\n")
  xgb_fit = xgboost::xgb.train(
    params=params_xgb,
    data=watchlist$train,
    watchlist=watchlist,
    nrounds=nrounds,
    verbose=1, 
    print_every_n=print_every_n,
    early_stopping_rounds=early_stopping_rounds
  )
  cat("[HOSEA]", timestamp::timestamp(prefix="", suffix="", quiet=T), "\n")
  out = list(
    xgb_fit=xgb_fit,
    imputer=imputer, 
    ids=ids,
    missingness=missingness
  )
  class(out) = "HOSEA.fit"
  return(out)
}
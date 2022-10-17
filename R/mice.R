#' Title
#'
#' @param df 
#' @param vars_to_impute 
#' @param n_rounds 
#' @param n_samples 
#' @param return_models 
#'
#' @return either the imputed df or a list of the df and the models
#' @export HOSEA.mice
HOSEA.mice = function(
  df, 
  vars_to_impute, 
  n_rounds=25,
  n_samples=1,
  return_models=T
){
  obj = HOSEA.mice.fit(df, vars_to_impute, n_rounds)
  imputed = impute.HOSEA.mice(obj, df, n_samples, n_rounds)
  if(return_models) return(list(imputed_df = imputed, models=obj)) else return(imputed)
}



#' Title
#'
#' @param df 
#' @param vars_to_impute 
#' @param n_rounds 
#' @param cluster
#'
#' @return the list of models
#' @export HOSEA.mice.fit
#' @import dplyr progress stats
HOSEA.mice.fit = function(
  df, 
  vars_to_impute, 
  n_rounds=10,
  cluster=NULL
){
  if (!requireNamespace("arm", quietly = TRUE)) {
    stop(
      "Package \"arm\" must be installed to use this function.",
      call. = FALSE
    )
  }
  models = lapply(vars_to_impute, function(x) list(coefficients=NULL))
  names(models) = vars_to_impute
  
  # find out which are categorical and continuous
  df %<>% arrange(id)
  binary = df %>% select(vars_to_impute) %>% 
    apply(2, function(x) length(unique(x, na.rm=T))) <= 3 
  bin_vars_to_impute = vars_to_impute[binary]
  
  # impute to start
  na_mask = is.na(df %>% select(vars_to_impute)) 
  wdf = HOSEA.srs(df, vars_to_impute, return_models=F)
  wdf %<>% arrange(id)

  # iterate through rounds
  prev_coef_mat = matrix(0, nrow=length(vars_to_impute), ncol=length(vars_to_impute))
  for(r in seq(n_rounds)){
    cat("[MICE] Iteration", r, "\n")
    pb = progress::progress_bar$new(
      format="       [:bar] :percent in :elapsed (:current/:total, eta: :eta)",
      total=length(vars_to_impute),
      width=60, clear=F
    )
    if(r>1) prev_fitted = lapply(models, function(m) m$fitted)
    for(v in vars_to_impute){
      bin = v %in% bin_vars_to_impute
      rest_of_vars = vars_to_impute[vars_to_impute!=v]
      fm = paste(v, "~", paste(rest_of_vars, collapse= " + "), collapse = " ")
      to_train = wdf %>% dplyr::filter(!na_mask[, v]) %>% pull(id)
      to_predict = wdf %>% dplyr::filter(na_mask[, v]) %>% pull(id)
      models[[v]] = mgcv::bam(
        formula=formula(fm),
        data=wdf %>% dplyr::filter(id %in% to_train),
        family=ifelse(bin, binomial(link="logit"), gaussian())[[1]],
        cluster=cluster, gc.level=1, samfrac=1,
        coef=models[[v]]$coefficients
      )
      # drop things to save memory
      models[[v]]$residuals = NULL
      models[[v]]$fitted.values = NULL
      models[[v]]$linear.predictors = NULL
      models[[v]]$effects = NULL
      models[[v]]$weights = NULL
      models[[v]]$data = NULL
      models[[v]]$y = NULL
      models[[v]]$hat = NULL
      models[[v]]$model = NULL
      
      if(length(to_predict)>0){
        if(bin){
          pred = list()
          pred$fit = predict(models[[v]], newdata=wdf %>% dplyr::filter(id %in% to_predict), type="response", cluster=cluster)
          pred$logit = predict(models[[v]], newdata=wdf %>% dplyr::filter(id %in% to_predict), type="link", cluster=cluster)
          new_values = (stats::runif(length(pred$fit)) < pred$fit) %>% as.integer()
          models[[v]]$fitted = pred$logit
        }else{
          pred = predict(models[[v]], newdata=wdf %>% dplyr::filter(id %in% to_predict), type="response", se.fit=!bin, cluster=cluster)
          predsd = sqrt(pred$se.fit^2 + models[[v]]$sig2)
          new_values = stats::rnorm(n=length(pred$fit)) * predsd + pred$fit
          models[[v]]$fitted = pred$fit
        }
        names(new_values) = to_predict
        wdf[wdf %>% pull(id) %in% to_predict, v] = new_values
      }else{
        models[[v]]$fitted = NA
      }
      pb$tick()
    }
    if(r>1){
      rel_change = sapply(names(models), function(m){
        bin = m %in% bin_vars_to_impute
        f0 = prev_fitted[[m]]
        f1 = models[[m]]$fitted
        sq_change = (f1-f0)^2
        return(c(ss=sum(sq_change), var=stats::var(f0), n=length(f0)))
      }) %>% t() %>% data.frame()
      tss = (rel_change$ss/rel_change$var) %>% sum(na.rm=T)
      n = sum(rel_change$n, na.rm=T)
      cat("       Lin. pred. rel. change: ", tss / n, "\n")
      
      coef_mat = sapply(models, function(m) m$coefficients)
      L1_coef_diff = 2 * (abs(coef_mat - prev_coef_mat) / (abs(prev_coef_mat) + abs(coef_mat))) %>% mean()
      prev_coef_mat = coef_mat
      cat("       Coefficients rel. change (L1): ", L1_coef_diff, "\n")
    }
  }
  for(m in models) m$fitted = NULL
  out = list(models=models, n_rounds=r) # TODO: same the initial imputer as well
  class(out) = "HOSEA.mice"
  return(out)
}

#' Title
#'
#' @param obj 
#' @param df 
#' @param n_samples 
#' @param n_rounds 
#' @param ... 
#'
#' @return a df or a list of dfs
#' @export impute.HOSEA.mice
#' @import dplyr progress stats
#' @exportS3Method impute HOSEA.mice
impute.HOSEA.mice = function(
  obj, 
  df,
  n_samples=1,
  n_rounds=obj$n_rounds,
  cluster=NULL,
  ...
){
  df %<>% arrange(id)
  # check that we have enough columns
  df_cols = colnames(df)
  obj_cols = names(obj$model)
  stopifnot(sapply(obj_cols, function(x) x %in% df_cols) %>% all())
  
  # augment df
  dfs = lapply(seq(n_samples), function(i) df)
  dfs %<>% bind_rows()
  
  # impute to start
  na_mask = is.na(dfs %>% select(obj_cols)) 
  wdf = HOSEA.srs(dfs, obj_cols, return_models=F)
  wdf %<>% arrange(id)
  
  # iterate through rounds
  for(r in seq(n_rounds)){
    cat("[MICE] Sampling round", r, "\n")
    pb = progress::progress_bar$new(
      format="       [:bar] :percent in :elapsed (:current/:total, eta: :eta)",
      total=length(obj_cols),
      width=60, clear=F
    )
    if(r>1) prev_fitted = lapply(obj$models, function(m) m$fitted)
    for(v in obj_cols){
      bin = obj$models[[v]]$family$link == "logit"
      to_predict = wdf %>% dplyr::filter(na_mask[, v]) %>% pull(id)
      
      if(length(to_predict)>0){
        if(bin){
          pred = list()
          pred$fit = mgcv::predict.bam(obj$models[[v]], newdata=wdf %>% dplyr::filter(id %in% to_predict), type="response", cluster=cluster)
          pred$logit = mgcv::predict.bam(obj$models[[v]], newdata=wdf %>% dplyr::filter(id %in% to_predict), type="link", cluster=cluster)
          new_values = (runif(length(pred$fit)) < pred$fit) %>% as.integer()
          obj$models[[v]]$fitted = pred$logit
        }else{
          pred = mgcv::predict.bam(obj$models[[v]], newdata=wdf %>% dplyr::filter(id %in% to_predict), type="response", se.fit=!bin, cluster=cluster)
          predsd = sqrt(pred$se.fit^2 + obj$models[[v]]$sig2)
          new_values = rnorm(n=length(pred$fit)) * predsd + pred$fit
          obj$models[[v]]$fitted = pred$fit
        }
        names(new_values) = to_predict
        wdf[wdf %>% pull(id) %in% to_predict, v] = new_values
      }else{
        obj$models[[v]]$fitted = NA
      }
      pb$tick()
    }
    if(r>1){
      rel_change = sapply(names(obj$models), function(m){
        bin = obj$models[[v]]$family$link == "logit"
        f0 = prev_fitted[[m]]
        f1 = obj$models[[m]]$fitted
        sq_change = (f1-f0)^2
        return(c(ss=sum(sq_change), var=var(f0), n=length(f0)))
      }) %>% t() %>% data.frame()
      tss = (rel_change$ss/rel_change$var) %>% sum(na.rm=T)
      n = sum(rel_change$n, na.rm=T)
      cat("       Lin. pred. rel. change: ", tss / n, "\n")
    }
  }
  
  
  # return
  return(wdf)
}

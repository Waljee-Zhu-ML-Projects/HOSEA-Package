#' Title
#'
#' @param proba df with columns (id, y, ...)
#'
#' @return roc curves taking all columns in ... as risk scores
#' @export
#' @import dplyr magrittr
#'
#' @examples
roc = function(proba){
  models = proba %>% select(-id, -y) %>% colnames()
  
  out = lapply(models, function(name){
    fg = proba %>% filter(y==1) %>% pull(name)
    bg = proba %>% filter(y==0) %>% pull(name)
    
    roc = PRROC::roc.curve(fg, bg ,curve=TRUE)
    thin = ifelse(nrow(roc$curve) > 10000, ceiling(nrow(roc$curve)/1000), 1)
    roc$curve = roc$curve[seq(1, nrow(roc$curve), by=thin), ]
    roc$curve %<>% data.frame()
    colnames(roc$curve) = c("fpr", "recall", "tr")
    
    proc = pROC::roc(controls=bg, cases=fg)
    roc$ci = pROC::ci(proc, of="auc")
    roc$display.ci = paste0(
      format(round(roc$au, 3), nsmall=3), " [",
      format(round(roc$ci[1], 3), nsmall=3), ",",
      format(round(roc$ci[3], 3), nsmall=3), "]"
    )
    roc$display = round(roc$au, 3)
    return(roc)
  })
  names(out) = models
  return(out)
}

#' Title
#'
#' @param df 
#' @param ratio_male_to_female 
#' @param ratio_cases_male_to_female 
#'
#' @return
#' @export
representative_sample = function(df, ratio_male_to_female=1.0, ratio_cases_male_to_female=8.33){
  # this function assumes we have too few female cases and controls
  
  ids_cases_female = df%>%filter(gender==0,casecontrol==1)%>%pull(id)
  ids_cases_male = df%>%filter(gender==1,casecontrol==1)%>%pull(id)
  ids_controls_female = df%>%filter(gender==0,casecontrol==0)%>%pull(id)
  ids_controls_male = df%>%filter(gender==1,casecontrol==0)%>%pull(id)
  
  n_cases_female = length(ids_cases_female)
  n_controls_female = length(ids_controls_female)
  n_cases_male = round(ratio_cases_male_to_female*n_cases_female)
  n_controls_male = (n_controls_female + n_cases_female) * ratio_male_to_female  - n_cases_male
  
  ids_cases_male = sample(ids_cases_male, n_cases_male, replace=F)
  ids_controls_male = sample(ids_controls_male, n_controls_male, replace=F)
  
  ids = c(
    ids_cases_female,
    ids_cases_male,
    ids_controls_female,
    ids_controls_male
  )
  
  return(df %>% filter(id %in% ids))
}


#' Title
#'
#' @param proba 
#' @param y 
#' @param nbins 
#'
#' @return
#' @export
calibration_curve = function(proba, y, nbins=50){
  bins = c(0., quantile(proba, seq(0., 1., length.out=nbins+1))+1e-6)
  L = bins[-length(bins)]; U = bins[-1]
  which_bin = cut(proba, bins)
  df = data.frame(proba=proba, bin=which_bin, y=y)
  mid = df %>% group_by(bin) %>% summarise(mid=mean(proba))
  bindf = mid %>% bind_cols(data.frame(L=L, U=U))
  out = df %>% group_by(bin) %>% summarize(
    N=n(),
    N_cases=sum(y),
    prop_cases=mean(y)
  ) %>% left_join(bindf, by="bin")
  return(out)
}


#' Title
#'
#' @param pred 
#' @param y 
#' @param threshold 
#'
#' @return
#' @export
classification_metrics = function(pred, y, threshold=NULL){
  if(is.null(threshold)) threshold = c(
    seq(0, 200, 5),
    seq(200, 500, 20), 
    seq(500, 1000, 50)
  ) / 100000
  threshold = c(threshold)
  threshold = unique(threshold)
  threshold = sort(threshold)
  predmat = outer(pred, threshold, function(x, t) x>t)
  ymat = outer(y, threshold, function(x, t) x)
  out = data.frame(
    threshold=threshold*100000,
    N=colSums(!is.na(ymat)),
    p=colSums(predmat),
    tp=colSums(predmat*ymat),
    tn=colSums((1-predmat)*(1-ymat)),
    fp=colSums(predmat*(1-ymat)),
    fn=colSums(ymat*(1-predmat))
  ) %>% mutate(
    ppv=tp/p,
    tpr=tp/(tp+fn),
    det_prev=p/N
  )
  return(out)
}


#' Title
#'
#' @param pred 
#' @param obs 
#' @param n 
#' @param n_cases 
#'
#' @return
#' @export
hosmer_lemeshow = function(pred, obs, n, n_cases){
  nbins = length(pred)
  o1 = n_cases
  o0 = n-n_cases
  e1 = pred*n
  e0 = n-e1
  H = sum(((o1-e1)^2/e1 + (o0-e0)^2/e0))
  dof = nbins - 1
  pval = pchisq(H, dof, lower.tail=F)
  return(paste0("H=", round(H, 2), ", df=", dof, ", p=", round(pval, 3)))
}



#' Title
#'
#' @param master 
#' @param staging 
#'
#' @return
#' @export
patch_staging = function(
  master, 
  staging=paste0(system.file('extdata', package = 'HOSEA'), "/staging.csv")
){
  staging_df = read.csv(staging) %>% tibble::tibble()
  colnames(staging_df) %<>% tolower()
  master %<>% left_join(staging_df, 
                    by=c("stagegroupclinical", "clinicalt", "clinicaln", "clinicalm"))
  master %<>% mutate(
    nccn_stage_2017=ifelse(
      (casecontrol==1) & (nccn_stage_2017==""), 
      "missing", 
      nccn_stage_2017
    ))
  return(master)
}


#' Title
#'
#' @param df 
#' @param raw_df 
#' @param master 
#' @param missing_which 
#' @param outcome 
#' @param representative 
#' @param seed 
#'
#' @return
#' @export
prepare_test_set = function(
  df,
  raw_df,
  master,
  missing_which,
  outcome,
  representative,
  seed
){
  if(missing_which == "complete"){
    ids = complete_for_comparison(raw_df)
  }
  if(missing_which == "all"){
    ids = df %>% pull(id)
  }
  if(missing_which == "incomplete"){
    complete_ids = complete_for_comparison(raw_df)
    ids = df %>% filter(!(id %in% complete_ids)) %>% pull(id)
  }
  # working df
  imputed_wdf = df %>% filter(id %in% ids)
  imputed_wdf %<>% patch_outcome(master, outcome=outcome)
  # representative sample
  if(representative){
    set.seed(seed)
    imputed_wdf %<>% representative_sample()
  }
  return(imputed_wdf)
}
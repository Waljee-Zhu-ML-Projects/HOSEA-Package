feature_summary = function(df, mask=NULL, groupby=NULL){
  out = list()
  wdf = df
  for(n in colnames(mask)) wdf[[n]][mask[[n]]] = NA
  # demographic variables (binary)
  demo_vars_bin = c(
    'gender','asian','black','hawaiianpacific',
    'indianalaskan','agentorange',
    "smoke_current", "smoke_former"
  )
  if(!is.null(groupby)){
    grouped = wdf %>% 
      group_by_at(vars(groupby)) %>% 
      select(any_of(demo_vars_bin)) %>% 
      summarise_all(list(.prop=nanmean, .prop_na=prop_na))
  }else{grouped = NULL}
  ungrouped = wdf %>% 
    select(any_of(demo_vars_bin)) %>% 
    summarise_all(list(.prop=nanmean, .prop_na=prop_na))
  if(!is.null(groupby)) ungrouped[[groupby]] = NA
  demo_bin_summaries = bind_rows(grouped, ungrouped)
  demo_bin_summaries %<>% pivot_longer(-c(groupby), names_to="summary", values_to="value")
  demo_bin_summaries %<>% separate(summary, sep="_\\.", into=c("variable", "summary"), extra="merge")
  demo_bin_summaries %<>% add_row(
    variable="race", 
    summary="prop_inconsistent", 
    value=((wdf %>% 
      select(any_of(c("black", "asian", "hawaiianpacific", "indianalaskan"))) %>%
      apply(1, sum, na.rm=T)) > 1) %>% mean()
  )
}

prop_na = function(x) mean(is.na(x))
nanmean = function(x) mean(x, na.rm=T)
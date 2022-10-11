

feature_distribution = function(df){
  out = df %>% select(-id, -casecontrol) %>% 
    summarise(across(
      everything(),
      list(
        type=~ifelse(n_distinct(., na.rm=T)<3, "binary", "continuous"),
        n=~length(.),
        nc=~sum(!is.na(.)),
        propna=~mean(is.na(.)),
        mean=~mean(., na.rm=T),
        sd=~sd(., na.rm=T),
        min=~min(., na.rm=T),
        q25=~quantile(., 0.25, na.rm=T),
        median=~median(., na.rm=T),
        q75=~quantile(., 0.75, na.rm=T),
        max=~max(., na.rm=T)
      )
    )) %>% tidyr::pivot_longer(
      everything(),
      names_to=c("variable", ".value"),
      names_pattern="(.+)_(.+)"
    )
  fgroups = feature_groups()
  out %<>% left_join(fgroups, by=c("variable"="name")) %>% select(
    category, group, everything()
  )
  return(out)
}


feature_coherence = function(df){
  # empty df to store result
  out = tibble::tibble(
    category=character(),
    group=character(),
    variable=character(),
    n=numeric(),
    nc=numeric(),
    prop_coherent=numeric()
  )
  # race: check at most one (white=0)
  n_races = df %>% mutate(
    n_races=(asian + black + hawaiianpacific + indianalaskan)
  ) %>% pull(n_races)
  out %<>% tibble::add_row(
    category="Demographic",
    group="race",
    variable="At most 1",
    n=length(n_races),
    nc=sum(!is.na(n_races)),
    prop_coherent=mean(n_races<=1, na.rm=T)
  )
  # smoke: check at most one (cannot be former and current)
  smoking = df %>% mutate(
    smoking=(smoke_current+smoke_former)
  ) %>% pull(smoking)
  out %<>% tibble::add_row(
    category="Demographic",
    group="smoke",
    variable="At most 1",
    n=length(smoking),
    nc=sum(!is.na(smoking)),
    prop_coherent=mean(smoking<=1, na.rm=T)
  )
  # medication: mean<=max, maxdiff<=max
  for(med in med_vars){
    tf1 = df[[paste0(med, "_mean")]] <= df[[paste0(med, "_max")]]
    tf2 = df[[paste0(med, "_maxdiff")]] <= df[[paste0(med, "_max")]]
    out %<>% tibble::add_row(
      category="Medication",
      group=med,
      variable="mean <= max",
      n=length(tf1),
      nc=sum(!is.na(tf1)),
      prop_coherent=mean(tf1, na.rm=T)
    )
    out %<>% tibble::add_row(
      category="Medication",
      group=med,
      variable="maxdiff <= max",
      n=length(tf2),
      nc=sum(!is.na(tf2)),
      prop_coherent=mean(tf2, na.rm=T)
    )
  }
  # lab results: min<=mean, mean<=max, mindiff<=maxdiff 
  for(lab in lab_vars){
    tf1 = df[[paste0(lab, "_min")]] <= df[[paste0(lab, "_mean")]]
    tf2 = df[[paste0(lab, "_mean")]] <= df[[paste0(lab, "_max")]]
    tf3 = df[[paste0(lab, "_mindiff")]] <= df[[paste0(lab, "_maxdiff")]]
    out %<>% tibble::add_row(
      category="Lab",
      group=lab,
      variable="min <= mean",
      n=length(tf1),
      nc=sum(!is.na(tf1)),
      prop_coherent=mean(tf1, na.rm=T)
    )
    out %<>% tibble::add_row(
      category="Lab",
      group=lab,
      variable="mean <= max",
      n=length(tf2),
      nc=sum(!is.na(tf2)),
      prop_coherent=mean(tf2, na.rm=T)
    )
    out %<>% tibble::add_row(
      category="Lab",
      group=lab,
      variable="mindiff <= maxdiff",
      n=length(tf3),
      nc=sum(!is.na(tf3)),
      prop_coherent=mean(tf3, na.rm=T)
    )
  }
  out$type = "coherence"
  return(out %>% select(category, group, variable, type, everything()))
}


mask_observed = function(df, idf){
  mask = df %>% select(-id, -casecontrol) %>% is.na()
  X = idf %>% select(-id, -casecontrol) %>% as.matrix()
  X[!mask] = NA
  ddf = bind_cols(idf %>% select(id, casecontrol), X)
  return(ddf)
}


# df_list=list(raw=df, imputed=idf, only_imputed=ddf)
compare_dfs = function(df_list, ref=names(df_list)[1]){
  fdist = lapply(df_list, feature_distribution)
  fdist %<>% bind_rows(.id="df")
  coherence = lapply(df_list, feature_coherence)
  coherence %<>% bind_rows(.id="df")
  # conduct tests
  fdist$pvalue = NA
  vars = unique(fdist %>% filter("type" != "coherence") %>% pull(variable))
  df_others = setdiff(names(df_list), c(ref))
  for(var in vars){
    for(dfname in df_others){
      vtype = fdist %>% filter(variable==var, df==ref) %>% pull("type")
      if(vtype == "continuous"){ # t-test
        m0 = fdist %>% filter(variable==var, df==ref) %>% pull(mean)
        m1 = fdist %>% filter(variable==var, df==dfname) %>% pull(mean)
        sd0 = fdist %>% filter(variable==var, df==ref) %>% pull(sd)
        sd1 = fdist %>% filter(variable==var, df==dfname) %>% pull(sd)
        n0 = fdist %>% filter(variable==var, df==ref) %>% pull(nc)
        n1 = fdist %>% filter(variable==var, df==dfname) %>% pull(nc)
        sp = sqrt(((n0-1)*sd0^2 + (n1-1)*sd1^2)/(n0+n1-2))
        tstat = abs(m0-m1) / (sp * sqrt(1/n0 + 1/n1))
        pval = 2 * pt(tstat, n0+n1-2, lower.tail=F)
        fdist$pvalue[(fdist$variable==var) & (fdist$df==dfname)] = pval
      }
      if(vtype=="binary"){
        p0 = fdist %>% filter(variable==var, df==ref) %>% pull(mean)
        p1 = fdist %>% filter(variable==var, df==dfname) %>% pull(mean)
        n0 = fdist %>% filter(variable==var, df==ref) %>% pull(nc)
        n1 = fdist %>% filter(variable==var, df==dfname) %>% pull(nc)
        p = (p0*n0 + p1*n1) / (n0+n1)
        se = sqrt(p * (1-p) * (1/n0+1/n1))
        zstat = abs(p0-p1) / se
        pval = 2*pnorm(zstat, lower.tail=F)
        fdist$pvalue[(fdist$variable==var) & (fdist$df==dfname)] = pval
      }
    }
  }
  return(list(fdist=fdist, coherence=coherence))
}
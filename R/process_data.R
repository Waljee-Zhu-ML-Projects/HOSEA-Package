#' Processing raw data to final format
#'
#' @param dir directory containing all files
#' @param start starting year relative to index (default: -4)
#' @param end ending year relative to index (default: -0)
#' @param files_sample list of files containing demographic data
#' @param files_charlson list of files containing icd codes
#' @param files_labs list of files containing lab results
#' @param files_meds list of files containing medication
#' @param verbose how much to print: 0=none, 1=some, 2=most, 3=all (default)
#' @param icd which icd codes to use (default: "any")
#' @param icd10startdate index of beginning of icd10 coding
#' @param icd9enddate index of end of icd9 coding
#'
#' @return a data frame (df) containing all required for fitting and/or prediction and a data frame (master) contaiing some metadata
#' @export
#' @import dplyr magrittr
load_process_data = function(
  dir="unzipped_data/",
  files_sample=c("sample.sas7bdat"),
  files_charlson=c("alldxs.sas7bdat"),
  files_labs=c('alllabs.sas7bdat'),
  files_meds=c('allmeds.sas7bdat'),
  start=-4, end=0, 
  verbose=2,
  icd="any", icd10startdate=17229, icd9enddate=17229-3*31
){
  if(verbose) cat(paste0("Processing data from ", dir, " restricted to years ", start, " to ", end, "\n"))
  if(verbose) utils::timestamp()
  
  if(verbose) cat("Loading sample data...")
  dfs = lapply(files_sample, function(file) load_sas(paste0(dir, file), "sample", verbose=verbose-1))
  df = bind_rows(dfs)
  colnames(df) %<>% tolower()
  if(verbose) cat("done.\n")
  if(verbose) utils::timestamp()
  
  if(verbose) cat("Computing master table (window, type, etc.)...")
  master = df %>% select(one_of(c("id", "casecontrol", "cancertype", "stagegroupclinical", 
                                  "clinicaln", "clinicalm", "clinicalt", "visitin4yrs")))
  master$start = df$indexdate + start * 365 + 1
  master$end = df$indexdate + end * 365 + 1
  if(verbose) cat("done.\n")
  if(verbose) utils::timestamp()
  
  if(verbose) cat("Processing demographic variables...")
  df %<>% select(c(.data$id, .data$casecontrol, demo_vars))
  df %<>% mutate(age=.data$ageatindex+end) # shift age to end of window
  df %<>% mutate(gender=ifelse(.data$gender=="", NA, .data$gender))
  df %<>% mutate(gender=as.integer(.data$gender=="M"))
  df %<>% mutate(agentorange=as.integer(.data$agentorange=="YES"))
  df %<>% mutate(smoke_current=as.integer(.data$smokestatus==1))
  df %<>% mutate(smoke_former=as.integer(.data$smokestatus==2))
  df %<>% select(-c(.data$smokestatus, .data$ageatindex))
  if(verbose) cat("done.\n")
  if(verbose) utils::timestamp()
  
  if(verbose) cat("Processing Charlson indicators...\n")
  out = create_charlson_data(dir, files=files_charlson, master=master, verbose=verbose-1)
  master = out$master
  charlson_df = out$df
  df %<>% left_join(charlson_df, by="id") 
  rm(charlson_df, out); gc()
  if(verbose) cat("...done.\n")
  if(verbose) utils::timestamp()
  
  
  if(verbose) cat("Patching Charlson indicators using visitin4yrs...\n")
  df %<>% left_join(master %>% select(id, visitin4yrs), by="id")
  for(charl in charlson_vars){
    df %<>% mutate(
      !!charl := ifelse((visitin4yrs==1)&is.na(.data[[charl]]), 0, .data[[charl]])
    )
  }
  df %<>% select(-visitin4yrs)
  if(verbose) cat("...done.\n")
  if(verbose) timestamp()
  
  if(verbose) cat("Processing medication variables...")
  allmeds_df = create_meds_data(dir, files=files_meds, master=master, verbose=verbose-1)
  df %<>% left_join(allmeds_df, by="id")
  rm(allmeds_df); gc()
  if(verbose) cat("done.\n")
  if(verbose) utils::timestamp()
  
  if(verbose) cat("Processing lab variables...\n")
  lab_df = create_lab_data(dir, files=files_labs, master=master, verbose=verbose-1)
  df %<>% left_join(lab_df, by="id") 
  rm(lab_df); gc()
  if(verbose) cat("...done.\n")
  if(verbose) utils::timestamp()
  
  return(list(df=df, master=master))
}

create_charlson_data = function(dir="./unzipped_data/", files=c("alldxs.sas7bdat"), 
                                which=charlson_vars, master=NULL, verbose=T){

  out_df = list()
  for(file in files){
    if(verbose) cat(paste0("- ", file, " ...\n"))
    src_df = load_sas(paste0(dir, file), "charlson", verbose=verbose-1)
    colnames(src_df) %<>% tolower()
    # restrict to prediction window
    src_df %<>% left_join(master %>% select(.data$id, .data$start, .data$end), by="id")
    src_df %<>% dplyr::filter((.data$dxdate>=.data$start)&(.data$dxdate<=.data$end))
    
    dfs = list()
    if(verbose) cat("  ")
    for(charl in which){
      if(verbose) cat(paste0(charl, " "))
      icd9codes = charlson_icd(charl, "icd9")
      icd10codes = charlson_icd(charl, "icd10")
      tmp = src_df %>% mutate(charl9=icd9codes(.data$icd9code))
      tmp %<>% mutate(charl10=icd10codes(.data$icd10code))
      tmp %<>% group_by(.data$id) %>% select(.data$id, .data$charl9, .data$charl10) %>% summarize_all(max)
      tmp %<>% summarize(id=.data$id, charl=pmax(.data$charl9, .data$charl10))
      colnames(tmp) = c("id", charl)
      dfs[[charl]] = tmp
    }
    dff = dfs %>% purrr::reduce(full_join, by='id')
    out_df[[file]] = dff %>% group_by(.data$id) %>% summarize_all(max)
    if(verbose) cat("\n  ...done.\n")
  }
  out = bind_rows(out_df) %>% group_by(.data$id) %>% summarize_all(max)
  return(list(df=out, master=master))
}

create_meds_data = function(dir="./unzipped_data/", files=c("allmeds.sas7bdat"), 
                            which=med_vars, master=NULL, verbose=T){
  dfs = list()
  for(file in files){
    src_df = load_sas(paste0(dir, file), "meds", verbose=verbose-1)
    colnames(src_df) %<>% tolower()
    src_df %<>% mutate(med_type=tolower(.data$med_type))
    src_df %<>% dplyr::filter(.data$med_type %in% which) 
    # restrict to prediction window
    src_df %<>% left_join(master %>% select(.data$id, .data$start, .data$end), by="id")
    src_df %<>% dplyr::filter((.data$newenddate>=.data$start)&(.data$filldate<=.data$end)) # at least some overlap
    # ensure ordered
    src_df %<>% arrange(.data$id, .data$filldate)
    
    for(type in which){
      if(verbose) cat(paste0("- ", type, " ...\n"))
      tmp = src_df %>% dplyr::filter(.data$med_type==type)
      # clip dates to prediction window & compute lag
      tmp %<>% mutate(
        filldate=pmax(.data$start,.data$filldate),
        enddate=pmin(.data$end,.data$newenddate),
        next_ID=lead(.data$id),
        next_filldate=lead(.data$filldate, default=Inf)
      )
      tmp %<>% select(one_of("id", "filldate", "enddate", "next_ID", "next_filldate", "dd", "end", "start"))
      last_entry = tmp$id!=tmp$next_ID
      tmp$next_ID[last_entry] = NA; tmp$next_filldate[last_entry] = Inf
      if(verbose) cat("  clipped dates to prediction window\n")
      # add dummy rows for gaps with 0 dose (unless it goes beyond the prediction window)
      end_after = tmp$enddate<tmp$next_filldate
      tmp = bind_rows(
        tmp %>% select(.data$id, .data$filldate, .data$dd),
        tmp %>% dplyr::filter(end_after) %>% 
          mutate(dd=ifelse(.data$enddate==.data$end, .data$dd, 0), filldate=.data$enddate) %>% 
          select(.data$id, .data$filldate, .data$dd)
      )
      tmp %<>% arrange(.data$id, .data$filldate)
      colnames(tmp) = c("id", "date", "dose")
      if(verbose) cat("  added dummy rows for gaps\n")
      # compute variables
      tmp %<>% mutate(
        ID_next = lead(.data$id),
        date_next = lead(.data$date),
        dose_next = lead(.data$dose)
      )
      new_subject = tmp$id!=tmp$ID_next
      tmp$ID_next[new_subject] = NA
      tmp$date_next[new_subject] = NA
      tmp$dose_next[new_subject] = NA
      tmp = tmp %>% mutate(
        ddate=pmax(1, .data$date_next-.data$date),
        ddose=.data$dose_next-.data$dose
      ) %>% mutate(
        sdose=.data$ddose/.data$ddate,
        pdose=.data$dose*.data$ddate
      )
      tmp = tmp %>% group_by(.data$id) %>% summarize(
        int=safe_sum(.data$pdose),
        mean=safe_mean(.data$dose),
        max=safe_max(.data$dose),
        maxdiff=safe_max(.data$sdose),
        tv=safe_mean(abs(.data$sdose))
      )
      if(verbose) cat("  computed variables\n")
      colnames(tmp) = c("id", paste(tolower(type), c("int", "mean", "max", "maxdiff", "tv"), sep="_"))
      dfs[[paste0(file, "_", type)]] = tmp
      if(verbose) cat("  ...done.\n")
    }
  }
  dff = dfs %>% purrr::reduce(full_join, by="id")
  return(dff)
}

create_lab_data = function(dir="./unzipped_data/", files=c("alllabs.sas7bdat"),
                           which=lab_vars, master=NULL, verbose=T){
  dfs = list()
  
  for(file in files){
    if(verbose) cat(paste0("- ", file, " ...\n"))
    
    src_df = load_sas(paste0(dir, file), "labs", verbose=verbose-1)
    colnames(src_df) %<>% tolower()
    subtypes = tail(colnames(src_df), -2)
    # restrict to prediction window
    src_df %<>% left_join(master %>% select(.data$id, .data$start, .data$end), by="id")
    src_df %<>% dplyr::filter((.data$labdate>=.data$start)&(.data$labdate<=.data$end))
    # ensure ordered
    src_df %<>% arrange(.data$id, .data$labdate)
    
    if(verbose) cat("  ")
    for(type in intersect(subtypes, which)){
      if(verbose) cat(paste0(type, " "))
      tmp = src_df %>% select(.data$id, .data$labdate, !!type)
      tmp %<>% tidyr::drop_na(!!type)
      colnames(tmp) = c("id", "labdate", "v")
      # compute lag variables
      tmp %<>% mutate(
        labdate_lag = lag(.data$labdate),
        v_lag = lag(.data$v),
        id_lag = lag(.data$id)
      )
      # put NAs for lags of different ids
      tmp$v_lag[tmp$id!=tmp$id_lag] = NA
      # compute diff and slope
      tmp %<>% mutate(
        dlabdate = pmax(1, .data$labdate - .data$labdate_lag),
        dv = .data$v - .data$v_lag
      )
      tmp %<>% mutate(
        sv = .data$dv / .data$dlabdate
      )
      # compute summaries
      tmp = tmp %>% group_by(id) %>%
        summarize(
          mean = safe_mean(v),
          max = safe_max(v),
          min = safe_min(v),
          maxdiff = safe_max(sv),
          mindiff = safe_min(sv),
          tv = safe_mean(abs(sv)),
        )
      colnames(tmp) = c("id", paste(type, c("mean", "max", "min", "maxdiff", "mindiff", "tv"), sep="_")) 
      
      dfs[[paste(file, type)]] = tmp
    }
    if(verbose) cat("\n  ...done.\n")
  }
  dff = dfs %>% purrr::reduce(full_join, by="id")
  return(dff)
}























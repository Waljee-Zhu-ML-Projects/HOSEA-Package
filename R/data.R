#' Title
#'
#' @param dir directory containing all files
#' @param file_out not used
#' @param start starting year relative to index (default: -5)
#' @param end ending year relative to index (default: -1)
#'
#' @return
#' @export
#' @import dplyr magrittr
load_process_data = function(
  dir="unzipped_data/",
  start=-5, end=-1, icd10=F
){
        cat(paste0("CREATING DATA FROM ", dir, " WITH YEARS ", start, " to ", end, "\n"))
        timestamp()
  
        cat("Loading demographic data...")
  df = load_sas(paste0(dir, "sample.sas7bdat"), "sample")
        cat("done.\n")
        timestamp()
  
        cat("Computing window bounds...")
  master = df[,c('ID')]
  master$case = !is.na(df$datedx)
  master$start = df$IndexDate + start * 365 + 1
  master$end = df$IndexDate + end * 365 + 1
        cat("done.\n")
        timestamp()
        
        cat("Processing Charlson indicators...\n")
  out = create_charlson_data(dir, master=master, icd10=icd10)
  if(icd10) master = out$master
  charlson_df = out$df
  df %<>% left_join(charlson_df, by="ID") 
  rm(charlson_df); gc()
        cat("...done.\n")
        timestamp()
  
        cat("Processing demographic variables...")
  df %<>% select(demo_vars())
  df$Gender[df$Gender==''] = NA
  df$Gender = as.integer(df$Gender=='M')
  df$agentorange = as.integer(df$agentorange=='YES')
  df$smoke_current = as.integer(df$SmokeStatus==1)
  df$smoke_former = as.integer(df$SmokeStatus==2)
  df %<>% select(-SmokeStatus)
        cat("done.\n")
        timestamp()
  
        cat("Processing event variables...\n")
  event_df = create_event_data(dir, master=master)
  df %<>% left_join(event_df, by="ID") 
  rm(event_df); gc()
        cat("...done.\n")
        timestamp()
  
        cat("Processing medication variables...")
  allmeds_df = create_meds_data(dir, master=master)
  df %<>% left_join(allmeds_df, by="ID")
  rm(allmeds_df); gc()
        cat("done.\n")
        timestamp()
  
        cat("Processing lab variables...\n")
  lab_df = create_lab_data(dir, master=master)
  df %<>% left_join(lab_df, by="ID") 
  rm(lab_df); gc()
        cat("...done.\n")
        timestamp()
  
        cat("Simple imputation for some variables...")
  for(n in other_vars()){
    which = is.na(df[, n])
    df[which, n] = 0
  }
        cat("...done.\n")
        timestamp()
  return(list(df=df, master=master))
}


#' Title
#'
#' @param dir 
#' @param prefix 
#' @param which 
#' @param master 
#'
#' @return a tibble with columns (ID, *which); with 0/1 entries depending whether 
#' there was an ICD 9/10 code (see charlson_names()) during the prediction 
#' interval (given by master)
#' @export
#' @import dplyr magrittr
#' @importFrom purrr reduce
#'
#' @examples
create_charlson_data = function(dir="./unzipped_data/", prefix="alldxs", 
                                which=charlson_names(), master=NULL, icd10=F){
  if(icd10) master$min = master$end
  out_df = list()
  files = unique(sapply(list.files(dir, paste0(prefix, ".*")), function(str) sub("\\..*", "", str)))
  for(file in files){
    cat(paste0("- ", file, " ...\n"))
    src_df = load_sas(paste0(dir, file, ".sas7bdat"), file)
    # restrict to prediction window
    src_df %<>% left_join(master, by="ID")
    src_df %<>% filter((Dxdate>=start)&(Dxdate<=end))
    # patch for icd10 only
    if(icd10) {
      src_df %<>% filter(icd10code!="*Unknown at this time*")
      mindate = src_df %>% group_by(ID) %>% summarize(mindate=min(Dxdate))
      master %<>% left_join(mindate, by="ID")
      master %<>% mutate(start=pmin(start, mindate, na.rm=T))
    }
    
    dfs = list()
    cat("  ")
    for(charl in which){
      cat(paste0(charl, " "))
      icd9 = charlson_icd(charl, "icd9")
      icd10 = charlson_icd(charl, "icd10")
      tmp = src_df %>% mutate(charl9=icd9(icd9code))
      tmp %<>% mutate(charl10=ifelse(file=="alldxscx.sas7bdat", 0, icd10(icd10code))) # legacy from Peter's code, why?
      tmp %<>% group_by(ID) %>% select(ID, charl9, charl10) %>% summarize_all(max)
      tmp %<>% summarize(ID=ID, charl=pmax(charl9, charl10))
      colnames(tmp) = c("ID", charl)
      dfs[[charl]] = tmp
    }
    dff = dfs %>% purrr::reduce(full_join, by='ID')
    out_df[[file]] = dff %>% group_by(ID) %>% summarize_all(max)
    cat("\n  ...done.\n")
  }
  out = bind_rows(out_df) %>% group_by(ID) %>% summarize_all(max)
  master %<>% select(-mindate)
  return(list(df=out, master=master))
}




#' Title
#'
#' @param dir 
#' @param which 
#' @param master 
#'
#' @return
#' @export
#' @import dplyr magrittr
#' @importFrom purrr reduce
#'
#' @examples
create_event_data = function(dir="./unzipped_data/", which=event_vars(), master=NULL){
  dfs = list()
  for(file in which){
    cat(paste0("- ", file, " ...\n"))
    src_df = load_sas(paste0(dir, file, ".sas7bdat"), file)
    # restrict to prediction window
    src_df %<>% left_join(master, by="ID")
    cols = colnames(src_df); cols[2] = "date"; colnames(src_df) = cols
    src_df %<>% filter((date>=start)&(date<=end))
    # ensure ordered
    src_df %<>% arrange(ID, date)
    # lagged variables
    src_df %<>% mutate(ID_lag = lag(ID), date_lag=lag(date))
    new_subject = with(src_df, ID!=ID_lag)
    src_df$ID_lag[new_subject] = NA; src_df$date_lag[new_subject] = NA
    # difference and slope
    src_df = src_df %>% 
      mutate(ddate=ifelse(date-date_lag==0, NA, date-date_lag)) %>% 
      mutate(sdate=1/ddate)
    tmp = src_df %>% group_by(ID) %>% summarize(
      n=n(),
      maxdiff=safe_max(sdate)
    )
    colnames(tmp) = c("ID", paste(file, c("n", "maxdiff"), sep="_"))
    dfs[[file]] = tmp
    cat("  ...done.\n")
  }
  dff = dfs %>% purrr::reduce(full_join, by="ID")
  return(dff)
}




#' Title
#'
#' @param dir 
#' @param which 
#' @param master 
#'
#' @return
#' @export
#' @import dplyr magrittr
#' @importFrom purrr reduce
#'
#' @examples
create_meds_data = function(dir="./unzipped_data/", which=med_vars(), master=NULL){
  dfs = list()
  src_df = load_sas(paste0(dir, "allmeds.sas7bdat"), "allmeds")
  src_df %<>% filter(Med_Type %in% med_vars()) 
  # restrict to prediction window
  src_df %<>% left_join(master, by="ID")
  src_df %<>% filter((newenddate>=start)&(Filldate<=end)) # at least some overlap
  # ensure ordered
  src_df %<>% arrange(ID, Filldate)
  
  for(type in which){
    cat(paste0("- ", type, " ...\n"))
    tmp = src_df %>% filter(Med_Type==type)
    # clip dates to prediction window & compute lag
    tmp %<>% mutate(
      filldate=pmax(start,Filldate),
      enddate=pmin(end,newenddate),
      next_ID=lead(ID),
      next_filldate=lead(Filldate, default=Inf)
    )
    tmp %<>% select(ID, filldate, enddate, next_ID, next_filldate, dd, end, start)
    last_entry = with(tmp, ID!=next_ID)
    tmp$next_ID[last_entry] = NA; tmp$next_filldate[last_entry] = Inf
    cat("  clipped dates to prediction window\n")
    # add dummy rows for gaps with 0 dose (unless it goes beyond the prediction window)
    end_after = with(tmp, enddate<next_filldate)
    tmp = bind_rows(
      tmp %>% select(ID, filldate, dd),
      tmp %>% filter(end_after) %>% 
        mutate(dd=ifelse(enddate==end, dd, 0), filldate=enddate) %>% 
        select(ID, filldate, dd)
    )
    tmp %<>% arrange(ID, filldate)
    colnames(tmp) = c("ID", "date", "dose")
    cat("  added dummy rows for gaps\n")
    # compute variables
    tmp %<>% mutate(
      ID_next = lead(ID),
      date_next = lead(date),
      dose_next = lead(dose)
    )
    new_subject = with(tmp, ID!=ID_next)
    tmp$ID_next[new_subject] = NA
    tmp$date_next[new_subject] = NA
    tmp$dose_next[new_subject] = NA
    tmp = tmp %>% mutate(
      ddate=pmax(1, date_next-date),
      ddose=dose_next-dose
    ) %>% mutate(
      sdose=ddose/ddate,
      pdose=dose*ddate
    )
    tmp = tmp %>% group_by(ID) %>% summarize(
      int=safe_sum(pdose),
      mean=safe_mean(dose),
      max=safe_max(dose),
      maxdiff=safe_max(sdose),
      tv=safe_mean(abs(sdose))
    )
    cat("  computed variables\n")
    colnames(tmp) = c("ID", paste(tolower(type), c("int", "mean", "max", "maxdiff", "tv"), sep="_"))
    dfs[[type]] = tmp
    cat("  ...done.\n")
  }
  dff = dfs %>% purrr::reduce(full_join, by="ID")
  return(dff)
}


#' Title
#'
#' @param dir 
#' @param which 
#' @param master 
#'
#' @return
#' @export
#' @import dplyr magrittr
#' @importFrom purrr reduce
#'
#' @examples
create_lab_data = function(dir="./unzipped_data/", which=lab_types(), master=NULL){
  dfs = list()
  
  for(file in which){
    cat(paste0("- ", file, " ...\n"))
    
    src_df = load_sas(paste0(dir, file, ".sas7bdat"), "file")
    subtypes = tail(colnames(src_df), -2)
    # restrict to prediction window
    src_df %<>% left_join(master, by="ID")
    src_df %<>% filter((labdate>=start)&(labdate<=end))
    # ensure ordered
    src_df %<>% arrange(ID, labdate)
    
    cat("  ")
    for(type in subtypes){
      cat(paste0(type, " "))
      tmp = src_df %>% select(ID, labdate, type)
      colnames(tmp) = c("ID", "labdate", "var")
      # compute lag variables
      tmp %<>% mutate(
        ID_lag = lag(ID),
        labdate_lag = lag(labdate),
        var_lag = lag(var)
      )
      # compute diff and slope
      tmp %<>% mutate(
        dlabdate = pmax(1, labdate - labdate_lag),
        dvar = var - var_lag
      )
      tmp %<>% mutate(
        svar = dvar / dlabdate
      )
      # compute summaries
      tmp = tmp %>% group_by(ID) %>%
        summarize(
          mean = safe_mean(var),
          max = safe_max(var),
          min = safe_min(var),
          maxdiff = safe_max(svar),
          mindiff = safe_min(svar),
          tiv = safe_mean(abs(svar)),
        )
      colnames(tmp) = c("ID", paste(type, c("mean", "min", "max", "mindiff", "maxdiff", "tv"), sep="_"))
      
      dfs[[paste(file, type)]] = tmp
    }
    cat("\n  ...done.\n")
  }
  dff = dfs %>% purrr::reduce(full_join, by="ID")
  return(dff)
}



#' Title
#'
#' @param df 
#' @param dir 
#' @param which 
#'
#' @return
#' @export
#' @import dplyr magrittr
#'
#' @examples
patch_outcome = function(df, dir="./unzipped_data/", which="any"){
  if(which == "any"){ # do nothing, CaseControl already contains both types
    return(df)
  }else{
    stopifnot(which %in% c("EAC", "EDJAC"))
    file = "cancertype"
    src_df = load_sas(paste0(dir, file, ".sas7bdat"), file) 
    src_df %<>% filter(CancerType==which)
    df %<>% mutate(!!which := ifelse(ID %in% src_df$ID, 1, 0))
    return(df)
  }
}






















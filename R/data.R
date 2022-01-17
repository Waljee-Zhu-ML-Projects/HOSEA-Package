#' Title
#'
#' @param dir 
#' @param y0 
#' @param y1 
#'
#' @return
#' @export
create_data = function(
  dir_in="R_data/",
  file_out="R_data/processed_records_5-1",
  y0=-5, y1=-1
){
  timestamp()
  
  cat("Loading data...")
  df = read_sas(paste0(dir, "sample.sas7dbat"))
  cat("done.\n")
  timestamp()
  
  cat("Computing window bounds...")
  master = df[,c('ID')]
  master$case = !is.na(df$datedx)
  master$start = df$IndexDate + y0 * 365 + 1
  master$end = df$IndexDate + y1 * 365 + 1
  cat("done.\n")
  timestamp()
  
  cat("Processing demo variables...")
  df = df[, HOSEA:::demo_vars()]
  # Gender
  df$Gender[df$Gender==''] = NA
  df$Gender = as.integer(df$Gender=='M')
  # agentorange
  df$agentorange = as.integer(df$agentorange=='YES')
  # SmokeStatus (keep current and former)
  df$smoke_current = as.integer(df$SmokeStatus==1)
  df$smoke_former = as.integer(df$SmokeStatus==2)
  df = dplyr::select(df, -SmokeStatus)
  cat("done.\n")
  timestamp()
  
  cat("Processing Charlson indicator variables...\n")
  charlson_df = create_charlson_data(paste0(dir, "alldxs"))
  # charlson_df = readRDS(file=paste0(dir, "charlson", ".rds")) #TODO this should be a processing step
  names(charlson_df) = charlson_vars_new() #TODO: same
  for(n in charlson_vars_old()){
    df[[n]] = charlson_df[[n]]
  }
  rm(charlson_df); gc()
  cat("...done.\n")
  timestamp()
  
  cat("Processing event variables...\n")
  for(n in event_vars()){
    cat(paste0(n, "..."))
    event_df = readRDS(paste0(dir, tab, '_summary.rds')) #TODO this should be a processing step
    df = dplyr::left_join(df, event_df, by="ID")
    cat("done.\n")
  }
  rm(event_df); gc()
  cat("...done.\n")
  timestamp()
  
  cat("Processing medication variables...")
  allmeds_df = readRDS(paste0(dir, "allmeds_summary.rds")) #TODO this should be a processing step
  df = dplyr::left_join(df, allmeds_df, by="ID")
  rm(allmeds_df); gc()
  cat("done.\n")
  timestamp()
  
  cat("Processing lab variables...\n")
  for(n in lab_types()){
    cat(paste0(n, "..."))
    lab_df = readRDS(paste0(dir, tab, '_summary.rds')) #TODO this should be a processing step
    df = dplyr::left_join(df, lab_df, by="ID")
    cat("done.\n")
  }
  rm(lab_df); gc()
  cat("...done.\n")
  timestamp()
  
  cat("Simple imputation for some variables...")
  for(n in other_vars()){
    which = is.na(tmp)
    df[which, n] = 0
  }
  cat("...done.\n")
  timestamp()
  
  return(df)
}


create_charlson_data = function(dir="unzipped_data/", prefix="alldxs", which=HOSEA:::charlson_names()){
  df = tibble::tibble()
  files = list.files(dir, paste0(prefix, ".*"))
  for(file in files){
    cat(paste0("- ", file, " ..."))
    src_df = read_sas(paste0(dir, file))
    
    for(charl in which){
      icd9 = HOSEA:::charlson_icd(charl, "icd9")
      icd10 = HOSEA:::charlson_icd(charl, "icd10")
      tmp = dplyr::mutate(src_df, charl9=icd9(icd9code))
      tmp = dplyr::mutate(tmp, charl10=ifelse(file=="alldxscx.sas7bat", 0, icd10(icd10code))) # legacy from Peter's code, why?
      df = dplyr::bind_rows(df, tmp)
      df = dplyr::summarize_all(dplyr::group_by(df, ID), max, na.rm=T)
    }
    cat("done.\n")
  }
  
  return(df)
}
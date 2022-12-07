load_sas = function(filepath, filename, verbose=T, ...){
  rdspath = stringr::str_replace_all(filepath, "sas7bdat", "rds")
  # first check if in rds format, which is much faster to load
  try({
    df = readRDS(rdspath)
    if(verbose) cat(paste("  Loaded from RDS file: ", rdspath), fill=T)
    return(df)
  }, silent=T)
  # otherwise, read sas and create rds file
  df = haven::read_sas(filepath, ...)
  if(verbose) cat(paste("  Loaded from SAS format:", filepath), fill=T)
  # path column names
  try(df%<>%rename(id=.data$id_V2))
  # sort
  colnames(df) %<>% tolower()
  df = switch(filename,
              sample=df %>% arrange(.data$id, NA),
              labs=df %>% arrange(.data$id, .data$labdate),
              meds=df %>% arrange(.data$id, .data$filldate),
              charlson=df %>% arrange(.data$id, .data$dxdate),
              df # no match, e.e.g cancertype
  )
  saveRDS(df, rdspath)
  if(verbose) cat(paste("  Saved to RDS format:", rdspath), fill=T)
  return(df)
}
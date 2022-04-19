#' Load sas files into R
#'
#' @param filepath 
#' @param filename 
#' @param ... further arguments to pass to haven::read_sas
#'
#' @return
#' @export
#' @import dplyr
#' @importFrom haven read_sas
#' @importFrom stringr str_replace_all
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
  try(df%<>%rename(id=id_V2))
  # sort
  df = switch(filename,
              sample=arrange(df, id, NA),
              labs=arrange(df, id, labdate),
              meds=arrange(df, id, filldate),
              charlson=arrange(df, id, dxdate),
              df # no match, e.e.g cancertype
  )
  saveRDS(df, rdspath)
  if(verbose) cat(paste("  Saved to RDS format:", rdspath), fill=T)
  return(df)
}
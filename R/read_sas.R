#' Read a SAS file and perform some minor pre-processing
#' 
#' When loading a SAS file, a RDS copy is made for future faster loading. If a RDS
#' file already exists with the same name, then this file is loaded instead.
#'
#' @param filepath Path to the file to read
#' @param filename Name of the file
#' @param verbose How much to print
#' @param ... Arguments to pass to read_sas
#'
#' @return the loaded data frame
#' @export
#' @import dplyr
#' @importFrom magrittr %<>%
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
  try(df%<>%rename(id=.data$id_V2))
  # sort
  colnames(df) %<>% tolower()
  df = switch(filename,
              sample=df %>% arrange(.data$id, NA),
              labs=df %>% arrange(.data$id, .data$labdate),
              meds=df %>% arrange(.data$id, .data$filldate),
              charlson=df %>% arrange(.data$id, .data$dxdate),
              df # no match
  )
  saveRDS(df, rdspath)
  if(verbose) cat(paste("  Saved to RDS format:", rdspath), fill=T)
  return(df)
}
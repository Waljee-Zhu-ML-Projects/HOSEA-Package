#' Title
#'
#' @param filepath 
#'
#' @return
#' @export
#' @import dplyr
#' @importFrom haven read_sas
#' @importFrom stringr str_replace_all
load_sas = function(filepath, filename, ...){
  rdspath = stringr::str_replace_all(filepath, "sas7bdat", "rds")
  # first check if in rds format, which is much faster to load
  try({
    df = readRDS(rdspath)
    cat(paste("  Loaded from RDS file: ", rdspath), fill=T)
    return(df)
  }, silent=T)
  # otherwise, read sas and create rds file
  df = haven::read_sas(filepath, ...)
  cat(paste("  Loaded from SAS format:", filepath), fill=T)
  # path ID names
  try(df%<>%rename(ID=id_V2))
  df = switch(filename,
              sample=arrange(df, ID, NA),
              colonoscopy=arrange(df, ID, Procdate),
              labs_a1c=arrange(df, ID, labdate),
              labs_bmp=arrange(df, ID, labdate),
              labs_cbc=arrange(df, ID, labdate),
              labs_crp=arrange(df, ID, labdate),
              labs_fobt=arrange(df, ID, labdate),
              labs_lft=arrange(df, ID, labdate),
              labs_lipid=arrange(df, ID, labdate),
              allmeds=arrange(df, ID, Filldate),
              df # no match, e.e.g cancertype
  )
  saveRDS(df, rdspath)
  cat(paste("  Saved to RDS format:", rdspath), fill=T)
  return(df)
}
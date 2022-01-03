#' Convert a batch of SAS files to RDS
#'
#' @param files 
#' @param ... 
#'
#' @return
#' @export
#' 
#' @seealso \code{\link{sas_to_rds}}
batch_sas_to_rds = function(
  files=c(
    "sample",
    "colonoscopy",
    # "allmeds",
    "labs_a1c",
    "labs_bmp",
    "labs_cbc",
    "labs_crp",
    # "labs_fobt",
    "labs_lft",
    "labs_lipid",
  ),
  ...
){
  cat(paste0(rep.int("=", 80), collapse=""), fill=T)
  cat("READING SAS FILES\n")
  timestamp()
  for(file in files) sas_to_rds(filename=file, ...)
  cat("END READING SAS FILES\n")
  timestamp()
  cat(paste0(rep.int("=", 80), collapse=""), fill=T)
}


#' Import SAS data and convert to RDS
#'
#' @param path_in 
#' @param path_out 
#' @param filename 
#' @param format_in 
#' @param format_out 
#'
#' @export
sas_to_rds = function(
  path_in="unzipped_data/",
  path_out="R_data/",
  filename=NULL,
  format_in=".sas7dbat",
  format_out=".rds"
){
  cat("SAS to RDS conversion", fill=T)
  file_in = paste0(path, filename, format_in)
  file_out = paste0(path, filename, format_out)
  df = read_sas(file_in)
  cat(paste("Successfully imported", file_in), fill=T)
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
              df # no match
              )
  saveRDS(df, file_out)
  cat(paste("Successfully written", file_out), fill=T)
}
#' Title
#'
#' @param filepath 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' @importFrom haven read_sas
read_sas = function(filepath, filename, ...){
  df = haven::read_sas(filepath, ...)
  cat(paste("  Successfully imported", filepath), fill=T)
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
  return(df)
}
batch_read_sas = function(
  path_in="unzipped_data/", 
  format_in=".sas7dbat",
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
  )
){
  out = lapply(files, function(file) read_sas(paste0(path_in, file, format_in)))
  return(out)
}



read_sas = function(filepath){
  df = haven::read_sas(filepath)
  cat(paste("Successfully imported", filepath), fill=T)
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
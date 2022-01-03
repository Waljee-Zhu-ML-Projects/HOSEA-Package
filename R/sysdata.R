#' Demographic variable names
#'
#' @return
#' @export
#' @keywords internal
demo_vars = function(){
  vars = c(
    'ID','CaseControl',
    'ageatindex','Gender','bmi','weight',
    'Asian','Black','HawaiianPacific','IndianAlaskan',
    'SmokeStatus',
    'agentorange',
    'GerdAtIndex',
    'CHF','CTD','DEM','DIAB_C','HIV','MLD','MSLD','PARA','RD',
    'cd','copd','diab_nc','mi','pud','pvd'
  )
  return(vars)
}

#' Charlson variable names in new format
#'
#' @return
#' @export
#' @keywords internal
charlson_vars_new = function(){
  vars = c(
    "ID",       "case",     "start",    "end",      "CHF",      
    "CTD",      "DEM",      "DIAB_C",   "GerdAtIndex",    
    "HIV",      "MLD",      "MSLD",     "PARA",     "RD",       
    "cd",       "copd",     "diab_nc",  "mi",      
    "pud",      "pvd",      "n_visits"
  )
  return(vars)
}

#' Charlson variable names in old format
#'
#' @return
#' @export
#' @keywords internal
charlson_vars_old = function(){
  vars =  c(
    "CHF",      "CTD",      "DEM",      "DIAB_C",   "GerdAtIndex",    
    "HIV",      "MLD",      "MSLD",     "PARA",     "RD",       
    "cd",       "copd",     "diab_nc",  "mi",       "pud",      "pvd"
  )
  return(vars)
}

#' Lab measurements names
#'
#' @return
#' @export
#' @keywords internal
lab_types = function(){
  vars = c('labs_a1c',
          'labs_bmp',
          'labs_cbc',
          'labs_crp',
          'labs_lft',
          'labs_lipid')
  return(vars)
}

#' Lab measurement summaries names
#'
#' @return
#' @export
#' @keywords internal
lab_summaries = function(){
  vars = c("mean", "max", "min",
           "maxdiff", "mindiff", "tv")
  return(vars)
}


#' Event-type variables
#'
#' @return
#' @export
#' @keywords internal
event_vars = function(){
  vars = c("colonoscopy", "labs_fobt")
  return(vars)
}

#' Other clinical variable names (after transformations)
#'
#' @return
#' @export
#' @keywords internal
other_vars = function(){
  vars = c('colonoscopy_n','colonoscopy_maxdiff',
           'labs_fobt_n','labs_fobt_maxdiff',
           'h2r_int','h2r_mean','h2r_max','h2r_maxdiff','h2r_tv',
           'ppi_int','ppi_mean','ppi_max','ppi_maxdiff','ppi_tv')
  return(vars)
}

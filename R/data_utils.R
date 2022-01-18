
#' Title
#'
#' @param x 
#'
#' @return
#' @keywords internal
safe_mean <- function(x){
  if(all(is.na(x))){
    NA
  }
  else{
    mean(x,na.rm=TRUE)
  }
}

#' Title
#'
#' @param x 
#'
#' @return
#' @keywords internal
safe_max <- function(x){
  if(all(is.na(x))){
    NA
  }
  else{
    max(x,na.rm=TRUE)
  }
}

#' Title
#'
#' @param x 
#'
#' @return
#' @keywords internal
safe_min <- function(x){
  if(all(is.na(x))){
    NA
  }
  else{
    min(x,na.rm=TRUE)
  }
}

#' Title
#'
#' @param x 
#'
#' @return
#' @keywords internal
safe_sum <- function(x){
  if(all(is.na(x))){
    NA
  }
  else{
    sum(x,na.rm=TRUE)
  }
}

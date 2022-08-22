#' generic impute
#'
#' @param df 
#' @param obj 
#' @param n_samples 
#' @param ... 
#' 
#' @export
impute = function(obj, df, n_samples, ...) UseMethod("impute")

#' Get the ID of patient with sufficiently complete records
#' 
#' Required features:
#' * Gender
#' * Age 
#' * Smoking status
#' * BMI
#' * Race
#' * GERD
#' * Medication
#'
#' @param df The data frame to search for complete records
#'
#' @return a vector of ids
#' @export
#' @import dplyr
complete_for_comparison = function(df){
  complete_cases = !(
    is.na(df$gender) |
      is.na(df$age) |
      is.na(df$smoke_former) |
      is.na(df$smoke_current) |
      is.na(df$bmi) |
      is.na(df$black) | # will be NA iff same for other columns
      is.na(df$gerd) |
      (is.na(df$h2r_max) & is.na(df$ppi_max))
  )
  return(df %>% dplyr::filter(complete_cases) %>% pull(id))
}

#' Preprocessing of new columns for HUNT, Kunzmann and Guidlines
#'
#' @param df The original data frame
#'
#' @return a new df with just the relevant columns
#' @export
#' @import dplyr
#' @importFrom magrittr %<>%
compute_columns_for_comparison = function(df){
  df %<>% mutate(
    age = pmax(20, pmin(90, .data$age)),
    bmi = pmax(9, pmin(86, .data$bmi))
  )
  df %<>% mutate(
    white = !(.data$asian | .data$black | .data$hawaiianpacific | .data$indianalaskan),
    smoke_ever = (.data$smoke_current | .data$smoke_former),
    k_age_bin = cut(.data$age, breaks=c(0, 50, 55, 60, 65, 100)),
    k_bmi_bin = cut(.data$bmi, breaks=c(0, 25, 30, 35, 100)),
    k_ec = (.data$h2r_max>0) | (.data$ppi_max>0) | .data$gerd,
    h_age_bin = cut(.data$age, breaks=c(0, 50, 60, 70, 100)),
    h_bmi_bin = cut(.data$bmi, breaks=c(0, 30, 100))
  )
  return(df %>% select(c(
    "id", "casecontrol", 
    "smoke_current", "smoke_former", "smoke_ever", 
    "gender", 
    "gerd", "k_ec",
    "white", 
    "bmi", "k_bmi_bin", "h_bmi_bin",
    "age", "k_age_bin", "h_age_bin"
  )))
}

#' Computes the Kunzmann score
#' 
#' @description As described in Kunzmann et al. (2018, Table 2).
#' 
#' @details 
#' Age: 
#' * +0.0 for 55-
#' * +1.5 for (55,60]
#' * +2.5 for (60,65]
#' * +3.5 for 65+
#' 
#' Sex:
#' * +0.0 for female
#' * +4.0 for male
#' 
#' BMI:
#' * +0.0 for 25-
#' * +1.0 for (25,30]
#' * +1.5 for (30, 35]
#' * +2.5 for 35+
#' 
#' Smoking status:
#' * +0.0 if Never
#' * +2.5 if Former
#' * +3.5 if Current
#' 
#' Esophageal condition (GERD/PPI/H2R):
#' * +1.5 if Any 
#' 
#' Note that Esophageal condition is originally described as 
#' "Esophageal conditions included self-reported history of gastroesophageal 
#' reflux disease, Barrett’s esophagus, hiatus hernia, or esophageal stricture 
#' and/or esophageal fundoplication or hiatus hernia surgery and/or anti-reflux 
#' medication use (none or any)." However, we only consider those
#' that we have access to (GERD and Medication).
#'
#' @param df A pre-processed data frame (from [compute_columns_for_comparison])
#'
#' @return a data frame with two columns (id, Kunzmann score)
#' @export 
#' @references Kunzmann, A. T., Thrift, A. P., Cardwell, C. R., Lagergren, J., Xie, S., Johnston, B. T., Anderson, L. A., Busby, J., McMenamin, Ú. C., Spence, A. D., & Coleman, H. G. (2018). Model for Identifying Individuals at Risk for Esophageal Adenocarcinoma. Clinical gastroenterology and hepatology : the official clinical practice journal of the American Gastroenterological Association, 16(8), 1229–1236.e4. [https://doi.org/10.1016/j.cgh.2018.03.014](https://doi.org/10.1016/j.cgh.2018.03.014)
kunzmann_score = function(df){
  score = 0
  score = score + (df$k_age_bin == "(55,60]") * 1.5
  score = score + (df$k_age_bin == "(60,65]") * 2.5
  score = score + (df$k_age_bin == "(65,100]") * 3.5
  score = score + df$gender * 4
  score = score + (df$k_bmi_bin == "(25,30]") * 1
  score = score + (df$k_bmi_bin == "(30,35]") * 1.5
  score = score + (df$k_bmi_bin == "(35,100]") * 2.5
  score = score + df$smoke_former * 2.5
  score = score + df$smoke_current * 3.5
  score = score + df$k_ec * 1.5
  return(data.frame(id=df$id, Kunzmann=score))
}

#' Title
#'
#' @param df 
#'
#' @return
#' @export
#' @import utils
hunt_score = function(df){
  score = 3.6
  score = score * ifelse(df$gender, 1.9, 1.)
  score = score * ifelse(df$h_age_bin == "(50,60]", 2.1, 1.)
  score = score * ifelse(df$h_age_bin == "(60,70]", 3.2, 1.)
  score = score * ifelse(df$h_age_bin == "(70,100]", 3.1, 1.)
  score = score * ifelse(df$h_bmi_bin == "(30,100]", 1.8, 1.)
  score = score * ifelse(df$gerd, 3.7, 1.)
  score = score * ifelse(df$smoke_ever, 2.1, 1.)
  return(data.frame(id=df$id, HUNT=score/100000))
}

#' Title
#'
#' @param df 
#'
#' @return
#' @export
#' @import dplyr
#' @importFrom utils tail
guidelines = function(df){
  scores = df %>% mutate( 
    ACG2016     = as.integer(.data$gender & .data$gerd & ( ((.data$age>=50) + (.data$white) + (.data$bmi>30) + (.data$smoke_ever) ) > 1)),
    ACG2022     = as.integer(.data$gerd & ( (.data$gender + (.data$age>=50) + (.data$white) + (.data$bmi>30) + (.data$smoke_ever) ) > 2)),
    ACP2012     = as.integer(.data$gender & .data$gerd & (.data$age>=50) & ( ((.data$bmi>30) + (.data$smoke_ever) ) > 0)),
    AGA2011     = as.integer(( .data$gerd + .data$gender + (.data$age>=50) + (.data$white) + (.data$bmi>30) ) > 1),
    AGA2022     = as.integer(( .data$gerd + .data$gender + (.data$age>=50) + (.data$white) + (.data$bmi>30) + (.data$smoke_ever) ) > 2),
    ASGE2019    = as.integer(.data$gerd & ( (.data$gender + (.data$age>=50) + (.data$bmi>30) + (.data$smoke_ever) ) > 0)),
    BSG2013     = as.integer(.data$gerd & ( (.data$gender + (.data$age>=50) + (.data$white) + (.data$bmi>30) ) > 2)),
    ESGE2017    = as.integer(.data$gerd & ( (.data$gender + (.data$age>=50) + (.data$white) + (.data$bmi>30) ) > 1))
  )
  gnames = utils::tail(colnames(scores), 8)
  return(scores %>% select(id, all_of(gnames)))
}

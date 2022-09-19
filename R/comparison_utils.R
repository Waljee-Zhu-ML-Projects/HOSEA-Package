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
  return(df %>% filter(complete_cases) %>% pull(id))
}

compute_columns_for_comparison = function(df){
  df %<>% mutate(
    age = pmax(20, pmin(90, age)),
    bmi = pmax(9, pmin(86, bmi))
  )
  df %<>% mutate(
    white = !(asian | black | hawaiianpacific | indianalaskan),
    smoke_ever = (smoke_current | smoke_former),
    k_age_bin = cut(age, breaks=c(0, 50, 55, 60, 65, 100)),
    k_bmi_bin = cut(bmi, breaks=c(0, 25, 30, 35, 100)),
    k_ec = (h2r_max>0) | (ppi_max>0) | gerd,
    h_age_bin = cut(age, breaks=c(0, 50, 60, 70, 100)),
    h_bmi_bin = cut(bmi, breaks=c(0, 30, 100))
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

guidelines = function(df){
  scores = df %>% mutate( 
    ACG2016     = as.integer(gender & gerd & ( ((age>=50) + (white) + (bmi>30) + (smoke_ever) ) > 1)),
    ACG2022     = as.integer(gerd & ( (gender + (age>=50) + (white) + (bmi>30) + (smoke_ever) ) > 2)),
    ACP2012     = as.integer(gender & gerd & (age>=50) & ( ((bmi>30) + (smoke_ever) ) > 0)),
    AGA2011     = as.integer(( gerd + gender + (age>=50) + (white) + (bmi>30) ) > 1),
    AGA2022     = as.integer(( gerd + gender + (age>=50) + (white) + (bmi>30) + (smoke_ever) ) > 2),
    ASGE2019    = as.integer(gerd & ( (gender + (age>=50) + (bmi>30) + (smoke_ever) ) > 0)),
    BSG2013     = as.integer(gerd & ( (gender + (age>=50) + (white) + (bmi>30) ) > 2)),
    ESGE2017    = as.integer(gerd & ( (gender + (age>=50) + (white) + (bmi>30) ) > 1))
  )
  gnames = tail(colnames(scores), 8)
  return(scores %>% select(id, all_of(gnames)))
}

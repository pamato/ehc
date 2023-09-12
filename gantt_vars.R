## Code to produce Gantt Chart variables in R

baseurl <- "http://iserformrtest.essex.ac.uk/ukhls/"

## Residences
residences <- NULL;
click_res <- NULL;
entries <- length(which(!is.na(EHC_survey[,grep("country_R", names(EHC_survey))])))  
if(entries > 0){
  for (x in 1:entries){
    residences <- c(residences, gantt_event_row_res(x, EHC_survey, 'country_R', 'datefrom_R', 'dateto_R', 'current_residence_R', "dob"))
    click_res <- c(click_res, paste0("click r", x, " href", baseurl ,"5/"))
  }
}

## Education
education <- NULL;
click_edu <- NULL;
check.integer <- function(N){
  !grepl("[^[:digit:]]", format(N,  digits = 1, scientific = FALSE))
}
entries <- length(which(check.integer(EHC_survey[,grep("educationtitle_E", names(EHC_survey))])))  
if(entries > 0){
  for (x in 1:entries){
    education <- c(education, gantt_event_row_edu(x, EHC_survey,  'educationtitle_E', 'city_E', 'datefrom_E', 'dateto_E', 'current_education_E'))
    click_edu <- c(click_edu, paste0("click e", x, " href", ' "http://iserformrtest.essex.ac.uk/ukhls/6/" '))
  }
}

## Occupations
occupations <- NULL;
click_occ <- NULL;
entries <- length(which(!is.na(EHC_survey[,grep("occupationcompany_O", names(EHC_survey))])))  
if(entries > 0){
  for (x in 1:entries){
    occupations <- c(occupations, gantt_event_row_jobs(x, EHC_survey,  'occupationcompany_O', 'datefrom_O', 'dateto_O', 'current_occupation_O'))
    click_occ <- c(click_occ, paste0("click j", x, " href", ' "http://iserformrtest.essex.ac.uk/ukhls/7/" '))
  }
}

## Relationships
partners <- NULL;
click_partners <- NULL;
entries <- length(which(!is.na(EHC_survey[,grep("partner_P", names(EHC_survey))])))  
if(entries > 0){
  for (x in 1:entries){
    partners <- c(partners, gantt_event_row_rel(x, EHC_survey, 'partner_P', 'datefrom_P', 'dateto_P', 'current_partner_P'))
    click_partners <- c(click_partners, paste0("click p", x, " href", ' "http://iserformrtest.essex.ac.uk/ukhls/8/" '))
  }
}

## Fertility
`%>%` <- magrittr::`%>%`
children <- NULL;
click_children <- NULL;
entries <- length(which(!is.na(EHC_survey[,grep("\\bchild_C", names(EHC_survey))])))
if(entries > 0){
  for (x in 1:entries){
    var <- EHC_survey %>% dplyr::mutate(display_date=dplyr::case_when(EHC_survey[[paste0("type_C", x)]] == 1 ~ paste0("dfb_C", x),
                                                                      EHC_survey[[paste0("type_C", x)]] == 2 ~ paste0("dfa_C", x),
                                                                      EHC_survey[[paste0("type_C", x)]] == 3 ~ paste0("dfs_C", x))) %>% 
      dplyr::pull(display_date)
    children <- c(children, gantt_event_row_ch(x, EHC_survey,  'child_C', EHC_survey[[var]], "dateto_C", "current_child_C"))
    click_children <- c(click_children, paste0("click c", x, " href", ' "https://ukhls.formr.org/9/" '))
  }
}



## Health
health <- NULL;
click_ph <- NULL;
check.integer <- function(N){
  !grepl("[^[:digit:]]", format(N,  digits = 1, scientific = FALSE))
}
entries <- length(which(!is.na(EHC_survey[,grep("issue_mental_PH", names(EHC_survey))])))  
if(entries > 0){
  for (x in 1:entries){
    health <- c(health, gantt_event_row_ph(x, EHC_survey,  'issue_mental_PH', 'datefrom_PH', "dateto_PH", "current_mental_PH"))
    click_ph <- c(click_ph, paste0("click ph", x, " href", ' "https://ukhls.formr.org/10/" '))
  }
}
click_ih <- NULL;
check.integer <- function(N){
  !grepl("[^[:digit:]]", format(N,  digits = 1, scientific = FALSE))
}
entries <- length(which(!is.na(EHC_survey[,grep("issue_physical_IH", names(EHC_survey))])))  
if(entries > 0){
  for (x in 1:entries){
    health <- c(health, gantt_event_row_ih(x, EHC_survey,  'issue_physical_IH', 'datefrom_IH', "dateto_IH", "current_physical_IH"))
    click_ih <- c(click_ih, paste0("click ih", x, " href", ' "https://ukhls.formr.org/11/" '))
  }
}

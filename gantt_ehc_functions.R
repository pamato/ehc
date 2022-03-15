
gantt_event_row_res <- function (i, data, col_title, col_datefrom, col_dateto, col_current, datebirth) {
  empty <- ''
  
  if(!(paste0(col_title, i) %in% colnames(data))) {
    return (empty)    
  }
  
  title <- last(data[[paste0(col_title, i)]])
  if (is.na(title) || title == '') {
    return (empty)
  }
  
  length(last(data[[paste0(col_datefrom, i)]]))>0
  
  is_current <- last(data[[paste0(col_current, i)]]) == 1
  is_current <- ifelse(is.na(is_current), FALSE, is_current)
  is_current <- ifelse(length(is_current)==0, FALSE, is_current)
  datefrom <- data[[datebirth]]
  datefrom <- ifelse(length(last(data[[paste0(col_datefrom, i)]]))>0, last(data[[paste0(col_datefrom, i)]]), datefrom)
  dateto <- format(Sys.Date(), "%Y-%m-%d")
  dateto <- ifelse(length(last(data[[paste0(col_dateto, i)]]))>0, data[[paste0(col_dateto, i)]], dateto)
  tag <- ifelse(is_current, "active", "done");
  label <- paste0("r", i)
  
  paste0(title, " :", tag, ",", label, ",", datefrom, ",", dateto,  "     ")
  
}

gantt_event_row_edu <- function (i, data, col_title, city_title, col_datefrom, col_dateto, col_current) {
  empty <- ''
  
  if(!(paste0(col_title, i) %in% colnames(data))) {
    return (empty)    
  }
  
  title1 <- toString(last(data[paste0(col_title, i)]))
  if (is.na(title1) || title1 == '') {
    return (empty)
  }
  title1 <- toString(title1)
  title1 <- ifelse(title1=="1", "Primary", title1)
  title1 <- ifelse(title1=="2", "Secondary", title1)
  title1 <- ifelse(title1=="3", "Vocational Education", title1)
  title1 <- ifelse(title1=="4", "Higher Education", title1)
  title2 <- last(data[paste0(city_title, i)])
  if (is.na(title2) || title2 == '') {
    return (empty)
  }
  
  
  title <- paste0(title1, " (", title2, ")")
  if (is.na(title) || title == '') {
    return (empty)
  }
  
  is_current <- last(data[paste0(col_current, i)]) == 1
  datefrom <- last(data[paste0(col_datefrom, i)])
  dateto <- ifelse(is_current, format(Sys.Date(), "%Y-%m-%d"), last(data[paste0(col_dateto, i)]))
  tag <- ifelse(is_current, "active", "done");
  label <- paste0("e", i)
  
  paste(title, ":", tag, ",", label, ",", datefrom, ",", dateto,  "     ", sep = " ")
}

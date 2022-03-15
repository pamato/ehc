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
  label <- paste0("a", i)
  
  paste(title, ":", tag, ",", label, ",", datefrom, ",", dateto,  "     ", sep = " ")
}

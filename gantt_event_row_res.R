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
  label <- paste0("a", i)
  
  paste0(title, " :", tag, ",", label, ",", datefrom, ",", dateto,  "     ")
  
}
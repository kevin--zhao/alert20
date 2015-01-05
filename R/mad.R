#' get the MAD upper and lower bound of a time series data frame
#'
#' This function generatea the upper bound, lower bound and some other features of a time series data frame.
#'
#' @param df the data frame name
#' @param threshold the threshold of multiplier to MAD value (this MAD value has already been multiplied by 1.24)
#' @export
#' @examples
#' madValid(df,2) # not run
madValid<-function(df, threshold){ 
  date = df$session_start_dt[nrow(df)]  # get the last date (order by date while get data)
  print('**********************')
  print(date) 
  values = df$value
  value = values[length(values)] # get the last metric value according to the list
  values = values[-length(values)]  # get all metric value except for last
  values = values[-which(values == max(values))[1]]   # which, return index , remove the max value
  values = values[-which(values == min(values))[1]]   # remove the min value
  
  med = median(values)
  md = mad(values)
  
  minV = min(values)
  d = abs(value - med) / md
  
  lower = med - threshold * md
  upper = med + threshold * md
  ##if lower <0, then customize it
  #print(lower)
  if(!is.na(lower)){
    if (lower<0)
    {
      lower = minV
    }
  }
  result = data.frame(value, lower, upper, med, md, d, date)
  return(result)
}


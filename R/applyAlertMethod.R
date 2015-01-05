#' Apply method on time series data and get alerting plots
#'
#' This function Apply alerting method on time series data and get alerting plots
#' 
#' @param df data frame to apply alerting method, it shall have at least 3 columns: session_start_dt, value, name
#' @param trainPeriod How many days(hours/minutes) do you want to train your model for prediction
#' @param dateKept How many days(hours/minutes) do you want to keep in your alerting plot
#' @param madAdj mad adjustment multiplier
#' @param if the alert points exceeds the fraction of total kept dates, it will have a name alert in the plot
#' @export
#' @import ggplot2
#' @examples
#' value = rnorm(100);
#' session_start_dt = as.Date(1:100,origin="2014-01-01")
#' df=data.frame(session_start_dt=session_start_dt,value=value,name="Feature1")
#' applyAlertMethod(df,10,50,2,0.2)
  applyAlertMethod=function(df, trainPeriod,dateKept,madAdj,frac){
      result0 = data.frame()        
      for(m in (dim(df)[1]-dateKept):dim(df)[1]){
        alertData = df[((m-trainPeriod+1):m),]
        result = madValid(alertData,madAdj)
        result0=rbind(result0,result)
      }
      result1=tail(result0,n=20)
      if(dim(result1[result1$d>madAdj,])[1]/20 >frac){
        metricsName=paste("alert-",unique(alertData$name),unique(alertData$Device),unique(alertData$site),round(dim(result1[result1$d>madAdj,])[1]/20,digits=2),sep = "")
      }else{
        metricsName=paste("not-",unique(alertData$name),unique(alertData$Device),unique(alertData$site),round(dim(result1[result1$d>madAdj,])[1]/20,digits=2),sep = "")
        
      }
      plotReport(df=result0,metricsName=as.character(metricsName), filename=paste(metricsName,"png",sep="."))
}
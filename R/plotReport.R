
#' plot the predicted upper bound and lower bound of a time series
#'
#' This function plot the predicted upper bound, lower bound of a time series.
#'
#' @param df the data frame name
#' @param metricsName metric name in the data frame that you want to plot
#' @param filename file name you want to save the plot into
#' @export
#' @examples
#' plotReport(df=result0,metricsName=as.character(metricsName), filename=paste(metricsName,"png",sep=".")) # not run
plotReport<-function(df, metricsName, filename) {
  png(filename, width=400,height=300)
  #png(filename, width=400,height=300)
  plot <- ggplot(data=df , aes(x=date, y=value)) + geom_line(color="blue")
  plot = plot + geom_line(aes(y=upper), color="red",linetype="dotted")
  plot = plot + geom_line(aes(y=lower), color="red",linetype="dotted")
  plot = plot + labs(title=paste(metricsName)) + theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank())
  print(plot);
  dev.off();
}
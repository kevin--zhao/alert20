\name{plotReport}
\alias{plotReport}
\title{plot the predicted upper bound and lower bound of a time series}
\usage{
  plotReport(df, metricsName, filename)
}
\arguments{
  \item{df}{the data frame name}

  \item{metricsName}{metric name in the data frame that you
  want to plot}

  \item{filename}{file name you want to save the plot into}
}
\description{
  This function plot the predicted upper bound, lower bound
  of a time series.
}
\examples{
plotReport(df=result0,metricsName=as.character(metricsName), filename=paste(metricsName,"png",sep=".")) # not run
}


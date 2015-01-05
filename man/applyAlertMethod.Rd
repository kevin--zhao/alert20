\name{applyAlertMethod}
\alias{applyAlertMethod}
\title{Apply method on time series data and get alerting plots}
\usage{
  applyAlertMethod(df, trainPeriod, dateKept, madAdj, frac)
}
\arguments{
  \item{df}{data frame to apply alerting method, it shall
  have at least 3 columns: session_start_dt, value, name}

  \item{trainPeriod}{How many days(hours/minutes) do you
  want to train your model for prediction}

  \item{dateKept}{How many days(hours/minutes) do you want
  to keep in your alerting plot}

  \item{madAdj}{mad adjustment multiplier}

  \item{if}{the alert points exceeds the fraction of total
  kept dates, it will have a name alert in the plot}
}
\description{
  This function Apply alerting method on time series data
  and get alerting plots
}
\examples{
value = rnorm(100);
session_start_dt = as.Date(1:100,origin="2014-01-01")
df=data.frame(session_start_dt=session_start_dt,value=value,name="Feature1")
applyAlertMethod(df,10,50,2,0.2)
}


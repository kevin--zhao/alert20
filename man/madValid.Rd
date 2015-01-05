\name{madValid}
\alias{madValid}
\title{get the MAD upper and lower bound of the last value of a time series}
\usage{
  madValid(df, threshold)
}
\arguments{
  \item{df}{the data frame name, it shall contains 2
  columns session_start_date and value}

  \item{threshold}{the threshold of multiplier to MAD value
  (this MAD value has already been multiplied by 1.24)}
}
\description{
  This function generatea the upper bound, lower bound and
  some other features of the current(last) time stamp based
  on time series before it.
}
\examples{
value = rnorm(100);
session_start_dt = as.Date(1:100,origin="2014-01-01")
df=data.frame(session_start_dt=session_start_dt,value=value)
madValid(df,2)
output:
med is median of all values before the last value;
md is the mad value of all values before the last one;
d is a ratio: abs(value - med) / md
}


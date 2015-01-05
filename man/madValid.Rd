\name{madValid}
\alias{madValid}
\title{get the MAD upper and lower bound of a time series data frame}
\usage{
  madValid(df, threshold)
}
\arguments{
  \item{df}{the data frame name}

  \item{threshold}{the threshold of multiplier to MAD value
  (this MAD value has already been multiplied by 1.24)}
}
\description{
  This function generatea the upper bound, lower bound and
  some other features of a time series data frame.
}
\examples{
madValid(df,2) # not run
}


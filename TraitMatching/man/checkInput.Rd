% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classCommunity.R
\name{checkInput}
\alias{checkInput}
\title{check function for input}
\usage{
checkInput(
  a = NULL,
  b = NULL,
  z = NULL,
  community = NULL,
  response = NULL,
  positive = NULL
)
}
\arguments{
\item{a, }{group a}

\item{b, }{group b}

\item{z, }{interaction matrix for a and b, rows == a and cols == b}

\item{community, }{list with communities/groups or data.frames}

\item{response, }{in case of community, provide name of response column}

\item{positive, }{in case of community, provide the positive class}
}
\description{
check function for input
}

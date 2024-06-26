% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/protocol_class.R
\name{gettimes}
\alias{gettimes}
\title{Retrieve transition start and end times from 'protocol' corresponding to
list}
\usage{
gettimes(protocol, l)
}
\arguments{
\item{l}{list of 2 items: "start" and "end" that correspond to
the rownumber in 'protocol' that the transition starts and ends}
}
\value{
list of 2 items: "start" and "end" that are the time
in seconds of the start and end of the transition
}
\description{
Retrieve transition start and end times from 'protocol' corresponding to
list
}

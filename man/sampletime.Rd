% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampletime.R
\name{sampletime}
\alias{sampletime}
\title{nirsex_sampletime helper}
\usage{
sampletime(
  sampnum = integer(),
  sfreq = double(),
  start_sampnum = 1L,
  start_minute = 0L,
  start_second = NA_real_,
  start_ms = 0L
)
}
\arguments{
\item{sampnum}{integer sample number}

\item{sfreq}{sample frequency in Hertz}

\item{start_sampnum}{optional, sample number to set as start, defaults to 1}

\item{start_minute}{optional, minute to set as start, defaults to 0}

\item{start_second}{optional, second to set as start, defaults to 0}
}
\value{
nirsex_sampletime
}
\description{
nirsex_sampletime helper
}
\details{
sampnum remains the same no matter the provided inputs. sampnum always starts
with 1, where \code{sampnum = 1L} is recorded as \code{orig_min = 0}, \code{orig_sec = 0}, and
\code{orig_ms} = 0.

\code{orig_min}, \code{orig_sec}, and \code{orig_ms} are always calculated using
the provided \code{sfreq} in Hertz to find the time since sample number 1 was
collected.

if \code{start_sampnum} or \code{start_minute} and \code{start_second} are not provided,
\code{zeroed_min}, \code{zeroed_sec}, and \code{zeroed_ms} will match \code{orig_min}, \code{orig_sec},
and \code{orig_ms}.

if \code{start_sampnum} or \code{start_minute} and \code{start_second} are provided,
\code{zeroed_min}, \code{zeroed_sec}, and \code{zeroed_ms} will be recorded as
time since provided \code{start_sampnum} or \code{start_minute} and \code{start_second}.

if \code{start_sampnum}, \code{start_minute}, and \code{start_second} are all provided,
\code{start_sampnum} must match or calculate out to match \code{start_minute} and \code{start_second}.

If only \code{start_sampnum} is provided, \code{start_minute} and \code{start_second} can be
caluculated from \code{start_sampnum}.

\code{start_second} can be the only start parameter provided. If \code{start_second}
is the only start parameter provided and is greater than 60,
\code{start_min} will be calculated from \code{start_second}.
}

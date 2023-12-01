
#' Rawdata Class Constructor
#'
#' @param data data frame
#' @param info measurement metadata
#'
#' @return data frame of raw data
#' @export
new_rawdata <- function(data = data.frame(),
                        info = info()
                        ){
  stopifnot(is.data.frame(data))

  #validate_rawdata(data, info)

  first_samp = data[["samp_num"]][[1]]

  last_samp = data[["samp_num"]][[length(data[["samp_num"]])]]
  meas_start = first_samp / info[["sfreq"]]
  meas_end = last_samp / info[["sfreq"]]

  info[["bounds"]] = list("first_samp" = first_samp,
                          "last_samp" = last_samp,
                          "meas_start" = meas_start,
                          "meas_end" = meas_end)

  rawdata <- list("data" = data,
                  "info" = info)

  structure(rawdata, class = c("rawdata", class(list())))
}

#' Rawdata Class Validator
#'
#' @param data data frame
#' @param info measurement metadata
#'
#' @return data
#' @export
validate_rawdata <- function(data, info){

# \\TODO rawdata validation



  data
}

#' Rawdata Class Helper
#'
#' @param data data frame of raw data
#' @param info measurement metadata
#'
#' @return rawdata
#' @export
rawdata <- function(data = data.frame(),
                    chs = data.frame(),
                    device_info = NULL,
                    subj_info = NULL,
                    meas_date= NULL,
                    meas_start = NULL,
                    sfreq = NULL,
                    samps = "samp_num",
                    bads = NULL
                    ){

  info <- info(chs = chs,
               device_info = device_info,
               subj_info = subj_info,
               meas_date = meas_date,
               meas_start = meas_start,
               sfreq = sfreq,
               samps = samps,
               bads = bads)

  new_rawdata(data, info)
}

#' @export
print.rawdata <- function(x, ...){

  info <- x[["info"]]
  data <- x[["data"]]


  info
  head(data)
  invisible(x)
}

#' @export
plot.rawdata <- function(x, ...){

  data <- x[["data"]]
  info <- x[["info"]]

  plt <- ggplot2::ggplot(data,
                         ggplot2::aes(x = samp_num))

}

#' @export
adjust_start.rawdata <- function(x, adj_by){
  info = x[["info"]]

  info <- adjust_start(info, adj_by)

  x
}

#' @export
adjust_end.rawdata <- function(x, adj_by){

  info = x[["info"]]

  info <- adjust_end(info, adj_by)

  info
}



#' Info Class Constructor
#'
#' @param info list
#'
#' @return info list
#'
#'
#' @export
new_info <- function(info = list()){
  stopifnot(is.list(info))

  validate_info(info)

  inherit <- class(info)

  structure(info, class = c("info", inherit))
}

#' Info Class Validator
#'
#' @param info list
#'
#' @return list
#'
#' @export
validate_info <- function(info){

  info
}

#' Info Class
#'
#' @param chs data frame of channels in measurement where rows are channels
#' and columns are ch_name, ch_type, source_num, det_num, wavelength
#' @param device_info list containing device information
#' device type, model, serial, site
#' @param subj_info list containing subject information
#' @param meas_date datetime of the measurement
#' @param meas_start time to crop off beginning of file
#' @param sfreq sampling frequency in Hz
#' @param samps name of the column containing sample numbers. Defaults to
#' "samp_num" which is the default name if you use nirsex::read_nirs,
#' @param bads list of channels that have bad signal
#'
#' @return info
#'
#' Contains all metadata associated with the recording.
#'
#' @export
info <- function(chs = data.frame(),
                 device_info = NULL,
                 subj_info = NULL,
                 meas_date = NULL,
                 meas_start = NULL,
                 sfreq = NULL,
                 samps = "samp_num",
                 bads = NULL){

  bounds = list("meas_start" = meas_start,
                "meas_end" = NULL,
                "first_samp" = NULL,
                "last_samp" = NULL)

  info = list("chs" = chs,
              "device_info" = device_info,
              "subj_info" = subj_info,
              "meas_date" = meas_date,
              "bounds" = meas_start,
              "sfreq" = sfreq,
              "samps" = NULL,
              "bads" = bads
              )

  new_info(info)
}

#' @export
print.info <- function(x, ...){

  nchan = length(x[["chs"]][["ch_name"]])

  msg_nchan <- glue::glue("Measurement with {nchan} channels.")

  # \\TODO create more informative print method for info obejct
  # needs to be able to ignore missing/NULL elements

  #device_type <- x[["device_info"]][["type"]]
  #device_model <- x[["device_info"]][["model"]]
  #device_serial <- x[["device_info"]][["serial"]]

  #device <- glue::glue("")

  print(msg_nchan)
  invisible(x)
}

#'@export
adjust_start.info <- function(x, adj_by){

  start <- x[["bounds"]][["meas_start"]]

  if((start + adj_by) < 0){
    stop(
      "Cannot adjust meas_start to before data begins.",
      call. = FALSE
    )
  }

  end <- x[["bounds"]][["meas_end"]]

  if((start+adj_by) > end){
    stop(
      "Cannot adjust meas_start to after data ends.",
      call. = FALSE
    )
  }

  new = start + adj_by

  x[["bounds"]][["meas_start"]] <- new

  x
}

#' @export
adjust_end.info <- function(x, adj_by){

  start <- x[["bounds"]][["meas_start"]]
  end <- x[["bounds"]][["meas_end"]]

  if((end + adj_by) < start){
    stop(
      "Cannot adjust meas_end to before data begins.",
      call. = FALSE
    )
  }

  new = end + adj_by

  sfreq = x[["sfreq"]]
  end_samp = x[["bounds"]][["last_samp"]]

  if((new)*sfreq > end_samp){
    warning(
      "Adjusted meas_end after data ends. Setting meas_end to time of last sample.",
      call. = FALSE
    )
    new = end_samp / sfreq
  }

  x[["bounds"]][["meas_end"]] <- new

  x
}



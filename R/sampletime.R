#' nirsex_sampletime record vector constructor
#'
#' list representation of the time since starting the measurement.
#'
#' displays as
#' if sample zero is the first sample, then zeroed_min, zeroed_sec
#' and zeroed_ms will match orig_min, orig_sec, and orig_ms respectively
#'
#' if sample zero is not the first sample, then zeroed_min, zeroed_sec,
#' and zeroed_ms will refer to the time since sample zero
#' and orig_min, orig_sec, and orig_ms will refer to the original time
#'
#' @param sampnum sample number
#' @param orig_min minute of sample
#' @param orig_sec second of sample
#' @param orig_ms millisecond of sample
#' @param zeroed_min minute of sample after zeroed sample
#' @param zeroed_sec second of sample after zeroed sample
#' @param zeroed_ms millisecond of sample after zeroed sample
#'
#' @return nirsex_sampletime
#' @export
new_sampletime <- function(sampnum = integer(),
                           orig_min = integer(),
                           orig_sec = integer(),
                           orig_ms = integer(),
                           zeroed_min = integer(),
                           zeroed_sec = integer(),
                           zeroed_ms = integer()){

  if(!is.integer())


  vctrs::new_rcrd(list(sampnum = sampnum,
                       orig_min = orig_min,
                       orig_sec = orig_sec,
                       orig_ms = orig_ms,
                       zeroed_min = zeroed_min,
                       zeroed_sec = zeroed_sec,
                       zeroed_ms = zeroed_ms),
                  class = "nirsex_sampletime")
}

#' nirsex_sampletime helper
#'
#' @param sampnum integer sample number
#' @param sfreq sample frequency in Hertz
#' @param start_sampnum optional, sample number to set as start, defaults to 1
#' @param start_minute optional, minute to set as start, defaults to 0
#' @param start_second optional, second to set as start, defaults to 0
#'
#' @return nirsex_sampletime
#'
#'
#' @details
#' sampnum remains the same no matter the provided inputs. sampnum always starts
#' with 1, where `sampnum = 1L` is recorded as `orig_min = 0`, `orig_sec = 0`, and
#' `orig_ms` = 0.
#'
#' `orig_min`, `orig_sec`, and `orig_ms` are always calculated using
#' the provided `sfreq` in Hertz to find the time since sample number 1 was
#' collected.
#'
#' if `start_sampnum` or `start_minute` and `start_second` are not provided,
#' `zeroed_min`, `zeroed_sec`, and `zeroed_ms` will match `orig_min`, `orig_sec`,
#' and `orig_ms`.
#'
#' if `start_sampnum` or `start_minute` and `start_second` are provided,
#' `zeroed_min`, `zeroed_sec`, and `zeroed_ms` will be recorded as
#' time since provided `start_sampnum` or `start_minute` and `start_second`.
#'
#' if `start_sampnum`, `start_minute`, and `start_second` are all provided,
#' `start_sampnum` must match or calculate out to match `start_minute` and `start_second`.
#'
#' If only `start_sampnum` is provided, `start_minute` and `start_second` can be
#' caluculated from `start_sampnum`.
#'
#' `start_second` can be the only start parameter provided. If `start_second`
#' is the only start parameter provided and is greater than 60,
#' `start_min` will be calculated from `start_second`.
#'
#'
#' @export
#'
#' @examples
sampletime <- function(sampnum = integer(),
                       sfreq = double(),
                       start_sampnum = 1L,
                       start_minute = 0L,
                       start_second =NA_real_,
                       start_ms = 0L){

  if (start_sampnum < 1L){
    error_neg_argument("start_sampnum")
  } else if (sfreq < 0){
    error_neg_argument("sfreq")
  } else if(start_minute < 0){
    error_neg_argument("start_minute")
  } else if(start_second < 0){
    error_neg_argument("start_second")
  } else if (sampnum < 1L){
    error_neg_argument("sampnum")
  }

  # since sampnum must start with 1 where 1 would be time 0,
  # orig_seconds must subtract 1 from sampnum to begin at time 0
  orig_seconds = (sampnum - 1L) / sfreq

  # sample number that is greater than 1 minute into recording
  # find minutes
  # and subtract from orig_seconds to keep seconds less than 60
  if(orig_seconds >= 60){
    orig_minute = orig_seconds %/% 60
    orig_seconds = orig_seconds - (orig_minute * 60)
  } else{
    orig_minute = 0L
  }

  # if the sampling frequency is less than once per second
  # (i.e. 0.5 Hertz or once every two seconds) then
  # ms are not used.
  if(sfreq <= 1){
    orig_ms = NA_integer_
    zeroed_ms = NA_integer_
  } else if (orig_seconds == 0L){
    orig_ms = 0L
  } else {
    orig_ms = (orig_seconds %% 1) * 10
  }


  if(start_sampnum == 1L & is.na(start_second)){
    new_sampletime(
      sampnum = sampnum,
      orig_min = orig_min,
      orig_sec = orig_sec,
      orig_ms = orig_ms,
      zeroed_min = orig_min,
      zeroed_sec = orig_sec,
      zeroed_ms = orig_ms
    )
  }






}


#' @export
format.nirsex_sampletime <- function(x, ...){

}

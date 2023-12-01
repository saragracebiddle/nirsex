#' Adjust start generic dispatch
#'
#' @param x item to adjust on
#' @param adj_by time to adjust by in seconds
#'
#' @return item with new start time
#' @export
adjust_start <- function(x, adj_by){

  if(!is.double(adj_by)){
    stop(
      "`adj_by` must be numeric.",
      call.= FALSE
      )
  }

  UseMethod("adjust_start")
}


#' @describeIn adjust_start
#' @export
adjust_start.default <- function(x, adj_by){
  cls <- class(x)
  msg <- "There is not an `adjust_start` method for an object of class "

  msg <- paste0(msg, cls, ".")

  print(msg)
}


#' Adjust end generic dispatch
#'
#' @param x object to adjust on
#' @param adj_by time in seconds to adjust by
#'
#' @return item with new end time
#' @export
adjust_end <- function(x, adj_by){

  if(!is.double(adj_by)){
    stop(
      "`adj_by` must be numeric.",
      call.= FALSE
    )
  }

  UseMethod("adjust_end")

}

#' @export
adjust_end.default <- function(x, adj_by){
  cls <- class(x)
  msg <- "There is not an `adjust_end` method for an object of class "

  msg <- paste0(msg, cls, ".")

  print(msg)
}


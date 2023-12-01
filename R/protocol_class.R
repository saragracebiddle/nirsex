#' Protocol Class Constructor
#'
#' @param p data frame
#'
#' @return data frame of class protocol
#' @export
new_protocol <- function(p = data.frame()){
  stopifnot(is.data.frame(p))

  validate_protocol(p)

  structure(p, class = c("nirsex_protocol", "data.frame"))
}

#' Protocol Class Validator
#'
#' @param p data frame
#'
#' @return p
#' @export
validate_protocol <- function(p){

  nm = c("id" = "integer",
         "description" = "character",
         "duration" = "double",
         "endtime" = "double",
         "starttime" = "double")

  if(length(colnames(p)) != length(nm)){
    stop("The protocol must have columns `id`,`description`,`duration`,`starttime`, and `endtime`.",
         call. = FALSE)
  }

  if(!all(colnames(p) %in% names(nm))){
    stop("The protocol must have columns `id`,`description`,`duration`,`starttime`, and `endtime`.",
         call. = FALSE)
  }

  if(!all(names(nm) %in% colnames(p))){
    stop("The protocol must have columns `id`,`description`,`duration`,`starttime`, and `endtime`.",
         call. = FALSE)
  }

  t <- sapply(p, typeof)

  if(!identical(nm[match(names(t), names(nm))], t)){
    stop("The column types do not match what is expected for a protocol.")
  }

  p

}

#' Protocol Class Helper
#'
#' @param descriptions character vector of section descriptions
#' @param duration double vector of section durations
#' @param order optional integer vector
#'
#' @return protocol
#' @export
protocol <- function(descriptions = character(),
                     duration = double(),
                     order = NULL){

  # coerce to correct type
  descriptions = as.character(descriptions)
  duration = as.double(duration)

  # if order is null, create order as 1:length(states)
  if(is.null(order)){
    order <- seq(from = 1, to = length(descriptions), by = 1) |>
      as.integer()
  }

  order = as.integer(order)

  # if durations is a vector of 1,
  # all sections are implied to be the same length
  # if durations is a vector longer than 1,
  # there must be a duration to match each description
  if(length(duration) > 1){
    stopifnot(
      "Lengths of duration and descriptions must be equal, or duration must be a vector of length 1." = length(duration) == length(descriptions))
  }

  if(length(duration) == 1){
    duration = rep(duration, times = length(descriptions))
  }

  stopifnot(is.character(descriptions),
            is.double(duration),
            is.integer(order))

  info <- data.frame(
    id = 1:length(descriptions),
    description = descriptions,
    duration = duration
  )

  p <- info[match(order, info$id),]

  p$endtime <- cumsum(p$duration)

  p$starttime <- p$endtime - p$duration[[1]]

  p = new_protocol(p)

  p
}



#' Print Generic for protocol class
#'
#' @param p protocol
#'
#' @return print to console
#' @export
print.nirsex_protocol <- function(p){
  p = data.frame(p, stringsAsFactors = F)

  print(p[c("description","id", "starttime","endtime","duration")])
}

#' Get locations of transitions to analyze
#'
#' @param order vector of the order of each type of exercise in the protocol
#'
#' @return list containing start and end times
#'
#' @import data.table
#' @export
get_transitions <- function(protocol){

  # if(!("nirsex_protocol" %in% class(protocol))){
  #   stop("Must supply a nirsex_protocol object.")
  # }

  order =as.character( protocol$type )

  a <- unique(order)
  b <- unique(order)

  stringorder <- stringr::str_flatten(
    order
  )

  allcomb <- expand.grid(a,a)


  unq <- data.table::CJ(a,b)

  transitions <-   unq[, list(combo = stringr::str_c(a,b))
  ][, list(combo,
           count = stringr::str_count(stringorder, as.character(combo)))
  ][count>1,
  ]$combo

  locate = stringr::str_locate_all(stringorder, transitions)

  locate |>
    lapply(function(mtx) apply(mtx, MARGIN = 1, as.list))|>
    lapply(function(item) lapply(item, function(inneritem)
      gettimes(l = inneritem, protocol = protocol)))|>
    lapply(function(item)
      setNames(
        item,
        stringr::str_c("Transition_Number_", 1:length(item)))) |>
    setNames(stringr::str_c("Transition_Type_", 1:length(locate)))
}

#' Retrieve transition start and end times from 'protocol' corresponding to
#' list
#'
#' @param l list of 2 items: "start" and "end" that correspond to
#' the rownumber in 'protocol' that the transition starts and ends
#'
#' @return list of 2 items: "start" and "end" that are the time
#' in seconds of the start and end of the transition
gettimes <- function(protocol, l) {

  #l[["start"]] is the ROW, "starttime" is the COLUMN to retrieve from
  #protocol

  #l[["end"]] is the ROW, "endtime" is the COLUMN to retrieve
  # from protocol

  out = list(
    start = protocol[[l[["start"]], "starttime"]],
    end = protocol[[l[["end"]], "endtime"]],
    bllength = protocol[[l[["start"]], "length"]],
    desc = stringr::str_c(protocol[[l[["start"]], "desc"]],
                          "-",
                          protocol[[l[["end"]], "desc"]])
  )


  return(out)
}

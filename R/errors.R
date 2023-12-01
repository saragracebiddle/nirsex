#' Error File Does Not Exist
#'
#' @param file filepath or file name
#'
#' @return message for rlang::abort
error_file_dne <- function(file){
  msg <- glue::glue("{file} does not exist.")

  rlang::abort("error_file_dne",
               message = msg)
}

#' Error File Metadata Missing
#'
#' @param type type of metadata
#'
#' @return message for rlang::abort
error_file_metadata <- function(type){
  msg <- glue::glue("Error finding {type} in file metadata.")

  rlang::abort("error_file_metadata",
               message = msg)
}

#' Error Negative Argument
#'
#' @param arg argument
#'
#' @return message for rlang::abort
error_neg_argument <- function(arg){
  msg <- glue::glue("{arg} cannot be negative.")

  rlang::abort("error_neg_argument",
               message = msg)
}

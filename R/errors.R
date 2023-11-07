#' Error File Does Not Exist
#'
#' @param file filepath or file name
#'
#' @return message for rlang::abort
error_file_dne <- function(file){
  msg <- glue::glue("{file} does not exist.")
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

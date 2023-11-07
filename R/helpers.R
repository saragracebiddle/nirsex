#' Access nirsex example files
#'
#' @param path filename
#'
#' @return path to file
#' @export
#'
#' @examples
#' read_nirs_example()
#' read_nirs_example("opticaldensity_raw.txt")
read_nirs_example <- function(path = NULL){
  if(is.null(path)){
    dir(fs::path_package("extdata", package = "nirsex"))
  } else {
    fs::path_package("extdata", path, package = "nirsex")
  }
}

#' Get the file extension type
#'
#' @param file name of the file
#'
#' @return file extension type
#' * csv
#' * xlsx
#' * txt
#'
#' @export
extract_ext <- function(file){
  exttypes = c('csv','xlsx','txt')

  stringr::str_sub(
    stringr::str_extract(
      file,
      stringr::str_c(
        "(?<=\\.)", exttypes,"$", collapse = "|"
      )
    ),
    start = 2,
    end = -1
  )
}



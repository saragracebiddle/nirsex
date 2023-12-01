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
nirsex_example <- function(path = NULL){
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

    ext = stringr::str_extract(
      file,
      stringr::str_c(
        "(?<=\\.)", exttypes,"$", collapse = "|"
      )
    )

    if(is.na(ext)){
      rlang::abort("error_filetype",
                   message = "File Type {ext} not currently supported. Please input files as .txt, .csv, or .xlsx")
    }

    ext
}



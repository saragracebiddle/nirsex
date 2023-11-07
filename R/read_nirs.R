#' NIRS Data Input
#'
#' @description
#' Reads a file of NIRS data and creates a data frame from it.
#'
#' @param file file to read
#'
#' @return data.frame
#' @export
read_nirs <- function(file, ...){

  # throw file does not exist error if does not exist
  if(!file.exists(file)){
    rlang::abort("error_file_dne",
                 message = msg)
  }

  class(file) <- extract_ext(file)

  UseMethod("read_nirs")
}

#' Read a .txt file of NIRS data
#'
#' @param file filepath
#' @param sep column delimiter
#'
#' @return data frame
read_nirs.txt <- function(file, sep = "\t", colnames = "short"){

  class(file) <- "character"

  s = tryCatch(
    scan(file, n=1000, sep=sep, na.strings="", what=chatacter(), quiet=T, skipNul=T),
    error = function(e){
      rlang::abort("error_in_file_scan",
                   message= "Error when scanning file.")
    }
  )

  numSamples <- get_metadata(s, type = "num_samples")

  hz <- get_metadata(s, type = "hz")

  ot <- get_metadata(s, type = "optode_template")

  legend <- get_metadata(s, type = "legend")

  srows = scan(
    file,
    na.strings = "",
    what = character(),
    sep ="\n",
    skipNul = T,
    quiet = T,
    blank.lines.skip = F
  )

  row1 = stringr::str_which(
    srows,
    stringr::str_c(
      legend[!is.na(V1)][[1]], collapse = "\t"
    )
  ) +1


  problems = suppressWarnings(readr::parse_double(
    scan(
      file,
      na.strings = "",
      sep = "\t",
      what = character(),
      skip = row1,
      skipNul = T,
      quiet = T)
  )
  )

  out = data.frame(
    matrix(
      problems[!is.na(problems)],
      ncol = length(
        legend[!is.na(V1)][[1]][!stringr::str_detect(
          legend[!is.na(V1)][[2]],
          "Event"
        )]
      ),
      byrow = T)
  )

  collabels <- legend[!is.na(V1)][[2]][!stringr::str_detect(
    legend[!is.na(V1)][[2]], "Event"
  )]

  out = data.table::data.table(
    matrix(
      problems[!is.na(problems)],
      ncol = length(legend[!is.na(V1)][[2]][!stringr::str_detect(
        legend[!is.na(V1)][[2]],
        "Event")]
      ),
      byrow = T)
  )

  if(colnames == "short"){
    colnames(out) <- sapply(collabels, get_labels)
  } else {
    colnames(out) <- collabels
  }

  if(ot == "PortaMon TSI"){
    out |>
      dplyr::select(
        !dplyr::matches("T|tx3") & !dplyr::matches("TSI Fit Factor|TSIFF")
      )
  }
}

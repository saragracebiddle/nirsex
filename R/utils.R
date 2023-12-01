#' Get a list of channel names by channel type
#'
#' @param info info object
#'
#' @return list with names "ch_type" and value character vector of
#' respective "ch_names
#'
#' @importFrom purrr map
#' @export
get_ch_names_by_type <- function(info){

  chs <- info[["chs"]]

  ch_types <- unique(chs[["ch_type"]])

  ch_names <- map(ch_types, function(type) chs[chs$ch_type == type, ][["ch_name"]])

  names(ch_names) <- ch_types

  ch_names
}

#' Prepare the rawdata for input into a ggplot function
#'
#' @param rawdata rawdata object
#'
#' @return data.frame
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr reduce
#' @importFrom rlang expr
#' @importFrom tidyr pivot_longer
#' @export
prep_plot_data <- function(rawdata){

  prep <- rawdata[["data"]][setdiff(names(rawdata[["data"]]), rawdata[["info"]][["bads"]])]

  ch_names_by_type <- get_ch_names_by_type(rawdata[["info"]])

  exps <- map2(ch_names_by_type, names(ch_names_by_type),
               ~ expr(!!.x ~ !!.y))

  match_exp <- purrr::reduce(exps, ~ expr(!!.x, !!.y))

  prep |>
    pivot_longer(cols = !samp_num,
    names_to = "ch_name",
    values_to = "value") |>
    mutate(ch_type = case_match(ch_name,
                                match_exp))

}

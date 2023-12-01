#' Title
#'
#' @param rawdata
#'
#' @return ggplot
#' @export
#'
#' @importFrom dplyr case_match
#' @importFrom dplyr mutate
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 facet_grid
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr reduce
#' @importFrom rlang .data
#' @importFrom rlang expr
#' @importFrom tidyr pivot_longer
#' @examples
plot_rawdata <- function(rawdata){

  prep <- rawdata$data

  ch_names_by_type <- get_ch_names_by_type(rawdata$info)

  exps <- map2(names(ch_names_by_type), ch_names_by_type, expr(!!.x ~ !!.y))

  match_exp <- reduce(exps, ~expr(!!.x , !!.y))

  #removechannels marked as "bads"
  prep |>
    pivot_longer(cols = setdiff(rawdata$info$chs$ch_name, rawdata$info$bads),
                 names_to = "ch_name",
                 values_to = "value") |>
    mutate(ch_type  = case_match(ch_name,
                                 match_exp))



  layers <- map(pltcols, ~ geom_line(aes(y = {{.x}})))

  plt <- ggplot(rawdata$data, aes(x = .data[[samp_num]]))+
    layers +
    facet_grid()

  layers <- lapply(geom_line)

}

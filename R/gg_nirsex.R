#' Monoexponential function from coefficients
#'
#' @param Amp amplitude
#' @param Baseline baseline
#' @param TD time delay
#' @param tau time constant
#'
#' @return function
#' @export
gg_monoexp <- function(Amp, Baseline, TD, tau){
  function(x) Baseline + Amp*(1-exp(-(x-TD)/tau))
}

#' Create data.frame of values of monoexponential curve from coefficients
#'
#' @param xo start time
#' @param xf end time
#' @param xbin time increments
#' @param Amp amplitude
#' @param Baseline baseline
#' @param TD time delay
#' @param tau time constant
#'
#' @return data.frame
#' @export
create_monoexpgg <- function(xo, xf, xbin, Amp, Baseline, TD, tau){
  x = seq(xo, xf, by = xbin)
  f = gg_monoexp(Amp, Baseline, TD, tau)

  y = f(x)

  data.frame(x = x,
             y = y)
}






#' ggplot2 geom for a monoexponential curve
#'
#' @param mapping ggplot2 mapping
#' @param data ggplot2 data
#' @param stat ggplot2 stat name
#' @param position ggplot2 position
#' @param na.rm remove nas
#' @param show.legend show legend
#' @param inherit.aes inherit from parent
#' @param xo time start
#' @param xf time end
#' @param xbin time increments
#' @param ... other parameters
#'
#' @return ggplot2 stuff
#' @export
geom_monoexp = function(mapping = NULL,
                        data = NULL,
                        stat = "monoexp",
                        position = "identity",
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        xo = 0,
                        xf = 120,
                        xbin = 1,
                        ...

){
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      xo = xo,
      xf = xf,
      xbin = xbin,
      na.rm = na.rm,
      ...
    )
  )
}

#' ggplot2 custom geom for tau as a point on a line
#'
#' @param mapping ggplot2 mapping
#' @param data ggplot2 mapping
#' @param stat ggplot2 stat
#' @param position  ggplot2 position
#' @param ... other parameters
#' @param na.rm remove nas
#' @param show.legend show legend
#' @param inherit.aes inherit from parent
#'
#' @return ggplot2 stuff
#' @export
geom_tau = function(mapping = NULL,
                        data = NULL,
                        stat = "tau",
                        position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE
){
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' ggplot2 stat for monoexponential curves
#'
#' @param mapping ggplot2 mapping
#' @param data ggplot2 data
#' @param geom ggplot2 geom
#' @param position ggplot2 position
#' @param na.rm remove na
#' @param show.legend show legend
#' @param inherit.aes inherit parent aes
#' @param xo xstart
#' @param xf xend
#' @param xbin x increment by
#' @param ... other parameters
#'
#' @return ggplot2 layer
#' @export
stat_monoexp <- function(mapping = NULL,
                         data = NULL,
                         geom = "line",
                         position = "identity",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         xo = 0,
                         xf = 120,
                         xbin = 1,
                         ...
                         ) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatMonoexp,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      xo = xo,
      xf = xf,
      xbin = xbin,
      na.rm = na.rm,
      ...
    )
  )
}

#' ggplot2 stat tau points
#'
#' @param mapping ggplot2 mapping
#' @param data ggplot2 data
#' @param geom ggplot2 geom
#' @param position ggplot2 position
#' @param ... other parameters
#' @param na.rm remove na
#' @param show.legend show legend
#' @param inherit.aes inherit aes
#'
#' @return ggplot2 layer
#' @export
stat_tau <- function(mapping = NULL,
                         data = NULL,
                         geom = "point",
                         position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatTau,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}



#' ggplot2 geom for monoexpoential curve with tau points
#'
#' @param mapping ggplot2 mapping
#' @param data ggplot2 data
#' @param stat ggplot2 stat
#' @param position ggplot2 position
#' @param ... other parameters to pass
#' @param na.rm remove na
#' @param show.legend show legend
#' @param inherit.aes inherit aes
#'
#' @return ggplot2 layer
#' @export
geom_monoexptau <- function(mapping = NULL,
                            data = NULL,
                            stat = "identity",
                            position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMonoexpTau,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}





#' ggplot2 facet from the bottom to the top
#'
#' @param facets facets
#' @param nrow number of rows
#' @param ncol number of columns
#' @param strip.position where to put the facet strip; default to 'top'
#' @param labeller labeller for facets; defaults to 'label_value'
#'
#' @return ggplot2 ggproto object
#' @export
facet_bottomup = function(facets,
                          nrow,
                          ncol,
                          strip.position = "top",
                          labeller = "label_value"){
  ggproto(NULL, FacetBottomUp,
          params = list(
            facets = rlang::quos_auto_name(facets),
            strip.position = strip.position,
            labeller = labeller,
            ncol = ncol,
            nrow = nrow))
}

#' plot results of monoexponential regression
#'
#' @param results list of results
#'
#' @return cowplot
#' @export
plot_all_results <- function(results, cols){

  plots <- lapply(results, function(item)
  plot_results(item, cols = cols)
  )

  cowplot::plot_grid(plotlist = plots)
}


#' plot result from a single monoexponential regression
#'
#' @param resultslist list of regression output
#' @param signal nirs signal type
#' @param timecol name of time column
#'
#' @return ggplot2
#' @export
plot_one_result<- function(resultslist, signal, timecol = "Time"){
  coefs <- resultslist[["coefs"]][signal == signaltype,
                                  env = list(signaltype = I(signal))]

  resultslist[["data"]]|>
    ggplot2::ggplot(
      ggplot2::aes(x = .data[[timecol]],
                   y = .data[[signal]])
    )+
    ggplot2::geom_point(shape = 1)+
    ggplot2::theme_bw()+
    ggplot2::geom_segment(
      x = -30,
      y = coefs[["baseline"]],
      xend = 0,
      yend = coefs[["baseline"]],
      color = 'red'
    )+
    ggplot2::geom_function(
      xlim = c(0,120),
      fun = function(x) coefs[["baseline"]] + coefs[["Amp"]]*(1-exp(-(x-coefs[["TD"]])/coefs[["tau"]])),
      color = 'red'
    )
}

#' Create a plot for the results of one transition type
#'
#' @param resultslist list of results for the transition type
#' @param cols variables to facet by
#' @param timecol column of time data
#'
#' @return ggplot
#' @export
plot_results <- function(resultslist, cols, timecol= "Time"){

  plots <- lapply(cols,function(col) plot_one_result(signal = col,
                                            resultslist = resultslist))

  cowplot::plot_grid(plotlist = plots, ncol = 1)
}

#' plot averaged data
#'
#' @param dt averaged across signals
#' @param timecol column that contains the time
#' @param cols cols to facet
#'
#' @return ggplot object
#' @export
plot_averageddata = function(dt, timecol = "Time", cols){

    dt |>
      melt(id.vars = timecol,
           measure.vars = cols,
           value.name = 'value',
           variable.name = 'facets')|>
      ggplot2::ggplot(
        ggplot2::aes(x = .data[[timecol]],
                     y = value)
      )+
      ggplot2::facet_grid(rows = ggplot2::vars(facets))+
      ggplot2::geom_line()

}

#' plot raw data for a transition type
#'
#' @param timealigneddt data table that has been time aligned
#' with columns timecol, group, and variable cols
#' where group is a character describing which transition
#' the data is in
#' @param timecol character of column name containing times
#' @param cols variable columns to facet
#'
#' @return ggplot
#' @export
plot_rawtransitions = function(timealigneddt, timecol="Time", cols, name){
  timealigneddt |>
    melt(id.vars = c(timecol, "group"),
         measure.vars = cols,
         variable.name = "signal",
         value.name = "value")|>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[[timecol]],
        y = value,
        color = group
      )
    )+
    ggplot2::facet_grid(rows = ggplot2::vars(signal),
                        scales = "free_y")+
    ggplot2::geom_line()+
    ggplot2::geom_vline(xintercept = 0)+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "none")+
    ggplot2::ggtitle(paste(name))

}

#' Display a plot of all raw transition data
#'
#' @param l list of timealigned transition data
#' @param cols cols to facet by
#'
#' @return cowplot
#' @export
plot_all_raw_transitions = function(l, cols){
  raw_transitions = lapply(
    names(l), function(nm) plot_rawtransitions(
      timealigneddt = l[[nm]]
      , cols = cols
      , name = nm)
  )

  cowplot::plot_grid(plotlist = raw_transitions)
}


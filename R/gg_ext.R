#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
StatMonoexp = ggplot2::ggproto("StatMonoexp", ggplot2::Stat,
  setup_data = function(data, params){
    if (anyDuplicated(data$group)){
      data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
    }
    data
  },
  compute_panel = function(data, scales, xo = 0,xf = 120,xbin=1){
                                 cols_to_keep = setdiff(names(data),
                                                        c("Amp","Baseline","TD","tau"))

                                 monos = lapply(
                                   seq_len(nrow(data)),
                                   function(i) {
                                     mono_line = monoexp(Amp = data$Amp[i],
                                                         Baseline = data$Baseline[i],
                                                         TD = data$TD[i],
                                                         tau = data$tau[i],
                                                         xlen = xlen)
                                     cbind(mono_line, unclass(data[i, cols_to_keep]) )
                                   }
                                 )

                                 do.call(rbind, monos)
                               },
                               required_aes = c("Amp","Baseline","TD","tau")
)

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
StatTau = ggplot2::ggproto("StatTau", ggplot2::Stat,

                           setup_data = function(data, params){
                             if (anyDuplicated(data$group)){
                               data$group <- paste(data$group, seq_len(nrow(data)),
                                                   sep = "-")
                             }
                             data
                           },

                           compute_panel = function(data, scales){
                             cols_to_keep = setdiff(names(data),
                                                    c("Amp","Baseline","TD","tau"))

                             monos = lapply(
                               seq_len(nrow(data)),
                               function(i) {
                                 f = gg_monoexp(data$Amp[i],
                                                data$Baseline[i],
                                                data$TD[i],
                                                data$tau[i])
                                 y = f(data$tau[i])
                                 taupoint = data.frame(x =data$tau[i], y = y)
                                 cbind(taupoint, unclass(data[i, cols_to_keep]) )
                               }
                             )

                             do.call(rbind, monos)
                           },
                           required_aes = c("Amp","Baseline","TD","tau")
)

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
GeomMonoexpTau <- ggplot2::ggproto("GeomMonoexpTau", ggplot2::Geom,

                                   required_aes = c("Amp","Baseline","TD","tau"),

                                   default_aes = ggplot2::aes(
                                     color = "black",
                                     linewidth = .5,
                                     size = 2,
                                     linetype = 1,
                                     shape = 19,
                                     fill = NA,
                                     alpha = NA,
                                     stroke = 1,
                                     xo = 0,
                                     xf = 120,
                                     xbin = 1
                                   ),

                                   setupdata = function(data, params){
                                     if (anyDuplicated(data$group)){
                                       data$group <- paste(data$group, seq_len(nrow(data)),
                                                           sep = "-")
                                     }
                                     data
                                   },

                                   draw_panel = function(data, panel_params, coord, ...) {

                                     cols_to_keep = setdiff(names(data),
                                                            c("Amp","Baseline","TD","tau"))

                                     taup = lapply(
                                       seq_len(nrow(data)),
                                       function(i) {
                                         f = gg_monoexp(data$Amp[i],
                                                        data$Baseline[i],
                                                        data$TD[i],
                                                        data$tau[i])
                                         y = f(data$tau[i])
                                         taupoint = data.frame(x =data$tau[i], y = y)
                                         cbind(taupoint, unclass(data[i, cols_to_keep]) )
                                       }
                                     )

                                     taup =do.call(rbind, taup)


                                     monos = lapply(
                                       seq_len(nrow(data)),
                                       function(i) {
                                         mono_line = create_monoexpgg(0, 120, 1,
                                                                      data$Amp[i],
                                                                      data$Baseline[i],
                                                                      data$TD[i],
                                                                      data$tau[i])
                                         cbind(mono_line, unclass(data[i, cols_to_keep]) )
                                       }
                                     )

                                     monos = do.call(rbind, monos)

                                     # Return all three components
                                     grid::gList(
                                       GeomLine$draw_panel(monos, panel_params, coord, ...),
                                       GeomPoint$draw_panel(taup, panel_params, coord, ...)
                                     )
                                   }
)

#' @rdname ggraph-extensions
#' @format NULL
#' @usage NULL
#' @export
FacetBottomUp <- ggplot2::ggproto("FacetBottomUp", ggplot2::FacetWrap,

                                  setup_params = function(data, params) {
                                    params <- FacetWrap$setup_params(data, params)
                                    params$free <- list(x = FALSE, y = FALSE)
                                    return(params)
                                  },

                                  compute_layout = function(data, params) {

                                    # create a data frame with one column per facetting
                                    # variable, and one row for each possible combination
                                    # of values (i.e., one row per panel)
                                    panels <- combine_vars(
                                      data = data,
                                      env = params$plot_env,
                                      vars = params$facets,
                                      drop = FALSE
                                    )

                                    # Create a data frame with columns for ROW and COL,
                                    # with one row for each possible cell in the panel grid
                                    locations <- expand.grid(ROW = 1:params$nrow, COL = 1:params$ncol)
                                    locations = locations[order(locations$ROW),]

                                    # Randomly sample a subset of the locations
                                    o = c(seq(1, params$ncol-1), seq(params$ncol+1, params$nrow*params$ncol))

                                    # Assign each panel a location
                                    layout <- data.frame(
                                      PANEL = 1:nrow(panels),       # panel identifier
                                      ROW = locations$ROW[o], # row number for the panels
                                      COL = locations$COL[o], # column number for the panels
                                      SCALE_X = 1L,                 # all x-axis scales are fixed
                                      SCALE_Y = 1L                  # all y-axis scales are fixed
                                    )

                                    # Bind the layout information with the panel identification
                                    # and return the resulting specification
                                    return(cbind(layout, panels))
                                  }

)

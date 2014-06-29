#' Add guide colours
#'
#' @param labels defaults to \code{waiver()}. Can be for example,
#' \code{percent}
#' @param ... additional arguments passed to scale functions
#'
#' @export

scale_fill_heat <- function(labels = waiver(), ...){
  scale_fill_gradient2(low = 'royalblue1', high = 'firebrick1',
    na.value = 'grey65', labels = labels, ...)
}

#' Add guide colours
#'
#' @param ... additional arguments passed to scale functions
#'
#' @export
scale_colour_heat <- function(...){
  scale_colour_gradient2(na.value = "grey65",
                         low = "royalblue1", high ="firebrick1",
                         labels = percent, ...)
}

#' Add guide size
#'
#' @param colour colour of size bubbles in legend. Default is \code{black}.
#' @param ... additional arguments passed to scale functions
#'
#' @export
scale_colour_size <- function(colour = "black", ...){
  guides(size=guide_legend(override.aes=list(colour=colour), ...))
}

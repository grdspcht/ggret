#' stat_reticulation
#'
#' Plots reticulation edges for tree-based networks defined as
#' ape evonet objects

library(ggplot2)
library(ggtree)

# create_ret <- function(arg){
#   plot <- ggplot(arg) + geom_tree()
#   ret <- as.data.frame(arg$reticulation)
#   fromx <- c()
#   fromy <- c()
#   tox <- c()
#   toy <- c()
#
#   # Get coordinates for every reticulation
#   for(i in 1:nrow(ret)){
#     from <- ret[i,1]
#     to <- ret[i,2]
#     fromx <- c(fromx, plot$data$x[which(plot$data$node == from)])
#     fromy <- c(fromy, plot$data$y[which(plot$data$node == from)])
#     tox <- c(tox, plot$data$x[which(plot$data$node == to)])
#     toy <- c(toy, plot$data$y[which(plot$data$node == to)])
#
#   }
#
#   data.frame(
#     x = fromx,
#     y = fromy,
#     xend = tox,
#     yend = toy)
# }

create_ret <- function(arg, i){
  plot <- ggplot(arg) + geom_tree()
  ret <- as.data.frame(arg$reticulation)
  fromx <- c()
  fromy <- c()
  tox <- c()
  toy <- c()

  # Get coordinates for every reticulation
    from <- ret[i,1]
    to <- ret[i,2]
    fromx <- c(fromx, plot$data$x[which(plot$data$node == from)])
    fromy <- c(fromy, plot$data$y[which(plot$data$node == from)])
    tox <- c(tox, plot$data$x[which(plot$data$node == to)])
    toy <- c(toy, plot$data$y[which(plot$data$node == to)])

  data.frame(
    x = fromx,
    y = fromy,
    xend = tox,
    yend = toy)
}

# StatReticulation <- ggproto("StatReticulation", Stat,
#                       # setup_data = function(data, params) {
#                       #   if (anyDuplicated(data$group)) {
#                       #     data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
#                       #   }
#                       #   data
#                       # },
#                       compute_panel = function(data, scales){
#                         cols_to_keep <- setdiff(names(data), c("x", "xend", "y", "yend"))
#                         rets <- lapply(seq_len(nrow(data)), function(i) {
#                           ret_coords <- create_ret(data)
#                           cbind(ret_coords, unclass(data[i, cols_to_keep]))
#                         })
#                         do.call(rbind, rets)
#                       },
#                       required_aes = c("x", "y", "xend", "yend")
# )

StatReticulation <- ggproto("StatReticulation", Stat,
                            # setup_data = function(data, params) {
                            #   if (anyDuplicated(data$group)) {
                            #     data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
                            #   }
                            #   data
                            # },
                            compute_panel = function(data, scales){
                              cols_to_keep <- setdiff(names(data), c("x", "y", "xend", "yend"))
                              rets <- lapply(1:(nrow(data$reticulation)), function(i) {
                                ret_coords <- create_ret(data, i)

                                cbind(ret_coords, unclass(data[i, cols_to_keep]))
                              })
                              do.call(rbind, rets)
                            },
                            #required_aes = c("x", "y", "xend", "yend")
)

# geom_reticulation <- function(mapping = NULL,
#                         data = NULL,
#                         stat = "reticulation",
#                         position = "identity",
#                         ...,
#                         arrow = NULL,
#                         lineend = "butt",
#                         linejoin = "round",
#                         na.rm = FALSE,
#                         show.legend = NA,
#                         inherit.aes = TRUE
# ) {
#   layer(
#     data = data,
#     mapping = mapping,
#     stat = stat,
#     geom = GeomSegment,
#     position = position,
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     params = list(
#       arrow = arrow,
#       lineend = lineend,
#       linejoin = linejoin,
#       na.rm = na.rm,
#       ...
#     )
#   )
# }

stat_reticulation <- function(mapping = NULL, data = NULL, geom = "segment",
                        position = "identity", ..., na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatReticulation,
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

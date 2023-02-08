#' Adds tree-based network layer
#' @importFrom ggplot2
#' @import ggplot2
#' @import ggtree
#' @param arg A tree-based network
#' @return ARG plot
#' @examples

ggarg <- function(arg){
  plot <- ggplot(arg) + geom_tree()  + geom_tiplab()
  ret <- as.data.frame(arg$reticulation)
  fromx <- c()
  fromy <- c()
  tox <- c()
  toy <- c()

  # Get coordinates for every reticulation
  for(i in 1:nrow(ret)){
    from <- ret[i,1]
    to <- ret[i,2]
    fromx <- c(fromx, plot$data$x[which(plot$data$node == from)])
    fromy <- c(fromy, plot$data$y[which(plot$data$node == from)])
    tox <- c(tox, plot$data$x[which(plot$data$node == to)])
    toy <- c(toy, plot$data$y[which(plot$data$node == to)])

  }

  ret <- cbind(ret, fromx, fromy, tox, toy)

  # Plot reticulations and backbone tree
  plot = plot + geom_segment(data = ret, aes(x = fromx, y=fromy, xend=tox, yend=toy, colour = "red" ))
  return(plot)
}

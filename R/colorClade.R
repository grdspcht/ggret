#' group clades
#'
#' @param data evonet or treedata object
#' @param nodes nodes that define the clade
#' @param cladename Name of the clade
#' @param tiponly Should only tips be considered?
#'
#' @return
#' @export
#'
#' @examples
colorClade <- function(data, nodes, cladename, tiponly = FALSE) {
  mrca = treeio::MRCA(data, nodes)
  offsprings = treeio::offspring(data, mrca, tiponly)
  if (is(data, "evonet")) {
    labs <- c(data$tip.label, data$node.label)
    clades <- rep("Undefined", length(labs))
    for (u in cladename) {
      clades[which(labs %in% nodelab(data, offsprings))] <- u
    }

    attr(data, "clade") <- clades
  } else if (is(data, "treedata")) {
    labs <- c(data@phylo[["tip.label"]], data@phylo[["node.label"]])
    clades <- rep(NA, length(labs))
    for (u in cladename) {
      clades[which(labs %in% nodelab(data@phylo, offsprings))] <- u
    }

    attr(data@phylo, "clade") <- clades
  } else{
    warning("Function only supports evonet and treedata objects.")
    stop()
  }

  return(data)
}

findRetLayer <- function(layer, rets){
  retrow <- subset(layer[rets,], select = c("x", "y", "xend", "yend"))
  rest <- subset(layer[-rets,], select = c("x", "y", "xend", "yend"))
  if (anyNA(retrow) == FALSE & anyNA(rest) == TRUE){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' Change reticulation coloring
#'
#' @param plot ggplot object
#' @param assignto  Reticulation colour can be assigned to "donor" or "receiver"
#'
#' @return
#' @export
#'
modifyRet <- function(plot, assignto="donor"){
  # Diassemble ggplot object and extract needed variables
  decon <- ggplot_build(plot)
  data <- decon$plot$data
  layout <- attributes(data)$layout
  layers <- decon$data
  rets <- which(data$isRet == TRUE)
  nnode <- nrow(data)
  retlayers <- c()
  lablayers <- c()

# Iterate over plot layers and detect the reticulation layers and
  for (i in 1:(length(layers))){
    if(nrow(layers[[i]]) == nnode & all(c("x", "y", "xend", "yend") %in% colnames(layers[[i]]))){
      if(findRetLayer(layers[[i]], rets) == TRUE){
        retlayers <- c(retlayers, i)
      }
    } else if (all(data[rets, ]$label %in% data$data[[i]]$label)){

        lablayers <- c(lablayers, i)
    }
  }
  if(length(retlayers) == 0){
    stop("No reticulation layers detected. Aborting")
  }



# reassign colours in reticulation layers
  for (retl in retlayers){
    if(assignto == "donor"){
      donors <- data$donor[rets]
      layers[[retl]]$colour[rets] <- layers[[retl]]$colour[donors]
    }else if(assignto == "receiver"){
      rec <- data$node[rets]
      layers[[retl]]$colour[rets] <- layers[[retl]]$colour[rec]
    }else{
      stop("Reticulations can currently only be reassigned to donors or receivers. Aborting...")
    }
  }

  decon$data <- layers
  new <- ggplot_gtable(decon)
  grid::grid.newpage()
  grid::grid.draw(new)

}



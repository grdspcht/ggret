# TODO how to deal with reticulations within and outside of clades?

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

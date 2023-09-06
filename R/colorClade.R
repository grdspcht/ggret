# TODO how to deal with reticulations within and outside of clades?

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
    clades <- rep("Undefined", length(labs))
    for (u in cladename) {
      clades[which(labs %in% nodelab(data@phylo, offsprings))] <- u
    }

    attr(data@phylo, "clade") <- clades
  } else{
    warning("Function only supports evonet and treedata objects.")
    break()
  }

  return(data)
}

##TEST SCRIPT
# library(ggtree)
# MCC.arg=read.beast("../arthur.tree")
# #MCC.arg=MCC.arg@phylo
#
# MCC.arg <- colorClade(MCC.arg, nodes = c("3", "2"), "clade1")
#
# MCC.arg <- fortify.treedata(MCC.arg)
#
# ggplot(MCC.arg, aes(color=clade)) + geom_arg(retcol = "black")  + geom_tiplab(size=2) + geom_nodelab(size=2, vjust=-0.5, hjust=-.1) + theme_tree()
# # geom_nodelab(aes(label=posterior), color= "black",hjust=1.5,vjust=-0.5) +

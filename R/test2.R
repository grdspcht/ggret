# tr <- read.enewick2("../ARG.newick")
# tr <- phylo
# ftr <- fortify.evonet(tr)
# p <- ggplot(ftr) + geom_arg(retcol = "black")  + geom_tiplab(size=2) + geom_nodelab(size=2, vjust=-0.5, hjust=-.1) + theme_tree()
# #p <- ggplot(ftr) + geom_arg(retcol = "black", arrow = F) + geom_nodelab() + geom_tiplab() + geom_retlab() + geom_range(fin@data["height_95%_HPD"], color='grey', size=2, alpha=.75) + theme_tree()
# # # pdf("treeplot.pdf", paper = "a4r")
# plot(p)
# # dev.off()

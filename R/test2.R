# library(ggtree)
#
# #tr <- read.enewick2("../ARG.newick")
# nw <- read.beast("../arthur.tree")
#
# #tr <- colorClade(tr, c("2", "3"), "clade_A")
# #tr <- colorClade(tr, c("1"), "Outgroup")
# # tr <- phylo
# # ftr <- fortify.evonet(tr)
# #fnw <- fortify.treedata(nw)
# p <- ggplot(nw) + geom_arg() + theme_tree() + geom_tiplab() + geom_nodelab() +  geom_range(nw@data["height_95%_HPD"], color='grey', size=2, alpha=.75)
# # p <- ggplot(ftr) + geom_arg(retcol = "black", arrow = F) + geom_nodelab() + geom_tiplab() + geom_retlab() + geom_range(fin@data["height_95%_HPD"], color='grey', size=2, alpha=.75) + theme_tree()
# # pdf("treeplot.pdf", paper = "a4r")
# plot(p)


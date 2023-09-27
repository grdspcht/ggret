# library(ggtree)
#
# #tr <- read.enewick2("../ARG.newick")
# nw <- read.beast("../Data/HVB_upd_bact_max5.summary.acg")
#
#
# #tr <- colorClade(tr, c("2", "3"), "clade_A")
# #tr <- colorClade(tr, c("1"), "Outgroup")
# # tr <- phylo
# # ftr <- fortify.evonet(tr)
# #fnw <- fortify.treedata(nw)
# #nw <- colorClade(nw, c("AB111946|Vietnam|C1|0", "AB048704|Australia|C4|0"), "C")
# p <- ggplot(nw, aes(color=clade)) + geom_arg(rettype = "snake", layout = "rectangular", na.rm = T) + theme_tree2() + geom_tiplab() + geom_nodelab() +  geom_range("region", color='red', size=2, alpha=.5)
# p <- ggplot(nw) + geom_arg(rettype = "snake", layout = "rectangular", na.rm = T) + theme_tree2() + geom_tiplab() + geom_nodelab() +  geom_range("region", color='red', size=2, alpha=.5)
# # p <- ggplot(ftr) + geom_arg(retcol = "black", arrow = F) + geom_nodelab() + geom_tiplab() + geom_retlab() + geom_range(fin@data["height_95%_HPD"], color='grey', size=2, alpha=.75) + theme_tree()
# # pdf("treeplot.pdf", paper = "a4r")
# plot(p)


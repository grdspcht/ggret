tr <- read.enewick2("../simple_ARG.newick")
ftr <- fortify.evonet(tr)
p <- ggplot(ftr) + geom_arg() + geom_nodelab() + geom_tiplab() + theme_tree()
plot(p)



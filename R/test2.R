tr <- read.enewick2("../simple_ARG.newick")
ftr <- fortify.evonet(tr)
ggtree(ftr)



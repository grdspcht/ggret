#read retnet in newick format (creates an evonet object by default)
evo = read.enewick("data/retnet.nwk")
#read retnet in nexus format (creates treedata object by default)
trd =read.beast("data/retnet.nexus")
#convert evonet into treedata
treeio::as.treedata(evo) #doesn't work
#extract evonet object from treedata object
trd@phylo -> evo2
#and back to treedata
trd2 <- treeio::as.treedata(evo2) #works
#one difference between retnet.evonet and retnet.evonet.2 is the "class" attribute

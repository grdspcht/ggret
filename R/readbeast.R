library(treeio)
library(ape)
library(tibble)
library(stringr)

clean.colnames <- function(colnames){
  clean <- gsub("^\\[&|, |=$", "", colnames)
  clean <- unique(clean)
  return(clean)
}

get.colnames <- function (treeTexts){
  extractedCols<- str_extract_all(treeTexts, "\\[&(\\w+)=|, (.*?)=")
  extrUni <- unique(unlist(extractedCols))
  return(extrUni)
}

file <- normalizePath("../Data/HVB_upd_bact_max10_new.summary.acg")
treefile <- readLines(file)

treesIndex <- grep("tree .* = ", treefile)
treeTexts <- treefile[treesIndex]
#treeNames <- sub("^tree (.*) = ",  "", treeTexts, invert= TRUE)

# get trees
treeTexts <- sub("^tree (.*) = ",  "", treeTexts)

phylo <- read.enewick2(text = treeTexts)

# add temporary node and tip labels to network if needed
if(any(phylo$node.label == "")){
  emptyNodes <- which(phylo$node.label == "")
  phylo$node.label[emptyNodes] <- paste0("Node", seq(1:length(emptyNodes)))
}

if(any(phylo$tip.label == "")){
  emptyTips <- which(phylo$tip.label == "")
  phylo$tip.label[emptyTips] <- paste0("Tip", seq(1:length(emptyTips)))
}
pwl <- write.evonet(phylo)
phyLabel <- read.enewick2(text=pwl)

# Get metadata attributes (column names)
extrdCols <- get.colnames(treeTexts)
cleanCols <- clean.colnames(extrdCols)


nodesindex <- str_extract_all(pwl, "([[:alnum:]]+):|(#[[:alnum:]]+):")
nodesindex <- unlist(nodesindex)


# returns a vector of the positions of
labels <-  c(phyLabel$tip.label, phyLabel$node.label)
ordered <- match(nodesindex,labels )

dynlist <- list()
for (col in cleanCols){
  dynlist[[col]] <- rep(NA, length(nodesindex))
}

for (node in nodesindex){
  if(grepl("#", node)){
    next
  }else
  searchstring <- paste0(node, "\\[.*?\\]")
  nodedata <- str_extract_all(treeTexts, searchstring)
}


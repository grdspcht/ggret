library(treeio)
library(ape)
library(tibble)

clean.colnames <- function(colnames){
  clean <- gsub("^\\[&|, |=$", "", colnames)
  clean <- unique(clean)
  return(clean)
}

get.colnames <- function (treeTexts){
  extractedCols<- str_extract_all(treeTexts, "\\[&(\\w+)=|, (.*?)=")
  extrUni <- unique(unlist(extr))
  return(extrUni)
}

file <- normalizePath("../Data/HVB_upd_bact_max10_new.summary.acg")

treefile <- readLines(file)

treesIndex <- grep("tree .* = ", treefile)
treeTexts <- treefile[treesIndex]
#treeNames <- sub("^tree (.*) = ",  "", treeTexts, invert= TRUE)

treeTexts <- sub("^tree (.*) = ",  "", treeTexts)

phylo <- read.enewick2(text = treeTexts)

extrdCols <- get.colnames(treeTexts)
cleanCols <- clean.colnames(extrdCols)

nodesindex <- str_extract_all(treeTexts, "[^[:alnum:]]([[:alnum:]]+)\\[&|[^[:alnum:]](#[[:alnum:]]+)\\[&")
nodesindex <- unlist(nodesindex)
nodesindex <- gsub(pattern = "^[\\(\\)\\,]|\\[&$","", nodesindex) # Get rid of any leading brackets or commas, also remove trailing "[&"

dynlist <- list()
for (col in cleanCols){
  dynlist[[col]] <- rep(NA, length(nodesindex))
}

for (node in nodesindex){
  if(grepl("#", node)){

  }else
  searchstring <- paste0(node, "\\[.*?\\]")
  str_extract_all(treeTexts, searchstring)
}

#' Read Beast2 files with phylogenetic network data
#'
#' @title read beast files that contain phylogenetic networks
#' @param file Beast2 file with extended newick block
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr across
#' @importFrom dplyr everything
#' @importFrom dplyr coalesce
#' @importFrom ape write.evonet
#' @return treedata object
#' @export

read.beast.retnet <- function(file) {

  clean.colnames <- function(colnames) {
    clean <- gsub("^\\[&|, |=$|%", "", colnames)
    clean <- unique(clean)
    return(clean)
  }

  get.colnames <- function (treeTexts) {
    extractedCols <- str_extract_all(treeTexts, "\\[&(\\w+)=|, (.*?)=")
    extrUni <- unique(unlist(extractedCols))
    return(extrUni)
  }

  get_ret_parentlab <- function(phylo, nodelabel) {
    childid <- nodeid(phylo, nodelabel)
    row <- which(phylo$reticulation[, 2] == childid)
    parentid <- phylo$reticulation[row , 1]
    parentlab <- nodelab(phylo, parentid)
    return(parentlab)
  }

  fix_nodeids <- function(phylo, nodesindex, phylonodes) {
    ret_nodesindex <- which(grepl("#", nodesindex))
    ret_nodelabs <- unique(nodesindex[ret_nodesindex])

    for (lab in ret_nodelabs) {
      receivernodes <- which(nodesindex == lab)
      if (length(receivernodes == 2)) {
        donorlab <- get_ret_parentlab(phylo, lab)
        donorindex <- which(nodesindex == donorlab)
        if (length(donorindex) != 1) {
          warning("More than one reticulation donor node found. Breaking...")
          break
        } else{
          retbased <- which(receivernodes + 1 == donorindex)
          retbased <- receivernodes[retbased]
          phylonodes[retbased] <- nodeid(phylo, donorlab)
        }
      } else{
        warning("Number of reticulation receiver nodes is not 2. Skipping...")
        next
      }
    }
    return(phylonodes)
  }

  file <- normalizePath(file)
  treefile <- readLines(file)

  treesIndex <- grep("tree .* = ", treefile)
  treeTexts <- treefile[treesIndex]

  # get trees
  treeTexts <- sub("^tree (.*) = ",  "", treeTexts)

  # check if tree block contains extended newick
  tt <- gsub("\\[(.*?)\\]", "", treeTexts) # remove comments
  if (grepl("#", tt)) {
    phylo <- read.enewick(text = treeTexts)

    # add temporary node and tip labels to network if needed
    if (any(phylo$node.label == "")) {
      emptyNodes <- which(phylo$node.label == "")
      phylo$node.label[emptyNodes] <-
        paste0("Node", seq(1:length(emptyNodes)))
    }

    if (any(phylo$tip.label == "")) {
      emptyTips <- which(phylo$tip.label == "")
      phylo$tip.label[emptyTips] <-
        paste0("Tip", seq(1:length(emptyTips)))
    }
    pwl <- write.evonet(phylo)
    phyLabel <- read.enewick(text = pwl)

    # Get metadata attributes (column names)
    extrdCols <- get.colnames(treeTexts)
    cleanCols <- clean.colnames(extrdCols)

    nodesindex <-
      str_extract_all(pwl, "([[:alnum:]]+):|(#[[:alnum:]]+):")
    nodesindex <- unlist(nodesindex)
    nodesindex <- gsub("\\:", "", nodesindex)

    # returns a vector of the positions of
    labels <-  c(phyLabel$tip.label, phyLabel$node.label)
    ordered <- match(nodesindex, labels)

    dynlist <- list()
    for (col in cleanCols) {
      dynlist[[col]] <- rep(NA, length(nodesindex))
    }

    nodedata <- unlist(str_extract_all(treeTexts, "\\[.*?\\]"))
    nodedata <- gsub("^\\[&|]$|%", "", nodedata)
    nodedata <- str_split(nodedata, ", ")

    phylonodes <- rep(NA, length(nodesindex))
    for (i in 1:length(nodesindex)) {
      phylonodes[i] <- nodeid(phylo, nodesindex[i])
    }

    for (k in 1:length(nodedata)) {
      for (j in nodedata[[k]]) {
        keyval <- j
        keyval <- str_split_1(keyval, "=")
        key <- keyval[1]
        val <- keyval[2]
        if (grepl("\\{|\\}", val)) {
          val <- gsub("^\\{|\\}$", "", val)
          val <- str_split(val, ",")
        }
        dynlist[[key]][k] <- val
      }
    }
    # set appropriate type for data
    dynlist <- type.convert(dynlist, as.is = TRUE)
    dyndf <- as_tibble(dynlist)
    dyndf <- add_column(dyndf, node = phylonodes)


    # merge rows by nodeid (hybrid nodes appear twice in the data frame) to avoud
    # inconsistencies in plotting and fortified df structure


    dyndf <- dyndf %>%
      group_by(node) %>% # group df by nodes that duplicated (reticulation nodes)
      summarise(across(everything(), # merge rows with identical node attribute
                       ~ coalesce(.x) %>% # first non-NA element
                         `[`(!is.na(.)) %>%
                         `[`(1)))

    if (grep("Translate", treefile, ignore.case = T)) {
      start <- grep("^Translate$", treefile, ignore.case = T) + 1
      semicolon <- grep("^;$", treefile)
      end <- semicolon[which(grep("^;$", treefile) > start)[1]] - 1
      dflen <- end - start

      nodes <- c()
      labels <- c()

      for (i in start:end) {
        label <- treefile[i]
        label <- sub(" ", ";", label)
        splitlab <- str_split_1(label, ";")
        nodes <- c(nodes, splitlab[1])
        labels <- c(labels, splitlab[2])
      }
      nodes <- as.numeric(nodes)
      labels <- gsub(",$", "", labels)

      tr_df <- data.frame(nodes = nodes, labels = labels)
    }
    tiplabels <- as.numeric(phylo$tip.label)
    tr_df <- tr_df[match(tiplabels, tr_df$nodes),]
    phylo$tip.label <- tr_df$labels

    # Clean up (temporary) node labels
    phylo$node.label <- gsub("^[0-9]+$", "", phylo$node.label)
    phylo$node.label <- gsub("^Node[0-9]+$", "", phylo$node.label)

    # Extend classes
    class(phylo) <- c("phylo", "evonet")
    fin <-
      new(
        "treedata",
        treetext = treeTexts,
        phylo = phylo,
        data = dyndf,
        file = file
      )


    return(fin)
  } else {
    warning("No extended Newick in tree block detected.")
    fin <- treeio::read.beast(file = file)
    return(fin)
  }
}

##' @importFrom ape ladderize
##' @importFrom ape as.evonet
##' @importFrom treeio as.phylo
##' @importFrom treeio Nnode
##' @importFrom dplyr full_join
##' @importFrom dplyr mutate
##' @importFrom tidytree as_tibble
##' @method fortify evonet
##' @export
##'
library(treeio)
#' Fortify evonet objects
#'
#' Transforms evonet objects into dataframes that can be passed to ggplot2
#'
#' @param model evonet object to convert to data frame
#' @param data original data
#' @param layout Currently only 'rectangular' layout is supported
#' @param ladderize if TRUE it reorganizes the backbone tree to get an laddered structure
#' @param right specifies whether the smallest clade should be on the right or left
#' @param branch.length specifies branch length attribute
#' @param mrsd specifies mrsd attribute
#' @param as.Date specifies as.Date attribute
#' @param yscale specifies y scale attribute
#' @param root.position specifies root attribute
#' @param ...
#'
#' @return fortified data frame
#' @export
fortify.evonet <- function(model, data,
                           layout = "rectangular",
                           ladderize = TRUE,
                           right = FALSE,
                           branch.length = "branch.length",
                           mrsd = NULL,
                           as.Date = FALSE,
                           yscale = "none",
                           root.position = 0,
                           ...) {
  x <- model
  x$reticulation <- x$reticulation[order(x$reticulation[, 2]), ]

  # x <- as.phylo(x) ## reorder.phylo(get.tree(model), "postorder")
  if (ladderize == TRUE) {
    x <- ladderize(x, right = right)
  }

  if (!is.null(x$edge.length)) {
    if (anyNA(x$edge.length)) {
      warning("'edge.length' contains NA values...\n## setting 'edge.length' to NULL automatically when plotting the tree...")
      x$edge.length <- NULL
    }
  }

  if (layout %in% c("equal_angle", "daylight", "ape")) {
    res <- layout.unrooted(model, layout.method = layout, branch.length = branch.length, ...)
  } else {
    ypos <- getYcoord(x)
    N <- Nnode(x, internal.only = FALSE)

    if (is.null(x$edge.length) || branch.length == "none") {
      if (layout == "slanted") {
        sbp <- .convert_tips2ancestors_sbp(x, include.root = TRUE)
        xpos <- getXcoord_no_length_slanted(sbp)
        ypos <- getYcoord_no_length_slanted(sbp)
      } else {
        xpos <- getXcoord_no_length(x)
      }
    } else {
      xpos <- getXcoord(x)
    }

    xypos <- tibble::tibble(node = 1:N, x = xpos + root.position, y = ypos)

    df <- as_tibble(model) %>%
      mutate(isTip = !.data$node %in% .data$parent)

    isRet <- logical(N)
    donor <- rep(NA, N)
    ret.length <- rep(NA, N)
    if (is.null(dim(x$reticulation))) {
      retIndex <- which(xypos$node %in% x$reticulation[2])
      isRet[retIndex] <- TRUE
      donor[retIndex] <- x$reticulation[1]
    } else {
      retIndex <- which(xypos$node %in% x$reticulation[, 2])
      isRet[retIndex] <- TRUE
      donor[retIndex] <- x$reticulation[, 1]
    }



    if (!is.null(x$ret.length)) {
      ret.length[retIndex] <- x$ret.length
      reticulations <- tibble::tibble(node = 1:N, isRet = isRet, donor = donor, ret.length = ret.length)
    } else {
      reticulations <- tibble::tibble(node = 1:N, isRet = isRet, donor = donor)
    }


    res <- full_join(df, xypos, by = "node")
    res <- full_join(res, reticulations, by = "node")
  }

  ## add branch mid position
  # res <- calculate_branch_mid(res, layout=layout)
  res <- calculate_branch_mid(res)

  if (!is.null(mrsd)) {
    res <- scaleX_by_time_from_mrsd(res, mrsd, as.Date)
  }

  if (layout == "slanted") {
    res <- add_angle_slanted(res)
  } else {
    ## angle for all layout, if 'rectangular', user use coord_polar, can still use angle
    res <- calculate_angle(res)
  }
  res <- scaleY(as.phylo(model), res, yscale, layout, ...)
  res <- adjust_hclust_tip.edge.len(res, x)
  class(res) <- c("tbl_tree", class(res))
  attr(res, "layout") <- layout
  return(res)
}

#' Transforms treedata objects into data frames that can be passed to ggplot2
#'
#' @param model treedata object to convert to data frame
#' @param data original data
#' @param layout Currently only 'rectangular' layout is supported
#' @param ladderize if TRUE it reorganizes the backbone tree into a ladder-like structure
#' @param right specifies whether the smallest clade should be on the right or left
#' @param branch.length specifies branch length attribute
#' @param mrsd specifies mrsd attribute
#' @param as.Date specifies as.Date attribute
#' @param yscale specifies y scale attribute
#' @param root.position specifies root attribute
#' @param ...
#'
#' @return fortified data frame
#' @export
fortify.treedata <- function(model, data,
                             layout = "rectangular",
                             ladderize = TRUE,
                             right = FALSE,
                             branch.length = "branch.length",
                             mrsd = NULL,
                             as.Date = FALSE,
                             yscale = "none",
                             root.position = 0,
                             ...) {


  x <- fortify.evonet(model@phylo)

  xx <- merge(x, model@data, by="node")

  xx
}


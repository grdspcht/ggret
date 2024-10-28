#' Plot reticulation networks
#'
#' @aliases ggret-function
#' @param data either treedata or evonet objects
#' @param mapping aesthetic mappings
#' @param retcol reticulation edge colour
#' @param arrows if TRUE reticulation edges end with arrows
#' @param retlinetype line type aesthetic for reticulations
#' @param rettype Either 'straight' or 'snake'. Determines in which style reticulations are drawn
#' @param na.rm supresses NA value warnings
#' @param mrsd Most recent sampling date
#' @param as.Date logical whether using Date class for time
#' @param yscale Y-Scale
#' @param yscale_mapping Y-scale mapping for category variable,
#' @param ladderize Logical (default TRUE). Should the tree be re-organized to have a 'ladder' format?
#' @param right Logical. If ladderize = TRUE, should the ladder have the smallest clade on the right-hand side? See ape::ladderize() for more information.
#' @param branch.length Variable for scaling branch.
#' @param root.position Position of the root node (default = 0)
#' @param xlim X-Axis limits.
#' @param hang Defines the hang
#' @param layout.params  Layout parameters in list format

#' @param ... additional parameter
#'
#' @return reticulation network plot
#' @export
ggret <- function(data,
                  mapping        = NULL,
                  retcol         = "black",
                  arrows         = FALSE,
                  retlinetype    = 2,
                  rettype        = "snake",
                  na.rm          = TRUE,
                  mrsd           = NULL,
                  as.Date        = FALSE,
                  yscale         = "none",
                  yscale_mapping = NULL,
                  ladderize      = TRUE,
                  right          = FALSE,
                  branch.length  = "branch.length",
                  root.position  = 0,
                  xlim           = NULL,
                  layout.params  = list(as.graph = TRUE),
                  hang           = .1,
                  ...){
  if(is(data, "evonet") || (is(data, "treedata"))){
    if (is.null(mapping)) {
      mapping <- aes_(~x, ~y)
    } else {
      mapping <- modifyList(aes_(~x, ~y), mapping)
    }
    p <- ggplot(data           = data,
                mapping        = mapping,
                na.rm          = na.rm,
                mrsd           = mrsd,
                as.Date        = as.Date,
                yscale         = yscale,
                yscale_mapping = yscale_mapping,
                ladderize      = ladderize,
                right          = right,
                branch.length  = branch.length,
                root.position  = root.position,
                xlim = xlim,
                layout.params = layout.params,
                hang = hang,
                ...)
    p <- p + geom_ret(retcol = retcol,
                      arrows = arrows,
                      retlinetype = retlinetype,
                      rettype = rettype,
                      ...)
    p <- p + theme_tree()
    class(p) <- c("ggret", "ggtree", class(p))
    return(p)
  }else{
    stop("ggret requires an evonet or treedata object.")
  }
}




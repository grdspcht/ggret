#' Plot reticulation networks
#'
#' @param data either treedata or evonet objects
#' @param mapping aesthetic mappings
#' @param retcol reticulation edge colour
#' @param arrows if TRUE reticulation edges end with arrows
#' @param retlinetype line type aesthetic for reticulations
#' @param rettype Either 'straight' or 'snake'. Determines in which style reticulations are drawn
#' @param na.rm supresses NA value warnings
#' @param ... additional parameter
#'
#' @return reticulation network plot
#' @export
#'
#' @examples
ggret <- function(data,
                  mapping = NULL,
                  retcol = "black",
                  arrows = FALSE,
                  retlinetype = 2,
                  rettype = "snake",
                  na.rm = TRUE,
                  mrsd           = NULL,
                  as.Date        = FALSE,
                  yscale         = "none",
                  yscale_mapping = NULL,
                  ladderize      = TRUE,
                  right          = FALSE,
                  branch.length  = "branch.length",
                  root.position  = 0,
                  xlim = NULL,
                  hang = .1,
                  ...){
  if(is(data, "evonet") || (is(data, "evonet"))){
    if (is.null(mapping)) {
      mapping <- aes_(~x, ~y)
    } else {
      mapping <- modifyList(aes_(~x, ~y), mapping)
    }
    p <- ggplot(data, mapping = mapping,  retcol = retcol,
                arrows = arrows,
                retlinetype = retlinetype,
                rettype = rettype,
                na.rm = na.rm,
                mrsd = mrsd,
                as.Date = as.Date,
                yscale = yscale,
                yscale_mapping =  yscale_mapping,
                ladderize      = ladderize,
                right          = right,
                branch.length  = branch.length,
                root.position  = root.position,
                xlim = xlim,
                hang = hang,
                ...)
    p <- p + geom_ret()
    p <- p + theme_tree()
    class(p) <- c("ggret", "ggtree", class(p))
    return(p)
  }else{
    stop("ggret requires an evonet or treedata object.")
  }
}




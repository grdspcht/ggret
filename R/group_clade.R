#' Group clades
#'
#' @param data Evonet or treedata object.
#' @param nodes Nodes that define the clade.
#' @param cladename Name of the clade.
#' @param tiponly Should only tips be considered?
#' @param addtotreedata  If TRUE, adds group/clade information to the data table of a treedata object (in the "Clade" column)
#' @param undefinedclades value used to name undefined clades
#' @importFrom  tibble add_column
#'
#' @export
group_clade <- function(data,
                       nodes,
                       cladename,
                       tiponly = FALSE,
                       addtotreedata = FALSE,
                       undefinedclades = NA) {
  mrca = treeio::MRCA(data, nodes)
  offsprings = treeio::offspring(data, mrca, tiponly)
  if (is(data, "evonet")) {
    labs <- c(data$tip.label, data$node.label)
    if("clade" %in% names(attributes(data))){
      clades <- attributes(data)$clade
    }else{
      clades <- rep(undefinedclades, length(labs))
    }


    for (u in cladename) {
      clades[offsprings] <- u
    }

    attr(data, "clade") <- clades

  } else if (is(data, "treedata")) {
    labs <- c(data@phylo[["tip.label"]], data@phylo[["node.label"]])
    if ("clade" %in% names(attributes(data@phylo))){
      clades <- attributes(data@phylo)$clade
    } else {
      clades <- rep(undefinedclades, length(labs))
    }


    for (u in cladename) {
      clades[offsprings] <- u
    }

    attr(data@phylo, "clade") <- clades

    if (addtotreedata == TRUE){
      if("Clade" %in% colnames(data@data)){
        data@data$Clade <- clades
      }else{
        data@data <- add_column(data@data, "Clade" = clades)
      }
    }
  } else{
    warning("Function only supports evonet and treedata objects.")
    stop()
  }

  return(data)
}


findRetLayer <- function(layer, rets){
  retrow <- subset(layer[rets,], select = c("x", "y", "xend", "yend"))
  rest <- subset(layer[-rets,], select = c("x", "y", "xend", "yend"))
  if (anyNA(retrow) == FALSE & anyNA(rest) == TRUE){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

#' Change reticulation coloring
#'
#' @param plot ggplot object
#' @param assignto  Reticulation color can be assigned to "donor" or "receiver"
#'
#' @return Modified ggplot object
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggplot_gtable
#' @export
#'
modify_ret <- function(plot, assignto="donor"){
  # Diassemble ggplot object and extract needed variables
  decon <- ggplot_build(plot)
  data <- decon$plot$data
  layout <- attributes(data)$layout
  layers <- decon$data
  rets <- which(data$isRet == TRUE)
  nnode <- nrow(data)
  retlayers <- c()
  lablayers <- c()

# Iterate over plot layers and detect the reticulation layers and
  for (i in 1:(length(layers))){
    if(nrow(layers[[i]]) == nnode & all(c("x", "y", "xend", "yend") %in% colnames(layers[[i]]))){
      if(findRetLayer(layers[[i]], rets) == TRUE){
        retlayers <- c(retlayers, i)
      }
    } else if (all(data[rets, ]$label %in% data$data[[i]]$label)){

        lablayers <- c(lablayers, i)
    }
  }
  if(length(retlayers) == 0){
    stop("No reticulation layers detected. Aborting")
  }



# reassign colours in reticulation layers
  for (retl in retlayers){
    if(assignto == "donor"){
      donors <- data$donor[rets]
      layers[[retl]]$colour[rets] <- layers[[retl]]$colour[donors]
    }else if(assignto == "receiver"){
      rec <- data$node[rets]
      layers[[retl]]$colour[rets] <- layers[[retl]]$colour[rec]
    }else{
      stop("Reticulations can currently only be reassigned to donors or receivers. Aborting...")
    }
  }

  decon$data <- layers
  new <- ggplot_gtable(decon)
  grid::grid.newpage()
  grid::grid.draw(new)

}



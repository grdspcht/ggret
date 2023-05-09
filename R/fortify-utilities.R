calculate_angle <- function(data) {
  data$angle <- 360/(diff(range(data$y)) + 1) * data$y
  return(data)
}



scaleY <- function(phylo, df, yscale, layout, ...) {
  if (yscale == "none") {
    return(df)
  }
  if (! yscale %in% colnames(df)) {
    warning("yscale is not available...\n")
    return(df)
  }
  if (is.numeric(df[[yscale]])) {
    y <- getYcoord_scale_numeric(phylo, df, yscale, ...)
  }
  else {
    y <- getYcoord_scale_category(phylo, df, yscale, ...)
  }

  df[, "y"] <- y

  return(df)
}


adjust_hclust_tip.edge.len <- function(df, phylo){
  if (inherits(phylo, 'treedata')){
    tip.edge.len <- attr(phylo@phylo, 'tip.edge.len')
  }else{
    tip.edge.len <- attr(phylo, 'tip.edge.len')
  }
  if (!is.null(tip.edge.len)){
    mx <- max(df$x, na.rm=TRUE)
    df$x <- df$x - mx
    df$branch <- df$branch - mx
    df[df$isTip, "x", drop=TRUE] <- tip.edge.len
    attr(df, 'revts.done') = TRUE
  }
  return(df)
}

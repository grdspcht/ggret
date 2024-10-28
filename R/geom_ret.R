# !!!DISCLAIMER!!!
# This file contains source from the ggtree R package. ggret serves as an extension
# of ggtree and therefore frequently interacts with internal objects and functions
# of ggtree. For the sake of easier access they have been included in this package
# All rights reserved to the ggtree developers.
# https://github.com/YuLab-SMU/ggtree

##' geom_segment2 support aes(subset) via setup_data
##'
##' 'geom_segment2' is a modified version of geom_segment, with subset aesthetic supported
##'
##' @title geom_segment2
##' @param mapping Set of aesthetic mappings, defaults to NULL
##' @param data A layer specific dataset -
##'             only needed if you want to override the plot defaults.
##' @param stat Name of stat to modify data.
##' @param position The position adjustment to use for overlapping points on this layer.
##' @param lineend Line end style, one of butt (default), round and square.
##' @param na.rm If "FALSE" (default), missing values are removed with a warning. If "TRUE", missing values are silently removed, logical.
##' @param show.legend Whether to show legend, logical.
##' @param inherit.aes Whether to inherit aesthetic mappings, logical, defaults to "TRUE".
##' @param nudge_x adjust the horizontal position of the segments.
##' @param arrow specification for arrow heads, as created by arrow().
##' @param arrow.fill fill color to usse for the arrow head (if closed). `NULL` means use `colour` aesthetic.
##' @param ... additional parameter
##' @importFrom ggplot2 layer
##' @export
##' @seealso
##' [geom_segment][ggplot2::geom_segment]
##' @return add segment layer
##' @author Guangchuang Yu
geom_segment2 <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", lineend = "butt",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                          nudge_x = 0, arrow = NULL, arrow.fill = NULL,
                          ...) {

  default_aes <- aes_(node=~node)
  if (is.null(mapping)) {
    mapping <- default_aes
  } else {
    mapping <- modifyList(mapping, default_aes)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSegmentGGtree,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      lineend = lineend,
      na.rm = na.rm,
      nudge_x = nudge_x,
      ...
    ),
    check.aes = FALSE
  )
}


##' @importFrom ggplot2 GeomSegment
##' @importFrom ggplot2 draw_key_path
GeomSegmentGGtree <- ggproto("GeomSegmentGGtree", GeomSegment,
                             setup_data = function(data, params) {
                               if (is.null(data$subset))
                                 return(data)
                               data[which(data$subset),]
                             },

                             draw_panel = function(data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                                                   lineend = "butt", linejoin = "round", na.rm = FALSE, nudge_x = 0) {

                               data$x <- data$x + nudge_x

                               data <- ggplot2::remove_missing(data, na.rm = na.rm, c("x", "y", "xend",
                                                                                      "yend", "linetype", "linewidth", "shape"), name = "geom_segment")
                               if (empty(data))
                                 return(zeroGrob())
                               if (!coord$is_linear()) {
                                 tmpgroup <- data$group
                                 starts <- subset(data, select = c(-xend, -yend))
                                 starts$group <- 1
                                 ends <- rename(subset(data, select = c(-x, -y)), c("x" = "xend", "y" = "yend"))
                                 ends$group <- 2
                                 pieces <- rbind(starts, ends)

                                 trans <- coord$transform(pieces, panel_params)
                                 starts <- trans[trans$group==1, ,drop=FALSE]
                                 ends <- trans[trans$group==2, ,drop=FALSE]
                                 ends <- rename(subset(ends, select=c(x, y)), c("xend"="x", "yend"="y"))
                                 data <- cbind(starts, ends)
                                 data$group <- tmpgroup
                               }else{
                                 data <- coord$transform(data, panel_params)
                               }

                               arrow.fill <- arrow.fill %||% data$colour
                               return(grid::segmentsGrob(data$x, data$y, data$xend, data$yend,
                                                         default.units = "native", gp = gpar(col = alpha(data$colour,
                                                                                                         data$alpha), fill = alpha(arrow.fill, data$alpha),
                                                                                             lwd = data$linewidth * ggplot2::.pt, lty = data$linetype,
                                                                                             lineend = lineend, linejoin = linejoin), arrow = arrow)
                               )


                             }
)


empty <- getFromNamespace("empty", "ggplot2")
`%||%` <- getFromNamespace("%||%", "ggplot2")


geom_tree <- function(mapping=NULL, data=NULL, layout="rectangular", multiPhylo=FALSE, position="identity", ...) {

  stat_tree(data=data, mapping=mapping, geom="segment", position=position,
            layout=layout, multiPhylo=multiPhylo, ...)
}


stat_tree <- function(mapping=NULL, data=NULL, geom="segment", position="identity",
                      layout="rectangular", multiPhylo=FALSE, lineend="round", MAX_COUNT=5,
                      ..., arrow=NULL, rootnode=TRUE, show.legend=NA, inherit.aes=TRUE,
                      na.rm=TRUE, check.param=TRUE) {

  default_aes <- aes_(x=~x, y=~y,node=~node, parent=~parent)
  if (multiPhylo) {
    default_aes <- modifyList(default_aes, aes_(.id=~.id))
  }

  if (is.null(mapping)) {
    mapping <- default_aes
  } else {
    mapping <- modifyList(default_aes, mapping)
  }

  if (!is.null(arrow)) {
    rootnode <- FALSE
  }

  if (layout %in% c("rectangular", "dendrogram", "fan", "circular", "inward_circular")) {
    list(
      layer(data=data,
            mapping=mapping,
            stat=StatTreeHorizontal,
            geom = geom, ## GeomTreeHorizontal,
            position=position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params=list(layout = layout,
                        lineend = lineend,
                        na.rm = na.rm,
                        arrow = arrow,
                        rootnode = rootnode,
                        ...),
            check.aes = FALSE
      ),
      layer(data=data,
            mapping=mapping,
            stat=StatTreeHorizontal,
            geom = geom,
            position=position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params=list(layout = layout,
                        lineend = lineend,
                        na.rm = na.rm,
                        ## arrow = arrow,
                        rootnode = rootnode,
                        ...),
            check.aes = FALSE
      )
    )
  } else if (layout %in% c("slanted", "radial", "equal_angle", "daylight", "ape")) {
    line.type <- getOption(x="layout.radial.linetype", default="straight")
    geom <- switch(line.type, straight=GeomSegmentGGtree, curved=geom)
    layer(stat=StatTree,
          data=data,
          mapping=mapping,
          geom = geom,
          position=position,
          show.legend = show.legend,
          inherit.aes = inherit.aes,
          params=list(layout = layout,
                      lineend = lineend,
                      na.rm = na.rm,
                      arrow = arrow,
                      rootnode = rootnode,
                      ...),
          check.aes = FALSE
    )
  } else if (layout %in% c("ellipse", "roundrect")){
    mapping <- modifyList(mapping, aes_(isTip=~isTip))
    layer(stat=StatTreeEllipse,
          data=data,
          mapping=mapping,
          geom=GeomCurvelink,
          position=position,
          show.legend=show.legend,
          inherit.aes=inherit.aes,
          params=list(layout=layout,
                      lineend = lineend,
                      na.rm = na.rm,
                      arrow = arrow,
                      rootnode = rootnode,
                      ...),
          check.aes=FALSE
    )
  }
}


StatTreeHorizontal <- ggproto("StatTreeHorizontal", Stat,
                              required_aes = c("node", "parent", "x", "y"),
                              compute_group = function(data, params) {
                                data
                              },
                              compute_panel = function(self, data, scales, params, layout, lineend,
                                                        rootnode = TRUE, nsplit = 100, extend=0.002 ) {
                                .fun <- function(data) {
                                  df <- setup_tree_data(data)
                                  x <- df$x
                                  y <- df$y
                                  df$xend <- x
                                  df$yend <- y
                                  ii <- with(df, match(parent, node))
                                  df$x <- x[ii]

                                  if (!rootnode) {
                                    ## introduce this paramete in v=1.7.4
                                    ## rootnode = TRUE which behave as previous versions.
                                    ## and has advantage of the number of line segments is consistent with tree nodes.
                                    ## i.e. every node has its own line segment, even for root.
                                    ## if rootnode = FALSE, the root to itself line segment will be removed.

                                    df <- dplyr::filter(df, .data$node != tidytree:::rootnode.tbl_tree(df)$node)
                                  }
                                    return(df)
                                }
                                if ('.id' %in% names(data)) {
                                  ldf <- split(data, data$.id)
                                  df <- do.call(rbind, lapply(ldf, .fun))
                                } else {
                                  df <- .fun(data)
                                }
                                return(df)
                              }
)

StatTree <- ggproto("StatTree", Stat,
                    required_aes = c("node", "parent", "x", "y"),
                    compute_group = function(data, params) {
                      data
                    },
                    compute_panel = function(self, data, scales, params, layout, lineend,
                                             nsplit = 100, extend = 0.002, rootnode = TRUE) {
                      .fun <- function(data) {
                        df <- setup_tree_data(data)
                        x <- df$x
                        y <- df$y
                        ii <- with(df, match(parent, node))
                        df$x <- x[ii]
                        df$y <- y[ii]
                        df$xend <- x
                        df$yend <- y

                        if (!rootnode) {
                          df <- dplyr::filter(df, .data$node != rootnode.tbl_tree(df)$node)
                        }
                          return(df)
                      }
                      if ('.id' %in% names(data)) {
                        ldf <- split(data, data$.id)
                        df <- do.call(rbind, lapply(ldf, .fun))
                      } else {
                        df <- .fun(data)
                      }

                      return(df)
                    }
)


setup_tree_data <- function(data) {
  if (nrow(data) == length(unique(data$node)))
    return(data)

  data[match(unique(data$node), data$node),]
  ## data[order(data$node, decreasing = FALSE), ]
}


##'
##'
##'
##' @title Explicit phylogenetic network
##' @param retcol reticulation edge colour
##' @param arrows if TRUE reticulation edges end with arrows
##' @param retlinetype line type aesthetic for reticulations
##' @param na.rm logical
##' @param rettype Either 'straight' or 'snake'. Determines in which style reticulations are drawn
##' @param ... additional parameter
##' @return phylogenetic network layer
##' @importFrom ggplot2 geom_segment
##' @importFrom ggplot2 aes
##' @export
##' @author Yu Guangchuang, Gerd Specht
geom_ret <- function(retcol = "black",
                     arrows = FALSE,
                     retlinetype = 2,
                     rettype = "snake",
                     na.rm = TRUE,
                     ...) {
  x <- y <- parent <- NULL
  lend  = "round"
  if (arrows == TRUE){
    arrowtype = arrow(length = unit(0.03, "npc"), type = "closed")
  }else{
    arrowtype = NULL
  }



    backbone <- list(
      geom_segment(aes(x    = x[parent],
                       xend = x,
                       y    = y,
                       yend = y),
                   lineend = lend, ...),

      geom_segment(aes(x    = x[parent],
                       xend = x[parent],
                       y    = y[parent],
                       yend = y),
                   lineend = lend, ...))
      if (rettype == "snake"){
        append(backbone,
        # S-Segment Horizontal
        list(
        geom_segment(aes(x    = x[donor],
                         xend = x,
                         y    = (y[donor] + y)/2,
                         yend = (y[donor] + y)/2),
                     colour = retcol,
                     lineend = lend,
                     linetype = retlinetype,
                     na.rm = na.rm,
                     ...),
        # S-Segment Vertical Donor
        geom_segment(aes(x    = x[donor],
                         xend = x[donor],
                         y    = y[donor],
                         yend = (y[donor] + y)/2),
                     colour = retcol,
                     lineend = lend,
                     linetype = retlinetype,
                     na.rm = na.rm,
                     ...),
        # S-Segment Vertical Receiver
        geom_segment(aes(x    = x,
                         xend = x,
                         y    = (y[donor] + y)/2,
                         yend = y),
                     colour = retcol,
                     lineend = lend,
                     arrow = arrowtype,
                     linetype = retlinetype,
                     na.rm = na.rm,
                     ...)))
      } else if (rettype == "straight"){
        append(backbone,
        list(
        geom_segment(aes(x    = x[donor],
                         xend = x,
                         y    = y[donor],
                         yend = y),
                         colour = retcol,
                         lineend = lend,
                         arrow = arrowtype,
                         linetype = retlinetype,
                         na.rm = na.rm,
                         ...)))

      }
  }


ggproto_formals <- function(x) formals(environment(x)$f)

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

"%|W|%" <- function(a, b) {
  if (!is.waive(a)) a else b
}

# Check required aesthetics are present
# This is used by geoms and stats to give a more helpful error message
# when required aesthetics are missing.
#
# @param character vector of required aesthetics
# @param character vector of present aesthetics
# @param name of object for error message
# @keyword internal
check_required_aesthetics <- function(required, present, name, call = rlang::caller_env()) {
  if (is.null(required)) return()

  required <- strsplit(required, "|", fixed = TRUE)
  if (any(lengths(required) > 1)) {
    required <- lapply(required, rep_len, 2)
    required <- list(
      vapply(required, `[`, character(1), 1),
      vapply(required, `[`, character(1), 2)
    )
  } else {
    required <- list(unlist(required))
  }
  missing_aes <- lapply(required, setdiff, present)
  if (any(lengths(missing_aes) == 0)) return()
  message <- "{.fn {name}} requires the following missing aesthetics: {.field {missing_aes[[1]]}}"
  if (length(missing_aes) > 1) {
    message <- paste0(message, " {.strong or} {.field {missing_aes[[2]]}}")
  }
  cli::cli_abort(message, call = call)
}

# Concatenate a named list for output
# Print a `list(a=1, b=2)` as `(a=1, b=2)`
#
# @param list to concatenate
# @keyword internal
#X clist(list(a=1, b=2))
#X clist(par()[1:5])
clist <- function(l) {
  paste(paste(names(l), l, sep = " = ", collapse = ", "), sep = "")
}

# Return unique columns
# This is used for figuring out which columns are constant within a group
#
# @keyword internal
uniquecols <- function(df) {
  df <- df[1, sapply(df, is_unique), drop = FALSE]
  rownames(df) <- 1:nrow(df)
  df
}

#' Convenience function to remove missing values from a data.frame
#'
#' Remove all non-complete rows, with a warning if `na.rm = FALSE`.
#' ggplot is somewhat more accommodating of missing values than R generally.
#' For those stats which require complete data, missing values will be
#' automatically removed with a warning. If `na.rm = TRUE` is supplied
#' to the statistic, the warning will be suppressed.
#'
#' @param df data.frame
#' @param na.rm If true, will suppress warning message.
#' @param vars Character vector of variables to check for missings in
#' @param name Optional function name to improve error message.
#' @param finite If `TRUE`, will also remove non-finite values.
#' @keywords internal
#' @export
remove_missing <- function(df, na.rm = FALSE, vars = names(df), name = "",
                           finite = FALSE) {
  check_bool(na.rm)
  missing <- detect_missing(df, vars, finite)

  if (any(missing)) {
    df <- df[!missing, , drop = FALSE]
    if (!na.rm) {
      if (name != "") name <- paste(" ({.fn ", name, "})", sep = "")
      msg <- paste0(
        "Removed {sum(missing)} row{?s} containing ",
        if (finite) "non-finite" else "missing values or values",
        " outside the scale range", name, "."
      )
      cli::cli_warn(msg)
    }
  }

  df
}
detect_missing <- function(df, vars, finite = FALSE) {
  vars <- intersect(vars, names(df))
  !cases(df[, vars, drop = FALSE], if (finite) is_finite else is_complete)
}

# Returns a logical vector of same length as nrow(x). If all data on a row
# is finite (not NA, NaN, Inf, or -Inf) return TRUE; otherwise FALSE.
cases <- function(x, fun) {
  ok <- vapply(x, fun, logical(nrow(x)))

  # Need a special case test when x has exactly one row, because rowSums
  # doesn't respect dimensions for 1x1 matrices. vapply returns a vector (not
  # a matrix when the input has one row.
  if (is.vector(ok)) {
    all(ok)
  } else {
    # Find all the rows where all are TRUE
    rowSums(as.matrix(ok)) == ncol(x)
  }
}

# Wrapper around is.finite to handle list and character cols
is_finite <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else if (typeof(x) == "character") {
    !is.na(x)
  } else {
    is.finite(x)
  }
}

is_complete <- function(x) {
  if (typeof(x) == "list") {
    !vapply(x, is.null, logical(1))
  } else {
    !is.na(x)
  }
}


#' Used in examples to illustrate when errors should occur.
#'
#' @param expr code to evaluate.
#' @export
#' @keywords internal
#' @examples
#' should_stop(stop("Hi!"))
#' should_stop(should_stop("Hi!"))
should_stop <- function(expr) {
  res <- try(print(force(expr)), TRUE)
  if (!inherits(res, "try-error")) {
    cli::cli_abort("No error!")
  }
  invisible()
}


#' A waiver object.
#'
#' A waiver is a "flag" object, similar to `NULL`, that indicates the
#' calling function should just use the default value.  It is used in certain
#' functions to distinguish between displaying nothing (`NULL`) and
#' displaying a default value calculated elsewhere (`waiver()`)
#'
#' @export
#' @keywords internal
waiver <- function() structure(list(), class = "waiver")

is.waive <- function(x) inherits(x, "waiver")


rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

binned_pal <- function(palette) {
  function(x) {
    palette(length(x))
  }
}


has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    return(rep(FALSE, length(x)))
  }

  !is.na(nms) & nms != ""
}

# Use chartr() for safety since toupper() fails to convert i to I in Turkish locale
lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)

tolower <- function(x) {
  cli::cli_abort("Please use {.fn to_lower_ascii}, which works fine in all locales.")
}

toupper <- function(x) {
  cli::cli_abort("Please use {.fn to_upper_ascii}, which works fine in all locales.")
}

# Convert a snake_case string to camelCase
camelize <- function(x, first = FALSE) {
  x <- gsub("_(.)", "\\U\\1", x, perl = TRUE)
  if (first) x <- firstUpper(x)
  x
}

snakeize <- function(x) {
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  to_lower_ascii(x)
}

firstUpper <- function(s) {
  paste0(to_upper_ascii(substring(s, 1, 1)), substring(s, 2))
}

snake_class <- function(x) {
  snakeize(class(x)[1])
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is.waive(df)
}

is.discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

# This function checks that all columns of a dataframe `x` are data and returns
# the names of any columns that are not.
# We define "data" as atomic types or lists, not functions or otherwise.
# The `inherits(x, "Vector")` check is for checking S4 classes from Bioconductor
# and wether they can be expected to follow behavior typical of vectors. See
# also #3835
check_nondata_cols <- function(x) {
  idx <- (vapply(x, function(x) {
    is.null(x) || rlang::is_vector(x) || inherits(x, "Vector")
  }, logical(1)))
  names(x)[which(!idx)]
}

compact <- function(x) {
  null <- vapply(x, is.null, logical(1))
  x[!null]
}

is.formula <- function(x) inherits(x, "formula")

deparse2 <- function(x) {
  y <- deparse(x, backtick = TRUE)
  if (length(y) == 1) {
    y
  } else {
    paste0(y[[1]], "...")
  }
}

dispatch_args <- function(f, ...) {
  args <- list(...)
  formals <- formals(f)
  formals[names(args)] <- args
  formals(f) <- formals
  f
}

is_missing_arg <- function(x) identical(x, quote(expr = ))
# Get all arguments in a function as a list. Will fail if an ellipsis argument
# named .ignore
# @param ... passed on in case enclosing function uses ellipsis in argument list
find_args <- function(...) {
  env <- parent.frame()
  args <- names(formals(sys.function(sys.parent(1))))

  vals <- mget(args, envir = env)
  vals <- vals[!vapply(vals, is_missing_arg, logical(1))]

  ggplot2:::modify_list(vals, list(..., `...` = NULL))
}

# Used in annotations to ensure printed even when no
# global data
dummy_data <- function() data_frame0(x = NA, .size = 1)

with_seed_null <- function(seed, code) {
  if (is.null(seed)) {
    code
  } else {
    withr::with_seed(seed, code)
  }
}

seq_asc <- function(to, from) {
  if (to > from) {
    integer()
  } else {
    to:from
  }
}

# Needed to trigger package loading
#' @importFrom tibble tibble
NULL

# Wrapping vctrs data_frame constructor with no name repair
data_frame0 <- function(...) tibble::data_frame(..., .name_repair = "minimal")

# Wrapping unique0() to accept NULL
unique0 <- function(x, ...) if (is.null(x)) x else vec_unique(x, ...)

# Code readability checking for uniqueness
is_unique <- function(x) vec_unique_count(x) == 1L

is_scalar_numeric <- function(x) is_bare_numeric(x, n = 1L)

# Check inputs with tibble but allow column vectors (see #2609 and #2374)
as_gg_data_frame <- function(x) {
  x <- lapply(x, validate_column_vec)
  data_frame0(!!!x)
}
validate_column_vec <- function(x) {
  if (is_column_vec(x)) {
    dim(x) <- NULL
  }
  x
}
is_column_vec <- function(x) {
  dims <- dim(x)
  length(dims) == 2L && dims[[2]] == 1L
}

# Parse takes a vector of n lines and returns m expressions.
# See https://github.com/tidyverse/ggplot2/issues/2864 for discussion.
#
# parse(text = c("alpha", "", "gamma"))
# #> expression(alpha, gamma)
#
# parse_safe(text = c("alpha", "", "gamma"))
# #> expression(alpha, NA, gamma)
#
parse_safe <- function(text) {
  check_character(text)
  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
    expr <- parse(text = text[[i]])
    out[[i]] <- if (length(expr) == 0) NA else expr[[1]]
  }
  out
}

switch_orientation <- function(aesthetics) {
  # We should have these as globals somewhere
  x <- ggplot_global$x_aes
  y <- ggplot_global$y_aes
  x_aes <- match(aesthetics, x)
  x_aes_pos <- which(!is.na(x_aes))
  y_aes <- match(aesthetics, y)
  y_aes_pos <- which(!is.na(y_aes))
  if (length(x_aes_pos) > 0) {
    aesthetics[x_aes_pos] <- y[x_aes[x_aes_pos]]
  }
  if (length(y_aes_pos) > 0) {
    aesthetics[y_aes_pos] <- x[y_aes[y_aes_pos]]
  }
  aesthetics
}

has_flipped_aes <- function(data, params = list(), main_is_orthogonal = NA,
                            range_is_orthogonal = NA, group_has_equal = FALSE,
                            ambiguous = FALSE, main_is_continuous = FALSE,
                            main_is_optional = FALSE) {
  # Is orientation already encoded in data?
  if (!is.null(data$flipped_aes)) {
    not_na <- which(!is.na(data$flipped_aes))
    if (length(not_na) != 0) {
      return(data$flipped_aes[[not_na[1L]]])
    }
  }

  # Is orientation requested in the params
  if (!is.null(params$orientation) && !is.na(params$orientation)) {
    return(params$orientation == "y")
  }

  x <- data$x %||% params$x
  y <- data$y %||% params$y
  xmin <- data$xmin %||% params$xmin
  ymin <- data$ymin %||% params$ymin
  xmax <- data$xmax %||% params$xmax
  ymax <- data$ymax %||% params$ymax

  # Does a single x or y aesthetic corespond to a specific orientation
  if (!is.na(main_is_orthogonal) && xor(is.null(x), is.null(y))) {
    return(is.null(y) == main_is_orthogonal)
  }

  has_x <- !is.null(x)
  has_y <- !is.null(y)

  # Does a provided range indicate an orientation
  if (!is.na(range_is_orthogonal)) {
    if (!is.null(ymin) || !is.null(ymax)) {
      return(!range_is_orthogonal)
    }
    if (!is.null(xmin) || !is.null(xmax)) {
      return(range_is_orthogonal)
    }
  }

  # If ambiguous orientation = NA will give FALSE
  if (ambiguous && (is.null(params$orientation) || is.na(params$orientation))) {
    return(FALSE)
  }

  # Is there a single actual discrete position
  y_is_discrete <- is_mapped_discrete(y)
  x_is_discrete <- is_mapped_discrete(x)
  if (xor(y_is_discrete, x_is_discrete)) {
    return(y_is_discrete != main_is_continuous)
  }

  # Does each group have a single x or y value
  if (group_has_equal) {
    if (has_x) {
      if (length(x) == 1) return(FALSE)
      x_groups <- vapply(split(data$x, data$group), vec_unique_count, integer(1))
      if (all(x_groups == 1)) {
        return(FALSE)
      }
    }
    if (has_y) {
      if (length(y) == 1) return(TRUE)
      y_groups <- vapply(split(data$y, data$group), vec_unique_count, integer(1))
      if (all(y_groups == 1)) {
        return(TRUE)
      }
    }
  }

  # default to no
  FALSE
}

split_with_index <- function(x, f, n = max(f)) {
  if (n == 1) return(list(x))
  f <- as.integer(f)
  attributes(f) <- list(levels = as.character(seq_len(n)), class = "factor")
  unname(split(x, f))
}

is_bang <- function(x) {
  is_call(x, "!", n = 1)
}

is_triple_bang <- function(x) {
  if (!is_bang(x)) {
    return(FALSE)
  }

  x <- x[[2]]
  if (!is_bang(x)) {
    return(FALSE)
  }

  x <- x[[2]]
  if (!is_bang(x)) {
    return(FALSE)
  }

  TRUE
}

# Restart handler for using vec_rbind with mix of types
# Ordered is coerced to factor
# If a character vector is present the other is converted to character
with_ordered_restart <- function(expr, .call) {
  withCallingHandlers(
    expr,
    vctrs_error_incompatible_type = function(cnd) {
      x <- cnd[["x"]]
      y <- cnd[["y"]]

      class_x <- class(x)[1]
      class_y <- class(y)[1]

      restart <- FALSE

      if (is.ordered(x) || is.ordered(y)) {
        restart <- TRUE
        if (is.ordered(x)) {
          x <- factor(as.character(x), levels = levels(x))
        }
        if (is.ordered(y)) {
          y <- factor(as.character(y), levels = levels(y))
        }
      } else if (is.character(x) || is.character(y)) {
        restart <- TRUE
        if (is.character(x)) {
          y <- as.character(y)
        } else {
          x <- as.character(x)
        }
      } else if (is.factor(x) || is.factor(y)) {
        restart <- TRUE
        lev <- c()
        if (is.factor(x)) {
          lev <- c(lev, levels(x))
        }
        if (is.factor(y)) {
          lev <- c(lev, levels(y))
        }
        x <- factor(as.character(x), levels = unique(lev))
        y <- factor(as.character(y), levels = unique(lev))
      }

      # Don't recurse and let ptype2 error keep its course
      if (!restart) {
        return(zap())
      }

      msg <- paste0("Combining variables of class <", class_x, "> and <", class_y, ">")
      desc <- paste0(
        "Please ensure your variables are compatible before plotting (location: ",
        format_error_call(.call),
        ")"
      )

      deprecate_soft0(
        "3.4.0",
        I(msg),
        details = desc
      )

      x_arg <- cnd[["x_arg"]]
      y_arg <- cnd[["y_arg"]]
      call <- cnd[["call"]]

      # Recurse with factor methods and restart with the result
      if (inherits(cnd, "vctrs_error_ptype2")) {
        out <- vec_ptype2(x, y, x_arg = x_arg, y_arg = y_arg, call = call)
        restart <- "vctrs_restart_ptype2"
      } else if (inherits(cnd, "vctrs_error_cast")) {
        out <- vec_cast(x, y, x_arg = x_arg, to_arg = y_arg, call = call)
        restart <- "vctrs_restart_cast"
      } else {
        return(zap())
      }

      # Old-R compat for `tryInvokeRestart()`
      try_restart <- function(restart, ...) {
        if (!is_null(findRestart(restart))) {
          invokeRestart(restart, ...)
        }
      }
      try_restart(restart, out)
    }
  )
}

vec_rbind0 <- function(..., .error_call = current_env(), .call = rlang::caller_env()) {
  with_ordered_restart(
    vec_rbind(..., .error_call = .error_call),
    .call
  )
}

attach_plot_env <- function(env) {
  old_env <- getOption("ggplot2_plot_env")
  options(ggplot2_plot_env = env)
  withr::defer_parent(options(ggplot2_plot_env = old_env))
}

as_cli <- function(..., env = rlang::caller_env()) {
  cli::cli_fmt(cli::cli_text(..., .envir = env))
}

deprecate_soft0 <- function(..., user_env = NULL) {
  user_env <- user_env %||% getOption("ggplot2_plot_env") %||% rlang::caller_env(2)
  lifecycle::deprecate_soft(..., user_env = user_env)
}

deprecate_warn0 <- function(..., user_env = NULL) {
  user_env <- user_env %||% getOption("ggplot2_plot_env") %||% rlang::caller_env(2)
  lifecycle::deprecate_warn(..., user_env = user_env)
}

dapply <- function(df, by, fun, ..., drop = TRUE) {
  grouping_cols <- .subset(df, by)
  fallback_order <- unique0(c(by, names(df)))
  apply_fun <- function(x) {
    res <- fun(x, ...)
    if (is.null(res)) return(res)
    if (length(res) == 0) return(data_frame0())
    vars <- lapply(stats::setNames(by, by), function(col) .subset2(x, col)[1])
    if (is.matrix(res)) res <- ggplot2:::split_matrix(res)
    if (is.null(names(res))) names(res) <- paste0("V", seq_along(res))
    if (all(by %in% names(res))) return(data_frame0(!!!unclass(res)))
    res <- ggplot2:::modify_list(unclass(vars), unclass(res))
    res <- res[intersect(c(fallback_order, names(res)), names(res))]
    data_frame0(!!!res)
  }

  # Shortcut when only one group
  if (all(vapply(grouping_cols, single_value, logical(1)))) {
    return(apply_fun(df))
  }

  ids <- id(grouping_cols, drop = drop)
  group_rows <- split_with_index(seq_len(nrow(df)), ids)
  result <- lapply(seq_along(group_rows), function(i) {
    cur_data <- ggplot2:::df_rows(df, group_rows[[i]])
    apply_fun(cur_data)
  })
  vec_rbind0(!!!result)
}


single_value <- function(x, ...) {
  UseMethod("single_value")
}

' Adds missing elements to a vector from a default vector
#'
#' This function appends a given named vector or list with additional elements
#' from a default vector, only adding those that does not already exist in the
#' first.
#'
#' @param x,y Named vectors or lists
#'
#' @return `x` with missing values from `y` appended
#'
#' @keywords internal
#' @noRd
#'
defaults <- function(x, y) c(x, y[setdiff(names(y), names(x))])
# Remove rownames from data frames and matrices
unrowname <- function(x) {
  if (is.data.frame(x)) {
    attr(x, "row.names") <- .set_row_names(.row_names_info(x, 2L))
  } else if (is.matrix(x)) {
    dimnames(x)[1] <- list(NULL)
  } else {
    cli::cli_abort("Can only remove rownames from {.cls data.frame} and {.cls matrix} objects")
  }
  x
}
#' Rename elements in a list, data.frame or vector
#'
#' This is akin to `dplyr::rename` and `plyr::rename`. It renames elements given
#' as names in the `replace` vector to the values in the `replace` vector
#' without touching elements not referenced.
#'
#' @param x A data.frame or a named vector or list
#' @param replace A named character vector. The names identifies the elements in
#' `x` that should be renamed and the values gives the new names.
#'
#' @return `x`, with new names according to `replace`
#'
#' @keywords internal
#' @noRd
#'
rename <- function(x, replace) {
  current_names <- names(x)
  old_names <- names(replace)
  missing_names <- setdiff(old_names, current_names)
  if (length(missing_names) > 0) {
    replace <- replace[!old_names %in% missing_names]
    old_names <- names(replace)
  }
  names(x)[match(old_names, current_names)] <- as.vector(replace)
  x
}
# Adapted from plyr:::id_vars
# Create a unique id for elements in a single vector
id_var <- function(x, drop = FALSE) {
  if (length(x) == 0) {
    id <- integer()
    n = 0L
  } else if (!is.null(attr(x, "n")) && !drop) {
    return(x)
  } else if (is.factor(x) && !drop) {
    x <- addNA(x, ifany = TRUE)
    id <- as.integer(x)
    n <- length(levels(x))
  } else {
    levels <- sort(unique0(x), na.last = TRUE)
    id <- match(x, levels)
    n <- max(id)
  }
  attr(id, "n") <- n
  id
}
#' Create an unique integer id for each unique row in a data.frame
#'
#' Properties:
#' - `order(id)` is equivalent to `do.call(order, df)`
#' - rows containing the same data have the same value
#' - if `drop = FALSE` then room for all possibilites
#'
#' @param .variables list of variables
#' @param drop Should unused factor levels be dropped?
#'
#' @return An integer vector with attribute `n` giving the total number of
#' possible unique rows
#'
#' @keywords internal
#' @noRd
#'
id <- function(.variables, drop = FALSE) {
  nrows <- NULL
  if (is.data.frame(.variables)) {
    nrows <- nrow(.variables)
    .variables <- unclass(.variables)
  }
  lengths <- lengths(.variables)
  .variables <- .variables[lengths != 0]
  if (length(.variables) == 0) {
    n <- nrows %||% 0L
    id <- seq_len(n)
    attr(id, "n") <- n
    return(id)
  }
  if (length(.variables) == 1) {
    return(id_var(.variables[[1]], drop = drop))
  }
  ids <- rev(lapply(.variables, id_var, drop = drop))
  p <- length(ids)
  ndistinct <- vapply(ids, attr, "n", FUN.VALUE = numeric(1), USE.NAMES = FALSE)
  n <- prod(ndistinct)
  if (n > 2^31) {
    char_id <- inject(paste(!!!ids, sep = "\r"))
    res <- match(char_id, unique0(char_id))
  }
  else {
    combs <- c(1, cumprod(ndistinct[-p]))
    mat <- inject(cbind(!!!ids))
    res <- c((mat - 1L) %*% combs + 1L)
  }
  if (drop) {
    id_var(res, drop = TRUE)
  }
  else {
    res <- as.integer(res)
    attr(res, "n") <- n
    res
  }
}
#' Count number of occurences for each unique combination of variables
#'
#' Each unique combination of the variables in `df` given by `vars` will be
#' identified and their occurences counted. If `wt_var` is given the counts will
#' be weighted by the values in this column.
#'
#' @param df A data.frame
#' @param vars A vector of column names. If `NULL` all columns in `df` will be
#' used
#' @param wt_var The name of a column to use as weight
#'
#' @return A data.frame with the unique combinations counted along with a `n`
#' column giving the counts
#'
#' @keywords internal
#' @noRd
#'
count <- function(df, vars = NULL, wt_var = NULL) {
  df2 <- if (is.null(vars)) df else df[vars]
  id <- id(df2, drop = TRUE)
  u_id <- !duplicated(id)
  labels <- df2[u_id, , drop = FALSE]
  labels <- labels[order(id[u_id]), , drop = FALSE]
  if (is.null(wt_var)) {
    freq <- tabulate(id, attr(id, "n"))
  } else {
    wt <- .subset2(df, wt_var)
    freq <- vapply(split(wt, id), sum, numeric(1))
  }
  data_frame0(labels, n = freq)
}
# Adapted from plyr::join.keys
# Create a shared unique id across two data frames such that common variable
# combinations in the two data frames gets the same id
join_keys <- function(x, y, by) {
  joint <- vec_rbind0(x[by], y[by])
  keys <- id(joint, drop = TRUE)
  n_x <- nrow(x)
  n_y <- nrow(y)
  list(x = keys[seq_len(n_x)], y = keys[n_x + seq_len(n_y)],
       n = attr(keys, "n"))
}
#' Replace specified values with new values, in a factor or character vector
#'
#' An easy to use substitution of elements in a string-like vector (character or
#' factor). If `x` is a character vector the matching elements will be replaced
#' directly and if `x` is a factor the matching levels will be replaced
#'
#' @param x A character or factor vector
#' @param replace A named character vector with the names corresponding to the
#' elements to replace and the values giving the replacement.
#'
#' @return A vector of the same class as `x` with the given values replaced
#'
#' @keywords internal
#' @noRd
#'
revalue <- function(x, replace) {
  if (is.character(x)) {
    replace <- replace[names(replace) %in% x]
    if (length(replace) == 0) return(x)
    x[match(names(replace), x)] <- replace
  } else if (is.factor(x)) {
    lev <- levels(x)
    replace <- replace[names(replace) %in% lev]
    if (length(replace) == 0) return(x)
    lev[match(names(replace), lev)] <- replace
    levels(x) <- lev
  } else if (!is.null(x)) {
    stop_input_type(x, "a factor or character vector")
  }
  x
}
# Iterate through a formula and return a quoted version
simplify_formula <- function(x) {
  if (length(x) == 2 && x[[1]] == as.name("~")) {
    return(simplify(x[[2]]))
  }
  if (length(x) < 3)
    return(list(x))
  op <- x[[1]]
  a <- x[[2]]
  b <- x[[3]]
  if (op == as.name("+") || op == as.name("*") || op ==
      as.name("~")) {
    c(simplify(a), simplify(b))
  }
  else if (op == as.name("-")) {
    c(simplify(a), bquote(-.(x), list(x = simplify(b))))
  }
  else {
    list(x)
  }
}
#' Create a quoted version of x
#'
#' This function captures the special meaning of formulas in the context of
#' facets in ggplot2, where `+` have special meaning. It works as
#' `plyr::as.quoted` but only for the special cases of `character`, `call`, and
#' `formula` input as these are the only situations relevant for ggplot2.
#'
#' @param x A formula, string, or call to be quoted
#' @param env The environment to a attach to the quoted expression.
#'
#' @keywords internal
#' @noRd
#'
as.quoted <- function(x, env = parent.frame()) {
  x <- if (is.character(x)) {
    lapply(x, function(x) parse(text = x)[[1]])
  } else if (is.formula(x)) {
    simplify_formula(x)
  } else if (is.call(x)) {
    as.list(x)[-1]
  } else {
    cli::cli_abort("Must be a character vector, call, or formula")
  }
  attributes(x) <- list(env = env, class = 'quoted')
  x
}
# round a number to a given precision
round_any <- function(x, accuracy, f = round) {
  check_numeric(x)
  f(x/accuracy) * accuracy
}

#' Apply function to unique subsets of a data.frame
#'
#' This function is akin to `plyr::ddply`. It takes a single data.frame,
#' splits it by the unique combinations of the columns given in `by`, apply a
#' function to each split, and then reassembles the results into a sigle
#' data.frame again.
#'
#' @param df A data.frame
#' @param by A character vector of column names to split by
#' @param fun A function to apply to each split
#' @param ... Further arguments to `fun`
#' @param drop Should unused factor levels in the columns given in `by` be
#' dropped.
#'
#' @return A data.frame if the result of `fun` does not include the columns
#' given in `by` these will be prepended to the result.
#'
#' @keywords internal
#' @noRd
dapply <- function(df, by, fun, ..., drop = TRUE) {
  grouping_cols <- .subset(df, by)
  fallback_order <- unique0(c(by, names(df)))
  apply_fun <- function(x) {
    res <- fun(x, ...)
    if (is.null(res)) return(res)
    if (length(res) == 0) return(data_frame0())
    vars <- lapply(stats::setNames(by, by), function(col) .subset2(x, col)[1])
    if (is.matrix(res)) res <- ggplot2:::split_matrix(res)
    if (is.null(names(res))) names(res) <- paste0("V", seq_along(res))
    if (all(by %in% names(res))) return(data_frame0(!!!unclass(res)))
    res <- ggplot2:::modify_list(unclass(vars), unclass(res))
    res <- res[intersect(c(fallback_order, names(res)), names(res))]
    data_frame0(!!!res)
  }

  # Shortcut when only one group
  if (all(vapply(grouping_cols, single_value, logical(1)))) {
    return(apply_fun(df))
  }

  ids <- id(grouping_cols, drop = drop)
  group_rows <- split_with_index(seq_len(nrow(df)), ids)
  result <- lapply(seq_along(group_rows), function(i) {
    cur_data <- ggplot2:::df_rows(df, group_rows[[i]])
    apply_fun(cur_data)
  })
  vec_rbind0(!!!result)
}

single_value <- function(x, ...) {
  UseMethod("single_value")
}
#' @export
single_value.default <- function(x, ...) {
  # This is set by id() used in creating the grouping var
  identical(attr(x, "n"), 1L)
}
#' @export
single_value.factor <- function(x, ...) {
  # Panels are encoded as factor numbers and can never be missing (NA)
  identical(levels(x), "1")
}


theme_tree <- function(bgcolor="white", ...) {

  list(xlab(NULL),
       ylab(NULL),
       theme_tree2_internal() +
         theme(panel.background=element_rect(fill=bgcolor, colour=bgcolor),
               axis.line.x = element_blank(),
               axis.text.x = element_blank(),
               axis.ticks.x = element_blank(),
               ...)
  )

  ## theme_void() +
  ##     theme(panel.background=element_rect(fill=bgcolor, colour=bgcolor),
  ##           ...)
}

theme_tree2_internal <- function(bgcolor="white", fgcolor="black",
                                 legend.position="right",
                                 panel.grid.minor=element_blank(),
                                 panel.grid.major=element_blank(),
                                 panel.border=element_blank(),
                                 axis.line.y=element_blank(),
                                 axis.ticks.y=element_blank(),
                                 axis.text.y=element_blank(),...) {
  ## need to set axis.line otherwise the setting cannot be inherited.
  ## https://github.com/GuangchuangYu/ggtree/issues/218

  theme_bw() +
    theme(legend.position=legend.position,
          panel.grid.minor=panel.grid.minor,
          panel.grid.major=panel.grid.major,
          panel.background=element_rect(fill=bgcolor, colour=bgcolor),
          panel.border=panel.border,
          axis.line=element_line(color=fgcolor),
          ##axis.line.x=element_line(color=fgcolor),
          axis.line.y=axis.line.y,
          axis.ticks.y=axis.ticks.y,
          axis.text.y=axis.text.y,
          ...)
}

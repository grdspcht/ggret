geom_retlab <- function(mapping = NULL, nudge_x = 0, nudge_y = 0, geom = "text", hjust = 0.5, ...) {
  self_mapping <- aes_(subset = ~isRet)
  if (is.null(mapping)) {
    mapping <- self_mapping
  } else {
    mapping <- modifyList(self_mapping, mapping)
  }

  geom_tiplab(mapping, offset = nudge_x, nudge_y = nudge_y, geom = geom, hjust = hjust, ...)
}

enum <- function(...) {
  nms <- eval(substitute(alist(...)))
  x <- as.list(setNames(seq_along(nms), nms))
  x
}

variance_types <- enum(homoskedastic, hetereskedastic)
#algorithms <- enum(em, cem)

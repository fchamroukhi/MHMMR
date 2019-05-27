enum <- function(...) {
  nms <- eval(substitute(alist(...)))
  x <- as.list(setNames(seq_along(nms), nms))
  return(x)
}

variance_types <- enum(homoskedastic, heteroskedastic)

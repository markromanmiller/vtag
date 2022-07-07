# `tags(v)` result with list of named pairs
# `tag(v, name)` is single value, potentially null.
# `tags()<-` and tag(x) <- do
# across(.cols = where(~((tag(.x, "coord")) == ") coord = "raw" / "root" / "visible")

tag <- function(v, ...) {
  vtag <- attr(v, "vtag")
  dots <- list(...)

  for (i in seq_along(dots)) {
    name_i <- names(dots[i])
    if (is.null(name_i) || name_i == "") {
      vtag <- append(vtag, dots[[i]])
    } else {
      vtag[[name_i]] <- dots[[i]]
    }
  }

  attr(v, "vtag") <- vtag
  invisible(v)
}

has_tag <- function(v, tag_value) {
  tag_value %in% tags(v)
}

tags <- function(v) {
  attr(v, "vtag")
}

tag_factory <- function(...) {
  function(v) {
    tag(v, ...)
  }
}

tag_predicate <- function(tag_value) {
  function(v) {
    has_tag(v, tag_value)
  }
}

carry_tags <- function(v, fn, ...) {
  vtag <- tags(v)
  result <- fn(v)
  attr(result, "vtag") <- vtag
  tag(result, ...)
}

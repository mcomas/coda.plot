utils::globalVariables(c(
  "W", "x1", "x2", "x1end", "x2end",
  "c1", "c2", "c3", ".A", ".B", ".C", "group", "lx.diff", "part", "shape", "lab",
  "x", ".x", "xend", ".xend", "xlower", "xlower5", "xmiddle", "xupper", "xupper5",
  "y", ".y", "yend", ".yend", "text", "nudge_x", "label", "ymin_minor", "ymin_major",
  "ymax_minor", "ymax_major", "gid"
))
# internal helper in R/utils-optional.R
.require_suggested <- function(pkg, min_version = NULL, reason = NULL, ask_install = TRUE) {
  ok <- requireNamespace(pkg, quietly = TRUE)
  if (ok && !is.null(min_version)) {
    ok <- utils::packageVersion(pkg) >= min_version
  }
  if (ok) return(invisible(TRUE))

  msg <- sprintf(
    "The suggested package '%s'%s is required for %s.\nInstall it with install.packages('%s').",
    pkg,
    if (!is.null(min_version)) paste0(" (>= ", min_version, ")") else "",
    ifelse(is.null(reason), "this functionality", reason),
    pkg
  )

  if (ask_install && interactive()) {
    cat(msg, "\nWould you like me to install it now? [y/N] ")
    ans <- tolower(trimws(readline()))
    if (startsWith(ans, "y")) {
      utils::install.packages(pkg)
      if (requireNamespace(pkg, quietly = TRUE)) return(invisible(TRUE))
    }
  }

  stop(msg, call. = FALSE)
}

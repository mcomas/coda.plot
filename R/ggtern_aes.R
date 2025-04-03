# -----------------------------------------------------------------------------
# Title: Adaptation of functions from 'ggtern'
# Author: Marc Comas
# Date: April 3, 2025
#
# Description:
# This script reuses and slightly modifies code from the 'ggtern' package,
# originally developed by Nicholas Hamilton. The purpose of this adaptation
# is to remove dependencies on other packages.
#
# License:
# This script is distributed under the GNU General Public License v2 (GPL-2).
# In accordance with the original license of the 'ggtern' package, any
# redistribution or modification of this code must preserve the same license.
#
# Original copyright: Nicholas Hamilton
# Original package: ggtern (https://cran.r-project.org/web/packages/ggtern/index.html)
# Original license: GPL-2
# -----------------------------------------------------------------------------


aes <- function(x,y,z,...) {
  aes <- structure(as.list(match.call()[-1]), class = "uneval")
  rename_aes(aes)
}

find_global_tern <- function (name, env=environment(),mode='any'){
  if(!is.character(name)){stop("'name' must be provided as a character")}
  if(!inherits(environment(),"environment")){stop("'env' must inherit the environment class")}

  if (exists(name, envir = env, mode = mode)){
    return(get(name, envir = env, mode = mode))
  }

  nsenv <- asNamespace("ggtern")
  if(exists(name, envir = nsenv, mode=mode)){
    return(get(name, envir = nsenv, mode = mode))
  }

  nsenv <- asNamespace("ggplot2")
  if(exists(name, envir = nsenv, mode=mode)){
    return(get(name, envir = nsenv, mode = mode))
  }

  NULL
}
# Rename American or old-style aesthetics name
rename_aes <- function(x) {
  aa = c(getFromNamespace('.all_aesthetics','ggplot2'),"T","L","R","zend")
  # Convert prefixes to full names
  full <- match(names(x),aa)
  names(x)[!is.na(full)] <- aa[full[!is.na(full)]]

  rename_base = function(x, name_map) {
    old_names = names(name_map)
    new_names = unname(name_map)

    current_names = names(x)
    matched = match(old_names, current_names)
    for (i in seq_along(matched)) {
      if (!is.na(matched[i])) {
        names(x)[matched[i]] = new_names[i]
      }
    }
    x
  }
  # rename(x, find_global_tern(".base_to_ggplot"), warn_missing = FALSE)
  rename_base(x, find_global_tern(".base_to_ggplot"))
}

# Look up the scale that should be used for a given aesthetic -- ternary version
aes_to_scale_tern = function (var){
  var = ggint$aes_to_scale(var)
  var[var %in% c("z", "zmin", "zmax", "zend", "zintercept")] <- "z"
  var
}

# Figure out if an aesthetic is a position aesthetic or not
is_position_aes <- function(vars) {
  aes_to_scale_tern(vars) %in% c("x", "y", "z")
}


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


ifthenelse <- function(x,a,b){
  if(!is.logical(x))stop("x argument must be logical")
  if(x){a}else{b}
}

empty = function (df) {
  (is.null(df) || nrow(df) == 0 || ncol(df) == 0)
}


is.numericor <- function(A,B){
  if(!is.numeric(B)){stop("b must be numeric")}
  if(is.numeric(A)){A}else{B}
}

"%||%"  <- function(a, b) {if (!is.null(a)) a else b}

find_global_tern <- function (name, env=environment(),mode='any'){
  if(!is.character(name)){stop("'name' must be provided as a character")}
  if(!inherits(environment(),"environment")){stop("'env' must inherit the environment class")}

  if (exists(name, envir = env, mode = mode)){
    return(get(name, envir = env, mode = mode))
  }

  # nsenv <- asNamespace("ggtern")
  # if(exists(name, envir = nsenv, mode=mode)){
  #   return(get(name, envir = nsenv, mode = mode))
  # }

  nsenv <- asNamespace("ggplot2")
  if(exists(name, envir = nsenv, mode=mode)){
    return(get(name, envir = nsenv, mode = mode))
  }

  NULL
}

rgb2hex = function(r = 0, g = 0, b = 0){
  df = data.frame(r, g, b)
  check = function(x, ix = NULL){
    nm = deparse(substitute(x))
    ix = as.character({ix %||% ''})
    if(!is.numeric(x))  stop(sprintf("'%s%s' must be numeric",             nm,ix),call. = FALSE)
    if(length(x) != 1)  stop(sprintf("'%s%s' must be scalar",              nm,ix),call. = FALSE)
    if(!is.finite(x))   stop(sprintf("'%s%s' must be finite",              nm,ix),call. = FALSE)
    if(x < 0 | x > 255) stop(sprintf("'%s%s' must be in the range [0,255]",nm,ix),call. = FALSE)
  }
  nr = nrow(df)
  sapply( c(1:nr), function(ix){
    n = if(nr > 1){ ix }else{ NULL }
    r = df$r[ix]; check(r,n)
    g = df$g[ix]; check(g,n)
    b = df$b[ix]; check(b,n)
    sprintf("#%.2x%.2x%.2x",r,g,b)
  })
}

breaks_tern = function(limits = c(0,1), isMajor = TRUE, n = 5){
  if(is.null(limits) || !all(is.numeric(limits)))
    limits = c(0,1)

  if(diff(range(limits)) == 0){
    ret = if(isMajor) getOption("tern.breaks.default") else getOption("tern.breaks.default.minor")
    return(ret)
  }

  ret = pretty(limits,n = n)
  if(!isMajor){
    r = range(ret)
    d = diff(r)/(length(ret)-1)
    minor = seq(min(ret)-d/2,max(ret)+d/2,by = d)
    minor = minor[which(minor > min(limits) & minor < max(limits))]
    ret   = minor[which(!minor %in% ret)]
  }
  ret
}




labels_tern = function(limits = c(0,1), breaks = breaks_tern(limits), format = "%g", factor = 100){
  if(!is.numeric(breaks))
    stop("'breaks' must be numeric",call.=FALSE)

  #Default Result
  result = factor[1]*breaks

  #Try and process...
  tryCatch({
    if(!is.numeric(factor))
      stop("'factor' must be numeric",call.=FALSE)
    result = sprintf(format,factor[1]*breaks)

    #Stop First Label interfering with the main label
    if(breaks[1] == min(limits))
      result[1] = ''

  },error=function(e){ })

  #Done
  result
}

#internal
.makeValid <- function(x){
  x = x[[1]]
  if(is(x,'character')){
    x = gsub("%","'%'",x)
    x = gsub('([[:punct:]])\\1+', '\\1', x)
    x = gsub(" ","~",x)
  }
  x
}

.trimAndPad <- function(x){
  x = gsub("^(\\s+)","",gsub("(\\s+)$","",x))
  if(nchar(x) == 1) x = sprintf(" %s ",x)
  x
}

arrow_label_formatter             = function(label,suffix=NULL,sep="/",...) UseMethod("arrow_label_formatter")

#' @export
arrow_label_formatter.default     = function(label,suffix=NULL,sep="/",...) arrow_label_formatter.character( as.character(label), suffix, sep, ...)

#' @export
arrow_label_formatter.call        = function(label,suffix=NULL,sep="/",...) arrow_label_formatter.expression(as.expression(label),suffix, sep, ...)

#' @export
arrow_label_formatter.expression  = function(label,suffix=NULL,sep="/",...){
  suffix = if(suffix  == "")   NULL else suffix
  sep    = if(is.null(suffix)) ""   else .trimAndPad(sep)
  parse(text=paste(as.character(label),suffix,sep))
}

#' @export
arrow_label_formatter.character   = function(label,suffix=NULL,sep="/",latex = FALSE,...) {
  suffix = if(suffix  == "")   NULL else suffix
  sep    = if(is.null(suffix)) ""   else .trimAndPad(sep)
  result = paste(label,suffix,sep=sep)
  if(latex[1]) result = TeX(result)
  result
}

.trimAndPad <- function(x){
  x = gsub("^(\\s+)","",gsub("(\\s+)$","",x))
  if(nchar(x) == 1) x = sprintf(" %s ",x)
  x
}

label_formatter = function(label,...){ arrow_label_formatter(label,suffix="",sep="",...) }

joinCharacterSeries <- function(x,lastWord='and'){
  if(!is.character(x) | !is.vector(x)) stop("'x' must be character vector",call.=FALSE)
  if(length(x) > 1){ x = paste(paste(x[-length(x)],collapse="', '"),x[length(x)],sep=sprintf("' %s '",lastWord)) }
  sprintf("'%s'",x)
}

identityInv = function(z) identity(z)


getFormulaVars = function(x,dependent=TRUE) {
  if(!is(x,'formula')) stop("x argument must be a formula",call.=FALSE)
  all.vars(x[[if(dependent) 3 else 2]])
}

scales_add_missing_tern <- function(plot){

  #Run some checks
  stopifnot(inherits(plot,'ggplot'))
  stopifnot(inherits(plot$coordinates,'CoordTern'))

  #Ensure required scales have been added
  rs = plot$coordinates$required_scales

  aesthetics  = setdiff(rs, plot$scales$input())
  env = plot$plot_env
  for (aes in aesthetics) {
    scale_name <- paste("scale", aes, "continuous", sep = "_")
    scale_f <- find_global_tern(scale_name, env, mode = "function")
    plot$scales$add(scale_f())
  }

  #ggint$scales_add_missing(plot,rs,plot$plot_env) ##NH
  #plot$scales$scales = plot$scales$scales[!sapply(plot$scales$scales,is.null)]
  #plot$scales$scales = compact(plot$scales$scales)

  #Push some details to the coordinates
  plot$coordinates$scales        = sapply(rs,plot$scales$get_scales) ##NH
  for(r in rs)
    plot$coordinates$limits[[r]] = plot$scales$get_scales(r)$limits
  plot$coordinates$labels_coord  = plot$labels
  plot$coordinates$theme         = ggint$plot_theme(plot) #NH

  #done
  plot
}

layers_add_or_remove_mask = function(plot){
  theme = ggint$plot_theme(plot) #NH
  mask  = calc_element('tern.panel.mask.show',theme)[1] %||% TRUE
  if(is.na(mask) || mask){
    if(!"GeomMask" %in% unlist(lapply(plot$layers,function(x){ class(x$geom) })))
      plot = plot + geom_mask()
  }else{
    # plot$layers = plyr::compact(lapply(plot$layers,function(x){
    #   if(inherits(x$geom,'GeomMask')) return(NULL) else x
    # }))
    plot$layers = Filter(Negate(is.null), lapply(plot$layers,function(x){
      if(inherits(x$geom,'GeomMask')) return(NULL) else x
    }))
  }
  plot
}




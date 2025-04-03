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


geom_mask <- function() {
  layer(
    data        = data.frame(x=1,y=1,z=1),
    mapping     = NULL,
    stat        = "identity",
    geom        = GeomMask,
    position    = "identity",
    show.legend = FALSE,
    inherit.aes = FALSE,
    params      = list(
      na.rm     = TRUE
    )
  )
}

GeomMask <- ggproto("GeomMask", Geom,
  default_aes = aes("x","y","z"),
  draw_panel  = function(self, data, panel_params, coord){

    #Initially Empty Items
    items = list()

    #Only for coord tern
    if(inherits(coord,'CoordTern')){

      tryCatch({
        theme = coord$theme %||% theme_get()
        e     = calc_element('tern.plot.background',theme,verbose=FALSE)

        if(!identical(e,element_blank())){

          #Debug Mode
          dbg   = getOption('tern.mask.debug',FALSE)

          #1st pass is master triangle.
          ex  = .get.tern.extremes(coord,panel_params,transform=FALSE)
          ex  = coord$transform(ex, panel_params = panel_params)
          ex  = rbind(ex,ex[1,,drop=F])

          #Build a specific viewport value
          vp <- grid::viewport(x     = 0.5,
                         y     = 0.5,
                         width = 1,
                         height= 1,
                         just  = c("center","center"),
                         clip  = 'inherit' #OFF
          )

          #Key Limits
          a = c(0.0,1.0)
          b = c(0.5,0.5)

          #1st pass traces the all borders includeing the inside triangle,
          #2nd pass renders the convex hull (outer border)
          for(ix in c(1:2)){

            #Build the xvalues and yvalues
            #When ix == 1, include the center triangular cut-out
            xvals = c(a[1],a[1],if(ix==1){c(b[1],ex$x,b[2])},a[2],a[2],a[1])
            yvals = c(a[1],a[2],if(ix==1){c(a[2],ex$y,a[2])},a[2],a[1],a[1])

            #Local Fill Variable
            fillLoc = if(ix == 2 | is.null(e$fill)) NA else e$fill

            #Draw the full set of mask lines if in debug mode, for debugging.
            if(dbg){
              sizeLoc = if(ix == 2) 0.5 else 1
              colLoc  = if(ix == 2) 'black' else 'red'
            }else{
              sizeLoc = if(ix == 2) is.numericor(e$linewidth,0) else 0
              colLoc  = if(ix == 2) e$colour else fillLoc
            }

            #Build the Grob with the custom viewport
            grob     <- grid::polygonGrob(  x = xvals,
                                      y = yvals,
                                      default.units = "npc",
                                      id   = rep(1,length(xvals)),
                                      vp   = vp,
                                      name = sprintf("mask-%i",ix),
                                      gp   = grid::gpar(  col  = colLoc %||% 'transparent',
                                                    fill = fillLoc %||% 'transparent',
                                                    lwd  = sizeLoc,
                                                    lty  = e$linetype)

            )

            #Add the grob to the items
            items[[length(items) + 1]] = grob
          }
        }

        #Render Axis items on top of the mask, if grids are on top, this
        #will be rendered in the coord_tern render_fg routine instead.
        if(!.theme.get.gridsontop(theme)){
          extrm = .get.tern.extremes(coord,panel_params,transform=TRUE)
          items = .render.fgset(coord,extrm,scale_details,theme,items)
        }

      },error=function(e){
        writeLines(as.character(e))
      })
    }

    do.call(grid::gList,items)
  },
  draw_key = FALSE
)

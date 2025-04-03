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


ggtern <- function(data=NULL,mapping=aes(),...,environment=parent.frame()){
  ## Suppress the warning notice of new coordinate system
  suppressMessages({
    ggplot(data = data, mapping = mapping, environment = environment, ... ) + coord_tern()
  })
}

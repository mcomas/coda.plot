library(ggtern)
library(tidyverse)
coda_segment = function(A, B, steps = 100, NMS = c('x','y','z')){
  require(tidyverse)
  tibble(s = c(0, ppoints(steps), 1)) %>%
    mutate(map_df(s, ~set_names(A^(1-.x) * B^.x, NMS)))
}
ggtern(data = tibble(x=numeric(0), y=numeric(0), z=numeric(0)), aes(x,y,z)) +
  geom_path(data = coda_segment(A = c(10,1,5), B = c(1,10,5)), col = 'blue') +
  geom_path(data = coda_segment(A = c(100,1,10), B = c(1,100,10)), col = 'red') +
  geom_path(data = coda_segment(A = c(1000,1,100), B = c(1,1000,100)), col = 'green') +
  theme_minimal()


# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse, gtools, ncdf4, lubridate, cwhmisc, chron, epitools, doSNOW, foreach, parallel)

# Initial setup  ----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
opions(scipen = 999)

# Functions to use --------------------------------------------------------
myExtract <- function(gcm, variable, yr){
  # gcm <- gcms[1]
  # variable <- vars[1]
  # yr <- yrs[1]
  fle <- grep(gcm, fls, value = TRUE) %>% 
    grep(variable, ., value = TRUE) %>% 
    grep(yr, ., value = TRUE)
  stk <- stack(fle)
  stk <- raster::crop(stk, bsn) %>% 
    raster::mask(., bsn)
  tbl <- rasterToPoints(stk) %>% 
    as_tibble() %>% 
    gather(day, value, -x, -y) %>% 
    mutate(gc = gcm, 
           variabl = variable,
           year = yr) %>% 
    dplyr::select(gc, variabl, x, y, year, day, value)
  saveRDS(object = tbl, file = paste0('rds/clim/', gcm, '_', variable, '_', yr, '.rds'))
  return(tbl)
}

# Load data ---------------------------------------------------------------
fls <- list.files('nc/ext', full.names = TRUE, pattern = '.nc$') %>% 
  mixedsort()
bsn <- shapefile('../_shp/_base/basins_geo.shp')
bsn <- bsn[bsn@data$name %in% 'Tua',]

# Extracting by variables
tmax <- grep('tasmax', fls, value = TRUE)
tmin <- grep('tasmin', fls, value = TRUE)
prec <- grep('pr', fls, value = TRUE)

# Extracting each gcm
gcms <- str_sub(tmax, 37, nchar(tmax) - 8) %>% 
  unique()
yrs <- str_sub(fls, start = nchar(fls) - 6, end = nchar(fls) - 3) %>% 
  unique() %>% 
  mixedsort() %>% 
  as.numeric()
vars <- c('tasmax', 'tasmin', 'pr')

# Apply the fucntion ------------------------------------------------------
detectCores()
cl <- makeCluster(10)
registerDoSNOW(cl)

# Precipitacion
foreach(g = 1:length(gcms), .packages = c('raster', 'rgdal', 'dplyr', 'tidyverse', 'foreach', 'parallel', 'gtools'), .verbose = TRUE) %dopar% {
  foreach(y = 1:length(yrs)) %do% {    
    myExtract(gcm = gcms[g], variable = 'pr', yr = yrs[y])
  }
} 
stopCluster(cl)

# T Max
foreach(g = 1:length(gcms), .packages = c('raster', 'rgdal', 'dplyr', 'tidyverse', 'foreach', 'parallel', 'gtools'), .verbose = TRUE) %dopar% {
  foreach(y = 1:length(yrs)) %do% {    
    myExtract(gcm = gcms[g], variable = 'tasmax', yr = yrs[y])
  }
} 
stopCluster(cl)

# T min
foreach(g = 1:length(gcms), .packages = c('raster', 'rgdal', 'dplyr', 'tidyverse', 'foreach', 'parallel', 'gtools'), .verbose = TRUE) %dopar% {
  foreach(y = 1:length(yrs)) %do% {    
    myExtract(gcm = gcms[g], variable = 'tasmin', yr = yrs[y])
  }
} 
stopCluster(cl)












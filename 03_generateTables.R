
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Function to use ---------------------------------------------------------
generateTable <- function(var){
  # var <- 'tm'
  if(var == 'prec'){
    x <- tbl %>% 
      dplyr::select(gc, year, day, prec) %>%
      dplyr::mutate(day = parse_number(day)) %>% 
      dplyr::filter(gc != 'MIROC-ESM') %>% 
      dplyr::group_by(year, day) %>% 
      dplyr::summarise(prec = mean(prec)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(prec = round(prec, 1))
  } else {
    x <- tbl %>% 
      dplyr::select(gc, year, day, tmax, tmin) %>%
      dplyr::mutate(day = parse_number(day)) %>% 
      dplyr::filter(gc != 'MIROC-ESM') %>% 
      dplyr::group_by(year, day) %>% 
      dplyr::summarise(tmax = mean(tmax),
                       tmin = mean(tmin)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(tmax = round(tmax,1 ),
                    tmin = round(tmin, 1))
    
  }
  print('Done!')
  return(x)
}

# Load data ---------------------------------------------------------------
tbl <- read_csv('tbl/clim_future_RCP85.csv')

# Quality Control ---------------------------------------------------------
gcms <- unique(tbl$gc)

tbl %>% 
  group_by(gc, year) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  distinct(count)

# Apply the function ------------------------------------------------------
pr <- generateTable(var = 'prec')
tm <- generateTable(var = 'tm')

# Creating the final table ------------------------------------------------
pr <- pr %>% 
  dplyr::select(prec) 
write.table(pr, paste0('tbl/pr.txt'), row.names = FALSE, col.names = TRUE, quote = FALSE)

tm <- tm %>% 
  dplyr::select(tmax, tmin) 
write.table(tm, paste0('tbl/tm.txt'), row.names = FALSE, col.names = TRUE, quote = FALSE, sep = ',')

dem <- raster('../_tif/_dem/bsn/dem_tua') * 1
crd <- data.frame(lon = -72.8924, lat = 4.90402)
vls <- raster::extract(dem, crd[,1:2])



# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
myRbind <- function(var){
  # var <- vrs[1]
  tbl <- grep(var, fls, value = TRUE) %>% 
    map(.x = ., .f = readRDS) %>% 
    bind_rows()
  if(var == 'pr'){
    tbl <- tbl %>% 
      mutate(value = round(value * 86400, digits = 1))
  } else {
    tbl <- tbl %>% 
      mutate(value = round(value - 273.15, digits = 1))
  }
  print('Done!')
  return(tbl)
}

reviewDays <- function(tbl){
  # tbl <- prec
  x <- tbl %>% 
    group_by(gc, year) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    distinct(count)
  y <- tbl %>% 
    group_by(gc, year) %>% 
    summarise(count = n()) %>% 
    ungroup()
  z <- y %>% 
    filter(count %in% c(730, 732))
  print('Done!')
  return(z)
}

# Load data ---------------------------------------------------------------
fls <- list.files('rds/clima/', full.names = TRUE, patter = '.rds$')
vrs <- c('pr', 'tasmax', 'tasmin')

# Join tables -------------------------------------------------------------
prec <- myRbind(var = vrs[1]) %>% 
  rename(prec = value) %>% 
  dplyr::select(-variabl)
tmax <- myRbind(var = vrs[2]) %>% 
  rename(tmax = value) %>% 
  dplyr::select(-variable)
tmin <- myRbind(var = vrs[3]) %>% 
  rename(tmin = value) %>% 
  dplyr::select(-variable)

# Review tables -----------------------------------------------------------
prec_rvw <- reviewDays(prec)
tmax_rvw <- reviewDays(tmax)
tmin_rvw <- reviewDays(tmin)

tble <- inner_join(tmax, tmin, by = c('gc' = 'gc', 'year' = 'year', 'day' = 'day', 'x' = 'x', 'y' = 'y'))
tble <- inner_join(tble, prec, by = c('gc' = 'gc', 'year' = 'year', 'day' = 'day', 'x' = 'x', 'y' = 'y'))

# Quality control ---------------------------------------------------------
tble <- tble %>% 
  mutate(comparison = ifelse(tmax > tmin, TRUE, FALSE))
write.csv(tble, 'tbl/clim_future_RCP85.csv', row.names = FALSE)

# --------------------------------------------------------------------------
# END ----------------------------------------------------------------------
# --------------------------------------------------------------------------






# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, velox, sf, tidyverse)

# Initial setup -----------------------------------------------------------
g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
tbl <- read_csv('tbl/clim_future_RCP85.csv')
gcm <- unique(tbl$gc)

c1 <- data.frame(cl = 1, gcm = c('bcc-csm1-1', 'MRI-CGCM3', 'MPI-ESM-LR'))
c2 <- data.frame(cl = 2, gcm = c('NorESM1-M', 'MIROC-ESM-CHEM', 'BNU-ESM', 'CESM1-BGC', 'GFDL-ESM2M'))
c3 <- data.frame(cl = 3, gcm = c('MIROC5', 'CCSM4', 'GFDL-ESM2G', 'inmcm4'))
c4 <- data.frame(cl = 4, gcm = c('CSIRO-Mk3-6-0', 'MPI-ESM-MR', 'GFDL-CM3', 'IPSL-CM5A-LR', 'IPSL-CM5A-MR', 'ACCESS1-0', 'CanESM2'))
cl <- rbind(c1, c2, c3, c4) %>% as_tibble() %>% mutate(gcm = as.character(gcm))

# Summarise ---------------------------------------------------------------
tb2 <- inner_join(tbl, cl, by = c('gc' = 'gcm'))
cls <- tb2 %>% 
  group_by(cl, year, day) %>% 
  summarise(tmax = mean(tmax),
            tmin = mean(tmin),
            prec = mean(prec)) %>% 
  ungroup() %>% 
  mutate(day = parse_number(day)) %>% 
  arrange(year, day)

# Generating the tables ---------------------------------------------------
createTable <- function(c, v){
  # c <- 1; v <- 'temp'
  if(v == 'temp'){
    d <- cls %>% 
      filter(cl == c) %>% 
      dplyr::select(tmax, tmin) %>% 
      mutate(tmax = round(tmax, 1),
             tmin = round(tmin, 1))
    write.table(d, paste0('tbl/tm_', c, '.txt'), row.names = FALSE, col.names = TRUE, quote = FALSE, sep = ',')
  } else {
    d <- cls %>% 
      filter(cl == c) %>% 
      dplyr::select(prec) %>% 
      mutate(prec = round(prec, 1))
    write.table(d, paste0('tbl/pr_', c, '.txt'), row.names = FALSE, col.names = TRUE, quote = FALSE, sep = ',')
  }
  print('Done!') 
}
  
for(i in 1:4){
  createTable(c = i, v = 'temp')
}  

for(i in 1:4){
  createTable(c = i, v = 'prec')
}  











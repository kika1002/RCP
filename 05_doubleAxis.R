
# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(tidyverse, gtools, stringr, epitools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
ftr <- read_csv('tbl/clim_future_RCP85.csv')
crn_tmp <- read_csv('D:/_request/_casanare/_workspace/_ideam/ideam/int/tbl_int_all_all.csv')
crn_ppt <- read_csv('D:/_request/_casanare/_workspace/_chirps/table_ppt.csv')

# Label table -------------------------------------------------------------
c1 <- data.frame(cl = 1, gcm = c('bcc-csm1-1', 'MRI-CGCM3', 'MPI-ESM-LR'))
c2 <- data.frame(cl = 2, gcm = c('NorESM1-M', 'MIROC-ESM-CHEM', 'BNU-ESM', 'CESM1-BGC', 'GFDL-ESM2M'))
c3 <- data.frame(cl = 3, gcm = c('MIROC5', 'CCSM4', 'GFDL-ESM2G', 'inmcm4'))
c4 <- data.frame(cl = 4, gcm = c('CSIRO-Mk3-6-0', 'MPI-ESM-MR', 'GFDL-CM3', 'IPSL-CM5A-LR', 'IPSL-CM5A-MR', 'ACCESS1-0', 'CanESM2'))
cl <- rbind(c1, c2, c3, c4) %>% as_tibble() %>% mutate(gcm = as.character(gcm))
rm(c1, c2, c3, c4)

# Processing data ---------------------------------------------------------
# Current -------
crn_tmp <- crn_tmp %>% filter(stt %in% c(35085050, 35195020, 35095110))
crn_ppt <- crn_ppt %>% filter(stt %in% c('Tua_1', 'Tua_2', 'Tua_3'))

crn_ppt_smm <- crn_ppt %>% 
  group_by(stt, year, mnth) %>% 
  summarise(value = sum(value)) %>% 
  ungroup() %>% 
  group_by(year, mnth) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  group_by(mnth) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  inner_join(., data.frame(mnth = 1:12, month = month.abb), by = 'mnth') %>% 
  dplyr::select(month, value) %>% 
  rename(prec = value)
crn_tmp_smm <- crn_tmp %>% 
  group_by(variable, stt, year, month) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  group_by(variable, year, month) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  group_by(variable, month) %>% 
  summarise(value = mean(value)) %>% 
  ungroup() %>% 
  spread(variable, value) %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  arrange(month)

crn <- inner_join(crn_ppt_smm, crn_tmp_smm, by = 'month')
crn <- mutate(crn, gcm = 'current')
rm(crn_ppt, crn_ppt_smm, crn_tmp, crn_tmp_smm)

# Future --------
ftr <- inner_join(ftr, cl, by = c('gc' = 'gcm'))
ftr <- ftr %>% mutate(day = parse_number(day)) 

mnts <- c(rep('Jan', 31), rep('Feb', 28), rep('Mar', 31), rep('Apr', 30), rep('May', 31), rep('Jun', 30),
          rep('Jul', 31), rep('Aug', 31), rep('Sep', 30), rep('Oct', 31), rep('Nov', 30), rep('Dec', 31))
no_leap <- data.frame(julian = 1:365,
                      month = mnts)
mnts <- c(rep('Jan', 31), rep('Feb', 29), rep('Mar', 31), rep('Apr', 30), rep('May', 31), rep('Jun', 30),
          rep('Jul', 31), rep('Aug', 31), rep('Sep', 30), rep('Oct', 31), rep('Nov', 30), rep('Dec', 31))
leap <- data.frame(julian = 1:366,
                   month = mnts)
yrs_leap <- c(2016, 2020, 2024, 2028, 2032, 2036, 2040, 2044)
yrs_no_leap <- c(2017:2019, 2021:2023, 2025:2027, 2029:2031, 2033:2035, 2037:2039, 2041:2043, 2045)

# Leap
ftr_leap <- ftr %>% filter(year %in% yrs_leap)
ftr_leap <- inner_join(ftr_leap, leap, by = c('day' = 'julian')) 

# No leap
ftr_no_leap <- ftr %>% filter(year %in% yrs_no_leap)
ftr_no_leap <- inner_join(ftr_no_leap, no_leap, by = c('day' = 'julian')) 

# Join
ftr <- rbind(ftr_leap, ftr_no_leap)
ftr_smm <- ftr %>% 
  group_by(cl, gc, year, month) %>% 
  summarise(tmax = mean(tmax),
            tmin = mean(tmin),
            prec = sum(prec)) %>% 
  ungroup() %>% 
  group_by(cl, gc, month) %>% 
  summarise(tmax = mean(tmax),
            tmin = mean(tmin),
            prec = mean(prec)) %>% 
  ungroup() %>%
  group_by(cl, month) %>% 
  summarise(tmax = mean(tmax),
            tmin = mean(tmin),
            prec = mean(prec)) %>% 
  ungroup() %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  arrange(cl, month) %>% 
  rename(gcm = cl) %>% 
  dplyr::select(month, prec, tmax, tmin, gcm)

# Ensemble
ensemble <- ftr_smm %>% 
  group_by(month) %>% 
  summarise(prec = mean(prec),
            tmax = mean(tmax),
            tmin = mean(tmin)) %>% 
  ungroup() %>% 
  mutate(gcm = 'Ensemble')

# Join the 3 tables (crn, ftr, ensemble)
colnames(ftr_smm)
colnames(crn)
colnames(ensemble)

all <- rbind(crn, ftr_smm, ensemble) %>% 
  mutate(month = factor(month, levels = month.abb)) %>%  
  mutate(gcm = case_when(gcm == 'current' ~ 'Current',
                         gcm == '1' ~ 'Cluster 1',
                         gcm == '2' ~ 'Cluster 2',
                         gcm == '3' ~ 'Cluster 3',
                         gcm == '4' ~ 'Cluster 4',
                         gcm == 'Ensemble' ~ 'Ensemble'))

write.csv(all, 'table_gcms.csv', row.names = FALSE)
makeGraph <- function(gc){
  # gc <- 'Cluster 1'
  tb <- all %>% 
    filter(gcm %in% c('Current', gc)) %>% 
    mutate(month = factor(month, levels = month.abb),
           gcm = factor(gcm, levels = c('Current', gc)))
  
  lims <- summary(tb$prec)
  
  gg <- ggplot(data = tb, aes(x = month)) +
    geom_bar(aes(y = prec, group = gcm, fill = gcm), stat = 'identity', position = position_dodge()) +
    scale_fill_manual(name = 'Precipitation', 
                      values = c('#58ACFA', '#0431B4')) +
    ylab('Precipitation (mm)') +
    xlab('')
  
  pr <- pull(tb, prec)
  tm <- pull(tb, tmax)
  rlc <- mean(pr) / mean(tm) * 2
      
  gg <- gg +
    geom_line(data = tb, aes(y = tmin*rlc, colour = gcm, group = gcm, linetype = 'D'), size = 1.2) +
    geom_line(data = tb, aes(y = tmax*rlc, colour = gcm, group = gcm, linetype = 'E'), size = 1.2) +
    scale_y_continuous(sec.axis = sec_axis(~./rlc, name = 'Temperature ºC')) +
    scale_color_manual(name = 'Temperature',
                       values = c('#A4A4A4', '#1C1C1C')) +
    scale_linetype_manual(name = ' ', 
                          values = c("D" = 2, 'E' = 1), 
                          labels = c("D" = "Min", 'E' = 'Max')) +
    theme_bw() + 
    theme(legend.position = 'bottom',
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          axis.text = element_text(size = 18),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 20, hjust = 0.5),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 18, face = 'bold'),
          legend.key = element_rect(size = 4),
          legend.key.size = unit(2, 'lines'))  +
    guides(linetype = guide_legend(nrow = 2, keywidth = 3, order = 4, title.position = 'top', size = 15),
           color = guide_legend(nrow = 2, keywidth = 3, order = 3, title.position = 'top', size = 15),
           fill = guide_legend(order = 1, title.position = 'top', size = 15),
           size = guide_legend(order = 2, nrow = 2, title.position = 'top', size = 15))
  ggsave(plot = gg, filename = paste0('gg_gcm_', gc, '.png'), units = 'in', width = 12, height = 9, dpi = 300)             
  print('Done!')
  
}

gcms <- unique(all$gcm)[2:6]
map(.x = gcms, .f = makeGraph)


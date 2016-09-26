# ***************************************************************************
# This will eventually become a function called:
# 
# process_data()
# ***************************************************************************

rm(list = ls())

source("R/read_params.R")

df <- read_sas("data/alsfjord.sas7bdat") %>%
  as_tibble() %>%
  mutate(dato = as.Date(as.character(dato), format = "%Y%m%d")) %>% 
  distinct(
    station, 
    dato,
    x_utm,
    y_utm,
    dybde,
    lat_art,
    art_daekpct,
    .keep_all = TRUE
  )

# ObservationsstedID = transect
# proveID = same as depth

# Eventually by hard_sub

# How to break the depth into "classes"
mybreaks <- c(0, seq(1, 21, by = 2), 100)

res <- df %>%
  group_by(
    vandomraade,
    ObservationsstedID,
    dato,
    kildestationsnavn,
    proevetager,
    proeveID,
    hardbund_daekpct, 
    totcover_daekpct,
    dybde
  ) %>%
  summarise(
    cumcov = sum(art_daekpct[Steneck < 7]),
    opportunist = sum(art_daekpct[Steneck <= 3]),
    n_perenial = sum(Steneck < 7 & art_daekpct >= 1 & Growth_strategy == "P")
  ) %>% 
  ungroup() %>% 
  mutate(hardbund_daekpct = ifelse(hardbund_daekpct < 0, NA, hardbund_daekpct)) %>% 
  mutate(opportunist = ifelse(cumcov > 0 & is.na(opportunist), 0, opportunist)) %>% 
  mutate(n_perenial = ifelse(is.na(cumcov) & is.na(n_perenial), 0, n_perenial)) %>% 
  
  mutate(prop_opportunist = opportunist / cumcov) %>% 
  mutate(arsin_prop_opportunist = asin(sqrt(prop_opportunist))) %>% 
  mutate(log_n_perenial = log(n_perenial + 1)) %>% 
  mutate(log_cumcov = log(cumcov + 1)) %>% 
  mutate(log_cumcov = ifelse(cumcov < (totcover_daekpct - 20), NA, log_cumcov)) %>% 
  
  mutate(depth_class = cut(dybde, mybreaks, right = FALSE, include.lowest = TRUE)) %>% 
  mutate(haard_ind1 = ifelse(hardbund_daekpct < 50, hardbund_daekpct, 50)) %>% 
  mutate(haard_ind2 = ifelse(hardbund_daekpct < 50, 0, hardbund_daekpct - 50)) %>% 
  mutate(month = lubridate::month(dato)) %>% 
  mutate(year = lubridate::year(dato)) %>% 
  
  filter(month %in% 5:9) %>% 
  filter(dybde < 17)


res

# ***************************************************************************
# At this point we have the data correctly processed at the first level.
# Now, lets calculate log_cumcover_mod.
# ***************************************************************************

params <- read_params()[[2]] %>% 
  select(Effect, month, Estimate)

parm_depth <- params$Estimate[params$Effect == "depth"]
parm_H1 <- params$Estimate[params$Effect == "haard_ind1"]
parm_H2 <- params$Estimate[params$Effect == "haard_ind2"]

res <- filter(params, Effect == "month") %>% 
  full_join(res, ., by = "month") %>% 
  mutate(log_cumcover_mod = log_cumcov - Estimate - parm_depth * dybde - haard_ind1 * parm_H1 - haard_ind2 * parm_H2) %>% 
  drop_na(log_cumcover_mod)

write_csv(res, "/home/persican/Desktop/data.csv")

# ***************************************************************************
# Here we can do the GLMM
# ***************************************************************************

# glmer
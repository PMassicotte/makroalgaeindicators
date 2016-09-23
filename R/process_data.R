# ***************************************************************************
# This will eventually become a function called:
# 
# process_data()
# ***************************************************************************

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
mybreaks <- c(seq(1, 21, by = 2), 100)

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
  # filter(cumcov >= (art_daekpct - 20)) %>% 
  mutate(hardbund_daekpct = ifelse(hardbund_daekpct < 0, hardbund_daekpct, 0)) %>% 
  mutate(opportunist = ifelse(cumcov > 0 & is.na(opportunist), 0, opportunist)) %>% 
  mutate(n_perenial = ifelse(is.na(cumcov) & is.na(n_perenial), 0, n_perenial)) %>% 
  
  mutate(prop_opportunist = opportunist / cumcov) %>% 
  mutate(arsin_prop_opportunist = asin(sqrt(prop_opportunist))) %>% 
  mutate(log_n_perenial = log(n_perenial + 1)) %>% 
  mutate(log_cumcov = log(cumcov + 1)) %>% 
  
  mutate(depth_class = cut(dybde, mybreaks, right = FALSE, include.lowest = TRUE)) %>% 
  mutate(month = lubridate::month(dato)) %>% 
  mutate(year = lubridate::year(dato))

res


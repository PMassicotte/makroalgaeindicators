library(tidyverse)
library(haven)

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

res <- df %>%
  group_by(
    vandomraade,
    ObservationsstedID,
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
  mutate(prop_opportunist = opportunist / cumcov)

res


write_csv(res, "data/results.csv")


rm(list = ls())

easypackages::libraries("tidyverse", "lubridate")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DIR <- "../data/"
df <- readxl::read_xlsx(
  paste0(DIR, "av-radius-length_by-age-class_2023-03-28.xlsx"))

###Temperatur
#############
tmp <- read_csv2(paste0(DIR, "tmp_sta-see.csv"),
                 skip = 8) %>% 
  select(1,3) %>% 
  set_names(c("datum", "temp")) %>% 
  mutate(monat = month(datum),
         wachstumsjahr = year(datum)) %>% 
  filter(wachstumsjahr %in% 1995:2020,
         monat %in% 2:6) %>% 
  group_by(wachstumsjahr) %>% 
  summarise(mn_temp = round(mean(temp),2),
            mdn_temp = median(temp)) %>% 
  mutate(wachstumsjahr = as.character(wachstumsjahr))

###P-ges
########
phos <- read_csv2(paste0(DIR, "Pges_sta-see2.csv"),
                  skip = 8) %>% 
  select(1,5) %>% 
  set_names(c("datum", "p_ges")) %>% 
  filter(p_ges != "< BG") %>% 
  mutate(datum = lubridate::dmy_hm(datum),
         monat = month(datum),
         wachstumsjahr = year(datum),
         p_ges = as.numeric(p_ges)) %>% 
  filter(wachstumsjahr %in% 1995:2020,
         monat %in% 2:6) %>% 
  mutate(wachstumsjahr = as.factor(wachstumsjahr)) %>% 
  group_by(wachstumsjahr) %>% 
  summarise(mdn_p = median(p_ges),
            mn_p = round(mean(p_ges), 4))

###merge
########
mrg <- list(df, phos, tmp) %>%
  reduce(left_join, by = "wachstumsjahr")

writexl::write_xlsx(
  mrg, paste0(DIR, "scale-radius-and-env.data_2023-04-05.xlsx")
  )



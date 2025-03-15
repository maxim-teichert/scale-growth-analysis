
rm(list = ls())

library(tidyverse)
library(readxl)

###read data
############
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DIR <- "../data/"
df <- read_xlsx(paste0(DIR, "n_circuli_2023-03-28.xlsx"))

###deduct length_pix by age group from each other
################################################
dst_ak_length <- df %>% 
  filter(typ %in% c("r1","r2","r3", "rt")) %>% 
  pivot_wider(names_from = typ, values_from = length_pix) %>% 
  mutate(length_r2 = r2 - r1,
         length_r3 = r3 - r2,
         length_rt = rt - r3) %>% 
  select(-r2, -r3, -rt) %>% 
  rename(r2 = "length_r2",
         r3 = "length_r3",
         rt = "length_rt") %>% 
  pivot_longer(cols = c(r1, r2, r3, rt), names_to = "typ", values_to = "length_pix_adj") %>% 
  mutate(jahr = as.numeric(format(datum,'%Y')),
         gewicht = as.numeric(gewicht),
         wachstumsjahr = case_when(typ == "r3" ~ jahr - 1,
                                   typ == "r2" ~ jahr - 2,
                                   typ == "r1" ~ jahr - 3,
                                   typ == "rt" ~ jahr),
         jahr = as.factor(jahr),
         wachstumsjahr = as.factor(wachstumsjahr))

writexl::write_xlsx(dst_ak_length, paste0(
  DIR, "av-radius-length_by-age-class_2023-03-28.xlsx"))

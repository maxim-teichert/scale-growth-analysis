
rm(list = ls())

library(tidyverse)
library(lubridate)


###read csv data
################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DIR <- "../data/"

#data Versuchs- & Berufsfischerei
sylv <- readxl::read_xlsx(
  paste0(
    DIR, "fish-lengths_population.xlsx")
) %>% 
  mutate(jahr = year(Fangdatum)) %>% 
  janitor::clean_names() %>% 
  filter(alter == 3)

sylv_coll <- sylv %>% 
  group_by(jahr) %>% 
  summarise(mn_gewicht = mean(gewicht_g),
            mn_laenge = mean(lange_cm),
            sd_gewicht = sd(gewicht_g),
            sd_laenge = sd(lange_cm)) %>% 
  mutate(typ = rep("sylv"))

sylv_tly <- sylv %>% 
  group_by(jahr) %>% 
  tally() %>% 
  filter(!jahr < 1998 & !jahr > 2020)

#scale data
chris <- read_csv(paste0(
  DIR, "scales_raw_2023-02-23.csv")
) %>% 
  janitor::clean_names() %>% 
  rename(laenge = "lange") %>% 
  select(1:4) %>% 
  distinct(fisch_lauf_nr, datum, .keep_all = TRUE) %>% 
  mutate(jahr = year(datum))

chris_coll <- chris %>% 
  group_by(jahr) %>% 
  summarise(mn_gewicht = mean(gewicht),
            mn_laenge = mean(laenge),
            sd_gewicht = sd(gewicht),
            sd_laenge = sd(laenge)) %>% 
  mutate(typ = rep("chris"))

chris_tly <- chris %>% 
  group_by(jahr) %>% 
  tally()

#merge
mrg <- rbind(sylv_coll, chris_coll) %>% 
  filter(!jahr < 1998 & !jahr > 2020)

mrg_long <- mrg %>% 
  pivot_longer(2:3, names_to = "var", values_to = "val") %>% 
  select(1, 4, 5, 6, 2, 3) %>% 
  mutate(grp = paste(typ, var, sep = "_"),
         grp = str_remove(grp, "_mn"),
         grp = str_remove(grp, "_sd"),
         grp = fct_recode(grp, "length, subsample" = "chris_laenge",
                          "length" = "sylv_laenge",
                          "weight, subsample" = "chris_gewicht",
                          "weight" = "sylv_gewicht"))

len <- mrg_long %>% 
  filter(var == "mn_laenge")
t.test(val ~ typ, data = len)

gew <- mrg_long %>% 
  filter(var == "mn_gewicht")
t.test(val ~ typ, data = gew)

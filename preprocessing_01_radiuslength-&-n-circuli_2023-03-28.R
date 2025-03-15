
rm(list = ls())

library(tidyverse)
library(quantmod)
library(magrittr)

options(scipen = 999)

###read csv data
################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DIR <- "../data/"
df <- read_csv(paste0(DIR, "scales_raw_2023-02-23.csv"))

###find peaks
#############
pks_lst <- df %>% 
  group_by(Fisch_Lauf_Nr, Datum, Schuppen_Lauf_Nr, Typ) %>% 
  group_map(~ findPeaks(.$Gray_Value))

fn <- function(x){
  tail(x, 1) - head(x, 1)
}

pks_n <- lapply(pks_lst, length) %$% do.call(rbind.data.frame, .)
pks_dist <- sapply(pks_lst, fn) %>% data.frame()


###collapse df and merge
########################
dst <- df %>% 
  janitor::clean_names() %>% 
  distinct(fisch_lauf_nr, datum, schuppen_lauf_nr, typ, .keep_all = TRUE) %>% 
  arrange(fisch_lauf_nr, datum, schuppen_lauf_nr, typ) %>% 
  cbind(pks_n, pks_dist) %>% 
  select(-distance_pixels, -gray_value, -starts_with("c")) %>% 
  rename(length_pix = ".", laenge = "lange") %>% 
  group_by(datum, fisch_lauf_nr) %>% 
  mutate(fish_id = cur_group_id()) %>% 
  group_by(datum, fisch_lauf_nr, schuppen_lauf_nr) %>% 
  mutate(scale_id = cur_group_id()) %>% 
  arrange(datum, fisch_lauf_nr, schuppen_lauf_nr)
  
  #group_by(Datum, Fisch_Lauf_Nr, Typ) %>%
  #mutate(mn_circuli = round(mean(n_circuli), 0),
  #       length_vs_mn_circuli = round(length_pix/mn_circuli, 2)) %>% 
  #distinct(mn_circuli, .keep_all = TRUE) %>% 
  #arrange(Datum, Fisch_Lauf_Nr, Typ) %>% 
  #select(-n_circuli)


###export
#########
writexl::write_xlsx(dst, paste0(DIR, "n_circuli_2023-03-28.xlsx"))

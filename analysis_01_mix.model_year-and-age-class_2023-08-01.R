rm(list = ls())

easypackages::libraries("tidyverse", "nlme", "emmeans", "patchwork", "gstat",
                        "gt")

###read & manipulate data
#########################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DIR <- "../data/"
df <- readxl::read_xlsx(paste0(DIR, "scale-radius-and-env.data_2023-04-05.xlsx"))

df <- df %>% 
  select(2, 8:11, 13, 15, 16) %>% 
  rename(age_class = typ,
         radius_length = length_pix_adj,
         year = wachstumsjahr,
         phos = mn_p,
         temp = mn_temp) %>% 
  mutate(year = as.factor(year),
         year_zero = as.numeric(year)) %>% 
  filter(age_class != "rt",
         radius_length != 0) %>%
  mutate(age_class = as.factor(age_class)) %>% 
  na.exclude()

###mixed model 1 - only year and age class included
###################################################
mm <- lme(radius_length ~ 
            year_zero +
            age_class +
            year_zero:age_class,
          random = ~1|fish_id/scale_id,
          method = "REML",
          data = df)

summary(mm)
car::Anova(mm, type = "3")

###check model residuals for homogeneity, etc.
##############################################
E1 <- resid(mm, type = "normalized")
F1 <- fitted(mm)
errors <- df %>% mutate(E1 = E1, F1 = F1)

E1_vs_F1 <- ggplot(aes(x = F1, y = E1), data = errors) +
  geom_point() +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = "fitted", y = "residuals")

E1_vs_covar1 <- ggplot(aes(x = year_zero, y = E1), data = errors) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = "year", y = "residuals")

E1_vs_covar4 <- ggplot(aes(x = age_class, y = E1), data = errors) +
  geom_boxplot() +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = "age class", y = "residuals")

qq <- ggplot(aes(sample = E1), data = errors) +
  stat_qq() +
  stat_qq_line(color = "blue")

E1_vs_F1 + E1_vs_covar1 + E1_vs_covar4 + qq
#'no obvious violation of homogeneity, or residual patterns;
#'non-normal distribution of residuals, but can be ignored for regression
#'analyses


###test for autocorrelation of all data (not split into age-class groups)
#########################################################################
df_vario_E1 <- errors %>%  
  mutate(jday = julian(.$datum, origin = min(.$datum)) + 1,
         jday = as.numeric(jday)) %>% 
  select(E1, jday, age_class) %>% 
  group_by(age_class) %>% 
  mutate(ones = mean(E1))

sp::coordinates(df_vario_E1) <- c("jday", "ones")

v <- variogram(E1 ~ jday + ones, data = df_vario_E1) 
v.fit <- fit.variogram(v, vgm(psill = NA,
                              model = "Sph",
                              range = NA,
                              nugget = NA)) 
vario_line <- variogramLine(v.fit, maxdist = 2500)

ggplot() +
  geom_point(aes(x = dist, y = gamma), data = v) +
  geom_line(aes(x = dist, y = gamma), data = vario_line, color = "blue") +
  labs(x = "Distance", y = "Semi-variogram") 
#'no temporal autocorrelation


###pretty table using gt-package
################################
tbl_org <- car::Anova(mm, type = "3")
row.names(tbl_org)[row.names(tbl_org) == "year_zero"] <- "year"
row.names(tbl_org)[row.names(tbl_org) == "age_class"] <- "age class"
row.names(tbl_org)[row.names(tbl_org) == "year_zero:age_class"] <- "year*age class"

table_data <- tibble("Variable" = rownames(tbl_org),
                     "Chisq" = tbl_org$Chisq,
                     "Df" = tbl_org$Df,
                     "Pr(>Chisq)" = tbl_org$`Pr(>Chisq)`)

tbl_form <- gt(table_data) %>%
  fmt_number(columns = "Chisq", decimals = 2) %>%
  fmt_number(columns = "Pr(>Chisq)", decimals = 4) %>% 
  tab_style(style = cell_fill(color = "#f2f2f2"),
            locations = cells_body(rows = c(1,3))) %>% 
  tab_style(style =  cell_text(weight = "bold"),
            locations = cells_column_labels())

###emmeans
##########
emm <- emtrends(mm,
         specs = pairwise ~ "age_class",
         var = "year_zero",
         infer = TRUE)

emm_df_trends <- data.frame(emm$emtrends) %>% 
  rename("age class" = age_class,
         coefficient = year_zero.trend) %>% 
  mutate(coefficient = round(coefficient, 2),
         SE = round(SE, 2),
         lower.CL = round(lower.CL, 2),
         upper.CL = round(upper.CL, 2),
         t.ratio = round(t.ratio, 2),
         p.value = round(p.value, 3))

emm_df_contrasts <- data.frame(emm$contrasts) %>% 
  mutate(estimate = round(estimate, 2),
         SE = round(SE, 2),
         lower.CL = round(lower.CL, 2),
         upper.CL = round(upper.CL, 2),
         t.ratio = round(t.ratio, 2),
         p.value = round(p.value, 3))

gt(emm_df_trends) %>%
  tab_style(style = cell_fill(color = "#f2f2f2"),
            locations = cells_body(rows = c(1,3))) %>% 
  tab_style(style =  cell_text(weight = "bold"),
            locations = cells_column_labels())

gt(emm_df_contrasts) %>%
  tab_style(style = cell_fill(color = "#f2f2f2"),
            locations = cells_body(rows = c(1,3))) %>% 
  tab_style(style =  cell_text(weight = "bold"),
            locations = cells_column_labels())






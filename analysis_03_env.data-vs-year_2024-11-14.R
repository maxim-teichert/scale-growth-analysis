
rm(list = ls())

easypackages::libraries("tidyverse", "lubridate", "performance", "lmtest",
                        "nlme", "sandwich")

###Temperatur
#############
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
DIR <- "../data/"

tmp_raw <- read_csv2(paste0(DIR, "tmp_sta-see.csv"),
                     skip = 8) %>% 
  select(1,3) %>% 
  set_names(c("datum", "temp")) %>% 
  mutate(monat = month(datum),
         jahr = year(datum)) %>% 
  filter(jahr %in% 1995:2020,
         monat %in% 2:6)

tmp <- tmp_raw %>% 
  group_by(jahr) %>% 
  summarise(mn_temp = round(mean(temp),2),
            mdn_temp = median(temp),
            sd_temp = sd(temp)) %>% 
  mutate(jahr = as.character(jahr))

tmp_tly <- tmp_raw %>% 
  group_by(jahr) %>% 
  tally()
tmp_tly <- tmp_tly %>% filter(jahr >= 1995)


###P-ges
########
phos_raw <- read_csv2(paste0(DIR, "Pges_sta-see2.csv"),
                      skip = 8) %>% 
  select(1,5) %>% 
  set_names(c("datum", "p_ges")) %>% 
  filter(p_ges != "< BG") %>% 
  mutate(datum = lubridate::dmy_hm(datum),
         monat = month(datum),
         jahr = year(datum),
         p_ges = as.numeric(p_ges)) %>% 
  filter(jahr %in% 1995:2020,
         monat %in% 2:6) %>% 
  mutate(jahr = as.factor(jahr))

phos <- phos_raw %>% 
  group_by(jahr) %>% 
  summarise(mn_p = round(mean(p_ges), 3),
            mdn_p = median(p_ges),
            sd_p = sd(p_ges))

phos_tly <- phos_raw %>% 
  mutate(jahr = as.numeric(as.character(jahr))) %>% 
  group_by(jahr) %>% 
  tally()
phos_tly <- phos_tly %>% filter(jahr >= 1995)

###merge
########
mrg <- tmp %>%
  left_join(phos) %>% 
  mutate(jahr = as.numeric(jahr))
mrg <- mrg %>% filter(jahr >= 1995)

###lm temperature
#################

#plot
mrg %>% 
  ggplot(aes(x = jahr, y = mn_temp)) +
  geom_point(size = 4, alpha = 0.4) +
  geom_smooth(method = "lm")

#model
mod_temp <- lm(mn_temp ~ jahr, data = mrg)
summary(mod_temp)
summary.aov(mod_temp)
check_model(mod_temp)
bptest(mod_temp)
acf(residuals(mod_temp))
dwtest(mod_temp)

#re-run model with robust standard errors (RSE)
coeftest(mod_temp, vcov = vcovHC(mod_temp, type = "HC3"))

#RSE more conservative, i.e. does not assume homogeneity, but allows
#for unequal variance -> RSE larger than SE -> affects calculation of
#t- and subsequent p-value

###plot difference
##################
# Get predictions and standard errors
new_data <- data.frame(jahr = seq(min(mrg$jahr), max(mrg$jahr), length.out = 100))
pred <- predict(mod_temp, newdata = new_data, se.fit = TRUE)

# Get robust standard errors
robust_se <- sqrt(diag(vcovHC(mod_temp, type = "HC3")))
X_new <- cbind(1, new_data$jahr)
robust_se_fit <- sqrt(diag(X_new %*% vcovHC(mod_temp, type = "HC3") %*% t(X_new)))

# Create plot comparing both
ggplot(mrg, aes(x = jahr, y = mn_temp)) +
  # Points
  geom_point(size = 4, alpha = 0.4) +
  
  # Regular SE bands
  geom_line(data = data.frame(jahr = new_data$jahr, 
                              fit = pred$fit),
            aes(x = jahr, y = fit),
            inherit.aes = FALSE,
            color = "blue") +
  geom_ribbon(data = data.frame(jahr = new_data$jahr,
                                lower = pred$fit - pred$se.fit,
                                upper = pred$fit + pred$se.fit),
              aes(x = jahr, ymin = lower, ymax = upper),
              inherit.aes = FALSE,
              alpha = 0.2,
              fill = "blue") +
  
  # Robust SE bands
  geom_line(data = data.frame(jahr = new_data$jahr,
                              fit = pred$fit),
            aes(x = jahr, y = fit),
            inherit.aes = FALSE,
            color = "blue",
            lwd = 2) +
  geom_ribbon(data = data.frame(jahr = new_data$jahr,
                                lower = pred$fit - robust_se_fit,
                                upper = pred$fit + robust_se_fit),
              aes(x = jahr, ymin = lower, ymax = upper),
              inherit.aes = FALSE,
              alpha = 0.2,
              fill = "red") +
  
  labs(title = "Temperature Trend with Standard Error Bands",
       subtitle = "Blue = Regular SE, Red = Robust SE",
       x = "Year",
       y = "Mean Temperature (°C)") +
  theme_minimal()


###lm phosphorus
################

#plot
mrg %>% 
  ggplot(aes(x = jahr, y = mn_p)) +
  geom_point(size = 4, alpha = 0.4) +
  geom_smooth(method = "lm")

#model
mod_p <- lm(mn_p ~ jahr, data = mrg)
summary(mod_p)
summary.aov(mod_p)
check_model(mod_p)
bptest(mod_p)

acf(residuals(mod_p))


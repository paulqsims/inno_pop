################################################################################
# Function: Plots of predictors of innovation and population comparison
# Author: Paul Q. Sims
# Contact: paul.q.sims@gmail.com
# Date: July 2020
# Purpose: Plots for Sims and Reader 2020, Behavioural Processes
################################################################################

# Housekeeping -----

library(tidyverse)
library(nlme)
library(ggplot2)
library(ggsignif) # sig stars
library(effects) # marginal effects

# Read in data
data_analysis <- 
  read_csv_wfact("data/data_Sims-Reader_2020_mod.csv")

# Create dataset without NAs - lme won't remove them
data_analysis_NA_inno <-
  data_analysis %>%
  select(goal_z_lat, goal_z_lat_LN, tot_z_sc, pop, group,
         site_uni, body_length_sc, trial) %>%
  drop_na() 

# Create reduced innovation model 
inno_predict_reduc_m <- 
  lme(goal_z_lat_LN ~ tot_z_sc * pop + body_length_sc + 
        trial,
      weights = varIdent(form = ~ 1|site_uni * pop),
      random = ~ 1 | group,
      data = data_analysis_NA_inno)

# Obtain raw values for innovation, averaged between trial 1 and trial 2
data_inno_mean_plot <-
  data_analysis_NA_inno %>%
  group_by(group) %>%
  mutate(inno_mean = mean(goal_z_lat,
                          na.rm = TRUE),
         Population = pop) %>% # needed for total zones predictor plot for plot legend
  filter(trial == 1) 

# ----- end

# Innovation : Population comparison ----

# Obtain predicted values for each population
# marginal means, mean of trial and total zones entered
data_inno_means <- 
  as.data.frame(
    Effect("pop", inno_predict_reduc_m,
           data_analysis_NA_inno)) %>%
    mutate(across(.cols = fit:upper, ~ exp(.x))) # backtransform to normal

# Plot estimated mean of innovation for each population
# data points represent raw latency values, averaged across trials
p <- ggplot(data_inno_mean_plot, 
            aes(x = pop,
                y = inno_mean,
                color = pop,
                shape = pop,
                fill = pop)) + 
  geom_point(size = 2.8,
             alpha = 0.65,
             position = position_jitterdodge(),
             show.legend = F) +
  geom_point(data = data_inno_means,
             aes(x = pop,
                 y = fit,
                 color = pop,
                 shape = pop,
                 fill = pop),
             size=5.8) +
  geom_errorbar(data = data_inno_means, 
                aes(x = pop,
                    y = fit,
                    ymin=lower,
                    ymax=upper),
                width=.3,                    
                position = position_dodge(width = 0.9),
                size = 1.25) +
  geom_signif(comparisons = list(c("Lower Aripo", "Upper Aripo")), 
              map_signif_level = T, annotation = "*", vjust = 0.65, 
              y_position = 6.6, color = "black", size = 1,
              textsize = 12, fontface = "bold") +
  xlab("Population") +
  ylab(expression(paste(bold(Goal~zone~latency~log)[italic(e)]~bold((sec))~bold(""%+-%"")*bold("95"*"%"~C.I.)))) +
  scale_color_manual(values = c("#E41A1C", "#377EB8","#377EB8"),
                     name = "Population") +
  scale_shape_discrete(name = "Population") + 
  scale_y_continuous(trans = "log",
                     breaks = scales::trans_breaks("log", function(x) exp(x)), 
                     labels = number_format(accuracy = 1)) +
  coord_trans(y = 'log') +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        axis.title.y = element_text(face = "bold"),
        panel.border = element_blank(),
        axis.line = element_line(),
        text = element_text(size = 19),
        legend.position = "none") 

print(p)

# Innovation : predicted by total zones entered ----

# Get min and max values for total zones entered for lower and upper aripo 
#   populations, restrict line plot values by raw data
tot_z_range <-  
  data_analysis_NA_inno %>%
  group_by(pop) %>%
  select(tot_z_sc) %>%
  summarize(min_totz = min(tot_z_sc, na.rm = TRUE),  # get real ranges of tot z
            max_totz = max(tot_z_sc, na.rm = TRUE)) %>%
  pivot_wider(names_from = pop, values_from = c(min_totz,
                                                max_totz)) %>%
  rename(min_LA = `min_totz_Lower Aripo`, max_LA = `max_totz_Lower Aripo`,
         min_UA = `min_totz_Upper Aripo`, max_UA = `max_totz_Upper Aripo`)

# Obtain predicted values for each population
# marginal means, mean of trial and total zones entered
data_inno_means <- 
  as.data.frame(
    Effect(c("tot_z_sc", "pop"), inno_predict_reduc_m,
           data_analysis_NA_inno)) %>%
  mutate(across(.cols = fit:upper, ~ exp(.x)),  # backtransform to normal
         tot_z_sc = ifelse(  # remove tot z values outside real range
           pop == "Lower Aripo" & tot_z_sc < tot_z_range$min_LA |
           pop == "Lower Aripo" & tot_z_sc > tot_z_range$max_LA, NA,
           ifelse(
             pop == "Upper Aripo" & tot_z_sc < tot_z_range$min_UA |
             pop == "Upper Aripo" & tot_z_sc > tot_z_range$max_UA, NA,
               tot_z_sc))) %>%
  rename(Population = "pop") # crucial for plot legend to say Population

# Plot estimated innovation predicted by total zones entered by population
# data points represent raw latency values, averaged across trials and total
#   zones entered for trial 1

# Get custom y label values and breaks
# custom_labels.y <- round(exp(seq(from = 1,to = 6, by = 1)), digits = 0)
custom_labels.y <- c(expression(atop("3","(e"^1*")")),
                     expression(atop("7","(e"^2*")")),
                     expression(atop("20","(e"^3*")")),
                     expression(atop("55","(e"^4*")")),
                     expression(atop("148","(e"^5*")")),
                     expression(atop("403","(e"^6*")")))
custom_breaks.y <- round(exp(seq(from = 1,to = 6, by = 1)), digits = 0)

p <- ggplot(data_inno_mean_plot,
            aes(x = tot_z_sc,
                y = inno_mean,
                color = Population,
                shape = Population)) + 
  geom_point(size = 3.5,
             alpha = 0.9) +
  geom_line(data = data_inno_means,
            aes(x = tot_z_sc,
                y = fit,
                color = Population,
                linetype = Population),
            size = 1.4) +
  xlab("Total zones entered (standardized)") +
  ylab(expression(paste(bold(Goal~zone~latency~log)[italic(e)]~bold((sec))))) +
  scale_color_manual(values = c("#E41A1C", "#377EB8","#377EB8"),
                     name = "Population") +
  scale_shape_discrete(name = "Population") + 
  scale_y_continuous(trans = "log",
                     breaks = custom_breaks.y, 
                     labels = custom_labels.y) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.title.x = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        axis.title.y = element_text(face = "bold"),
        panel.border = element_blank(),
        axis.line = element_line(),
        text = element_text(size = 19),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14,
                                    face = "bold"),
        legend.position = c(0.85, 0.87),
        plot.margin = margin(1, 0.5, 0.5, 0.25, "cm")) 

print(p)
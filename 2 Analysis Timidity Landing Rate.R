source("1 Custom Functions.R")

library(Hmisc)
library(viridis)

(Data_Angling <- readRDS(here::here("Data", "Data_Angling_Boat_Grouped.rds")))

Data_Angling %>%
  group_by(Site)%>%
  summarise(Landing_Rate_mean = mean(x = Landing_Rate, na.rm = T),
            Landing_Rate_sd   = sd(x = Landing_Rate, na.rm = T),
            Landing_Rate_weighted_mean = weighted.mean(x = Landing_Rate, weights = Lan_Bit, na.rm = T),
            Landing_Rate_weighted_sd = sqrt(Hmisc::wtd.var(x = Landing_Rate, weights = Att_Foll, na.rm = T)))


#Calculate odds from raw data
Data_Angling %>% 
  dplyr::group_by(Site) %>%
  dplyr::summarize(Bit = sum(All_Bites),
                   Lan_Bit = sum(Lan_Bit),
                   Landed  = sum(Captured_Pike_No),
                   Landed_Per_Landed_And_Bites = (Landed / Lan_Bit),
                   Landed_Per_Bites = (Landed / Bit))



(Landingrate_plot <-
    Data_Angling %>% 
    ggplot(aes(x = Site, y = Landing_Rate)) +
    geom_point(aes(size = Att_Foll, color = Att_Foll),
               position = position_jitter(width = 0.3), alpha = 0.8) +
    scale_x_discrete(labels = c(MPA = "MPA", OA = "OA")) +
    labs(y = "Landing Rate") +
    scale_colour_gradient(name = "  ∑  Pike\nEncounters", low = "grey85", high = "grey20", breaks = seq(0, 80, by = 10)) +
    scale_size(name = "  ∑  Pike\nEncounters", breaks = seq(0, 80, by = 10)) +
    guides(color = guide_legend(), size = guide_legend()) +
    theme_bw() +
    theme(axis.title.x = element_blank(),
          axis.title = element_text(color="black",size=12,family="Arial"),
          panel.grid = element_blank(),
          legend.text = element_text(family="Arial"),
          axis.text = element_text(color="black",size=12,family="Arial"),
          axis.ticks.length = unit(.25, "cm"),
          legend.box.margin=margin(-10,0,0,0))
)

# # #     #       #   # # #   # # # #   # # # #   #         
# # #     # #   # # #       # #       # #         #         
# # #     #   #   # #       # #       # # # # #   #         
# # #     #       # #       # #       # #         #         
# # #     #       #   # # #   # # # #   # # # #   # # # # # 

(mod_landing <-
  lme4::glmer(cbind(All_Bites, Captured_Pike_No) ~ Site +
              (1|Date) +
              (1|Area_Name) +
              (1|Area_Name:Site),
            family = binomial,
            data   = Data_Angling)
)

summary(mod_landing)

exp(0.3943)
sjPlot::tab_model(mod_landing)
(conf_landing <- confint(mod_landing))

# PROBLEM: We can't calculate confidence intervals - singular fit: Nicht genügen kombinationen von explanatory variables to estimate variance
# We have to reduce our random variables. But which ones?

# (1|Area) accounts for differences in mean values between areas
# (1|Area:Site) is the same as "hard-coding" sites, estimates differences between all sites as individual locations
# But what is better to keep?

###
### Modelle vergleichen
###

mod_landing_reduced <- lme4::glmer(cbind(All_Bites, Captured_Pike_No) ~ Site +
                                     (1|Date),
                                   family = binomial,
                                   data   = Data_Angling)

mod_landing_onlynested <- lme4::glmer(cbind(All_Bites, Captured_Pike_No) ~ Site +
                                        (1|Date) +
                                        (1|Area_Name:Site),
                                      family = binomial,
                                      data   = Data_Angling)

anova(mod_landing_onlynested, mod_landing_reduced)
# mod_landing_onlynested signifikant besser! Area:Site drin lassen

# Option 2: Ist (1|Area) ausreichend?

mod_landing_onlyarea <- lme4::glmer(cbind(All_Bites, Captured_Pike_No) ~ Site +
                                      (1|Date) +
                                      (1|Area_Name),
                                    family = binomial,
                                    data   = Data_Angling)

anova(mod_landing_onlyarea, mod_landing_reduced)
# nicht signifikant - Area ist grundsätzlich nicht wichtig aber area:site ist wichtig!


# mod_landing_onlynested und mod_landing_onlyarea können nicht gegeneinander getestet werden weil es kein subset von dem anderen modell ist!


# # #     # # # # #     #         # # # # # #       # # # # #             # # # #   # # # # #   # # # # # # # # #     #       #   # # #   # # # #   # # # # # #             # # # # #   # # #   #       # #       # # # # #   
# # #             #     #         #       # # #     # #       #   #       #       # #         #             #         # #   # # #       # #       # #         #             #         #       # #       # # #     # #       # 
# # #     # # # # #     #         # # # # # #   #   # #       #           # # # #   # # # #     # # #       #         #   #   # #       # #       # # # # #   #             # # # #   #       # #       # #   #   # #       # 
# # #     #             #         #       # #     # # #       #   #       #       # #                 #     #         #       # #       # #       # #         #             #         #       # #       # #     # # #       # 
# # #     # # # # #     # # # # # #       # #       # # # # #             # # # #   # # # # # # # # #       #         #       #   # # #   # # # #   # # # # # # # # # #     #           # # #     # # #   #       # # # # #   

# The winner is: mod_landing_onlynested

summary(mod_landing_onlynested)
exp(0.4150)
# estimate still the same as in first model

# (conf_landing <- confint(mod_landing_onlynested))

paste0(round(exp(0.4150), 2), " [", round(exp(-0.06744509), 2), "-", round(exp(0.8121882), 2), "; 95% CL]")


# View interpreted results with tab_model
sjPlot::tab_model(mod_landing_onlynested)

emmeans::emmeans(mod_landing_onlynested, pairwise ~ Site ,type = "response")

car::Anova(mod_landing_onlynested, type = 2) %>% extract_Anova()


# # #             #         #     # # # #   #       # #       # # # # #   # # # # # #       #   # # # #     # # # #   #           # # #   # # # # # 
# # #             #         #     #         # #   # # # #   # # #         #       # # #     # #             #     #   #         #       #     #     
# # #             #         #     # # # #   #   #   # #   #   # # # # #   # # # # # #   #   #   # # #       # # # #   #         #       #     #     
# # #     #       # #       #     #         #       # #       # #         #       # #     # #         #     #         #         #       #     #     
# # #     # # # #   # # # #       # # # #   #       # #       # # # # #   #       # #       # # # # #       #         # # # # #   # # #       #     


Data_Angling$Site_Base_OA <- forcats::fct_relevel(Data_Angling$Site, "OA")
mod_landing_onlynested <- lme4::glmer(cbind(Captured_Pike_No, All_Bites) ~ Site_Base_OA +
                                        (1|Date) +
                                        (1|Area_Name:Site),
                                      family = binomial,
                                      data   = Data_Angling)

# Outcome is the same but flipped:
emmeans::emmeans(mod_landing_onlynested,specs = c("Site_Base_OA"),type = "response")

(Landingrate_plot_modelmeans <- 
    Landingrate_plot +
    geom_pointrange(data = as.data.frame(emmeans::emmeans(mod_landing_onlynested, specs = c("Site_Base_OA"),type = "response")),
                    aes(y = prob,
                        x = Site_Base_OA,
                        ymin = asymp.UCL,
                        ymax = asymp.LCL), color = "tomato2", size = 1.5)+
    geom_line(data = as.data.frame(emmeans::emmeans(mod_landing_onlynested,specs = c("Site_Base_OA"),type = "response")),
              aes(y = prob, x = Site_Base_OA, group = 1), color = "tomato2", lty = "1111")
)

## Combine attack and landing rate
library(patchwork)

source("2 Analysis Timidity Attack Rate.R")
(Attack_plus_Landing <- Attackrate_plot_modelmeans + Landingrate_plot_modelmeans)

# ggsave(plot = Attack_plus_Landing,
#        filename = here::here("Figures", "Attack_Landing_Rate.png"),
#        width = 8,
#        height = 5,
#        units = "in",
#        dpi = 600)

###
### Copy-paste summary table to word file
###
(mod_landingrate_table_out <- mod_landing_onlynested %>% 
    broom.mixed::tidy() %>% 
    mutate(
      effect = ifelse(effect == "fixed", "fixed", "random"),
      term = ifelse(is.na(group), term, group),
      term = stringr::str_replace_all(term, "Area_Name", "Area ")
    ) %>% 
    mutate(
      across(where(is.numeric), 
             ~ ifelse(round(., 4) == 0, "< 0.0000", sprintf("%.4f", .)))
    ) %>% 
    select(effect, term, estimate, std.error, p.value)
    # knitr::kable()
    # word_table()
)

report::report(mod_landing_onlynested)
summary(mod_landing_onlynested)
performance::performance(mod_landing_onlynested)



source("1 Custom Functions.R")

library(Hmisc)
library(viridis)
library(ggplot2)

(Data_Angling <- readRDS(here::here("Data", "Data_Angling_Boat_Grouped.rds")))

# # #     # # # #   # # # #   # # # #     # # # # # # # #     # # #   # # # #   # # # # #   # # #   #       # # # # #   
# # #     #       # #         #         #         #     #       #     #     #       #         #     #       # #         
# # #     #       # # # # #   #           # # #   # # # #       #     # # # #       #         #     #       # # # # #   
# # #     #       # #         #                 # #   #         #     #             #         #       #   #   #         
# # #     # # # #   # # # #   # # # #   # # # #   #     #     # # #   #             #       # # #       #     # # # #  

Data_Angling %>% 
  dplyr::group_by(Site) %>%
  dplyr::summarize(Followers_sum = sum(Follower, na.rm=T),
                   Attacks_sum = sum(Attacks, na.rm=T),
                   Attacks_from_Interactions = sum(Attacks) / sum(Att_Foll)
                   )



# calculate weighted means
(Att_mean <-
    Data_Angling %>%
    group_by(Site )%>%
    summarise(Attack_Rate_mean = mean(x = Attack_Rate, na.rm = T),
              Attack_Rate_sd   = sd(x = Attack_Rate, na.rm = T),
              Attack_Rate_weighted_mean = weighted.mean(x = Attack_Rate, weights = Att_Foll, na.rm = T),
              Attack_Rate_weighted_sd = sqrt(Hmisc::wtd.var(x = Attack_Rate, weights = Att_Foll, na.rm = T)))
)


#Calculate odds from raw data
Data_Angling %>% 
  dplyr::group_by(Site) %>%
  dplyr::summarize(Att_Foll = sum(Att_Foll),
                   Att      = sum(Attacks),
                   Fol      = Att_Foll - Att,
                   Foll_Per_Interact = Fol / Att_Foll,
                   ratio    = Att/Fol)


(Attackrate_plot <-
    Data_Angling %>%
    ggplot(aes(x = Site, y = Attack_Rate)) +
    geom_point(aes(size = Att_Foll, color = Att_Foll),
               position = position_jitter(width = 0.3), alpha = 0.8)+
    scale_x_discrete(labels = c(MPA = "MPA", OA = "OA")) +
    #scale_color_viridis_c(breaks=seq(0, 80, by=10))+
    scale_colour_gradient(name = "  ∑  Pike\nEncounters", low = "grey85", high = "grey20", breaks = seq(0, 80, by = 10)) + # GER: ∑ Hechtkontakte
    scale_size(name = "  ∑  Pike\nEncounters", breaks = seq(0, 80, by = 10))+
    labs(y = "Attack Rate")+
    guides(color = guide_legend(), size = guide_legend())+
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.title = element_text(color="black",size=12,family="Arial"),
          panel.grid = element_blank(),
          legend.text = element_text(family="Arial"),
          axis.text = element_text(color="black",size=12,family="Arial"),
          axis.ticks.length = unit(.25, "cm"),
          legend.box.margin=margin(-10,0,0,0)) +
    theme(legend.position = "none")
)


# Johannes schlägt vor: Keine Rate :
#
# 2 Stadien: 1 - Follower / 2 - Attackieren
# Was von beidem er macht hängt ab von site
# Binomial Modell
# erklärt wahrscheinichkeit of ein fisch followt oder attackiert - er kann nur eins von beidem sein
# Wichtig ist, dass sich follower und attack hier ausschließen! Es kann nur eins von beidem sein!
# cbind um zwei columns zu erklären:
# Ich könnte auch verhältnis ausrechnen, dann aber keine Gewichtung drin, um wie viele Fische es geht (50 follower, 100 attacks oder 5 follower, 10 attacks!
# Das wird hier berücksichtigt!

mod_att <- lme4::glmer(cbind(Follower, Attacks) ~ Site +
                         (1|Date) +
                         (1|Area_Name) +
                         (1|Area_Name:Site),
                       family = binomial,
                       data   = Data_Angling)
summary(mod_att)
# estimate SiteOA
exp(0.7346)
emmeans::emmeans(mod_att,specs = c("Site"),type = "response")

# Die wahrscheinlichkeit 1 gegenüber Wahrscheinlichkeit 2 ist 83% höher. Keine magnitude, sodern wie oft das eine zum anderen eintritt.
# conf <- confint(mod_att)

# Ich kann in einzelnen modellen auch cbind(All_Bites, Follower) und cbind(Attacks, Captured) messen
# sjPlot::tab_model(mod_att)
sjPlot::plot_model(mod_att, type = "pred")


###
### "Anbisswahrscheinlichkeit" - change baseline of site and flip order in cbind
###

Data_Angling$Site_Base_OA <- forcats::fct_relevel(Data_Angling$Site, "OA")
mod_att2 <- lme4::glmer(cbind(Attacks, Follower) ~ Site_Base_OA + # Order of followers and attacks flipped
                          (1|Date) +
                          (1|Area_Name) +
                          (1|Area_Name:Site),
                        family = binomial,
                        data   = Data_Angling)

# sjPlot::tab_model(mod_att2)

summary(mod_att2)

# Outcome is the same but flipped:
# Pike in MPA are 1.84 as likely to attack
emmeans::emmeans(mod_att2,specs = c("Site_Base_OA"),type = "response")


(Attackrate_plot_modelmeans <-
    Attackrate_plot +
    geom_pointrange(data = as.data.frame(emmeans::emmeans(mod_att2, specs = c("Site_Base_OA"),type = "response")),
                    aes(y = prob,
                        x = Site_Base_OA,
                        ymin = asymp.UCL,
                        ymax = asymp.LCL),color = "tomato2", size = 1.5)+
    geom_line(data = as.data.frame(emmeans::emmeans(mod_att2,specs = c("Site_Base_OA"),type = "response")),
              aes(y = prob, x = Site_Base_OA, group = 1), color = "tomato2", lty = "1111")
)

# ggsave(plot = Attackrate_plot_modelmeans,
#        filename = here::here("Plots", "2023_05_26 Attackrate_plot_JJ.png"),
#        width = 5,
#        height = 5,
#        units = "in",
#        dpi = 600)

###
### REPORT MODEL RESULTS
###

car::Anova(mod_att2) %>% extract_Anova()


summary(mod_att2)

odds <- round(exp(0.7734), 2)

# Calculate confidence intervals
# confint(mod_att2)
 
# > confint(mod_att2)
# Computing profile confidence intervals ...
# 2.5 %    97.5 %
#   .sig01           0.64462757 1.1602634
# .sig02           0.00000000 0.8731185
# .sig03           0.00000000 0.9954185
# (Intercept)      0.61230339 1.9171283
# Site_Base_OAMPA -0.03595275 1.6097382
odds_min <- round(exp(-0.03595275), 2)
odds_max <- round(exp(1.6097382), 2)

paste0(odds, " [", odds_min, "-", odds_max, "; 95% CL]")

sjPlot::tab_model(mod_att2)



emmeans::emmeans(mod_att2, pairwise ~ Site_Base_OA ,type = "response")

report::report(mod_att2)
summary(mod_att2)
performance::performance(mod_att2)


###
### Copy-paste summary table to word file
###
(mod_attackrate_table_out <- mod_att2 %>%
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
    select(effect, term, estimate, std.error, p.value) %>%
    knitr::kable()
  # word_table()
)


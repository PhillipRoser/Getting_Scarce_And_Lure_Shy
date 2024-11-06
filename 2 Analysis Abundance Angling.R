source("1 Custom Functions.R")
library(ggplot2)

(Data_Angling <- readRDS(here::here("Data","Data_Angling_Boat_Grouped.rds")) %>%
  mutate(Site = forcats::fct_relevel(Site, c("OA", "MPA")))
)

# # #     # # # #   # # # #     # # # # # # # #   # # # #     # # #   # # # #   # # # # #   # # #   #       # # # # #   
# # #     #       # #         #         #         #     #       #     #     #       #         #     #       # #         
# # #     #       # # # # #     # # #   #         # # # #       #     # # # #       #         #     #       # # # # #   
# # #     #       # #                 # #         #   #         #     #             #         #       #   #   #         
# # #     # # # #   # # # #   # # # #   # # # #   #     #     # # #   #             #       # # #       #     # # # #  

unique(Data_Angling$Date) %>% length()
# 60 Fishing days

range(Data_Angling$Date)


Data_Angling %>% 
  group_by(Area_Name) %>%
  dplyr::summarize(n_days = n_distinct(Date))

# Area_Name                n_days
# <chr>                     <int>
# 1 Grabow                       29
# 2 Neuensiener_Selliner_See     16
# 4 Ummanz                       15


# EFFORT
(Angling_Descriptives <-
    Data_Angling %>%
    dplyr::group_by(Date, Site, Area_Name) %>%
    dplyr::summarize( 
      MPA_Comparison           = first(MPA_Comparison),
      n_boats                  = n_distinct(Boat_Captain),
      n_anglers                = sum(n_anglers),
      samplers_per_boat        = n_anglers / n_boats,
      Session_Hours_Aggregated = sum(Sum_Effort_h)
    ) %>% 
    # filter(MPA_Comparison == "Random_Angling") %>%   # OPTIONAL: only randomized angling
    group_by(Site) %>%                               # Site
    # group_by(Area_Name, Site) %>%                    # Area and Site
    dplyr::summarize(Days                 = n_distinct(Date),
                     Samplers             = paste(round(mean(n_anglers),1), round(sd(n_anglers),1), sep=" ± "),
                     Boats                = paste(round(mean(n_boats),1),   round(sd(n_boats),1), sep=" ± "),
                     Samplers_per_boat    = paste(round(mean(samplers_per_boat),1), round(sd(samplers_per_boat),1), sep=" ± "),
                     total_effort         = sum(Session_Hours_Aggregated)
    )   
)

# CATCH / CPUE
(Angling_Descriptives2 <- 
  Data_Angling %>%
  
  filter(Season_Mon == "Spring") %>%
  filter(lubridate::year(Date) == "2022") %>%
  
  dplyr::group_by(Site) %>%
  dplyr::summarize(Days                 = n_distinct(Date),
                   total_effort         = sum(Sum_Effort_h),
                   n_pike               = sum(Captured_Pike_No),
                   n_meterpike          = sum(Captured_Meter_Pike_No),
                   CPUE                 = paste(round(n_pike / total_effort, 3), 
                                                round(sd(Captured_Pike_No / Sum_Effort_h),3), 
                                                sep=" ± "),
                   CPUE_1m              = paste(round(n_meterpike / total_effort, 3), 
                                                round(sd(Captured_Meter_Pike_No / Sum_Effort_h),3), 
                                                sep=" ± "),
                   hours_to_meterpike   = total_effort / n_meterpike
  )
)

405/498
# CPUE 4.2 times higher
0.437/0.104
# Only Spring 2022
0.38/0.122

# Effort per season
Data_Angling %>% 
  group_by(Area_Name, Season_Mon) %>%
  dplyr::summarize(n = n_distinct(Date))

# Area_Name                Season_Mon     n
# <chr>                    <fct>      <int>
# 1 Grabow                   Spring         7
# 2 Grabow                   Summer         5
# 3 Grabow                   Autumn        12
# 4 Grabow                   Winter         4
# 5 Neuensiener_Selliner_See Spring         4
# 6 Neuensiener_Selliner_See Summer         5
# 7 Neuensiener_Selliner_See Autumn         6
# 8 Neuensiener_Selliner_See Winter         1
# 9 Rassower_Strom           Spring         1
# 10 Ummanz                  Spring         6
# 11 Ummanz                  Summer         5
# 12 Ummanz                  Autumn         5

Data_Angling %>% 
  group_by(Season_Mon) %>%
  dplyr::summarize(n = n_distinct(Date))


Data_Angling %>% 
  # filter(MPA_Comparison == "Random_Angling") %>%
  group_by(Area_Name) %>%
  dplyr::summarize(
    n_days = n_distinct(Date),
    n_pike = sum(Captured_Pike_No),
    effort = sum(Sum_Effort_h)
  )

###
### RANDOM ANGLING
###

(grouped_randomized <- 
    Data_Angling %>% 
    filter(MPA_Comparison == "Random_Angling") %>%
    group_by(Area_Name, Site) %>%
    dplyr::summarize(
      n_days = n_distinct(Date),
      n_pike = sum(Captured_Pike_No),
      effort = sum(Sum_Effort_h),
      CPUE = paste( round(mean(Captured_Pike_No/Sum_Effort_h),3), 
                    round(sd(Captured_Pike_No/Sum_Effort_h),3),
                    sep=" ± ")
    )
)

# CPUE contrasts

0.389 / 0.216 #Sellin
0.45 / 0.0649 # Ummanz
0.492 / 0.063 #Werderbucht

Data_Angling %>% 
  filter(MPA_Comparison == "Random_Angling") %>%
  group_by(Area_Name) %>%
  dplyr::summarize(n = sum(Captured_Pike_No))



# # #     #       #   # # #   # # # #   # # # #   #         #           # # #   #       # # # # # # 
# # #     # #   # # #       # #       # #         #         #             #     # #     # #         
# # #     #   #   # #       # #       # # # # #   #         #             #     #   #   # #   # # # 
# # #     #       # #       # #       # #         #         #             #     #     # # #       # 
# # #     #       #   # # #   # # # #   # # # #   # # # # # # # # # #   # # #   #       # # # # #   


mod_Abundance_Angling <- glmmTMB::glmmTMB(Captured_Pike_No ~ 
                                            offset(log(Sum_Effort_h)) +
                                            Site + 
                                            (1|Boat_Captain) +
                                            (1|Area_Name) +
                                            (1|Date) +
                                            (1|Area_Name:Site),
                                          family = poisson(link = "log"),
                                          data = Data_Angling,
                                          REML = FALSE
)


# DHARMa::simulateResiduals(mod_Abundance_Angling, plot = T)
# DHARMa::testOverdispersion(mod_Abundance_Angling, plot = T)
# performance::check_model(mod_Abundance_Angling)

summary(mod_Abundance_Angling)

car::Anova(mod_Abundance_Angling) %>% extract_Anova()

broom.mixed::tidy(mod_Abundance_Angling)
sjPlot::tab_model(mod_Abundance_Angling) 

(gtsummary_A <- 
  gtsummary::tbl_regression(
  mod_Abundance_Angling,
  exponentiate = TRUE,  # To get exponentiated coefficients for interpretation
  add_estimate_to_reference_rows = TRUE  # To include the intercept in the table
)
)

gtsummary::tbl_merge(list(gtsummary_N, gtsummary_A), tab_spanner = c("Gillnets", "Angling"))


summary(mod_Abundance_Angling)

emmeans::emmeans(mod_Abundance_Angling)

emmeans::emmeans(mod_Abundance_Angling, pairwise ~ Site, type = "response") %>% extract_emmeans()
# These values refer to an effort of 10.2 hours
emmeans::ref_grid(mod_Abundance_Angling)

report::report(mod_Abundance_Angling)



# # #     # # # #   #           # # #   # # # # # 
# # #     #     #   #         #       #     #     
# # #     # # # #   #         #       #     #     
# # #     #         #         #       #     #     
# # #     #         # # # # #   # # #       #     

data_summary_no_sd <- function(x) {
  m <- mean(x)
  ymin <- m - sd(x)
  ymax <- m + sd(x)
  return(c(y    = m,
           ymin = m,
           ymax = m))
}

(plot_siteabund_A_raw <- 
    
    ggplot(data = Data_Angling %>% 
             mutate(Site = case_when(Site == "MPA" ~ "pMPA", TRUE ~ Site)) %>% 
             mutate(Site = factor(Site, levels = c("pMPA", "OA"))), 
           aes(
             x = Site, 
             y = Captured_Pike_No / Sum_Effort_h
           )
    ) +
    
    
    geom_jitter(aes(
      color = Area_Name, 
      group = Area_Name), 
      size = 1.5, alpha = 1, width = .3) + 
    
    geom_boxplot(size = .35, alpha = 0, outlier.shape = NA) +
    
    stat_summary(fun.data = "data_summary_no_sd",
                 colour = "black", 
                 shape = 23,
                 position = position_dodge(.75),
                 size = .5) +
    
    scale_color_manual(breaks = c("Grabow", "Ummanz", "Neuensiener_Selliner_See"),
                       values = c(Color_Wer, Color_Umm, Color_Sel),
                       labels = c("Grabow", "Ummanz", "Selliner-/Neuensiener See")) +
    scale_fill_manual(breaks = c("Grabow", "Ummanz", "Neuensiener_Selliner_See"),
                      values = c(Color_Wer, Color_Umm, Color_Sel), 
                      labels = c("Grabow", "Ummanz", "Selliner-/Neuensiener See")) +
    
    labs(y = "Angling CPUE\n(pike/h)", color = NULL, fill = NULL) +
    
    geom_segment(aes(x = "pMPA", y = 1.6, xend = "OA", yend = 1.6)) +
    
    theme_bw(base_size = 18) +
    
    annotate(
      "text",
      x = 1.5,
      y = 1.62,
      label = "***")+
    
    theme(legend.position = "right",
          # axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.title.x = element_blank() # removes x label
    ) 
)   



fct3 = .4
# ggsave(plot = plot_siteabund_A_raw,
#        filename = here::here("Figures", "Abundance_Angling.png"),
#        width = 10*fct3,
#        height = 12*fct3,
#        units = "in",
#        dpi = 600)

source("2 Analysis Abundance Spring Gillnets.R") # obtain object "plot_siteabund_N_raw"
plot_siteabund_AN <- ggpubr::ggarrange(plot_siteabund_N_raw, plot_siteabund_A_raw, nrow=1, common.legend = TRUE, legend="top")

fct4 = .5
# ggsave(plot = plot_siteabund_AN,
#        filename = here::here("Figures", "Abundance_Gillnets_And_Angling.png"),
#        width = 18*fct4,
#        height = 12*fct4,
#        units = "in",
#        dpi = 600)

source("1 Custom Functions.R")

SI_Gillnets <- readRDS(here::here("Data", "Data_Spring_Gillnets.rds")) 

# # #     # # # #   # # # #     # # # # # # # #   # # # #     # # #   # # # #   # # # # #   # # #   #       # # # # #   
# # #     #       # #         #         #         #     #       #     #     #       #         #     #       # #         
# # #     #       # # # # #     # # #   #         # # # #       #     # # # #       #         #     #       # # # # #   
# # #     #       # #                 # #         #   #         #     #             #         #       #   #   #         
# # #     # # # #   # # # #   # # # #   # # # #   #     #     # # #   #             #       # # #       #     # # # #  

# EFFORT
SI_Gillnets %>%
  dplyr::group_by(Area_Name, Site, Net_Type) %>%
  dplyr::summarize(n = n())

# CATCHES
SI_Gillnets$Captured_Pike_No %>% sum()
# n pike with gillnet: 114

# CATCHES BY SITE
SI_Gillnets %>%
  group_by(Site) %>%
  dplyr::summarize(
    n_pike = sum(Captured_Pike_No),
    n_meterpike = sum(Captured_Meter_Pike_No, na.rm = T)
  )
73/114 # 64 % in MPA

# CPUE per 12h and 100m 
SI_Gillnets %>% 
  group_by(Site) %>%
  dplyr::summarize(
    CPUE_Pikenets = paste(round(mean(if_else(Net_Type == "Pike_Gillnets", Captured_Pike_No / Session_Hours * 12, NA_real_), na.rm = TRUE), 2),
                          round(sd(if_else(Net_Type == "Pike_Gillnets", Captured_Pike_No / Session_Hours * 12, NA_real_), na.rm = TRUE), 2),
                          sep = " ± "
                          ),
    # Add factor 100/40 for baitfish nets to correct for different length of nets
    CPUE_Baitnets = paste(round(mean(if_else(Net_Type == "Bait_Gillnets", Captured_Pike_No / Session_Hours * 12 * 100/40, NA_real_), na.rm = TRUE), 2),
                          round(sd(if_else(Net_Type == "Bait_Gillnets", Captured_Pike_No / Session_Hours * 12 * 100/40, NA_real_), na.rm = TRUE), 2),
                          sep = " ± "
    ),
  )

3.206 / 1.084 
1.358 / 0.762 



# # #     #       #   # # #   # # # #   # # # #   #         #           # # #   #       # # # # # # 
# # #     # #   # # #       # #       # #         #         #             #     # #     # #         
# # #     #   #   # #       # #       # # # # #   #         #             #     #   #   # #   # # # 
# # #     #       # #       # #       # #         #         #             #     #     # # #       # 
# # #     #       #   # # #   # # # #   # # # #   # # # # # # # # # #   # # #   #       # # # # #   


mod_Abundance_Gillnets <- glmmTMB::glmmTMB(Captured_Pike_No ~ 
                                             offset(log(Session_Hours)) +
                                             Site +
                                             (1|Net_Type) +
                                             (1|Date) +
                                             (1|Area_Name) +
                                             (1|Area_Name:Site), # Nesting,
                                           family = poisson(link = "log"),
                                           data = SI_Gillnets, 
                                           REML = FALSE
)


# DHARMa::simulateResiduals(mod_Abundance_Gillnets, plot = T)
# DHARMa::testOverdispersion(mod_Abundance_Gillnets)

###
### LOOK AT MODEL RESULTS
###

summary(mod_Abundance_Gillnets)

car::Anova(mod_Abundance_Gillnets) %>%
  extract_Anova()


emmeans::emmeans(mod_Abundance_Gillnets,  ~ Site, type = "response")
emmeans::ref_grid(mod_Abundance_Gillnets)

(EMM_Net <- emmeans::emmeans(mod_Abundance_Gillnets,  ~ Site, type = "response", offset = log(12))) # korrekte predictions
EMM_Net@grid

# Check contrast ratios between areas!
emmeans::emmeans(mod_Abundance_Gillnets, pairwise ~ Site, type = "response", offset = log(12)) %>% extract_emmeans()

report::report(mod_Abundance_Gillnets)

(gtsummary_N <- 
  gtsummary::tbl_regression(
  mod_Abundance_Gillnets,
  exponentiate = TRUE,  # To get exponentiated coefficients for interpretation
  add_estimate_to_reference_rows = TRUE  # To include the intercept in the table
)
)


sjPlot::tab_model(mod_Abundance_Gillnets,
                  show.df = T,
                  show.se = T)




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

(plot_siteabund_N_raw <- 
    
    ggplot(data = SI_Gillnets %>% 
             mutate(Site = case_when(Site == "MPA" ~ "pMPA", TRUE ~ Site)) %>% 
             mutate(Site = forcats::fct_relevel(Site, c("pMPA", "OA"))), 
           aes(
             x = Site, 
             y = Captured_Pike_No / Session_Hours * 12
           )
    ) +
    
    geom_jitter(aes(
      # shape = Net_Type, 
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
    # scale_fill_manual(breaks = c("Werderbucht", "Ummanz", "Neuensiener_Selliner_See"),
    #                   values = c(Color_Wer, Color_Umm, Color_Sel),
    #                   labels = c("Grabow", "Ummanz", "Selliner-/Neuensiener See")) +
    
    labs(y = "Gillnet CPUE\n(pike/100m*12h)", color = NULL, fill = NULL) +
    
    geom_segment(aes(x = "pMPA", y = 5.8, xend = "OA", yend = 5.8)) +
    
    theme_bw(base_size = 18) +
    
    annotate(
      "text",
      x = 1.5,
      y = 5.88,
      label = "***")+
    
    theme(legend.position = "none",
          # axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.title.x = element_blank() # removes x label
    ) 
)   



fct3 = .4
# ggsave(plot = plot_siteabund_N_raw,
#        filename = "C:/Users/Phill/OneDrive/MPA_Pike_Paper/Figures/Abundance_Gillnets.png",
#        width = 10*fct3,
#        height = 12*fct3,
#        units = "in",
#        dpi = 600)

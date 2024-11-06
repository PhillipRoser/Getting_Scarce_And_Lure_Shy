source("1 Custom Functions.R")

(Data_Length <- readRDS(here::here("Data", "Data_Length.rds")))

nrow(Data_Length)

# MEAN LENGTH
Data_Length %>%
  group_by(Site) %>%
  dplyr::summarise(
    range   = paste0(min(Total_Length_cm, na.rm = T), " - ", max(Total_Length_cm, na.rm = T)),
    mean_sd = paste0(round(mean(Total_Length_cm, na.rm = T),1), " ± ", round(sd(Total_Length_cm, na.rm = T),1)),
    L90 = quantile(Total_Length_cm, 0.9, na.rm = T),
    Meterpike = sum(Total_Length_cm > 100, na.rm = T)
  )

filter(Data_Length, Total_Length_cm > 100)

ggpubr::ggboxplot(data = Data_Length, x = "Area_Name", y = "Total_Length_cm", color = "Site", add = "jitter")

mod_Length <- glmmTMB::glmmTMB(Total_Length_cm ~
                                 Sex +
                                 Site +
                                 (1|Area_Name) +
                                 (1|Area_Name:Site),
                               data = Data_Length,
                               family = gaussian(link = "identity")
                               )

DHARMa::simulateResiduals(mod_Length, plot = T)
summary(mod_Length)

mod_Length_int <- update(mod_Length, . ~ . + Site:Area_Name)

anova(mod_Length_int, mod_Length)


summary(mod_Length_int)
car::Anova(mod_Length_int) %>% extract_Anova()
as.data.frame(emmeans::emmeans(mod_Length_int, specs = c("Site"),type = "response"))

broom.mixed::tidy(mod_Length_int)


###
### There is an interaction...
###

mod_Length_Grab <- lm(Total_Length_cm ~ Sex + Site, data = filter(Data_Length, Area_Name == "Grabow"))
summary(mod_Length_Grab)
car::Anova(mod_Length_Grab)
broom.mixed::tidy(mod_Length_Grab)



mod_Length_Umm <- lm(Total_Length_cm ~ Sex + Site, data = filter(Data_Length, Area_Name == "Ummanz"))
summary(mod_Length_Umm)
car::Anova(mod_Length_Umm)


mod_Length_Sel <- lm(Total_Length_cm ~ Sex + Site, data = filter(Data_Length, Area_Name == "Neuensiener_Selliner_See"))
summary(mod_Length_Sel)
car::Anova(mod_Length_Sel)

sjPlot::tab_model(mod_Length_Grab)


gtsummary::tbl_summary(mod_Length_Grab)



# # #     #     #     # # #   #         #       #   # # #   # # # # #   # # #   # # # #     # # #   #       #       # # # # #       #   # # #   # # # #   #       #   # # #   #       #     # # # # # # # # #     # # # # # # # # # 
# # #     #   #     #       # #         # #   # # #       # #         #       # #     #   #       # #       #     #         # #   # #     #     #     #   # #     # #       # #       #         #     #         #             #     
# # #     # #       #       # #         #   #   # #       # #   # # # #       # # # # #   #       # #       #       # # #   #   #   #     #     # # # #   #   #   # #       # #       #         #     # # # #     # # #       #     
# # #     #   #     #       # #         #       # #       # #       # #       # #   #     #       #   #   #               # #       #     #     #   #     #     # # #       #   #   #           #     #                 #     #     
# # #     #     #     # # #   # # # # # #       #   # # #   # # # #     # # #   #     #     # # #       #         # # # #   #       #   # # #   #     #   #       #   # # #       #             #     # # # #   # # # #       #     

# Keep in mind that the KS test is sensitive to differences in shape, location, and scale between distributions. It is a non-parametric test, meaning it doesn't make assumptions about the specific form of the underlying distributions.
# Johannes: KS-Test doesn't test for central tendency but for the spread. Compare cumulative histograms.

# Extract Total_Length_cm for each site
length_mpa <- Data_Length$Total_Length_cm[Data_Length$Site == "MPA"]
length_oa <- Data_Length$Total_Length_cm[Data_Length$Site == "OA"]

# Perform Kolmogorov-Smirnov test

ks.test(length_mpa, length_oa, alternative = "two.sided")

# length distributions differed between oa 

# Perform test for each area separately
for(i in unique(Data_Length$Area_Name)){
  cat(i, "\n")
  data <- filter(Data_Length, Area_Name == i)
  
  mpa <- data$Total_Length_cm[data$Site == "MPA"]
  oa  <- data$Total_Length_cm[data$Site == "OA"]
  
  # Perform Kolmogorov-Smirnov test
  ks.test(mpa, oa, alternative = "two.sided")$p.value %>% round(3) %>% cat(., "\n\n")
}

# Cumulative density plot at the end of script 


# # #     #             # # # # #   # # #   
# # #     #             #       # #       # 
# # #     #             # # # # # #       # 
# # #     #                     # #       # 
# # #     # # # # #     # # # # #   # # #   

Data_Length %>%
  # group_by(Site) %>%
  group_by(Area_Name, Site) %>%
  dplyr::summarize(
    n = n(),
    mean = mean(Total_Length_cm, na.rm = T),
    L90 = quantile(Total_Length_cm, 0.9, na.rm = T),
    prop_stock_dens = round(sum(Total_Length_cm > 85, na.rm = T) / sum(Total_Length_cm > 50, na.rm = T) * 100)
  ) %>%
  knitr::kable()



# # #     # # # # # # # # #     # # # # # # # # #     #         # # # # #   # # #   
# # #         #     #         #             #         #         #       # #       # 
# # #         #     # # # #     # # #       #         #         # # # # # #       # 
# # #         #     #                 #     #         #                 # #       # 
# # #         #     # # # #   # # # #       #         # # # # # # # # # #   # # #   

Data_L90 <- Data_Length %>%
  group_by(Area_Name) %>%
  # Add area-specific L90
  mutate(L90 = quantile(Total_Length_cm, 0.9, na.rm = T),) %>%   # pull(L90) %>% unique()
  ungroup() %>%
  mutate(L90_True = ifelse(Total_Length_cm >= L90, TRUE, FALSE),
         Site_Base_OA = forcats::fct_relevel(Site, c("OA", "MPA")))


mod_L90 <- glmmTMB::glmmTMB(L90_True ~ 
                              Site_Base_OA +
                              (1|Area_Name) +
                              (1|Area_Name:Site_Base_OA),
                            family = binomial(),
                            data = Data_L90)


summary(mod_L90)
car::Anova(mod_L90) %>% extract_Anova()
sjPlot::tab_model(mod_L90)
paste(exp(0.7243), " times more likely")

Data_L90 %>% 
  group_by(Area_Name, Site) %>%
  dplyr::summarize(Share_L90 = sum(L90_True, na.rm = T)/n())

# # #     # # # # #     # # # # # # # # #   # # # #   # # # # #   # # # #       # # # # # # # #   # # # #   # # # #   # # # #   # # # # # # # # # # # # # #   
# # #             #     #       # #     #   #         #       # #             #         #         #     #   #         #     #   #       #     #     #         
# # #     # # # # #     # # # # # # # # #   # # # #   # # # # #   # # #         # # #   # # # #   # # # #   # # # #   # # # #   # # # # #     #     # # # #   
# # #             #     #       # #   #     #         #       #         #             # #         #         #         #   #     #       #     #     #         
# # #     # # # # #     #       # #     #   # # # #   #       # # # # #       # # # #   # # # #   #         # # # #   #     #   #       #     #     # # # #   


# is the difference still given when we reduce our n to nach the n in OA?
Data_Length %>% group_by(Site) %>% dplyr::summarize(n = n())

set.seed(42)
select <- sample(1:466, 136)

ausgewogenes_sample <- rbind(Data_Length[which(Data_Length$Site == "OA"),],
                             Data_Length[which(Data_Length$Site == "MPA"),][select,]
)


(Only_Randomized <- ausgewogenes_sample %>% 
    
    group_by(Area_Name, Site) %>%
    # group_by(Gear, Area_Name, Site) %>%
    
    dplyr::summarize(prop_stock_dens = round(sum(Total_Length_cm > 85, na.rm = T) / sum(Total_Length_cm > 50, na.rm = T) * 100),
                     share_females   = round( (sum(Sex == "f", na.rm = T) / n() ) * 100),
                     n = n())
)

# Ja # aber: fluktuiert stark mit set.seed





##
## facets
##

Name_Grabow <- "Area 1: Grabow"
Name_Ummanz <- "Area 2: Ummanz"
Name_Sellin <- "Area 3: Selliner-/Neuensiener See"

Plot_Lengthdata <- 
Data_Length %>%
  mutate(Area_Name = case_when(Area_Name == "Neuensiener_Selliner_See" ~ Name_Sellin, 
                               Area_Name == "Grabow" ~ Name_Grabow,
                               Area_Name == "Ummanz" ~ Name_Ummanz,
                               TRUE ~ Area_Name),
         ) %>%
  rbind(., Data_Length %>% mutate(Area_Name = "Pooled")) %>%
  mutate(Area_Name = forcats::fct_relevel(Area_Name, c("Pooled", Name_Grabow, Name_Ummanz, Name_Sellin)),
         Site = forcats::fct_recode(Site, pMPA = "MPA")
         )

(Raincloud_Plot3areas <- 
    ggplot(Plot_Lengthdata, 
           aes(x = 1, y = Total_Length_cm, fill = Site, color = Site)) +
    ggrain::geom_rain(boxplot.args.pos = list(
      position = ggpp::position_dodgenudge(x = 0, width = 0.065), 
      width = 0.05, color = "black", alpha = .2
    ),
    violin.args.pos = rlang::list2(side = "r", width = 0.3,
                                   position = position_nudge(x = 0.05), alpha = .2),
    point.args.pos = rlang::list2(position = position_jitterdodge(
      jitter.width = .05, jitter.height = 0, dodge.width = .07), size = .2, color = "black", alpha = .15
    )
    ) +
    
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 16,  # Use a solid dot
      size = 2,
      color = "black",
      position = position_dodge(width = 0.07),
      show.legend = FALSE
    ) +
    
    # display 90th percentile
    stat_summary(
      fun.data = function(x) {
        y = quantile(x, 0.9)
        return(data.frame(y = y))
      },
      geom = "point",
      shape = 23,
      size = 2,
      color = "black",
      position = position_dodge(width = 0.07),
      show.legend = FALSE
    ) +
    
    scale_fill_manual(values = c(Color_MPA, Color_OA)) +
    scale_color_manual(values = c(Color_MPA, Color_OA)) +
    scale_y_continuous(breaks       = seq(40, 120, 10),
                       minor_breaks = seq(40, 120, 5)) +
    scale_x_continuous(limits = c(0.95, 1.2), breaks = c(0)) +
    ggthemes::theme_gdocs(base_size = 20) +
    labs(
      x = "",
      y = "Total Length [cm]"
    ) +
    facet_wrap(~Area_Name, ncol = 1) +
    theme(panel.spacing.y = unit(.1, "lines")) +
    coord_flip() +
    theme(legend.position = c(0.5, 1.035), 
          legend.direction = "horizontal",
          legend.background = element_blank()) 
)

fct2 = 1
# ggsave(plot = Raincloud_Plot3areas,
#        filename = "C:/Users/Phill/OneDrive/MPA_Pike_Paper/Figures/Length_Raincloud3areas.png",
#        width = 10*fct2,
#        height = 12*fct2,
#        units = "in",
#        dpi = 600)


###########
########### Cumulative densities (K-S test)
###########

ann_text <- data.frame(Area_Name = factor(c("Pooled", "Area 1: Grabow", "Area 2: Ummanz", "Area 3: Selliner-/Neuensiener See"),
                                             levels = c("Pooled", "Area 1: Grabow", "Area 2: Ummanz", "Area 3: Selliner-/Neuensiener See")),
                       Site = NA,
                       lab = c("p < 0.001", "p = 0.62", "p = 0.66", "p = 0.07")
                       )


(Cumulative_Density <- ggplot(Plot_Lengthdata, aes(x = Total_Length_cm, color = Site)) +
  stat_ecdf(geom = "step") +
  labs(x = "Total Length (cm)", y = "Cumulative Proportion") +
  scale_color_manual(values = c(Color_MPA, Color_OA)) + 
  theme_minimal() +
  facet_wrap(~ Area_Name, 
             # ncol = 4
             ncol = 2
             ) +
 theme(legend.position = "top",
       legend.margin = margin(0, 0, -10, 0)) +
    geom_text(
      data    = ann_text,
      mapping = aes(x = -Inf, y = -Inf, label = lab),
      hjust   = -4,
      vjust   = -2,
      size    = 3,
      show.legend = FALSE
    )
)

fct3 = 1
# ggsave(plot = Cumulative_Density,
#        filename = here::here(Figures", "Length_Cumulative_Densities.png"),
#        # width = 10*fct3,
#        # height = 6*fct3,
#        width = 6*fct3,
#        height = 6*fct3,
#        units = "in",
#        bg = "white",
#        dpi = 600)


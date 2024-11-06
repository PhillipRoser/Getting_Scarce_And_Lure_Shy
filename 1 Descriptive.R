source("1 Custom Functions.R")

# # #       # # #   # # # #     # # # # # # # #   # # # #   #       # # # # #   # # # #       # # # #   # # # # # # # # # #   # # #   # # # #   # # # # # 
# # #     #       # #       # #         #         #     #   #       # #         #       #     #         #         #         #       # #     #       #     
# # #     #       # # # # #     # # #   # # # #   # # # #   #       # # # # #   #       #     # # # #   # # # #   # # # #   #       # # # # #       #     
# # #     #       # #       #         # #         #   #       #   #   #         #       #     #         #         #         #       # #   #         #     
# # #       # # #   # # # #   # # # #   # # # #   #     #       #     # # # #   # # # #       # # # #   #         #           # # #   #     #       #     


library(dplyr)
library(tidyverse)
library(lubridate)

(Effort <- readRDS(here::here("Data", "Observed Effort Zerodays Added.rds")))

Effort %>%
  group_by(gear, area, site) %>%
  summarize(
    sum = sum(n),
    mean_day    = sum(n)/n(),
    sd_mean_day = sd(n),
    range       = paste0(min(n), "-", max(n)),
    mean_sd     = paste0(round(mean_day, 1), " ± ", round(sd_mean_day, 1), " / ",range)
    ) %>%
  mutate(Area_Site = paste(area, site, sep = "_")) %>%
  select(Area_Site, mean_sd) %>%
  tidyr::pivot_wider(names_from = gear, values_from = mean_sd) %>%
  knitr::kable()



mod_effort <- glm(data = Effort, n ~  area*gear*site, family = poisson(link = "log"))
emm <- emmeans::emmeans(mod_effort, ~ area*gear*site)
simp <- pairs(emm, simple = "each")

emmeans::test(simp[[3]], by = NULL, adjust = "mvt") # [3] looks at 3rd term in interaction

# individual site
Effort %>% filter(area == "Werderbucht", gear == "Anglers") %>% glm(data = ., n ~ site, family = poisson(link = "log")) %>% summary()

###
### Sum instead of mean
###

site.col <- c(Color_MPA, Color_OA)
(p1_sums <- Effort %>% 
    group_by(gear, area, site) %>%
    summarize(sum = sum(n),
              mean_day    = sum(n)/n(),
              sd_mean_day = sd(n),
              mean_sd     = paste0(round(mean_day, 1), " ± ", round(sd_mean_day, 1)),
              range       = paste0(min(n), "-", max(n))
    ) %>%
    
    mutate(gear = case_when(gear == "Eel_Fyke" ~ "Eel Fyke",
                            gear == "Fish_Trap" ~ "Pound Net",
                            gear == "Anglers" ~ "Angling Boats",
                            TRUE ~ gear)) %>%
    
    ggplot() +
    
    geom_col(aes(x = site, y = sum, color = site, fill = site), 
             position = position_dodge(.7), 
             width = .5, 
             show.legend = F, 
             alpha = .3
    ) + 
    
    geom_point(aes(x = site, y = mean_day)) +
    
    geom_text(aes(x = site, y = 15, label = mean_sd), size = 5) +
    
    scale_fill_manual(values = site.col, breaks = c("MPA", "OA")) +
    scale_color_manual(values = site.col, breaks = c("MPA", "OA")) +
    
    # ylim(c(0, 5)) +
    theme_bw(base_size = 20) + 
    
    facet_grid(area ~ gear) + 
    
    labs(x = "", y = "∑ observations", title = "") +    
    
    theme(plot.title = element_text(hjust = 0.5),
          panel.spacing.y = unit(.0, "lines"),
          panel.spacing.x = unit(.0, "lines")
    )
)

g <- ggplot_gtable(ggplot_build(p1_sums))
stripr <- which(grepl('strip-r', g$layout$name))
fills <- c(Color_Wer, Color_Umm, Color_Sel)
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid::grid.draw(g)

# ggsave(here::here("Plots", "2023_10_13 Observed_Effort_sums.png"), width = 250, height = 180, units = "mm", bg = "white", g)

# ggsave(here::here("Plots", "2023_10_13 Observed_Effort_sums.png"), width = 250, height = 200, units = "mm", bg = "white", p1_sums)



# # #     # # # # # # # # #     # # #     # # #   # # # # #   # # #   # # # #     # # # # 
# # #     #       # #       #     #     #       #     #         #     #         #         
# # #     # # # # # # # # #       #     #       #     #         #     #           # # #   
# # #     #       # #       #     #     #       #     #         #     #                 # 
# # #     #       # # # # #     # # #     # # #       #       # # #   # # # #   # # # #   


SI_orig <- readRDS(here::here("Data", "SI_orig 2023_07_04.rds")) %>% 
  filter(!is.na(MPA_Comparison))

SI_orig %>%
  group_by(Site) %>% 
  summarize(Temp = mean(Temperature_C, na.rm = T))



# # #     # # # # # # # # #     # # #   # # # # #   # # #   # # # #     # # # #       # # # # # # # # # # # # # # # # # # #   # # # # 
# # #     #       # #       # #       #     #         #     #         #             #             #     #       #     #     #         
# # #     # # # # # # # # #   #       #     #         #     #           # # #         # # #       #     # # # # #     #       # # #   
# # #     #       # #       # #       #     #         #     #                 #             #     #     #       #     #             # 
# # #     #       # # # # #     # # #       #       # # #   # # # #   # # # #       # # # #       #     #       #     #     # # # #   


(SI_orig <- readRDS(here::here("Data", "SI_orig.rds")) %>%
    filter(!is.na(MPA_Comparison) & Area_Name != "Rassower_Strom")
  )

Depth <- readRDS(here::here("Data", "Depth_Krigged_Core_Areas.rds")) %>%
  mutate(Area_Name = case_when(Area == "Werderbucht" ~ "Grabow",
                               Area == "Sellin" ~ "Neuensiener_Selliner_See",
                               Area == "Ummanz" ~ "Ummanz"))

# Macrophyte Data
data.frame(
  Area = rep(c("Grabow", "Ummanz", "Sellin"), each = 6),
  Season = rep(rep(c("Spring", "Summer", "Autumn"), each = 2), 3),
  Site = rep(c("OA", "MPA"), 9),
  Value = c(34.6, 20.4, 62.2, 65.3, 45.9, 47.6, 22.5, 24.5, 29.7, 24.9, 54.0, 66.9, 14.5, 12.3, 39.6, 44.6, 44.6, 6.38)
) %>% 
  tidyr::pivot_wider(names_from = Site, values_from = Value) %>%
  knitr::kable()



Env <- plyr::rbind.fill(SI_orig, Depth) %>%
  mutate(Area_Name = forcats::fct_relevel(Area_Name, c("Grabow", "Ummanz", "Neuensiener_Selliner_See")))

mean_sd <- function(variable, round_by = 1){
  mean <- round(mean(variable, na.rm = T), round_by)
  sd   <- round(sd(variable, na.rm = T),   round_by)
  return(paste(mean, sd, sep = " ± "))
}

# Summarize means+-sd
Env %>%
  mutate(Visibility_m = Visibility_cm/100) %>%
  group_by(Area_Name) %>%
  dplyr::summarize(
    across(c(Salinity_PSU, Temperature_C, Visibility_m, Oxygenlevel_mgL, depth), \(x) mean_sd(x))
  ) %>%
  knitr::kable()

Env %>%
  group_by(Area_Name) %>%
  dplyr::summarize(
    across(c(Salinity_PSU, Temperature_C, Visibility_cm, Oxygenlevel_mgL, depth), \(x) mean(x, na.rm = T))
  ) 

### STAT TEST
get_t_test_pval <- function(formula){
  tryCatch({t.test(formula)$p.value}, error = function(cond) NA)
}

# get_t_test_pval <- function(formula){
#   tryCatch({pairwise.t.test(formula, p.adjust.method="bonferroni")$p.value}, error = function(cond) NA)
# }

pairwise.t.test(data$score, data$technique, p.adjust.method="bonferroni")

pairwise.t.test(Env$Salinity_PSU, Env$Site, p.adjust.method="bonferroni")

# test
t.test(Salinity_PSU ~ Site, data = Env)$p.value

Env %>%
  group_by(Area_Name) %>%
  dplyr::summarize(
    across(c(Salinity_PSU, Temperature_C, Visibility_cm), \(x) get_t_test_pval(x ~ Site))
    # formerly:  t_test_pval = get_t_test_pval(Salinity_PSU ~ Site)
  ) %>% 
  word_table()



library(dplyr)


# # #     # # # # # #       # # # # # # #           # # #   #       # # # # # #     # # # #   # # # # # # # # # # # # # # # 
# # #     #       # # #     # #         #             #     # #     # #             #       # #       #     #     #       # 
# # #     # # # # # #   #   # #   # # # #             #     #   #   # #   # # #     #       # # # # # #     #     # # # # # 
# # #     #       # #     # # #       # #             #     #     # # #       #     #       # #       #     #     #       # 
# # #     #       # #       # # # # #   # # # # #   # # #   #       # # # # #       # # # #   #       #     #     #       # 


(SI_orig <- readRDS(here::here("Data", "SI_orig 2023_07_04.rds")) %>%
  
  dplyr::filter(!Date %in% c("2023-03-04", "2023-03-05", "2023-03-18", "2023-03-19")) %>%  # On these weekends angling was distributed 2/3 in MPA and 1/3 in OA
  
  mutate(Area_Name = case_when(Area_Name %in% c("Werderbucht", 
                                                "Grosse_Wiek", 
                                                "Grosse Wiek",
                                                "Butterwiek", 
                                                "Grosse Wiek", 
                                                "Aue",
                                                "Open_Grabow") ~ "Grabow",
                                      TRUE ~ Area_Name),
         Area_Name   = case_when(Date == "2023-04-22" ~ "Grabow",
                                    TRUE ~ Area_Name),
         
         MPA_Comparison = case_when(SI_ID %in% c(1254:1285) ~ "Nonrandom_Angling",    # These Instances belong to comparison angling
                                    TRUE ~ MPA_Comparison),
         Boat_Captain   = case_when(SI_ID %in% c(1254:1269) ~ "Joerg",
                                    SI_ID %in% c(1270:1285) ~ "Frank",
                                    TRUE ~ Boat_Captain)
         ) %>%
    
  dplyr::filter(!Area_Name %in% c("Kubitzer_MPA", "Rassower_Strom"))
)


# saveRDS(SI_orig, here::here("Data", "SI_orig.rds") 
# (SI_orig <- readRDS(here::here("Data", "SI_orig.rds")))


(SI_ang <- SI_orig %>%
    filter(MPA_Comparison %in% c("Nonrandom_Angling", "Random_Angling")) %>%
    
    select("SI_ID", "Date", "Sampler_ID", "Starttime", "Endtime", "Duration", "Session_Hours", "Gear", "Waterbody", "Boat_Captain", 
           "Temperature_C" ,"Salinity_PSU", "Oxygenlevel_mgL", "Visibility_cm", "Season_Mon", contains("Bite"), 
           "Site", "MPA_Comparison", "Area_Name", "Follower", contains("Captured"), "Notes") %>%
    mutate(
      Captured_Meter_Pike_No = ifelse(is.na(Captured_Meter_Pike_No), 0, Captured_Meter_Pike_No),
      Bites     = ifelse(is.na(Bites), 0, Bites),
      All_Bites = ifelse(is.na(Safe_Bites), Bites, rowSums(cbind(Bites, Safe_Bites), na.rm = TRUE)),
      Follower  = ifelse(is.na(Follower), 0, Follower)
    )
)


SI_ang %>% 
  dplyr::group_by(Site) %>%
  dplyr::summarize(Followers_sum = sum(Follower, na.rm=T))

# Warum wir pro Boot aggregiert haben:
SI_ang %>% 
  mutate(
    Attacks = All_Bites + Captured_Pike_No,
    Att_Foll     = Attacks + Follower,
    Attack_Rate  = Attacks / Att_Foll,
    Lan_Bit      = Captured_Pike_No + All_Bites,
    Landing_Rate = Captured_Pike_No / Lan_Bit,
  )

# Daher:
(Data_Angling <- 
    SI_ang %>% 
    dplyr::group_by(Date, Site, Boat_Captain) %>%
    dplyr::summarize(
      Season_Mon = first(Season_Mon),
      Site        = first(Site),
      Area_Name   = first(Area_Name),
      MPA_Comparison = first(MPA_Comparison),
      
      Follower        = sum(Follower),
      All_Bites       = sum(All_Bites),
      Captured_Pike_No= sum(Captured_Pike_No),
      Captured_Meter_Pike_No = sum(Captured_Meter_Pike_No),
      
      Attacks      = sum(All_Bites, Captured_Pike_No, na.rm = TRUE),
      Att_Foll     = Attacks + Follower,
      Attack_Rate  = Attacks / Att_Foll,
      Lan_Bit      = Captured_Pike_No + All_Bites,
      Landing_Rate = Captured_Pike_No / Lan_Bit,
      n_anglers    = n_distinct(Sampler_ID),
      Sum_Effort_h = sum(Session_Hours)
    ) %>%
    arrange(Date)
)

# saveRDS(Data_Angling, here::here("Data", "Data_Angling_Boat_Grouped.rds")) 
# (Data_Angling <- readRDS(here::here("Data", "Data_Angling_Boat_Grouped.rds")))


# # #     # # # # #   # # #   #         #         #       # # # # #   # # # # #     # # # #   # # # # # # # # # # # # # # # 
# # #     #             #     #         #         # #     # #             #         #       # #       #     #     #       # 
# # #     #   # # #     #     #         #         #   #   # # # # #       #         #       # # # # # #     #     # # # # # 
# # #     #       #     #     #         #         #     # # #             #         #       # #       #     #     #       # 
# # #     # # # #     # # #   # # # # # # # # # # #       # # # # #       #         # # # #   #       #     #     #       # 


SI_Gillnets <- SI_orig %>% 
  filter(MPA_Comparison == "Random_Gillnets" & Season == "Spring") %>% 
  mutate(
    Net_Length = ifelse(Net_Type == "Bait_Gillnets", 40, 100), 
    Area_Name = forcats::fct_relevel(Area_Name, c("Grabow", "Ummanz", "Neuensiener_Selliner_See"))
  ) %>%
  mutate(Site = forcats::fct_relevel(Site, c("OA", "MPA"))) %>%
  select(SI_ID, Date, Site, Starttime, Session_Hours, Gear, Net_ID, Net_Type, Net_Length, Area_Name, Captured_Pike_No, Captured_Meter_Pike_No)

# saveRDS(SI_Gillnets, here::here("Data", "Data_Spring_Gillnets.rds")) 


# # #     #         # # # #   #       # # # # # # # # # # # #       #     # # # #   # # # # # # # # # # # # # # # 
# # #     #         #         # #     # #             #     #       #     #       # #       #     #     #       # 
# # #     #         # # # #   #   #   # #   # # #     #     # # # # #     #       # # # # # #     #     # # # # # 
# # #     #         #         #     # # #       #     #     #       #     #       # #       #     #     #       # 
# # #     # # # # # # # # #   #       # # # # #       #     #       #     # # # #   #       #     #     #       # 

(SI_orig <- readRDS(here::here("Data", "SI_orig.rds")))

(Data_Length <- readRDS(here::here("Data", "1Pike_Latest_Version.rds"))  %>% 
  
  mutate(Total_Length_cm = Total_Length_mm / 10) %>%
  
  select(Total_Length_cm, SI_ID, Sex) %>%
  
  dplyr::left_join(SI_orig, by = "SI_ID") %>%
  
  filter(SI_ID %in% c(SI_ang$SI_ID, SI_Gillnets$SI_ID)) %>%
    
  select(SI_ID, Date, Area_Name, Site, Gear, Sex, Total_Length_cm) %>%
    
  mutate(Area_Name   = case_when(Date == "2023-04-22" ~ "Grabow",
                                 TRUE ~ Area_Name),
         Sex = ifelse(Sex == "u", NA, Sex)) %>%
    
  mutate(Area_Name = forcats::fct_relevel(Area_Name, c("Grabow", "Ummanz", "Neuensiener_Selliner_See")))
)

Data_Length %>% group_by(Gear) %>% dplyr::summarise(n = n())
# Discrepancy with Captured_Pike_No because several fish escaped before measuring

Data_Length$Area_Name %>% unique()
Data_Length$Date %>% unique() %>% sort()
Data_Length$MPA_Comparison %>% unique()

# saveRDS(Data_Length, here::here("Data", "Data_Length.rds")) 
# (Data_Length <- readRDS(here::here("Data", "Data_Length.rds")))

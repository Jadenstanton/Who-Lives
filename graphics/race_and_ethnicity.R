source(here("inputs/datacenter_colors.R"))

############################################
# # RACE/ETHNICITY # #
############################################




##### mockup 1 - population over time

totalpop_metro %>%
  ggplot(aes(year, metro_pop)) +
  geom_line(size = 1, color = DCcolor.p2limegreen) +
  scale_y_continuous(labels = comma_format(), limits = c(1150000, 1400000)) + 
  scale_x_continuous(breaks = c(1980, 1990, 2000, 2010,2023),  labels = c("1980", "1990", "2000", "2010", "2023")) + 
  scale_color_manual(values = c(DCcolor.p2limegreen)) +
themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16, hjust = .5)) + 
  labs(title = "Total population since 1980, Metro",
       x="",
       y="") 

#### 1 - African American, white, and Hispanic population ####

### PEP ###
AAwhthispGraphic <- AAWhiteHispan %>%
  select(est2000, population, raceSimple, race.fac) %>%
  gather(-raceSimple,-race.fac, key=variable, value =val) %>%
  mutate(description = ifelse(variable == "est2000", "2000", YEAR_PEP.char)) %>%
  ggplot(aes(race.fac, val, fill=description)) +
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .7,
           color="gray50") +
  geom_text(aes(label = comma(val)), position=position_dodge(width = .7), vjust = -.7, size=3, family="Asap") +
  scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0,0), limits = c(0,350000)) +
  scale_fill_manual(values = c(DCcolor.p1skyblue,
                               DCcolor.p1mediumblue)) +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        plot.title = element_text(hjust = .5)) +
  labs(title = "Black, White, Hispanic, and Asian population, Orleans Parish",
       x="",
       y="")


#### 2 -  Demographic bar charts for 8 parishes, metro, and US ####

### PEP ###
ParishDemoforGraphic <- ParishDemo %>%
  select(PlaceName,
         contains('pct'),
         contains('2000')) %>%
  filter(PlaceName != "Louisiana") %>%
  mutate(PlaceName.fac = factor(.$PlaceName,levels = c("Orleans", "Jefferson", "Plaquemines",
                                                       "St. Bernard","St. Charles", "St. James",
                                                       "St. John the Baptist", "St. Tammany", "Metro", "United States"))) %>%
  gather(key = variable, value = value, contains("pct"), contains("2000")) %>%
  mutate(description  = NA,
         description = ifelse(grepl("pct",variable), YEAR_PEP, description),     #if variable contains 'pct'
         description = ifelse(grepl("2000", variable), 2000, description)) %>%     #if variable contains '2000'
  mutate(description.fac = factor(.$description, levels = c( "2000",
                                                             YEAR_PEP.char)))%>%
  mutate(race = NA,
         race = ifelse(grepl("white", variable),"White", race),
         race = ifelse(grepl("black", variable),"Black", race),
         race = ifelse(grepl("hisp", variable),"Hispanic", race),
         race = ifelse(grepl("asian", variable),"Asian", race)) %>%
  mutate(race.fac = factor(.$race, levels = c("White",
                                              "Black",
                                              "Hispanic",
                                              "Asian"))) %>%
  mutate(val = ifelse(as.numeric(value)<.01,"<1%",paste0(round.off(as.numeric(value)*100),"%")))

chart.demo.allparishes <- ParishDemoforGraphic %>%
  ggplot(aes(race.fac, value, fill=description.fac, label = val)) +
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .7,
           color="gray50") +
  facet_wrap(~PlaceName.fac, ncol = 2, scales = "free") +
  geom_text(aes(label = val), position=position_dodge(width = .8), vjust = -.7, hjust = .4, size=4, family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0,0), limits = c(0,1)) +
  scale_fill_manual(values = c(DCcolor.p1skyblue,
                               DCcolor.p1mediumblue)) +
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        panel.spacing = unit(6, "lines"),
        strip.text = element_text(size=12),
        axis.text.x = element_text(size = 12, vjust=1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(size = 20)) +
  labs(title = "White, Black, Hispanic, and Asian, Metro New Orleans parishes and U.S.",
       x="",
       y="")

#### 3 - African American population, New Orleans

### PEP ###
AAhistGraphic <- AAhistorical %>%
  ggplot(aes(year, POP, label = comma(POP, accuracy = 1))) +
  geom_bar(stat="identity", fill = DCcolor.p2blue, width = .7) +
  geom_text(data = subset(AAhistorical,  year %in% c("2000", "2006", "2017", "")),      #remove labels for years without data
            size = 3.75,
            position = position_stack(vjust = 1.05),
            family="Asap") +
  scale_y_continuous(labels = comma_format(), expand = c(0,0), limits = c(0,350000)) +
  themeDC_horizontal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5, size = 24)) +
  labs(title = "Black population, New Orleans",
       x="",
       y="")


####4 - -Hispanic population change by parish

### PEP ###
HispanicPopforGraphic  <- HispanicPop %>%
  mutate(PlaceName.fac = factor(.$PlaceName,levels = c("Orleans", "Jefferson", "St. Tammany", "Plaquemines",
                                                       "St. Bernard","St. Charles", "St. James", "St. John the Baptist"))) %>%
  gather(-PlaceName,-PlaceName.fac,key = variable, value = value) %>%
  mutate(description = ifelse(variable == "est2000", "2000", YEAR_PEP.char)) %>%
mutate(description.fac = factor(.$description, levels = c(YEAR_PEP.char, "2000")))

HispanicPopGraphic <- HispanicPopforGraphic %>%
  ggplot(aes(PlaceName.fac, value, fill=description.fac, label = comma(value))) +
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .7,
           color="gray50") +
  geom_text(aes(label = comma(value)),position=position_dodge(width = .7), vjust = .5, hjust = -.5, size=2.75, family="Asap") +
  scale_y_continuous(labels = comma_format(), expand = c(0,0), limits = c(0,90000)) +
  scale_fill_manual(values = c(DCcolor.p1mediumblue, DCcolor.p1skyblue), guide = guide_legend(reverse = T)) +
  themeDC_vertical() +
  coord_flip()+     #it's sideways
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        axis.text.x = element_text(size = 12, vjust=1),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = .5)) +
  labs(title = "Hispanic population change by parish",
       x="",
       y="")


####5- Hispanic population ofes in metro by year

### PEP ###

# Note! In July 2024, we changed this graphic due to updates in how the Census accounted for Hispanics in the PEP data post-2020 decennial.
# To avoid confusion, we decided to remove the year-over-year graph. For now, the old graph text is commented out in case we decide to do something different.

## old graph code:
# HispanpopYearsforGraphic <- HISPpopM  %>%
#   filter(place %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
#                           "St. James", "St. John the Baptist", "St. Tammany")) %>%
#   mutate(PlaceName.fac = factor(.$place,levels = c("St. James", "Plaquemines", "St. John the Baptist",
#                                                        "St. Charles", "St. Bernard", "St. Tammany", "Orleans", "Jefferson"))) %>%
# add_row(year = 2001, PlaceName.fac = "Jefferson", POP = 0) %>%
#   add_row(year = 2002, PlaceName.fac = "Jefferson", POP = 0) %>%
#   add_row(year = 2003, PlaceName.fac = "Jefferson", POP = 0) %>%
#   add_row(year = 2004, PlaceName.fac = "Jefferson", POP = 0) %>%
#   add_row(year = 2005, PlaceName.fac = "Jefferson", POP = 0)
# 
# chart.HispanpopYears.allparishes <- HispanpopYearsforGraphic %>%
#   ggplot(aes(year, as.numeric(POP), fill=PlaceName.fac)) +
#   geom_bar(stat="identity",
#            position="stack",
#            color = "gray30") +
#   scale_fill_manual(values = c(DCcolor.p2orangered,
#                                DCcolor.p2orange,
#                                DCcolor.p2green,
#                                DCcolor.p2teal,
#                                DCcolor.p2purple,
#                                DCcolor.p2violet,
#                                DCcolor.p2limegreen,
#                                DCcolor.p1darkblue90)) +
#   scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0,0), limits = c(0,150000)) +
#   scale_x_continuous(breaks = 2000:2023)+
#   themeDC_horizontal() +
#   theme(legend.position = "right",
#         legend.title = element_blank(),
#         legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
#         legend.spacing.y = unit(10, "lines"),
#         plot.title = element_text(hjust = .5, size = 24),
#         axis.text.x = element_text(size = 12, angle = -45, vjust = -.5, hjust = 1, family="Asap"),
#         axis.text.y = element_text(size = 12)) +
#   labs(title = "Hispanic population by year, Metro",
#        x="",
#        y="")

## new graph code:
  #old way option:
HispanpopYearsforGraphic <- HISPpopM %>%
  filter(year %in% c(2000, 2010, 2020, 2023) &
           place %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                        "St. James", "St. John the Baptist", "St. Tammany")) %>%
    mutate(PlaceName.fac = factor(.$place,levels = c("St. James", "Plaquemines", "St. John the Baptist",
                                                     "St. Charles", "St. Bernard", "St. Tammany", "Orleans", "Jefferson")))
    
chart.HispanpopYears.allparishes <- ggplot(HispanpopYearsforGraphic, aes(factor(year), as.numeric(POP), fill=place)) +
  geom_bar(stat="identity",
           position="stack",
           color = "gray30") +
  scale_fill_manual(values = c(DCcolor.p2orangered,
                               DCcolor.p2orange,
                               DCcolor.p2green,
                               DCcolor.p2teal,
                               DCcolor.p2purple,
                               DCcolor.p2violet,
                               DCcolor.p2limegreen,
                               DCcolor.p1darkblue90)) +
  scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0,0), limits = c(0,150000)) +
  scale_x_discrete(breaks = c(2000, 2010, 2020, 2023))+
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        legend.spacing.y = unit(10, "lines"),
        plot.title = element_text(hjust = .5, size = 24),
        axis.text.x = element_text(size = 12, family="Asap"),
        axis.text.y = element_text(size = 12)) +
  labs(title = "Hispanic population by year, Metro",
       x="",
       y="")

# new option:

Hispanpop_lessyears_forGraphic  <- ggplot(RacepopestRaw %>% filter(year != 2020), aes(race, POP, fill = year)) + 
  geom_bar(stat="identity",
           position="dodge",
           color = "gray30") +
  scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0,0), limits = c(0,1000000)) +
  scale_fill_manual(values = c(DCcolor.p1lightskyblue,
                    DCcolor.p1mediumblue,
                    DCcolor.p1grayblue,
                    DCcolor.p1darkblue)) + 
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        legend.spacing.y = unit(10, "lines"),
        plot.title = element_text(hjust = .5, size = 24),
        axis.text.x = element_text(size = 12, family="Asap"),
        axis.text.y = element_text(size = 12)) +
  labs(title = "Total population by race, Metro",
       x = "",
       y = "")
  



####6 - Hispanic Origin, 

hispan2018 <- hispan %>%
  select(place, 
         Cubanpct,
         Dominicanpct,
         Mexicanpct,
         PuertoRicanpct,
         Honduranpct,
         Guatemalanpct,
         Nicaraguanpct,
         Salvadoranpct,
         OtherCApct,
         SouthAmericanpct,
         Otherpct,
         contains('SIG')) %>%
  mutate(place = c( "Orleans", "Metro", "United States"))  %>% 
  mutate(PlaceName.fac = factor(.$place,levels = c( "Orleans","Metro","United States"))) %>%
  gather(-place, -PlaceName.fac, -contains('SIG'), key=variable, value = value) %>% 
  .[-(45:48),] %>%
  mutate(description = NA,
         description = ifelse(variable == "Cubanpct", "Cuban", description),
         description = ifelse(variable == "Dominicanpct", "Dominican", description),
         description = ifelse(variable == "Mexicanpct", "Mexican", description),
         description = ifelse(variable == "PuertoRicanpct", "Puerto Rican", description),
         description = ifelse(variable == "Honduranpct", "Honduran", description),
         description = ifelse(variable == "Guatemalanpct", "Guatemalan", description),
         description = ifelse(variable == "Nicaraguanpct", "Nicaraguan", description),
         description = ifelse(variable == "Salvadoranpct", "Salvadoran", description),
         description = ifelse(variable == "OtherCApct", "Other Central American", description),
         description = ifelse(variable == "SouthAmericanpct", "South American", description),
         description = ifelse(variable == "Otherpct", "Other", description)) %>%
  mutate(description.fac = factor(.$description, levels = c("Puerto Rican",
                                                            "Cuban",
                                                            "Dominican",
                                                            "Mexican",
                                                            "Guatemalan",
                                                            "Honduran",
                                                            "Salvadoran",
                                                            "Nicaraguan",
                                                            "Other Central American",
                                                            "South American",
                                                            "Other")))%>% 
  mutate(val = ifelse(as.numeric(value)<.01,     #is too crowded with <.01
                      "",
                      paste0(round.off(as.numeric(value)*100),"%",ifelse((variable == "Cubanpct" & CubanSIG == "no"& place != "United States")
                                                                     |(variable == "Dominicanpct" & DominicanSIG == "no"& place != "United States")
                                                                     |(variable == "Mexicanpct" & MexicanSIG == "no"& place != "United States")
                                                                     |(variable == "PuertoRicanpct" & PuertoRicanSIG == "no"& place != "United States")
                                                                     |(variable == "Honduranpct"& HonduranSIG == "no"& place != "United States")
                                                                     |(variable == "Guatemalanpct" & GuatemalanSIG == "no"& place != "United States")
                                                                     |(variable == "Nicaraguanpct" & NicaraguanSIG == "no"& place != "United States")
                                                                     |(variable == "Salvadoranpct" & SalvadoranSIG == "no"& place != "United States")
                                                                     |(variable == "OtherCApct" & OtherCASIG == "no"& place != "United States")
                                                                     |(variable == "SouthAmericanpct" & SouthAmericanSIG == "no"& place != "United States")
                                                                     |(variable == "Otherpct" & OtherSIG == "no"& place != "United States"), "*", ""))))


chart.hispan2018.allparishes <- hispan2018 %>% 
  ggplot(aes(x=PlaceName.fac, y=as.numeric(value), label = val, fill=description.fac)) + #,
  geom_bar(stat="identity", 
           position=position_stack(),
           width = .7,
           color="gray50") +
  scale_fill_manual(values = c(DCcolor.p1lightskyblue,
                               DCcolor.p2blue,
                               DCcolor.p2teal,
                               DCcolor.p2green,
                               DCcolor.p2limegreen,
                               DCcolor.p2yellow,
                               DCcolor.p2orange,
                               DCcolor.p2orangered,
                               DCcolor.p2magenta,
                               DCcolor.p2violet,
                               DCcolor.p2purple)) +
  geom_text(size = 3, position =position_stack(vjust = 0.5), family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0,0), limits = c(0,1)) + 
  themeDC_vertical() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12), 
        plot.title = element_text(hjust = .5, size = 14)) +
  labs(title = "Hispanic origin, 2022",
       x="",
       y="")
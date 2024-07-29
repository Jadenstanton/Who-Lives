library(extrafont)
library(scales)
library(directlabels)
library(grid)
library(here)
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
############################################
# # POPULATION BY AGE AND HOUSEHOLD TYPES # #
############################################

# 
###7 - Population by age group, 2000
agepop2000forGraphic <- Agepop %>%
  mutate(PlaceName.fac = factor(.$PlaceName,levels = c("St. John the Baptist","St. James", "St. Charles",
                                                       "St. Bernard", "Plaquemines", "St. Tammany","Jefferson","Orleans"))) %>%
  mutate(age.fac = factor(.$age, levels = c("Under 5 years", "5 to 9","10 to 14","15 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 to 79","80 to 84","85 plus")))

chart.agepop2000.allparishes <- agepop2000forGraphic %>%
  ggplot(aes(age.fac, as.numeric(est2000), fill=PlaceName.fac)) +
  geom_bar(stat="identity",
           position="stack",
           color = "gray30") +
  scale_fill_manual(values = c(DCcolor.p2teal50,
                               DCcolor.p2teal,
                               DCcolor.p1lightskyblue,
                               DCcolor.p1skyblue,
                               DCcolor.p2blue70,
                               DCcolor.p1mediumblue,
                               DCcolor.p2blue90,
                               DCcolor.p1darkblue90)) +
  scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0,0), limits = c(0,120000)) +
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        axis.text.x = element_text(size = 12, angle = -45, vjust = -1, family="Asap"),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = .5, size = 16)) +
  labs(title = "Population by age group, 2000",
       x="",
       y="")



#   
####8 - Population by age group, 2022


### PEP ###
agepopCurrentforGraphic <- Agepop %>%
  mutate(PlaceName.fac = factor(.$PlaceName,levels = c("St. John the Baptist","St. James", "St. Charles",
                                                       "St. Bernard", "Plaquemines", "St. Tammany","Jefferson","Orleans"))) %>%
  mutate(age.fac = factor(.$age, levels = c("Under 5 years", "5 to 9","10 to 14","15 to 19","20 to 24","25 to 29","30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59","60 to 64","65 to 69","70 to 74","75 to 79","80 to 84","85 plus")))

chart.agepopCurrent.allparishes <- agepopCurrentforGraphic %>%
  ggplot(aes(age.fac, as.numeric(population), fill=PlaceName.fac)) +
  geom_bar(stat="identity",
           position="stack",
           color = "gray30") +
  scale_fill_manual(values = c(DCcolor.p2teal50,
                               DCcolor.p2teal,
                               DCcolor.p1lightskyblue,
                               DCcolor.p1skyblue,
                               DCcolor.p2blue70,
                               DCcolor.p1mediumblue,
                               DCcolor.p2blue90,
                               DCcolor.p1darkblue90)) +
  scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0,0), limits = c(0,120000)) +
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        axis.text.x = element_text(size = 12, angle = -45, vjust = -1, family="Asap"),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = .5, size = 16))+
  labs(title = "Population by age group, 2023",
       x="",
       y="")


####9 - Households with own children under 18

hwcGraphic <- dodgedBar(hwc, 
                        quo(pcthwc), 
                        "Households with own children under 18")

####10 -  Single Person Households

singGraphic <- dodgedBar(sing, 
                         quo(pctsing), 
                         "Single-person households",
                         yscale = c(0,.55))

####11 - Under 18 population

### PEP ###
popunder18forGraphic <- popunder18 %>%
  mutate(PlaceName.fac = factor(.$PlaceName,levels = c("Orleans", "Jefferson","St. Tammany","Metro"))) %>%
  pivot_longer(cols = c(under18, est2000), names_to = "variable", values_to = "val") %>%
  #gather(PlaceName,-PlaceName.fac, key=variable, value =val) %>%
  mutate(description = ifelse(variable == "est2000", "2000", YEAR_PEP.char))

popunder18Graphic <- popunder18forGraphic %>%
  ggplot(aes(PlaceName.fac, val, fill=description, label = comma(val))) +
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .7,
           color="gray50") +
  geom_text(position=position_dodge(width = .7), vjust = -.7, size=3, family="Asap") +
  scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0,0), limits = c(0,400000)) +
  scale_fill_manual(values = c(DCcolor.p1skyblue, DCcolor.p1mediumblue)) +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        plot.title = element_text(hjust = .5)) +
  labs(title = "Under 18 population",
       x="",
       y="")
  
############################################
# # EDUCATIONAL ATTAINMENT, INCOME, AND INTERNET ACCESS # #
############################################

####12 - Less than a high school degree, adults 25 and older

hsGraphic <- dodgedBar(hs, 
                       quo(pctless), 
                       "Less than a high school degree, adults 25 and older", 
                       yscale = c(0,.35))

####13 - Bachelor's degree or higher, adults 25 and older

bachGraphic <- dodgedBar(bach, 
                         quo(pctbach), 
                         "Rate of bachelor's degree or higher, adults 25 and older")

####14 - Median household income, 2016 inflation-adjusted dollars

medhhGraphic <- dodgedBar(medhh,
                          quo(MedianHHIncome),
                          "Median household income in 2022 dollars",
                          yscale = c(0,1.3*max(medhh$MedianHHIncome)),
                          colors = c(DCcolor.p2teal50, DCcolor.p1mediumblue),
                          pct = FALSE,
                          comparisonyear = "1999")


dataGraphic <-  medhh %>% select(-contains("moeprop")) %>%      #dplyr rejects the format of moeprop, so we drop it  mutate(placenames = NA,
  mutate(placenames = NA,
         placenames = ifelse(place == "103", "St. Tammany", placenames),
         placenames = ifelse(place == "051", "Jefferson", placenames),
         placenames = ifelse(place == "071", "Orleans", placenames),
         placenames = ifelse(place == "35380","Metro",placenames),
         placenames = ifelse(place == "1", "U.S.", placenames)) %>%
  mutate(place.fac = factor(.$placenames,levels = c("Orleans", "Jefferson","St. Tammany","Metro", "U.S."))) %>%     #vars of type "factor" allow you to control order
  select(one_of("census2000", "sf2004", "sf1999"), !!quo(MedianHHIncome), placenames, place.fac, significant) %>%     #one_of() chooses correct comparison vals/!! is the second part or the quo() tool
  gather(-placenames,-place.fac, -significant, key=variable, value=value) %>% 
  mutate(description = as.factor(ifelse(variable == "census2000"|variable =="sf2004"|variable =="sf1999", 1999, year))) %>%     #creates legend info
  mutate(valp = ifelse(value<.01,ifelse(significant == "no" & description == year, "<1%*", "<1%"),     #creates pct labels
                       paste0(round.off(value*100, digits = 0),"%",ifelse((significant == "no" & description == year), "*", "")))) %>%
  mutate(vald = ifelse((significant == "no" & description == year),      #creates dollar labels
                       paste0(dollar(value, largest_with_cents = 1),"*"), 
                       dollar(value, largest_with_cents = 1)))

medhhGraphic <- dataGraphic %>% 
  ggplot(aes(place.fac, value, fill=description)) + 
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .8,
           color='gray50') +    #bar outline
  geom_text(data = subset(dataGraphic, as.numeric(value) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = vald), 
            #position = position_dodge(width=ifelse(data$vald== "56,438",1.1,.7)),
            position=position_dodge(width = .7),  #change width accordingly
            vjust = -.7, 
            size=2.75, 
            family="Asap") +
  scale_y_continuous( expand = c(0,0), limits = c(0,1.3*max(medhh$MedianHHIncome))) + 
  scale_fill_manual(values = c(DCcolor.p2teal50, DCcolor.p1mediumblue)) + 
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(hjust = .5, size=16)) + 
  labs(title = "Median household income in 2022 dollars",
       x="",
       y="")


####15 - Internet Access

intaforGraphic <- inta %>% 
  select(place,
         contains('pct'),
         contains('SIG')) %>%
  mutate(PlaceNames = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))  %>% 
  mutate(PlaceName.fac = factor(.$PlaceNames,levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
  gather(key = variable, value = value, contains("pct")) %>% 
  mutate(description = NA,
         description = ifelse(variable == "broadbandpct", "Broadband and all other", description),
         description = ifelse(variable == "noaccpct", "No Internet access", description),
         description = ifelse(variable == "cellonlypct", "Cellular only", description),
         description = ifelse(variable == "nosubpct", "Access with no subscription", description)) %>%
  mutate(description.fac = factor(.$description, levels = c( "Access with no subscription",
                                                             "Cellular only",
                                                             "No Internet access",
                                                             "Broadband and all other")))%>% 
  mutate(val = ifelse(value<.01,ifelse((variable == "broadbandpct" & broadbandSIG == "no" & PlaceNames != "U.S.")
                                       |(variable == "noaccpct" & noaccSIG == "no" & PlaceNames != "U.S.")
                                       |(variable == "cellonlypct" & cellonlySIG == "no" & PlaceNames != "U.S.")
                                       |(variable == "nosubpct" & nosubSIG == "no" & PlaceNames != "U.S."), "<1%*", ""),
                      paste0(round.off(value*100),"%",ifelse((variable == "broadbandpct" & broadbandSIG == "no" & PlaceNames != "U.S.")
                                                         |(variable == "noaccpct" & noaccSIG == "no" & PlaceNames != "U.S.")
                                                         |(variable == "cellonlypct" & cellonlySIG == "no" & PlaceNames != "U.S.")
                                                         |(variable == "nosubpct" & nosubSIG == "no" & PlaceNames != "U.S."), "*", ""))))


#<1%",paste0(round.off(value*100),"%")))

chart.inta.allparishes <- intaforGraphic %>% 
  ggplot(aes(PlaceName.fac, as.numeric(value), fill=description.fac, label = val)) +
  geom_bar(stat="identity", position="fill", color = "gray50") +
  scale_fill_manual(values = c(DCcolor.p2orangered,
                               DCcolor.p2yellow,
                               DCcolor.p2violet,
                               DCcolor.p1skyblue)) +
  geom_text(size = 3, position = position_stack(vjust = 0.6), family="Asap") + 
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0,0), limits = c(0,1)) + 
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10), 
        plot.title = element_text(hjust = .5)) +
  labs(title = "Household internet access, 2022",
       x="",
       y="") 
############################################
# # POVERTY AND ACCESS TO VEHICLES # #
############################################


####16 - Poverty rate, population for whom poverty has been determined

povGraphic <- dodgedBar(pov, 
                        quo(pctpov), 
                        "Poverty rate, population for whom poverty has been determined", 
                        yscale = c(0,.35), 
                        colors = c(DCcolor.p2teal50, DCcolor.p1mediumblue),
                        comparisonyear = "1999")

####17 - Children in poverty, population for whom poverty has been determined

childpovGraphic <- dodgedBar(childpov, 
                             quo(pctBelowChildPov), 
                             "Children in poverty, population for whom poverty has been determined",
                             colors = c(DCcolor.p2teal50, DCcolor.p1mediumblue),
                             comparisonyear = "1999")

####18 - Households without access to a vehicle

vehGraphic <- dodgedBar(veh, 
                        quo(vehpct), 
                        "Households without access to a vehicle", 
                        yscale = c(0,.35))

############################################
# # FOREIGN-BORN POP # #
############################################

####19 - Population not U.S. citizens at birth

forborGraphic <- dodgedBar(forbor, quo(forborpct),"Population not U.S. citizens at birth", yscale = c(0,.18), digits = 0)

####20 - Population who moved in the past year

mobforGraphic <- mob %>% 
  select(placename, 
         contains('SIG'),
         contains('pct'),
         contains('2004')) %>%
  mutate(PlaceNames = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))  %>% 
  mutate(PlaceName.fac = factor(.$PlaceNames,levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
  select(-samehousepct, -sf2004samehouse) %>% 
  gather(key = variable, value = value, contains("pct"), (contains("2004") & !contains("MOE"))) %>% 
  mutate(description = NA,
         description = ifelse(variable == "mobabroadpct"|variable == "sf2004mobabroad", "Moved from abroad", description),
         description = ifelse(variable == "mobStatespct"|variable == "sf2004states", "Moved from out of state", description),
         description = ifelse(variable == "difparishpct"|variable == "sf2004difparish", "Moved from different parish in state", description),
         description = ifelse(variable == "withinparishpct"|variable == "sf2004withinparish", "Moved within same parish", description)) %>%
  mutate(year = NA,
         year = ifelse(variable == "mobabroadpct"|variable == "mobStatespct"|variable == "difparishpct"|variable == "withinparishpct", 2018, year),
         year = ifelse(variable == "sf2004mobabroad"|variable == "sf2004states"|variable == "sf2004difparish"|variable == "sf2004withinparish", 2004,year)) %>%
  mutate(description.fac = factor(.$description, levels = c( "Moved from abroad",
                                                             "Moved from out of state",
                                                             "Moved from different parish in state",
                                                             "Moved within same parish")))%>%
  mutate(year.fac = factor(.$year, levels = c("2004",
                                              "2018"))) %>%
  mutate(val = ifelse(value<.01,ifelse((abroadSIG == "no" & variable == "mobabroadpct")
                                              |(statesSIG ==  "no" & variable == "mobStatespct")
                                              |(difparishSIG ==  "no" & variable == "difparishpct")
                                              |(withinparishSIG ==  "no" & variable == "withinparishpct"), "<1%*", "<1%"),
         paste0(round.off(value*100),"%",ifelse((abroadSIG == "no" & variable == "mobabroadpct")
                                                                         |(statesSIG ==  "no" & variable == "mobStatespct")
                                                                         |(difparishSIG ==  "no" & variable == "difparishpct")
                                                                         |(withinparishSIG ==  "no" & variable == "withinparishpct"), "*", ""))))

chart.mob.allparishes <- mobforGraphic %>% 
  ggplot(aes(year.fac, as.numeric(value), fill=description.fac, label = val)) +
  geom_bar(stat="identity", 
           position="stack",
           color="gray50") +
  facet_wrap(~PlaceName.fac, ncol = 5) + 
  scale_fill_manual(values = c(DCcolor.p1lightskyblue,
                               DCcolor.p1skyblue,
                               DCcolor.p2teal,
                               DCcolor.p2blue90)) +
  geom_text(data = subset(mobforGraphic, 
                          as.numeric(value) != 0), 
            size = 4, 
            position = position_stack(vjust = 0.6), 
            family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0,0), limits = c(0,.18)) +
  scale_x_discrete(labels = c("2004","2022"))+
  themeDC_horizontal() +
  theme(plot.title = element_text(hjust = .5, size = 18),
        strip.text = element_text(size=12),
        legend.position = "right",
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),  
        axis.text.x = element_text(size = 12, vjust = 1),
        axis.text.y = element_text(size = 12),
        legend.title = element_blank()) +
  labs(title = "Population who moved in the past year",
       x="",
       y="") 


############################################
# # HOMEOWNERSHIP # #
############################################

####21 - Homeownership rates

hoGraphic <- dodgedBar(ho, 
                       quo(Ownerpct), 
                       "Homeownership rates", 
                       yscale = c(0,.85))

####22 - Homeowners without a mortgage

honomoGraphic <- dodgedBar(honomo, 
                           quo(honomopct), 
                           "Homeowners without a mortgage", 
                           yscale = c(0,.55))

############################################
# # HOUSING COSTS & COMMUTING # #
############################################

####23 - Renters with severe housing cost burdens

rentburGraphic <- dodgedBar(rentbur, 
                            quo(rentburpct), 
                            "Renters with severe housing cost burdens",
                            colors = c(DCcolor.p2limegreen60, DCcolor.p1mediumblue),
                            comparisonyear = "2004")



####24 - Homeowners with severe housing cost burdens

hoburGraphic <- dodgedBar(hobur, 
                          quo(hoburpct), 
                          "Homeowners with severe housing cost burdens", 
                          yscale = c(0,.2), 
                          colors = c(DCcolor.p2limegreen60, DCcolor.p1mediumblue),
                          comparisonyear = "2004")

####25 - Median gross rent, inflation-adjusted dollars

medrentGraphic <- dodgedBar(medrent, 
                            quo(Rent), 
                            "Median gross rent in 2022 dollars",
                            yscale = c(0,1.2*max(medrent$Rent)), 
                            pct = FALSE, 
                            colors = c(DCcolor.p2limegreen60, DCcolor.p1mediumblue),
                            comparisonyear = "2004")

####26 - Year structure built, 201* housing units

yrbuiltforGraphic <- yrbuilt %>% 
  select(contains('pct'),
         contains('SIG')) %>%
  mutate(PlaceName = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))  %>% 
  mutate(PlaceName.fac = factor(.$PlaceName,levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
  gather(-PlaceName, -PlaceName.fac, -contains('SIG'), key=variable, value = value) %>% 
  mutate(description = NA,
         description = ifelse(variable == "orLater1990pct", "1990 or later", description),
         description = ifelse(variable == "mid1950to1989pct", "1950-1989", description),
         description = ifelse(variable == "orbefore1949pct", "1949 or earlier", description)) %>%
  mutate(description.fac = factor(.$description, levels = c( "1990 or later",
                                                             "1950-1989",
                                                             "1949 or earlier")))%>% 
  mutate(val = ifelse(as.numeric(value)<.01,ifelse((variable == "orLater1990pct" & orLater1990SIG == "no" & PlaceName != "U.S.")
                                                   |(variable == "mid1950to1989pct" & mid1950to1989SIG == "no" & PlaceName != "U.S.")
                                                   |(variable == "orbefore1949pct" & orbefore1949SIG == "no" & PlaceName != "U.S."), "<1%*", "<1%"),
                      paste0(round.off(as.numeric(value)*100), "%",ifelse((variable == "orLater1990pct" & orLater1990SIG == "no" & PlaceName != "U.S.")
                                                                      |(variable == "mid1950to1989pct" & mid1950to1989SIG == "no" & PlaceName != "U.S.")
                                                                      |(variable == "orbefore1949pct" & orbefore1949SIG == "no" & PlaceName != "U.S."), "*", ""))))


chart.yrbuilt.allparishes <- yrbuiltforGraphic %>% 
  ggplot(aes(PlaceName.fac, as.numeric(value), fill=description.fac, label = val)) +
  geom_bar(stat="identity", 
           position="fill",
           size = .7,
           color="gray50") +
  scale_fill_manual(values = c(DCcolor.p2teal,
                               DCcolor.p1skyblue,
                               DCcolor.p2orangered)) +
  geom_text(size = 3, position = position_stack(vjust = 0.6), family="Asap") + 
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0,0), limits = c(0,1)) + 
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        axis.text.x = element_text(size = 10, vjust = 1),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = .5)) +
  labs(title = "Year structure built, 2022 housing units",
       x="",
       y="") 

####27 Means of transportation to work, workers 16 years and older

commuteforGraphic <- commute %>% 
  select(placename,
         contains('pct'),
         contains('SIG')) %>%
  mutate(PlaceNames = c("Orleans", "Jefferson", "Metro", "U.S."))  %>% 
  mutate(PlaceName.fac = factor(.$PlaceNames,levels = c("Orleans", "Jefferson", "Metro", "U.S."))) %>%
  gather(key = variable, value = value, contains("pct")) %>% 
  mutate(description = NA,
         description = ifelse(variable == "Drivepct"|variable == "Drivepct2000", "Drive Alone", description),
         description = ifelse(variable == "Carpoolpct"|variable == "Carpoolpct2000", "Carpool", description),
         description = ifelse(variable == "PublicTransitpct"|variable == "PublicTransitpct2000", "Public Transit", description),
         description = ifelse(variable == "bikepct"|variable == "bikepct2000", "Bike", description),
         description = ifelse(variable == "Walkpct"|variable == "Walkpct2000", "Walk", description),
         description = ifelse(variable == "Workhomepct"|variable == "Workhomepct2000", "Work at home", description),
         description = ifelse(variable == "Otherpct"|variable == "Otherpct2000", "Other", description)) %>%
  mutate(year = NA,
         year = ifelse(grepl("pct",variable), 2022, year),
         year = ifelse(grepl("2000", variable), 2000,year)) %>%
  mutate(description.fac = factor(.$description, levels = c("Other",
                                                            "Work at home",
                                                            "Walk",
                                                            "Bike",
                                                            "Public Transit",
                                                            "Carpool",
                                                            "Drive Alone")))%>% 
  mutate(year.fac = factor(.$year, levels = c("2000",
                                              "2022"))) %>%
  mutate(val = ifelse(value<.02, "",
                      paste0(round.off(value*100),"%",ifelse((DriveSIG == "no" & variable == "Drivepct")
                                                         |(carpoolSIG ==  "no" & variable == "Carpoolpct")
                                                         |(PublicTransitSIG ==  "no" & variable == "PublicTransitpct")
                                                         |(bikeSIG ==  "no" & variable == "bikepct")
                                                         |(walkSIG ==  "no" & variable == "walkpct")
                                                         |(workhomeSIG ==  "no" & variable == "Workhomepct")
                                                         |(otherSIG ==  "no" & variable == "Otherpct"), "*", ""))))

chart.commute.allparishes <- commuteforGraphic %>%
  ggplot(aes(year.fac, as.numeric(value), fill=description.fac, label = val)) +
  geom_bar(stat="identity", 
           position="fill",
           size = .7,
           color="gray50") +
  facet_wrap(~PlaceName.fac, ncol = 5) + 
  scale_fill_manual(values = c(DCcolor.p2yellow,
                               DCcolor.p2limegreen,
                               DCcolor.p2green,
                               DCcolor.p2teal,
                               DCcolor.p2blue,
                               DCcolor.p1skyblue,
                               DCcolor.p1lightskyblue)) +
  geom_text(size = 4, position = position_stack(vjust = 0.6), family="Asap") + 
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0,0), limits = c(0,1)) +
  scale_x_discrete(labels = c("2000",
                              "2022"))+
  themeDC_horizontal() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        axis.text.x = element_text(size = 10, vjust = 1),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust=.5)) +
  labs(title = "Means of transportation to work, workers 16 years and older",
       x="",
       y="") 

##############################
# Jenna's expanded graphs
###############################

#across geos median hh income bar chart

medhh.raceGeos_chart <- medhh_with_stats %>%
  ggplot(aes(x=placename.fac, y=val, fill=var.fac)) + 
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .6,
           color='gray70') +    #bar outlineas.factor
  geom_text(data = subset(medhh_with_stats, as.numeric(val) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = val_lab),
            position=position_dodge(width = .7),
            vjust = -1,
            size=2.75,
            family="Asap") +
  scale_y_continuous(labels = comma_format(accuracy = 1)) + 
  scale_fill_manual(values = c(DCcolor.p1darkblue90,DCcolor.p2green90,DCcolor.p2violet90,DCcolor.p3yellowochre90),
                    limits = levels(medhh_with_stats$var.fac)) +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16, hjust = .5)) + 
  labs(title = "Median household income by race/ethnicity, 2022",
       x="",
       y="")
medhh.raceGeos_chart
ggsave(medhh.raceGeos_chart,filename = "indicator expansion drafts/graphics/medhh.raceGeos.png",
       width = 10, height = 6, units = "in")

#Historical median hh income line chart 

medhh.hist_chart <- medhh.hist %>%
  filter(var != "All", Year != 2016) %>%
  ggplot()+
  geom_line(aes(x=Year,y=val, color = var.fac), size = 1) +
  scale_y_continuous(labels = dollar_format(accuracy = 1), limits = c(0,99000), breaks = c(0,30000,60000,90000)) + 
  scale_x_continuous( labels = c("1979", "1989", "1999", "2010", "2022")) +
  scale_color_manual(values = c( DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p3yellowochre)) +
  geom_text(data = subset(medhh.hist, Year %in% c("1979", "2022") & var != "All"), aes(x=Year,y=val, label = label_dollar(accuracy = 1)(val)), vjust = -1, family = "Asap") +
  geom_text(data = subset(medhh.hist, Year == "2022"), aes(x = Year, y = val, label = val_lab, hjust = -.7, vjust = .7), size = 6)+
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16, hjust = .5)) + 
  labs(title = "Median household income by race/ethnicity in 2022 dollars since 1979, Orleans Parish",
       x="",
       y="") 
medhh.hist_chart
ggsave(medhh.hist_chart,filename = "indicator expansion drafts/graphics/medhh.hist.png",
       width = 10, height = 6, units = "in")

# Across geos educational attainment bar chart

bach.raceGeos_chart <- bach_with_stats %>%
  ggplot(aes(x=placename.fac, y=val, fill=var.fac)) + 
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .6,
           color='gray70') +    #bar outlineas.factor
  geom_text(data = subset(bach_with_stats, as.numeric(val) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = val_lab),
            position=position_dodge(width = .7),
            vjust = -.7,
            size=2.75,
            family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_fill_manual(values = c(DCcolor.p1darkblue90,DCcolor.p2green90,DCcolor.p2violet90,DCcolor.p3yellowochre90),
                    limits = levels(bach_with_stats$var.fac)) +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16, hjust = .5)) + 
  labs(title = "Rate of bachelor's degree or higher, adults 25 years or older by race/ethnicity, 2022",
       x="",
       y="")
ggsave(bach.raceGeos_chart,filename = "indicator expansion drafts/graphics/bach.raceGeos.png",
       width = 10, height = 6, units = "in")

#Historical educational attainment line chart

EduAtt.hist_chart <- EduAtt.hist %>%
  filter(var != "All", year != 2016) %>%
  ggplot()+
  geom_line(aes(x=year,y=val, color = var.fac), size = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_x_continuous(labels = c("1980", "1990", "2000", "2010", "2022"))+
  scale_color_manual(values = c(DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p3yellowochre)) +
  geom_text(data = subset(EduAtt.hist, year %in% c("1980", "2022") & var != "All"), aes(x=year,y=val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
  geom_text(data = subset(EduAtt.hist, year == "2022"), aes(x = year, y = val, label = val_lab, hjust = -.7, vjust = .7), size = 6)+
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16, hjust = .5)) + 
  labs(title = "Rate of bachelor's degree or higher, adults 25 years or older by race/ethnicity,\nOrleans Parish",
       x="",
       y="") 

ggsave(EduAtt.hist_chart,filename = "indicator expansion drafts/graphics/bach.hist.png",
       width = 8, height = 6, units = "in")

# across geos pov bar chart



pov.raceGeos_chart <- pov_with_stats %>%
  ggplot(aes(x=placename.fac, y=val, fill=var.fac)) + 
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .6,
           color='gray70') +    #bar outlineas.factor
  geom_text(data = subset(pov_with_stats, as.numeric(val) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = val_lab),
            position=position_dodge(width = .7),
            vjust = -.7,
            size=2.75,
            family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_fill_manual(values = c(DCcolor.p1darkblue90,DCcolor.p2green90,DCcolor.p2violet90,DCcolor.p3yellowochre90),
                    limits = levels(pov_with_stats$var.fac)) +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16, hjust = .5)) + 
  labs(title = "Poverty rate by race/ethnicity, 2022",
       x="",
       y="")
ggsave(pov.raceGeos_chart,filename = "indicator expansion drafts/graphics/pov.raceGeos.png",
       width = 10, height = 6, units = "in")

#Historical total pov line chart

totalPov.hist_chart <- totalPov.hist %>%
  filter(var != "All", year != 2015) %>%
  ggplot()+
  geom_line(aes(x=year,y=val, color = var.fac), size = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = c(DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p3yellowochre)) +
  scale_x_continuous( labels = c("1979", "1989", "1999", "2010", "2022")) + 
  geom_text(data = subset(totalPov.hist, year %in% c("1979", "2022") & var != "All"), aes(x=year,y=val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
  geom_text(data = subset(totalPov.hist, year == "2022"), aes(x = year, y = val, label = val_lab, hjust = -.7, vjust = .7), size = 6)+
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16, hjust = .5)) + 
  labs(title = "Poverty rate by race/ethnicity since 1979, Orleans Parish",
       x="",
       y="") 

ggsave(totalPov.hist_chart,filename = "indicator expansion drafts/graphics/pov.hist.png",
       width = 8, height = 6, units = "in")



#child poverty

childpov.raceGeos_chart <- childpov_with_stats %>%
  ggplot(aes(x=placename.fac, y=val, fill=var.fac)) + 
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .6,
           color='gray70') +    #bar outlineas.factor
  geom_text(data = subset(childpov_with_stats, as.numeric(val) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = val_lab),
            position=position_dodge(width = .7),
            vjust = -.7,
            size=2.75,
            family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_fill_manual(values = c(DCcolor.p1darkblue90,DCcolor.p2green90,DCcolor.p2violet90,DCcolor.p3yellowochre90),
                    limits = levels(childpov_with_stats$var.fac)) +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16, hjust = .5)) + 
  labs(title = "Child poverty rate by race/ethnicity, 2022",
       x="",
       y="")
ggsave(childpov.raceGeos_chart,filename = "indicator expansion drafts/graphics/childpov.raceGeos.png",
       width = 10, height = 6, units = "in")

# Historical child pov line chart

childPov.hist_chart <- childPov.hist %>%
  filter(var != "All", Year != 2015) %>%
  ggplot()+
  geom_line(aes(x=Year,y=val, color = var.fac), size = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_color_manual(values = c( DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p3yellowochre)) +
  scale_x_continuous( labels = c("1979", "1989", "1999", "2010", "2022")) + 
  geom_text(data = subset(childPov.hist, Year %in% c("1980", "2022") & var != "All"), aes(x=Year,y=val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
  geom_text(data = subset(childPov.hist, Year == "2022"), aes(x = Year, y = val, label = val_lab, hjust = -.7, vjust = .7), size = 6)+
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16, hjust = .5)) + 
  labs(title = "Child poverty rate by race/ethnicity since 1979, Orleans Parish",
       x="",
       y="")

ggsave(childPov.hist_chart,filename = "indicator expansion drafts/graphics/childpov.hist.png",
       width = 8, height = 6, units = "in")

# Homeownership


ho.raceGeos_chart <- ho_with_stats %>%
  ggplot(aes(x=placename.fac, y=val, fill=var.fac)) + 
  geom_bar(stat="identity",
           position = position_dodge(),
           width = .6,
           color='gray70') +    #bar outlineas.factor
  geom_text(data = subset(ho_with_stats, as.numeric(val) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            aes(label = val_lab),
            position=position_dodge(width = .7),
            vjust = -.7,
            size=2.75,
            family="Asap") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) + 
  scale_fill_manual(values = c(DCcolor.p1darkblue90,DCcolor.p2green90,DCcolor.p2violet90,DCcolor.p3yellowochre90),
                    limits = levels(ho_with_stats$var.fac)) +
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16, hjust = .5)) + 
  labs(title = "Homeownership rate by race/ethnicity, 2022",
       x="",
       y="")
ggsave(ho.raceGeos_chart,filename = "indicator expansion drafts/graphics/homeownership.raceGeos.png",
       width = 10, height = 6, units = "in")


# Historical child homeownership line chart

homeownership.hist_chart <- homeownership.hist %>%
  filter(var != "All", Year != 2016) %>%
  filter(val != 0) %>%
  ggplot()+
  geom_line(aes(x=Year,y=val, color = var.fac), size = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(.2,.7)) + 
  scale_x_continuous( labels = c("1970", "1980", "1990", "2000", "2010","2022")) + 
  scale_color_manual(values = c( DCcolor.p1darkblue,DCcolor.p2green,DCcolor.p3yellowochre)) +
  geom_text(data = subset(homeownership.hist, Year %in% c("1970", "2022") & var != "All"), aes(x=Year,y=val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
  geom_text(data = subset(homeownership.hist,  Year == "1980"& var == "Hispanic,\nany race" ), aes(x=Year,y=val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
  geom_text(data = subset(homeownership.hist, Year == "2022"), aes(x = year, y = val, label = val_lab, hjust = -.7, vjust = .7), size = 6)+
  themeDC_horizontal() +
  theme(legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size=16, hjust = .5)) + 
  labs(title = "Homeownership rate by race/ethnicity since 1970, Orleans Parish",
       x="",
       y="") 
homeownership.hist_chart
ggsave(homeownership.hist_chart,filename = "indicator expansion drafts/graphics/homeownership.hist.png",
       width = 8, height = 6, units = "in")



# #For checking graphics
# #ctrl+shift+c to un-#
# AAwhthispGraphic #PEP
# chart.demo.allparishes #PEP
# AAhistGraphic #PEP
# HispanicPopGraphic #PEP
# chart.HispanpopYears.allparishes  #PEP
# chart.hispan.allparishes 
# chart.agepop2000.allparishes
# chart.agepopCurrrent.allparishes #PEP
# hwcGraphic
# singGraphic
# popunder18Graphic #PEP
# hsGraphic
# bachGraphic
# medhhGraphic
# chart.inta.allparishes
# povGraphic
# childpovGraphic
# vehGraphic
# forborGraphic
# chart.mob.allparishes
# hoGraphic
# honomoGraphic
# rentburGraphic
# hoburGraphic
# medrentGraphic
# chart.yrbuilt.allparishes
# chart.commute.allparishes


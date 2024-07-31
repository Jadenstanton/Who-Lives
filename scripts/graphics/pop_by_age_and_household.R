source(here("inputs/datacenter_colors.R"))

############################################
# # POPULATION BY AGE AND HOUSEHOLD TYPES # #
############################################

#
### 7 - Population by age group, 2000
agepop2000forGraphic <- Agepop %>%
    mutate(PlaceName.fac = factor(.$PlaceName, levels = c(
        "St. John the Baptist", "St. James", "St. Charles",
        "St. Bernard", "Plaquemines", "St. Tammany", "Jefferson", "Orleans"
    ))) %>%
    mutate(age.fac = factor(.$age, levels = c("Under 5 years", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 plus")))

chart.agepop2000.allparishes <- agepop2000forGraphic %>%
    ggplot(aes(age.fac, as.numeric(est2000), fill = PlaceName.fac)) +
    geom_bar(
        stat = "identity",
        position = "stack",
        color = "gray30"
    ) +
    scale_fill_manual(values = c(
        DCcolor.p2teal50,
        DCcolor.p2teal,
        DCcolor.p1lightskyblue,
        DCcolor.p1skyblue,
        DCcolor.p2blue70,
        DCcolor.p1mediumblue,
        DCcolor.p2blue90,
        DCcolor.p1darkblue90
    )) +
    scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0, 0), limits = c(0, 120000)) +
    themeDC_horizontal() +
    theme(
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        axis.text.x = element_text(size = 12, angle = -45, vjust = -1, family = "Asap"),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = .5, size = 16)
    ) +
    labs(
        title = "Population by age group, 2000",
        x = "",
        y = ""
    )



#
#### 8 - Population by age group, 2022


### PEP ###
agepopCurrentforGraphic <- Agepop %>%
    mutate(PlaceName.fac = factor(.$PlaceName, levels = c(
        "St. John the Baptist", "St. James", "St. Charles",
        "St. Bernard", "Plaquemines", "St. Tammany", "Jefferson", "Orleans"
    ))) %>%
    mutate(age.fac = factor(.$age, levels = c("Under 5 years", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", "35 to 39", "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 to 64", "65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 plus")))

chart.agepopCurrent.allparishes <- agepopCurrentforGraphic %>%
    ggplot(aes(age.fac, as.numeric(population), fill = PlaceName.fac)) +
    geom_bar(
        stat = "identity",
        position = "stack",
        color = "gray30"
    ) +
    scale_fill_manual(values = c(
        DCcolor.p2teal50,
        DCcolor.p2teal,
        DCcolor.p1lightskyblue,
        DCcolor.p1skyblue,
        DCcolor.p2blue70,
        DCcolor.p1mediumblue,
        DCcolor.p2blue90,
        DCcolor.p1darkblue90
    )) +
    scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0, 0), limits = c(0, 120000)) +
    themeDC_horizontal() +
    theme(
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        axis.text.x = element_text(size = 12, angle = -45, vjust = -1, family = "Asap"),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = .5, size = 16)
    ) +
    labs(
        title = "Population by age group, 2023",
        x = "",
        y = ""
    )


#### 9 - Households with own children under 18

hwcGraphic <- dodgedBar(
    hwc,
    quo(pcthwc),
    "Households with own children under 18"
)

#### 10 -  Single Person Households

singGraphic <- dodgedBar(sing,
    quo(pctsing),
    "Single-person households",
    yscale = c(0, .55)
)

#### 11 - Under 18 population

### PEP ###
popunder18forGraphic <- popunder18 %>%
    mutate(PlaceName.fac = factor(.$PlaceName, levels = c("Orleans", "Jefferson", "St. Tammany", "Metro"))) %>%
    pivot_longer(cols = c(under18, est2000), names_to = "variable", values_to = "val") %>%
    # gather(PlaceName,-PlaceName.fac, key=variable, value =val) %>%
    mutate(description = ifelse(variable == "est2000", "2000", YEAR_PEP.char))

popunder18Graphic <- popunder18forGraphic %>%
    ggplot(aes(PlaceName.fac, val, fill = description, label = comma(val))) +
    geom_bar(
        stat = "identity",
        position = position_dodge(),
        width = .7,
        color = "gray50"
    ) +
    geom_text(position = position_dodge(width = .7), vjust = -.7, size = 3, family = "Asap") +
    scale_y_continuous(labels = comma_format(accuracy = 1), expand = c(0, 0), limits = c(0, 400000)) +
    scale_fill_manual(values = c(DCcolor.p1skyblue, DCcolor.p1mediumblue)) +
    themeDC_horizontal() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        plot.title = element_text(hjust = .5)
    ) +
    labs(
        title = "Under 18 population",
        x = "",
        y = ""
    )

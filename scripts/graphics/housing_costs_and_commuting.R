source(here("inputs/datacenter_colors.R"))

############################################
# # HOUSING COSTS & COMMUTING # #
############################################

#### 23 - Renters with severe housing cost burdens

rentburGraphic <- dodgedBar(rentbur,
    quo(rentburpct),
    "Renters with severe housing cost burdens",
    colors = c(DCcolor.p2limegreen60, DCcolor.p1mediumblue),
    comparisonyear = "2004"
)



#### 24 - Homeowners with severe housing cost burdens

hoburGraphic <- dodgedBar(hobur,
    quo(hoburpct),
    "Homeowners with severe housing cost burdens",
    yscale = c(0, .2),
    colors = c(DCcolor.p2limegreen60, DCcolor.p1mediumblue),
    comparisonyear = "2004"
)

#### 25 - Median gross rent, inflation-adjusted dollars

medrentGraphic <- dodgedBar(medrent,
    quo(Rent),
    "Median gross rent in 2022 dollars",
    yscale = c(0, 1.2 * max(medrent$Rent)),
    pct = FALSE,
    colors = c(DCcolor.p2limegreen60, DCcolor.p1mediumblue),
    comparisonyear = "2004"
)

#### 26 - Year structure built, 201* housing units

yrbuiltforGraphic <- yrbuilt %>%
    select(
        contains("pct"),
        contains("SIG")
    ) %>%
    mutate(PlaceName = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S.")) %>%
    mutate(PlaceName.fac = factor(.$PlaceName, levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
    gather(-PlaceName, -PlaceName.fac, -contains("SIG"), key = variable, value = value) %>%
    mutate(
        description = NA,
        description = ifelse(variable == "orLater1990pct", "1990 or later", description),
        description = ifelse(variable == "mid1950to1989pct", "1950-1989", description),
        description = ifelse(variable == "orbefore1949pct", "1949 or earlier", description)
    ) %>%
    mutate(description.fac = factor(.$description, levels = c(
        "1990 or later",
        "1950-1989",
        "1949 or earlier"
    ))) %>%
    mutate(val = ifelse(as.numeric(value) < .01, ifelse((variable == "orLater1990pct" & orLater1990SIG == "no" & PlaceName != "U.S.") |
        (variable == "mid1950to1989pct" & mid1950to1989SIG == "no" & PlaceName != "U.S.") |
        (variable == "orbefore1949pct" & orbefore1949SIG == "no" & PlaceName != "U.S."), "<1%*", "<1%"),
    paste0(round.off(as.numeric(value) * 100), "%", ifelse((variable == "orLater1990pct" & orLater1990SIG == "no" & PlaceName != "U.S.") |
        (variable == "mid1950to1989pct" & mid1950to1989SIG == "no" & PlaceName != "U.S.") |
        (variable == "orbefore1949pct" & orbefore1949SIG == "no" & PlaceName != "U.S."), "*", ""))
    ))


chart.yrbuilt.allparishes <- yrbuiltforGraphic %>%
    ggplot(aes(PlaceName.fac, as.numeric(value), fill = description.fac, label = val)) +
    geom_bar(
        stat = "identity",
        position = "fill",
        size = .7,
        color = "gray50"
    ) +
    scale_fill_manual(values = c(
        DCcolor.p2teal,
        DCcolor.p1skyblue,
        DCcolor.p2orangered
    )) +
    geom_text(size = 3, position = position_stack(vjust = 0.6), family = "Asap") +
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0), limits = c(0, 1)) +
    themeDC_horizontal() +
    theme(
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        axis.text.x = element_text(size = 10, vjust = 1),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = .5)
    ) +
    labs(
        title = "Year structure built, 2022 housing units",
        x = "",
        y = ""
    )

#### 27 Means of transportation to work, workers 16 years and older

commuteforGraphic <- commute %>%
    select(
        placename,
        contains("pct"),
        contains("SIG")
    ) %>%
    mutate(PlaceNames = c("Orleans", "Jefferson", "Metro", "U.S.")) %>%
    mutate(PlaceName.fac = factor(.$PlaceNames, levels = c("Orleans", "Jefferson", "Metro", "U.S."))) %>%
    gather(key = variable, value = value, contains("pct")) %>%
    mutate(
        description = NA,
        description = ifelse(variable == "Drivepct" | variable == "Drivepct2000", "Drive Alone", description),
        description = ifelse(variable == "Carpoolpct" | variable == "Carpoolpct2000", "Carpool", description),
        description = ifelse(variable == "PublicTransitpct" | variable == "PublicTransitpct2000", "Public Transit", description),
        description = ifelse(variable == "bikepct" | variable == "bikepct2000", "Bike", description),
        description = ifelse(variable == "Walkpct" | variable == "Walkpct2000", "Walk", description),
        description = ifelse(variable == "Workhomepct" | variable == "Workhomepct2000", "Work at home", description),
        description = ifelse(variable == "Otherpct" | variable == "Otherpct2000", "Other", description)
    ) %>%
    mutate(
        year = NA,
        year = ifelse(grepl("pct", variable), 2022, year),
        year = ifelse(grepl("2000", variable), 2000, year)
    ) %>%
    mutate(description.fac = factor(.$description, levels = c(
        "Other",
        "Work at home",
        "Walk",
        "Bike",
        "Public Transit",
        "Carpool",
        "Drive Alone"
    ))) %>%
    mutate(year.fac = factor(.$year, levels = c(
        "2000",
        "2022"
    ))) %>%
    mutate(val = ifelse(value < .02, "",
        paste0(round.off(value * 100), "%", ifelse((DriveSIG == "no" & variable == "Drivepct") |
            (carpoolSIG == "no" & variable == "Carpoolpct") |
            (PublicTransitSIG == "no" & variable == "PublicTransitpct") |
            (bikeSIG == "no" & variable == "bikepct") |
            (walkSIG == "no" & variable == "walkpct") |
            (workhomeSIG == "no" & variable == "Workhomepct") |
            (otherSIG == "no" & variable == "Otherpct"), "*", ""))
    ))

chart.commute.allparishes <- commuteforGraphic %>%
    ggplot(aes(year.fac, as.numeric(value), fill = description.fac, label = val)) +
    geom_bar(
        stat = "identity",
        position = "fill",
        size = .7,
        color = "gray50"
    ) +
    facet_wrap(~PlaceName.fac, ncol = 5) +
    scale_fill_manual(values = c(
        DCcolor.p2yellow,
        DCcolor.p2limegreen,
        DCcolor.p2green,
        DCcolor.p2teal,
        DCcolor.p2blue,
        DCcolor.p1skyblue,
        DCcolor.p1lightskyblue
    )) +
    geom_text(size = 4, position = position_stack(vjust = 0.6), family = "Asap") +
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0), limits = c(0, 1)) +
    scale_x_discrete(labels = c(
        "2000",
        "2022"
    )) +
    themeDC_horizontal() +
    theme(
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        axis.text.x = element_text(size = 10, vjust = 1),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = .5)
    ) +
    labs(
        title = "Means of transportation to work, workers 16 years and older",
        x = "",
        y = ""
    )

##############################
# Jenna's expanded graphs
###############################

# across geos median hh income bar chart

medhh.raceGeos_chart <- medhh_with_stats %>%
    ggplot(aes(x = placename.fac, y = val, fill = var.fac)) +
    geom_bar(
        stat = "identity",
        position = position_dodge(),
        width = .6,
        color = "gray70"
    ) + # bar outlineas.factor
    geom_text(
        data = subset(medhh_with_stats, as.numeric(val) != 0), # leave out labels where data point doesn't exist (is placeheld with 0)
        aes(label = val_lab),
        position = position_dodge(width = .7),
        vjust = -1,
        size = 2.75,
        family = "Asap"
    ) +
    scale_y_continuous(labels = comma_format(accuracy = 1)) +
    scale_fill_manual(
        values = c(DCcolor.p1darkblue90, DCcolor.p2green90, DCcolor.p2violet90, DCcolor.p3yellowochre90),
        limits = levels(medhh_with_stats$var.fac)
    ) +
    themeDC_horizontal() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size = 16, hjust = .5)
    ) +
    labs(
        title = "Median household income by race/ethnicity, 2022",
        x = "",
        y = ""
    )
medhh.raceGeos_chart
ggsave(medhh.raceGeos_chart,
    filename = "indicator expansion drafts/graphics/medhh.raceGeos.png",
    width = 10, height = 6, units = "in"
)

# Historical median hh income line chart

medhh.hist_chart <- medhh.hist %>%
    filter(var != "All", Year != 2016) %>%
    ggplot() +
    geom_line(aes(x = Year, y = val, color = var.fac), size = 1) +
    scale_y_continuous(labels = dollar_format(accuracy = 1), limits = c(0, 99000), breaks = c(0, 30000, 60000, 90000)) +
    scale_x_continuous(labels = c("1979", "1989", "1999", "2010", "2022")) +
    scale_color_manual(values = c(DCcolor.p1darkblue, DCcolor.p2green, DCcolor.p3yellowochre)) +
    geom_text(data = subset(medhh.hist, Year %in% c("1979", "2022") & var != "All"), aes(x = Year, y = val, label = label_dollar(accuracy = 1)(val)), vjust = -1, family = "Asap") +
    geom_text(data = subset(medhh.hist, Year == "2022"), aes(x = Year, y = val, label = val_lab, hjust = -.7, vjust = .7), size = 6) +
    themeDC_horizontal() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size = 16, hjust = .5)
    ) +
    labs(
        title = "Median household income by race/ethnicity in 2022 dollars since 1979, Orleans Parish",
        x = "",
        y = ""
    )
medhh.hist_chart
ggsave(medhh.hist_chart,
    filename = "indicator expansion drafts/graphics/medhh.hist.png",
    width = 10, height = 6, units = "in"
)

# Across geos educational attainment bar chart

bach.raceGeos_chart <- bach_with_stats %>%
    ggplot(aes(x = placename.fac, y = val, fill = var.fac)) +
    geom_bar(
        stat = "identity",
        position = position_dodge(),
        width = .6,
        color = "gray70"
    ) + # bar outlineas.factor
    geom_text(
        data = subset(bach_with_stats, as.numeric(val) != 0), # leave out labels where data point doesn't exist (is placeheld with 0)
        aes(label = val_lab),
        position = position_dodge(width = .7),
        vjust = -.7,
        size = 2.75,
        family = "Asap"
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(
        values = c(DCcolor.p1darkblue90, DCcolor.p2green90, DCcolor.p2violet90, DCcolor.p3yellowochre90),
        limits = levels(bach_with_stats$var.fac)
    ) +
    themeDC_horizontal() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size = 16, hjust = .5)
    ) +
    labs(
        title = "Rate of bachelor's degree or higher, adults 25 years or older by race/ethnicity, 2022",
        x = "",
        y = ""
    )
ggsave(bach.raceGeos_chart,
    filename = "indicator expansion drafts/graphics/bach.raceGeos.png",
    width = 10, height = 6, units = "in"
)

# Historical educational attainment line chart

EduAtt.hist_chart <- EduAtt.hist %>%
    filter(var != "All", year != 2016) %>%
    ggplot() +
    geom_line(aes(x = year, y = val, color = var.fac), size = 1) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_x_continuous(labels = c("1980", "1990", "2000", "2010", "2022")) +
    scale_color_manual(values = c(DCcolor.p1darkblue, DCcolor.p2green, DCcolor.p3yellowochre)) +
    geom_text(data = subset(EduAtt.hist, year %in% c("1980", "2022") & var != "All"), aes(x = year, y = val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
    geom_text(data = subset(EduAtt.hist, year == "2022"), aes(x = year, y = val, label = val_lab, hjust = -.7, vjust = .7), size = 6) +
    themeDC_horizontal() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size = 16, hjust = .5)
    ) +
    labs(
        title = "Rate of bachelor's degree or higher, adults 25 years or older by race/ethnicity,\nOrleans Parish",
        x = "",
        y = ""
    )

ggsave(EduAtt.hist_chart,
    filename = "indicator expansion drafts/graphics/bach.hist.png",
    width = 8, height = 6, units = "in"
)

# across geos pov bar chart



pov.raceGeos_chart <- pov_with_stats %>%
    ggplot(aes(x = placename.fac, y = val, fill = var.fac)) +
    geom_bar(
        stat = "identity",
        position = position_dodge(),
        width = .6,
        color = "gray70"
    ) + # bar outlineas.factor
    geom_text(
        data = subset(pov_with_stats, as.numeric(val) != 0), # leave out labels where data point doesn't exist (is placeheld with 0)
        aes(label = val_lab),
        position = position_dodge(width = .7),
        vjust = -.7,
        size = 2.75,
        family = "Asap"
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(
        values = c(DCcolor.p1darkblue90, DCcolor.p2green90, DCcolor.p2violet90, DCcolor.p3yellowochre90),
        limits = levels(pov_with_stats$var.fac)
    ) +
    themeDC_horizontal() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size = 16, hjust = .5)
    ) +
    labs(
        title = "Poverty rate by race/ethnicity, 2022",
        x = "",
        y = ""
    )
ggsave(pov.raceGeos_chart,
    filename = "indicator expansion drafts/graphics/pov.raceGeos.png",
    width = 10, height = 6, units = "in"
)

# Historical total pov line chart

totalPov.hist_chart <- totalPov.hist %>%
    filter(var != "All", year != 2015) %>%
    ggplot() +
    geom_line(aes(x = year, y = val, color = var.fac), size = 1) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_color_manual(values = c(DCcolor.p1darkblue, DCcolor.p2green, DCcolor.p3yellowochre)) +
    scale_x_continuous(labels = c("1979", "1989", "1999", "2010", "2022")) +
    geom_text(data = subset(totalPov.hist, year %in% c("1979", "2022") & var != "All"), aes(x = year, y = val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
    geom_text(data = subset(totalPov.hist, year == "2022"), aes(x = year, y = val, label = val_lab, hjust = -.7, vjust = .7), size = 6) +
    themeDC_horizontal() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size = 16, hjust = .5)
    ) +
    labs(
        title = "Poverty rate by race/ethnicity since 1979, Orleans Parish",
        x = "",
        y = ""
    )

ggsave(totalPov.hist_chart,
    filename = "indicator expansion drafts/graphics/pov.hist.png",
    width = 8, height = 6, units = "in"
)



# child poverty

childpov.raceGeos_chart <- childpov_with_stats %>%
    ggplot(aes(x = placename.fac, y = val, fill = var.fac)) +
    geom_bar(
        stat = "identity",
        position = position_dodge(),
        width = .6,
        color = "gray70"
    ) + # bar outlineas.factor
    geom_text(
        data = subset(childpov_with_stats, as.numeric(val) != 0), # leave out labels where data point doesn't exist (is placeheld with 0)
        aes(label = val_lab),
        position = position_dodge(width = .7),
        vjust = -.7,
        size = 2.75,
        family = "Asap"
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(
        values = c(DCcolor.p1darkblue90, DCcolor.p2green90, DCcolor.p2violet90, DCcolor.p3yellowochre90),
        limits = levels(childpov_with_stats$var.fac)
    ) +
    themeDC_horizontal() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size = 16, hjust = .5)
    ) +
    labs(
        title = "Child poverty rate by race/ethnicity, 2022",
        x = "",
        y = ""
    )
ggsave(childpov.raceGeos_chart,
    filename = "indicator expansion drafts/graphics/childpov.raceGeos.png",
    width = 10, height = 6, units = "in"
)

# Historical child pov line chart

childPov.hist_chart <- childPov.hist %>%
    filter(var != "All", Year != 2015) %>%
    ggplot() +
    geom_line(aes(x = Year, y = val, color = var.fac), size = 1) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_color_manual(values = c(DCcolor.p1darkblue, DCcolor.p2green, DCcolor.p3yellowochre)) +
    scale_x_continuous(labels = c("1979", "1989", "1999", "2010", "2022")) +
    geom_text(data = subset(childPov.hist, Year %in% c("1980", "2022") & var != "All"), aes(x = Year, y = val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
    geom_text(data = subset(childPov.hist, Year == "2022"), aes(x = Year, y = val, label = val_lab, hjust = -.7, vjust = .7), size = 6) +
    themeDC_horizontal() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size = 16, hjust = .5)
    ) +
    labs(
        title = "Child poverty rate by race/ethnicity since 1979, Orleans Parish",
        x = "",
        y = ""
    )

ggsave(childPov.hist_chart,
    filename = "indicator expansion drafts/graphics/childpov.hist.png",
    width = 8, height = 6, units = "in"
)

# Homeownership


ho.raceGeos_chart <- ho_with_stats %>%
    ggplot(aes(x = placename.fac, y = val, fill = var.fac)) +
    geom_bar(
        stat = "identity",
        position = position_dodge(),
        width = .6,
        color = "gray70"
    ) + # bar outlineas.factor
    geom_text(
        data = subset(ho_with_stats, as.numeric(val) != 0), # leave out labels where data point doesn't exist (is placeheld with 0)
        aes(label = val_lab),
        position = position_dodge(width = .7),
        vjust = -.7,
        size = 2.75,
        family = "Asap"
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(
        values = c(DCcolor.p1darkblue90, DCcolor.p2green90, DCcolor.p2violet90, DCcolor.p3yellowochre90),
        limits = levels(ho_with_stats$var.fac)
    ) +
    themeDC_horizontal() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size = 16, hjust = .5)
    ) +
    labs(
        title = "Homeownership rate by race/ethnicity, 2022",
        x = "",
        y = ""
    )
ggsave(ho.raceGeos_chart,
    filename = "indicator expansion drafts/graphics/homeownership.raceGeos.png",
    width = 10, height = 6, units = "in"
)


# Historical child homeownership line chart

homeownership.hist_chart <- homeownership.hist %>%
    filter(var != "All", Year != 2016) %>%
    filter(val != 0) %>%
    ggplot() +
    geom_line(aes(x = Year, y = val, color = var.fac), size = 1) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(.2, .7)) +
    scale_x_continuous(labels = c("1970", "1980", "1990", "2000", "2010", "2022")) +
    scale_color_manual(values = c(DCcolor.p1darkblue, DCcolor.p2green, DCcolor.p3yellowochre)) +
    geom_text(data = subset(homeownership.hist, Year %in% c("1970", "2022") & var != "All"), aes(x = Year, y = val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
    geom_text(data = subset(homeownership.hist, Year == "1980" & var == "Hispanic,\nany race"), aes(x = Year, y = val, label = percent_format(accuracy = 1)(val)), vjust = -1, family = "Asap") +
    geom_text(data = subset(homeownership.hist, Year == "2022"), aes(x = year, y = val, label = val_lab, hjust = -.7, vjust = .7), size = 6) +
    themeDC_horizontal() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(size = 16, hjust = .5)
    ) +
    labs(
        title = "Homeownership rate by race/ethnicity since 1970, Orleans Parish",
        x = "",
        y = ""
    )
homeownership.hist_chart
ggsave(homeownership.hist_chart,
    filename = "indicator expansion drafts/graphics/homeownership.hist.png",
    width = 8, height = 6, units = "in"
)

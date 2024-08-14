source(here("inputs/datacenter_colors.R"))

############################################
# # FOREIGN-BORN POP # #
############################################

#### 19 - Population not U.S. citizens at birth

forborGraphic <- dodgedBar(forbor, quo(forborpct), "Population not U.S. citizens at birth", yscale = c(0, .18), digits = 0)

#### 20 - Population who moved in the past year

mobforGraphic <- mob %>%
    select(
        placename,
        contains("SIG"),
        contains("pct"),
        contains("2004")
    ) %>%
    mutate(PlaceNames = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S.")) %>%
    mutate(PlaceName.fac = factor(.$PlaceNames, levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
    select(-samehousepct, -sf2004samehouse) %>%
    gather(key = variable, value = value, contains("pct"), (contains("2004") & !contains("MOE"))) %>%
    mutate(
        description = NA,
        description = ifelse(variable == "mobabroadpct" | variable == "sf2004mobabroad", "Moved from abroad", description),
        description = ifelse(variable == "mobStatespct" | variable == "sf2004states", "Moved from out of state", description),
        description = ifelse(variable == "difparishpct" | variable == "sf2004difparish", "Moved from different parish in state", description),
        description = ifelse(variable == "withinparishpct" | variable == "sf2004withinparish", "Moved within same parish", description)
    ) %>%
    mutate(
        year = NA,
        year = ifelse(variable == "mobabroadpct" | variable == "mobStatespct" | variable == "difparishpct" | variable == "withinparishpct", 2018, year),
        year = ifelse(variable == "sf2004mobabroad" | variable == "sf2004states" | variable == "sf2004difparish" | variable == "sf2004withinparish", 2004, year)
    ) %>%
    mutate(description.fac = factor(.$description, levels = c(
        "Moved from abroad",
        "Moved from out of state",
        "Moved from different parish in state",
        "Moved within same parish"
    ))) %>%
    mutate(year.fac = factor(.$year, levels = c(
        "2004",
        "2018"
    ))) %>%
    mutate(val = ifelse(value < .01, ifelse((abroadSIG == "no" & variable == "mobabroadpct") |
        (statesSIG == "no" & variable == "mobStatespct") |
        (difparishSIG == "no" & variable == "difparishpct") |
        (withinparishSIG == "no" & variable == "withinparishpct"), "<1%*", "<1%"),
    paste0(round.off(value * 100), "%", ifelse((abroadSIG == "no" & variable == "mobabroadpct") |
        (statesSIG == "no" & variable == "mobStatespct") |
        (difparishSIG == "no" & variable == "difparishpct") |
        (withinparishSIG == "no" & variable == "withinparishpct"), "*", ""))
    ))

chart.mob.allparishes <- mobforGraphic %>%
    ggplot(aes(year.fac, as.numeric(value), fill = description.fac, label = val)) +
    geom_bar(
        stat = "identity",
        position = "stack",
        color = "gray50"
    ) +
    facet_wrap(~PlaceName.fac, ncol = 5) +
    scale_fill_manual(values = c(
        DCcolor.p1lightskyblue,
        DCcolor.p1skyblue,
        DCcolor.p2teal,
        DCcolor.p2blue90
    )) +
    geom_text(
        data = subset(
            mobforGraphic,
            as.numeric(value) != 0
        ),
        size = 4,
        position = position_stack(vjust = 0.6),
        family = "Asap"
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0), limits = c(0, .18)) +
    # TODO: Ask Haleigh about the label for date (change soon and transfer changes to Databricks)
    scale_x_discrete(labels = c("2004", "2022")) +
    themeDC_horizontal() +
    theme(
        plot.title = element_text(hjust = .5, size = 18),
        strip.text = element_text(size = 12),
        legend.position = "right",
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        axis.text.x = element_text(size = 12, vjust = 1),
        axis.text.y = element_text(size = 12),
        legend.title = element_blank()
    ) +
    labs(
        title = "Population who moved in the past year",
        x = "",
        y = ""
    )

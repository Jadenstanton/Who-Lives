source(here("inputs/datacenter_colors.R"))

############################################
# # EDUCATIONAL ATTAINMENT, INCOME, AND INTERNET ACCESS # #
############################################

#### 12 - Less than a high school degree, adults 25 and older

hsGraphic <- dodgedBar(hs,
    quo(pctless),
    "Less than a high school degree, adults 25 and older",
    yscale = c(0, .35)
)

#### 13 - Bachelor's degree or higher, adults 25 and older

bachGraphic <- dodgedBar(
    bach,
    quo(pctbach),
    "Rate of bachelor's degree or higher, adults 25 and older"
)

#### 14 - Median household income, 2016 inflation-adjusted dollars

medhhGraphic <- dodgedBar(medhh,
    quo(MedianHHIncome),
    "Median household income in 2022 dollars",
    yscale = c(0, 1.3 * max(medhh$MedianHHIncome)),
    colors = c(DCcolor.p2teal50, DCcolor.p1mediumblue),
    pct = FALSE,
    comparisonyear = "1999"
)


dataGraphic <- medhh %>%
    select(-contains("moeprop")) %>% # dplyr rejects the format of moeprop, so we drop it  mutate(placenames = NA,
    mutate(
        placenames = NA,
        placenames = ifelse(place == "103", "St. Tammany", placenames),
        placenames = ifelse(place == "051", "Jefferson", placenames),
        placenames = ifelse(place == "071", "Orleans", placenames),
        placenames = ifelse(place == "35380", "Metro", placenames),
        placenames = ifelse(place == "1", "U.S.", placenames)
    ) %>%
    mutate(place.fac = factor(.$placenames, levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>% # vars of type "factor" allow you to control order
    select(one_of("census2000", "sf2004", "sf1999"), !!quo(MedianHHIncome), placenames, place.fac, significant) %>% # one_of() chooses correct comparison vals/!! is the second part or the quo() tool
    gather(-placenames, -place.fac, -significant, key = variable, value = value) %>%
    mutate(description = as.factor(ifelse(variable == "census2000" | variable == "sf2004" | variable == "sf1999", 1999, year))) %>% # creates legend info
    mutate(valp = ifelse(value < .01, ifelse(significant == "no" & description == year, "<1%*", "<1%"), # creates pct labels
        paste0(round.off(value * 100, digits = 0), "%", ifelse((significant == "no" & description == year), "*", ""))
    )) %>%
    mutate(vald = ifelse((significant == "no" & description == year), # creates dollar labels
        paste0(dollar(value, largest_with_cents = 1), "*"),
        dollar(value, largest_with_cents = 1)
    ))

medhhGraphic <- dataGraphic %>%
    ggplot(aes(place.fac, value, fill = description)) +
    geom_bar(
        stat = "identity",
        position = position_dodge(),
        width = .8,
        color = "gray50"
    ) + # bar outline
    geom_text(
        data = subset(dataGraphic, as.numeric(value) != 0), # leave out labels where data point doesn't exist (is placeheld with 0)
        aes(label = vald),
        # position = position_dodge(width=ifelse(data$vald== "56,438",1.1,.7)),
        position = position_dodge(width = .7), # change width accordingly
        vjust = -.7,
        size = 2.75,
        family = "Asap"
    ) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 1.3 * max(medhh$MedianHHIncome))) +
    scale_fill_manual(values = c(DCcolor.p2teal50, DCcolor.p1mediumblue)) +
    themeDC_horizontal() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
        plot.title = element_text(hjust = .5, size = 16)
    ) +
    labs(
        title = "Median household income in 2022 dollars",
        x = "",
        y = ""
    )


#### 15 - Internet Access

intaforGraphic <- inta %>%
    select(
        place,
        contains("pct"),
        contains("SIG")
    ) %>%
    mutate(PlaceNames = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S.")) %>%
    mutate(PlaceName.fac = factor(.$PlaceNames, levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
    gather(key = variable, value = value, contains("pct")) %>%
    mutate(
        description = NA,
        description = ifelse(variable == "broadbandpct", "Broadband and all other", description),
        description = ifelse(variable == "noaccpct", "No Internet access", description),
        description = ifelse(variable == "cellonlypct", "Cellular only", description),
        description = ifelse(variable == "nosubpct", "Access with no subscription", description)
    ) %>%
    mutate(description.fac = factor(.$description, levels = c(
        "Access with no subscription",
        "Cellular only",
        "No Internet access",
        "Broadband and all other"
    ))) %>%
    mutate(val = ifelse(value < .01, ifelse((variable == "broadbandpct" & broadbandSIG == "no" & PlaceNames != "U.S.") |
        (variable == "noaccpct" & noaccSIG == "no" & PlaceNames != "U.S.") |
        (variable == "cellonlypct" & cellonlySIG == "no" & PlaceNames != "U.S.") |
        (variable == "nosubpct" & nosubSIG == "no" & PlaceNames != "U.S."), "<1%*", ""),
    paste0(round.off(value * 100), "%", ifelse((variable == "broadbandpct" & broadbandSIG == "no" & PlaceNames != "U.S.") |
        (variable == "noaccpct" & noaccSIG == "no" & PlaceNames != "U.S.") |
        (variable == "cellonlypct" & cellonlySIG == "no" & PlaceNames != "U.S.") |
        (variable == "nosubpct" & nosubSIG == "no" & PlaceNames != "U.S."), "*", ""))
    ))


#<1%",paste0(round.off(value*100),"%")))

chart.inta.allparishes <- intaforGraphic %>%
    ggplot(aes(PlaceName.fac, as.numeric(value), fill = description.fac, label = val)) +
    geom_bar(stat = "identity", position = "fill", color = "gray50") +
    scale_fill_manual(values = c(
        DCcolor.p2orangered,
        DCcolor.p2yellow,
        DCcolor.p2violet,
        DCcolor.p1skyblue
    )) +
    geom_text(size = 3, position = position_stack(vjust = 0.6), family = "Asap") +
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0), limits = c(0, 1)) +
    themeDC_horizontal() +
    theme(
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 10),
        plot.title = element_text(hjust = .5)
    ) +
    labs(
        # TODO: Ask Haleigh about year here
        title = "Household internet access, 2022",
        x = "",
        y = ""
    )

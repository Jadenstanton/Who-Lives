############################################
# # GRAPHICS # #
############################################

## Robby's Data Center graph themes
themeDC_horizontal <- function() {
    theme_light() +
        theme(
            text = element_text(family = "Asap"), # Change to Asap if necessary
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "gray90"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            plot.caption = element_text(hjust = 0),
            strip.text = element_text(color = "grey20", face = "bold"),
            strip.background = element_blank()
        )
}

themeDC_vertical <- function() {
    theme_light() +
        theme(
            text = element_text(family = "Asap"), # Change to Asap if necessary
            panel.grid.major.x = element_line(color = "gray90"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            plot.caption = element_text(hjust = 0),
            strip.text = element_text(color = "grey20", face = "bold"),
            strip.background = element_blank()
        )
}

## create dodged bar graphs to compare two diff years
## input: info created during api data pull
## output: bar chart
dodgedBar <- function(data,
                      stattograph, # variable name of current yar pct, must be in quo() for dplyr to be able to use it
                      title,
                      colors = c(DCcolor.p1skyblue, DCcolor.p1mediumblue),
                      yscale = c(0, .45),
                      pct = TRUE, # used when formatting pct vals vs dollar vals
                      comparisonyear = "2000",
                      # FIXME
                      year = "2022",
                      digits = 0,
                      lab_pos = position_dodge(width = .7)) { # for rounding, specifically for forbor
    dataGraphic <- data %>%
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
        select(one_of("census2000", "sf2004", "sf1999", "Ownerpct2000"), !!stattograph, placenames, place.fac, significant) %>% # one_of() chooses correct comparison vals/!! is the second part or the quo() tool
        gather(-placenames, -place.fac, -significant, key = variable, value = value) %>%
        mutate(description = as.factor(ifelse(variable == "census2000" | variable == "sf2004" | variable == "sf1999" | variable == "Ownerpct2000", comparisonyear, YEAR))) %>% # creates legend info
        mutate(valp = case_when(
            value == 0 ~ "  ",
            value < .01 & significant == "no" ~ "<1%*",
            value < .01 & significant == "yes" ~ "<1%",
            value > .01 & significant == "yes" ~ paste0(round(value * 100, digits = digits), "%"),
            value > .01 & significant == "no" ~ paste0(round(value * 100, digits = digits), "%*")
        )) %>%
        mutate(vald = case_when(
            value == 0 ~ "   ",
            significant == "no" ~ paste0(dollar(value, largest_with_cents = 1), "*"),
            significant == "yes" ~ dollar(value, largest_with_cents = 1)
        ))

    chart <- dataGraphic %>%
        ggplot(aes(place.fac, value, fill = description)) +
        geom_bar(
            stat = "identity",
            position = position_dodge(),
            width = .7,
            color = "gray50"
        ) + # bar outline
        geom_text( # data = subset(dataGraphic, as.numeric(value) != 0),     #leave out labels where data point doesn't exist (is placeheld with 0)
            data = dataGraphic,
            aes(label = ifelse(rep(pct, sum(dataGraphic$value >= 0)),
                valp,
                vald
            )),
            position = lab_pos,
            vjust = -.7,
            size = 2.75,
            family = "Asap"
        ) +
        scale_y_continuous(labels = ifelse(pct == TRUE, percent_format(accuracy = 1), comma_format(accuracy = 1)), expand = c(0, 0), limits = yscale) +
        scale_fill_manual(values = colors) +
        themeDC_horizontal() +
        theme(
            legend.title = element_blank(),
            legend.text = element_text(margin = margin(t = 2, l = 4, b = 6, unit = "pt"), size = 12),
            plot.title = element_text(hjust = .5, size = 16)
        ) +
        labs(
            title = title,
            x = "",
            y = ""
        )
    return(chart)
}


### for stat testing notes under new charts ###

### for he by race/by geography bar charts
raceList <- function(data) {
    data %>%
        mutate(
            insigList = "",
            insigList = ifelse(sig_wht_blk == "no", "White and Black", insigList),
            insigList = ifelse(sig_wht_asian == "no", paste0(insigList, "White and Asian", sep = ", "), insigList),
            insigList = ifelse(sig_wht_hisp == "no", paste0(insigList, "White and Hispanic", sep = ", "), insigList),
            insigList = ifelse(sig_blk_hisp == "no", paste0(insigList, "Black and Hispanic", sep = ", "), insigList),
            insigList = ifelse(sig_blk_asian == "no", paste0(insigList, "Black and Asian", sep = ", "), insigList),
            insigList = ifelse(sig_hisp_asian == "no", paste0(insigList, "Hispanic and Asian", sep = ", "), insigList),
            insigList = str_sub(insigList, 1, -3)
        ) %>%
        mutate(placename = ifelse(placename %in% c("Orleans", "Jefferson", "St. Tammany"), paste0(placename, " Parish"), placename)) %>%
        filter(insigList != "") %>%
        mutate(note = paste0("&#8224; = In ", placename, ", the difference between ", insigList, " is not statistically significant.")) %>%
        select(placename, note) %>%
        pivot_wider(names_from = "placename", values_from = "note") %>%
        unite("note", 1:dim(.)[2]) %>%
        as.data.frame()
}

round.off <- function(x, digits = 0) {
    posneg <- sign(x)
    z <- trunc(abs(x) * 10^(digits + 1)) / 10
    z <- floor(z * posneg + 0.5) / 10^digits
    return(z)
}

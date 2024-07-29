# Variable Declaration
readRenviron(".env.test")
CENSUS_KEY <- Sys.getenv("TEST_CENSUS_KEY")

############################################
# # PULLING DATA # #
############################################

## pull parish, metro, and US numbers with census api
## input: census api variable names, human-readable names, and vintage
## output: dataframe in same format as Who Lives data tables excel sheet

wholivesdatapull <- function(variables, names = variables, year = 2022, censusname = "acs/acs1") {
  censuskey <- CENSUS_KEY
  parishes <- getCensus(name = censusname, vintage = year, key = censuskey, vars = variables, region = "county:071,051,103,093", regionin = "state:22") ## pull parish data
  parishes$state <- NULL # state column pulled automatically & needs to be deleted
  colnames(parishes) <- c("place", names) # so names match between the three pulls for rbind
  metro <- getCensus(name = censusname, vintage = year, key = censuskey, vars = variables, region = ifelse(year == 2000, "consolidated metropolitan statistical area:5560", "metropolitan statistical area/micropolitan statistical area:35380"))
  colnames(metro) <- c("place", names)
  us <- getCensus(name = censusname, vintage = year, key = censuskey, vars = variables, region = "us:1")
  colnames(us) <- c("place", names)
  df <- switch(rbind(parishes, metro, us)
  )
  df[df == -555555555] <- 0

  df <- df %>%
    mutate(placename = case_when(
      place == "051" ~ "Jefferson",
      place == "071" ~ "Orleans",
      place == "103" ~ "St. Tammany",
      place == "35380" ~ "New Orleans Metro Area",
      place == "5560" ~ "New Orleans Metro Area",
      place == "1" ~ "United States"
    )) %>%
    filter(place != "093")
  return(df) # combine the three pulls, rows 1 & 2 (Jeff & Orl) switched
}

# Function to create the date and population estimate text based on the year
create_date <- function(year, start_year) {
  if (year == 1) {
    return(paste0("4/1/", start_year, " population estimates base"))
  } else {
    return(paste0("7/1/", start_year + year - 1, " population estimate"))
  }
}
start_year <- 2020

# warehouse who lives datapull - make sure WhoLives.csv in datalake is updated, and run the datalake-connection.R first

# wholivesdatapull <- function(variables, names = variables, dataframe = df, year = 2021){
#   df <- df %>% select(-c(row_num, variable_name)) %>% filter(vintage == year, key %in% variables)
#   df <- df %>% pivot_wider(names_from = key, values_from = value, values_fn = as.numeric)
#   df <-  df %>% select(geo_name, county_fips, variables)
#   df$county_fips[df$geo_name == "United States"] <- 1
#   df$county_fips[df$geo_name == "New Orleans-Metairie, LA Metro Area"] <- 35380 #doing this with case_when was giving me trouble
#   colnames(df) <- c("geo_name", "place", names)
#   df[df == -555555555] <- 0
#   df <- df %>% select(-geo_name)
#   df <- df %>% mutate(placename = (case_when(place == "051" ~ "Jefferson",
#                                         place == "071" ~ "Orleans",
#                                         place == "103" ~ "St. Tammany",
#                                         place == "35380" ~ "New Orleans Metro Area",
#                                         place == "1" ~ "United States"))) %>%
#     filter(place %in% c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")) %>%
#     mutate(place = factor(place, levels = c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States"))) %>% arrange(place)
#   return(df)
# }




########## Define function to pull variables

# Pull data. Note that this includes 2010-2019.
pullDataPEP <- function(variables, api, year, counties, metro) {
  parish <- getCensus(
    name = api,
    vintage = 2019,
    key = CENSUS_KEY,
    vars = variables,
    region = counties,
    regionin = "state:22"
  )

  state <- getCensus(
    name = api,
    vintage = 2019,
    key = CENSUS_KEY,
    vars = variables,
    region = "state:22"
  )

  usa <- getCensus(
    name = api,
    vintage = 2019,
    key = CENSUS_KEY,
    vars = variables,
    region = "us:1"
  )

  df <- parish %>%
    bind_rows(state) %>%
    bind_rows(usa) # Bind rows for counties, metro, state, usa

  rm(parish, state, usa) # remove large objects from environment

  df <- df %>%
    left_join(ageGroupCode) %>% # join verbose codes
    left_join(raceCode) %>%
    left_join(sexCode) %>%
    left_join(hispCode) %>%
    mutate(place = GEONAME) %>%
    mutate(
      POP = as.numeric(POP),
      place = GEONAME,
      place = ifelse(!is.na(county),
        str_sub(GEONAME, 1, nchar(GEONAME) - 18),
        GEONAME
      )
    )

  df <- df %>%
    select(place, DATE_DESC, hispCodeName, sexCodeName, raceCodeName, ageGroupCodeName, POP) %>%
    rename(
      hisp = hispCodeName,
      sex = sexCodeName,
      race = raceCodeName,
      age = ageGroupCodeName,
      population = POP,
      date = DATE_DESC
    ) %>%
    filter(race %in% c(
      "Total",
      "White alone",
      "Black or African American alone",
      "Asian alone"
    )) %>%
    mutate(
      raceSimple = NA, # make variable base on race alone that matches Who Lives races.
      raceSimple = ifelse(race == "Total" & hisp == "Total", "Total", raceSimple),
      raceSimple = ifelse(race == "White alone" & hisp == "Not Hispanic", "White", raceSimple),
      raceSimple = ifelse(race == "Black or African American alone" & hisp == "Not Hispanic", "Black", raceSimple),
      raceSimple = ifelse(race == "Asian alone" & hisp == "Not Hispanic", "Asian", raceSimple),
      raceSimple = ifelse(race == "Total" & hisp == "Hispanic", "Hispanic", raceSimple)
    ) %>%
    filter(!is.na(raceSimple)) # Filter out other races

  return(df)
}


############################################
# # ANALYSIS # #
############################################

## this function is used in "wholivesdatapull" to correctly order rows: Orl,Jeff,St.T/071,051,103
## input: dataframe (from censusapi pull)
## output: dataframe with rows 1 & 2 switched
switch <- function(dataframe) {
  dataframe2 <- dataframe[2, ] # extract row 2 (Orleans)
  dataframe <- rbind(dataframe2, dataframe[-2, ]) # move row 2 to be row 1
}

## calculates MOE for 2000 STF3 files.
## Formula on pg954 of documentation, table A "Unadjusted Standard Error for Estimated Totals"
## This is only for estimate totals.  Median and pcts will have to be done differently!!!***
## N = population
## Design factor table not found, so until we multiply by design factor, it's unadjusted std error.
## critical value for ACS is z = 1.645 (they use 90% CI)
moe2000 <- function(est, n, designfac = 1) {
  se_unadj <- sqrt(5 * est * (1 - (est / n)))
  se <- se_unadj * designfac # when designfac = 1 it produces the unadjusted standard errors
  MOE <- se * 1.645
  return(MOE)
}


## calculates MOE for aggregated estimates
## moe = sqrt(sum(estimateMOE^2))
## input: dataframe of estimates' MOEs (i.e. use cbind)
## output: column of MOEs
moeagg <- function(estimateMOE) {
  squares <- matrix(0, dim(estimateMOE)[1], dim(estimateMOE)[2])
  for (i in 1:dim(estimateMOE)[2]) {
    squares[, i] <- t(estimateMOE[, i] * estimateMOE[, i])
  }
  sumsquares <- apply(squares, 1, sum)
  return(sqrt(sumsquares))
}

## calculates MOE for proportions
## p = x/y
## moe = sqrt(moex^2 - p^2 * moey^2)/y
## input: columns of measures y, moex, moey, p
## output: column of MOEs
moeprop <- function(y, moex, moey, p) {
  mp <- matrix(0, length(y))
  for (i in 1:length(y)) {
    if ((moex[i] * moex[i] - p[i] * p[i] * moey[i] * moey[i]) < 0) {
      mp[i] <- (sqrt(moex[i] * moex[i] + p[i] * p[i] * moey[i] * moey[i])) / y[i]
    } else {
      mp[i] <- (sqrt(moex[i] * moex[i] - p[i] * p[i] * moey[i] * moey[i])) / y[i]
    }
  }
  return(mp)
}

## stat testing for 2000 vs 201* data
## input: columns of estimates and their MOEs (zeros for Census 2000)
## output: column of yes or no if significant
stattest <- function(x, moex = matrix(0, length(x)), y, moey, zscore = 1.96) {
  significant <- matrix(0, length(x))
  v <- abs((x - y) / sqrt((moex / zscore)^2 + (moey / zscore)^2))
  significant <- ifelse(v > zscore, "yes", "no")
  return(as.list(significant))
}



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
    mutate(description = as.factor(ifelse(variable == "census2000" | variable == "sf2004" | variable == "sf1999" | variable == "Ownerpct2000", comparisonyear, year))) %>% # creates legend info
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

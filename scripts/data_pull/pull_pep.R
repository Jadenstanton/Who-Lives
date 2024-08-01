############################################
# # PEP # #
############################################

#### Detailed age groups, race, sex, and hispanic origin from popest


# File layout:
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2017/sc-est2017-alldata6.pdf
# Unable to find full documentation for race codes.
# Age codes vary for detailed race, sex, and hispanic origin.
# Age and race required manual checking with American Factfinder. Please double check again!

# Age codes for joining later
ageGroupCodeName <-
    c(
        "Total",
        "Under 5 years",
        "5 to 9",
        "10 to 14",
        "15 to 19",
        "20 to 24",
        "25 to 29",
        "30 to 34",
        "35 to 39",
        "40 to 44",
        "45 to 49",
        "50 to 54",
        "55 to 59",
        "60 to 64",
        "65 to 69",
        "70 to 74",
        "75 to 79",
        "80 to 84",
        "85 plus",
        "Under 18 years",
        "5 to 13 years",
        "14 to 17 years",
        "18 to 64 years",
        "18 to 24 years",
        "25 to 44 years",
        "45 to 64 years",
        "65 years and over",
        "85 years and over",
        "16 years and over",
        "18 years and over",
        "15 to 44 years",
        "Median age (years)"
    )
ageGroupCode <- data.frame(AGEGROUP = as.character(0:31), ageGroupCodeName)

# Race codes for joining later
# See PEPSR6H and PEPSR5H on Am FactFinder, verified match for Orleans Parish
raceCodeName <-
    c(
        "Total",
        "White alone",
        "Black or African American alone",
        "American Indian and Alaska Native alone",
        "Asian alone",
        "Native Hawaiian and Other Pacific Islander alone",
        "Two or more races",
        "White combo",
        "Black or African American combo",
        "American Indian and Alaska Native combo",
        "Asian combo",
        "Native Hawaiian and Other Pacific Islander combo"
    )
raceCode <- data.frame(RACE = as.character(0:11), raceCodeName)

# Sex and hispanic origin for joining later
sexCodeName <- c("Total", "Male", "Female")
sexCode <- data.frame(SEX = as.character(0:2), sexCodeName)

hispCodeName <- c("Total", "Not Hispanic", "Hispanic")
hispCode <- data.frame(HISP = as.character(0:2), hispCodeName)


######### Define your geographies

mycounties <- "county:071,051,075,087,089,093,095,103"
mystate <- "state:22"

######### Define your variables

# Use API to generate the entire list of variables
charagegroupsVars <- censusapi::listCensusMetadata("pep/charagegroups", 2019, type = "variables")$name

######### Pull data

# allparishesRaw <- pullDataPEP(charagegroupsVars,
#                            api = "pep/charagegroups",
#                            year = 2019,
#                            counties = mycounties)
# save(allparishesRaw, file = "inputs/allparishesRaw.Rdata")

### post-2019 PEP data not available by API. pulling it in directly.

allparishesRaw <- read_csv(paste0("inputs/PEP_data/PEP", as.numeric(format(Sys.Date(), "%Y")) - 1, "charagegroups.csv"))
allparishesRaw <- allparishesRaw %>%
    filter(COUNTY %in% c("071", "051", "075", "087", "089", "093", "095", "103")) %>% # making a total column for each sex.  revise unneeded ones later
    mutate(
        NHWA_Total = NHWA_MALE + NHWA_FEMALE,
        NHBA_Total = NHBA_MALE + NHBA_FEMALE,
        NHAA_Total = NHAA_MALE + NHAA_FEMALE,
        H_Total = H_MALE + H_FEMALE,
        HWA_Total = HWA_MALE + HWA_FEMALE,
        HBA_Total = HBA_MALE + HBA_FEMALE,
        HAA_Total = HAA_MALE + HAA_FEMALE
    ) %>%
    pivot_longer(cols = TOT_POP:HAA_Total, names_sep = "_", names_to = c("race", "sex")) %>%
    mutate(
        place = CTYNAME,
        age = case_when(
            AGEGRP == 0 ~ "Total",
            AGEGRP == 1 ~ "Under 5 years",
            AGEGRP == 2 ~ "5 to 9",
            AGEGRP == 3 ~ "10 to 14",
            AGEGRP == 4 ~ "15 to 19",
            AGEGRP == 5 ~ "20 to 24",
            AGEGRP == 6 ~ "25 to 29",
            AGEGRP == 7 ~ "30 to 34",
            AGEGRP == 8 ~ "35 to 39",
            AGEGRP == 9 ~ "40 to 44",
            AGEGRP == 10 ~ "45 to 49",
            AGEGRP == 11 ~ "50 to 54",
            AGEGRP == 12 ~ "55 to 59",
            AGEGRP == 13 ~ "60 to 64",
            AGEGRP == 14 ~ "65 to 69",
            AGEGRP == 15 ~ "70 to 74",
            AGEGRP == 16 ~ "75 to 79",
            AGEGRP == 17 ~ "80 to 84",
            AGEGRP == 18 ~ "85 plus"
        ),
        date = sapply(years_since_start, create_date, start_year = start_year),
        sex = case_when(
            sex == "POP" | sex == "Total" ~ "Total",
            sex == "MALE" ~ "Male",
            sex == "FEMALE" ~ "Female"
        ),
        population = value
    ) %>%
    filter(sex == "Total") %>%
    select(place, date, sex, race, age, population)

allparishesRaw <- allparishesRaw %>% # for some reason this is working only when I keep it separate
    mutate(hisp = case_when(
        substr(allparishesRaw$race, 1, 1) == "H" ~ "Hispanic",
        allparishesRaw$race == "TOT" ~ "Total",
        substr(allparishesRaw$race, 1, 1) != "H" ~ "Not Hispanic"
    ))

allparishesRaw <- allparishesRaw %>%
    filter(race %in% c("TOT", "NHWA", "NHBA", "NHAA", "H")) %>%
    mutate(
        race = case_when(
            race == "TOT" ~ "Total",
            race == "NHWA" ~ "White alone",
            race == "NHBA" ~ "Black or African American alone",
            race == "NHAA" ~ "Asian alone",
            race == "H" ~ "Hispanic"
        ),
        raceSimple = case_when(
            race == "Total" & hisp == "Total" & sex == "Total" ~ "Total",
            race == "White alone" & hisp == "Not Hispanic" & sex == "Total" ~ "White",
            race == "Black or African American alone" & hisp == "Not Hispanic" & sex == "Total" ~ "Black",
            race == "Asian alone" & hisp == "Not Hispanic" & sex == "Total" ~ "Asian",
            hisp == "Hispanic" & sex == "Total" ~ "Hispanic"
        )
    ) %>%
    select(place, date, hisp, sex, race, age, population, raceSimple)

popunder18co <- read_csv(paste0("inputs/PEP_data/PEP", as.numeric(format(Sys.Date(), "%Y")) - 1, "_agesex.csv")) # for popunder18 measure
popunder18co <- popunder18co %>%
    filter(COUNTY %in% c("071", "051", "075", "087", "089", "093", "095", "103")) %>%
    select(CTYNAME, YEAR, AGE18PLUS_TOT) %>%
    mutate(
        place = CTYNAME,
        date = sapply(years_since_start, create_date, start_year = start_year),
        age = "18 years and over",
        race = "Total",
        raceSimple = "Total",
        sex = "Total",
        population = AGE18PLUS_TOT
    )

allparishesRaw <- allparishesRaw %>%
    full_join(popunder18co, by = c("place", "date", "age", "sex", "race", "raceSimple", "population")) %>%
    select(place, date, hisp, sex, race, age, population, raceSimple)
# pulling in entire US PEP data, then binding to allparishesRaw.  Doing this with the exact same code as from above.

allstates_pep <- read_csv(paste0("inputs/PEP_data/PEP", as.numeric(format(Sys.Date(), "%Y")) - 1, "charagegroups_allstates.csv"))

allstates_pep <- allstates_pep %>%
    mutate(
        NHWA_Total = NHWA_MALE + NHWA_FEMALE,
        NHBA_Total = NHBA_MALE + NHBA_FEMALE,
        NHAA_Total = NHAA_MALE + NHAA_FEMALE,
        H_Total = H_MALE + H_FEMALE,
        HWA_Total = HWA_MALE + HWA_FEMALE,
        HBA_Total = HBA_MALE + HBA_FEMALE,
        HAA_Total = HAA_MALE + HAA_FEMALE
    ) %>%
    pivot_longer(cols = TOT_POP:HAA_Total, names_sep = "_", names_to = c("race", "sex")) %>%
    mutate(
        place = CTYNAME,
        age = case_when(
            AGEGRP == 0 ~ "Total",
            AGEGRP == 1 ~ "Under 5 years",
            AGEGRP == 2 ~ "5 to 9",
            AGEGRP == 3 ~ "10 to 14",
            AGEGRP == 4 ~ "15 to 19",
            AGEGRP == 5 ~ "20 to 24",
            AGEGRP == 6 ~ "25 to 29",
            AGEGRP == 7 ~ "30 to 34",
            AGEGRP == 8 ~ "35 to 39",
            AGEGRP == 9 ~ "40 to 44",
            AGEGRP == 10 ~ "45 to 49",
            AGEGRP == 11 ~ "50 to 54",
            AGEGRP == 12 ~ "55 to 59",
            AGEGRP == 13 ~ "60 to 64",
            AGEGRP == 14 ~ "65 to 69",
            AGEGRP == 15 ~ "70 to 74",
            AGEGRP == 16 ~ "75 to 79",
            AGEGRP == 17 ~ "80 to 84",
            AGEGRP == 18 ~ "85 plus"
        ),
        date = sapply(years_since_start, create_date, start_year = start_year),
        sex = case_when(
            sex == "POP" | sex == "Total" ~ "Total",
            sex == "MALE" ~ "Male",
            sex == "FEMALE" ~ "Female"
        ),
        population = value
    ) %>%
    filter(sex == "Total") %>%
    select(place, date, sex, race, age, population)

allstates_pep <- allstates_pep %>% # for some reason this is working only when I keep it separate
    mutate(hisp = case_when(
        substr(allstates_pep$race, 1, 1) == "H" ~ "Hispanic",
        allstates_pep$race == "TOT" ~ "Total",
        substr(allstates_pep$race, 1, 1) != "H" ~ "Not Hispanic"
    ))

allstates_pep <- allstates_pep %>%
    filter(race %in% c("TOT", "NHWA", "NHBA", "NHAA", "H")) %>%
    mutate(
        race = case_when(
            race == "TOT" ~ "Total",
            race == "NHWA" ~ "White alone",
            race == "NHBA" ~ "Black or African American alone",
            race == "NHAA" ~ "Asian alone",
            race == "H" ~ "Hispanic"
        ),
        raceSimple = case_when(
            race == "Total" ~ "Total",
            race == "White alone" & hisp == "Not Hispanic" & sex == "Total" ~ "White",
            race == "Black or African American alone" & hisp == "Not Hispanic" & sex == "Total" ~ "Black",
            race == "Asian alone" & hisp == "Not Hispanic" & sex == "Total" ~ "Asian",
            hisp == "Hispanic" & sex == "Total" ~ "Hispanic"
        )
    ) %>%
    select(place, date, hisp, sex, race, age, population, raceSimple)

popunder18US <- read_csv(paste0("inputs/PEP_data/PEP", as.numeric(format(Sys.Date(), "%Y")) - 1, "_agesex_allstates.csv")) # for popunder18 measure
popunder18US <- popunder18US %>%
    select(CTYNAME, YEAR, AGE18PLUS_TOT) %>%
    mutate(
        place = CTYNAME,
        date = sapply(years_since_start, create_date, start_year = start_year),
        age = "18 years and over",
        race = "Total",
        raceSimple = "Total",
        sex = "Total",
        population = AGE18PLUS_TOT
    )

allstates_pep <- allstates_pep %>% full_join(popunder18US, by = c("place", "date", "sex", "race", "raceSimple", "age", "population"))


allstates_pep <- allstates_pep %>%
    group_by(date, hisp, sex, race, age, raceSimple) %>%
    summarize(
        place = "United States",
        population = sum(population)
    ) %>%
    select(place, date, hisp, sex, race, age, population, raceSimple) %>%
    ungroup()

allparishesRaw2020 <- rbind(allstates_pep, allparishesRaw) %>%
    filter(date == "7/1/2020 population estimate") %>%
    mutate(
        PlaceName = case_when(
            place == "Orleans Parish" ~ "Orleans",
            place == "Jefferson Parish" ~ "Jefferson",
            place == "Plaquemines Parish" ~ "Plaquemines",
            place == "St. Bernard Parish" ~ "St. Bernard",
            place == "St. Charles Parish" ~ "St. Charles",
            place == "St. James Parish" ~ "St. James",
            place == "St. John the Baptist Parish" ~ "St. John the Baptist",
            place == "St. Tammany Parish" ~ "St. Tammany",
            place == "United States" ~ "United States"
        ),
        PlaceName = factor(PlaceName, levels = c(
            "Orleans", "Jefferson", "Plaquemines",
            "St. Bernard", "St. Charles", "St. James",
            "St. John the Baptist", "St. Tammany", "Metro", "United States"
        ))
    )
save(allparishesRaw2020, file = "inputs/allparishesRaw2020.RData")

# allparishesRaw2021 <- rbind(allstates_pep, allparishesRaw) %>%
#     filter(date == "7/1/2021 population estimate") %>%
#     mutate(
#         PlaceName = case_when(
#             place == "Orleans Parish" ~ "Orleans",
#             place == "Jefferson Parish" ~ "Jefferson",
#             place == "Plaquemines Parish" ~ "Plaquemines",
#             place == "St. Bernard Parish" ~ "St. Bernard",
#             place == "St. Charles Parish" ~ "St. Charles",
#             place == "St. James Parish" ~ "St. James",
#             place == "St. John the Baptist Parish" ~ "St. John the Baptist",
#             place == "St. Tammany Parish" ~ "St. Tammany",
#             place == "United States" ~ "United States"
#         ),
#         PlaceName = factor(PlaceName, levels = c(
#             "Orleans", "Jefferson", "Plaquemines",
#             "St. Bernard", "St. Charles", "St. James",
#             "St. John the Baptist", "St. Tammany", "Metro", "United States"
#         ))
#     )
# save(allparishesRaw2021, file = "inputs/allparishesRaw2021.RData")

# allparishesRaw2022 <- rbind(allstates_pep, allparishesRaw) %>%
#     filter(date == "7/1/2022 population estimate") %>%
#     mutate(
#         PlaceName = case_when(
#             place == "Orleans Parish" ~ "Orleans",
#             place == "Jefferson Parish" ~ "Jefferson",
#             place == "Plaquemines Parish" ~ "Plaquemines",
#             place == "St. Bernard Parish" ~ "St. Bernard",
#             place == "St. Charles Parish" ~ "St. Charles",
#             place == "St. James Parish" ~ "St. James",
#             place == "St. John the Baptist Parish" ~ "St. John the Baptist",
#             place == "St. Tammany Parish" ~ "St. Tammany",
#             place == "United States" ~ "United States"
#         ),
#         PlaceName = factor(PlaceName, levels = c(
#             "Orleans", "Jefferson", "Plaquemines",
#             "St. Bernard", "St. Charles", "St. James",
#             "St. John the Baptist", "St. Tammany", "Metro", "United States"
#         ))
#     )
# save(allparishesRaw2022, file = "inputs/allparishesRaw2022.RData")

allparishesRaw2023 <- rbind(allstates_pep, allparishesRaw) %>%
    filter(date == "7/1/2023 population estimate") %>%
    mutate(
        PlaceName = case_when(
            place == "Orleans Parish" ~ "Orleans",
            place == "Jefferson Parish" ~ "Jefferson",
            place == "Plaquemines Parish" ~ "Plaquemines",
            place == "St. Bernard Parish" ~ "St. Bernard",
            place == "St. Charles Parish" ~ "St. Charles",
            place == "St. James Parish" ~ "St. James",
            place == "St. John the Baptist Parish" ~ "St. John the Baptist",
            place == "St. Tammany Parish" ~ "St. Tammany",
            place == "United States" ~ "United States"
        ),
        PlaceName = factor(PlaceName, levels = c(
            "Orleans", "Jefferson", "Plaquemines",
            "St. Bernard", "St. Charles", "St. James",
            "St. John the Baptist", "St. Tammany", "Metro", "United States"
        ))
    )
save(allparishesRaw2023, file = "inputs/allparishesRaw2023.RData")


#### Pulling PEP

## this is just for the 2010 inline measures - probably could be done differently but for now..
popestVars <- c("POP", "DATE_DESC", "DATE_CODE", "GEO_ID", "HISP", "RACE", "SEX", "POP") # added because between 2017 and 2018 they changes DATE to DATE_CODE
popestVars2000 <- c("POP", "DATE_DESC", "DATE_", "GEONAME", "HISP", "HISP", "RACE", "SEX", "POP")
# listCensusMetadata("pep/int_charagegroups", vintage = 2000, type = "variables")
allparishes_retro <- getCensus(
    name = "pep/charagegroups", # most recent
    vintage = 2019,
    key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
    vars = popestVars,
    region = "county: 071,051,075,087,089,093,095,103",
    regionin = "state:22"
) %>%
    rename(DATE = DATE_CODE) %>%
    bind_rows(getCensus(
        name = "pep/int_charagegroups", # Intercensal estimates
        vintage = 2000,
        key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
        vars = popestVars2000,
        region = "county:071,051,075,087,089,093,095,103",
        regionin = "state:22"
    )) %>%
    mutate(
        place = case_when(
            county == "071" ~ "Orleans",
            county == "051" ~ "Jefferson",
            county == "075" ~ "Plaquemines",
            county == "087" ~ "St. Bernard",
            county == "089" ~ "St. Charles",
            county == "093" ~ "St. James",
            county == "095" ~ "St. John the Baptist",
            county == "103" ~ "St. Tammany"
        ),
        year = str_sub(DATE_DESC, 5, 8)
    ) %>% # Clean July 1 from every year
    select(-GEO_ID, -DATE_, -DATE, -GEONAME) %>%
    filter(HISP == 0) %>%
    filter(DATE_DESC == "4/1/2010 Census population" | year == 2006 | year == 2007 | year == 2008 | year == 2009 | year == 2011 | year == 2012 | year == 2013 | year == 2014 | year == 2015 | year == 2016 | year == 2017 | year == 2018 | year == 2019)
allparishes_retro <- allparishes_retro %>%
    transmute(
        PlaceName = place,
        date = DATE_DESC,
        sex = case_when(SEX == 0 ~ "Total"),
        race = case_when(RACE == 0 ~ "Total"),
        hisp = case_when(HISP == 0 ~ "Total"),
        raceSimple = case_when(RACE == 0 ~ "Total"),
        population = POP
    ) %>%
    filter(sex == "Total" & race == "Total" & hisp == "Total")

save(allparishes_retro, file = "inputs/allparishes_retro.RData")



popestVars <- c("POP", "DATE_DESC", "DATE_CODE", "GEO_ID", "HISP", "RACE") # added because between 2017 and 2018 they changes DATE to DATE_CODE
popestVars2000 <- c("POP", "DATE_DESC", "DATE_", "GEONAME", "HISP")
# listCensusMetadata("pep/int_charagegroups", vintage = 2019, type = "variables")
hisppopestRaw <- getCensus(
    name = "pep/charagegroups", # most recent
    vintage = 2019,
    key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
    vars = popestVars,
    region = "county: 071,051,075,087,089,093,095,103",
    regionin = "state:22"
) %>%
    rename(DATE = DATE_CODE) %>%
    bind_rows(getCensus(
        name = "pep/int_charagegroups", # Intercensal estimates
        vintage = 2000,
        key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
        vars = popestVars2000,
        region = "county:071,051,075,087,089,093,095,103",
        regionin = "state:22"
    )) %>%
    mutate(
        place = case_when(
            county == "071" ~ "Orleans",
            county == "051" ~ "Jefferson",
            county == "075" ~ "Plaquemines",
            county == "087" ~ "St. Bernard",
            county == "089" ~ "St. Charles",
            county == "093" ~ "St. James",
            county == "095" ~ "St. John the Baptist",
            county == "103" ~ "St. Tammany"
        ),
        year = str_sub(DATE_DESC, 5, 8)
    ) %>% # Clean July 1 from every year
    select(-GEO_ID, -DATE_, -DATE, -GEONAME) %>%
    filter(HISP == 2) %>%
    filter(DATE_DESC == "4/1/2010 Census population" | year == 2006 | year == 2007 | year == 2008 | year == 2009 | year == 2011 | year == 2012 | year == 2013 | year == 2014 | year == 2015 | year == 2016 | year == 2017 | year == 2018 | year == 2019)

hisppopest20 <- allparishesRaw2020 %>%
    filter(race == "Hispanic" & age == "Total" & sex == "Total") %>%
    mutate(place = PlaceName) %>%
    select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>%
    mutate(state = "", county = "", year = 2020) %>%
    select(state, county, POP, DATE_DESC, HISP, place, year)
hisppopest21 <- allparishesRaw2021 %>%
    filter(race == "Hispanic" & age == "Total" & sex == "Total") %>%
    mutate(place = PlaceName) %>%
    select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>%
    mutate(state = "", county = "", year = 2021) %>%
    select(state, county, POP, DATE_DESC, HISP, place, year)
hisppopest22 <- allparishesRaw2022 %>%
    filter(race == "Hispanic" & age == "Total" & sex == "Total") %>%
    mutate(place = PlaceName) %>%
    select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>%
    mutate(state = "", county = "", year = 2022) %>%
    select(state, county, POP, DATE_DESC, HISP, place, year)
hisppopest23 <- allparishesRaw2023 %>%
    filter(race == "Hispanic" & age == "Total" & sex == "Total") %>%
    mutate(place = PlaceName) %>%
    select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>%
    mutate(state = "", county = "", year = 2023) %>%
    select(state, county, POP, DATE_DESC, HISP, place, year)
hisppopestRaw <- rbind(hisppopestRaw, hisppopest20, hisppopest21, hisppopest22, hisppopest23)

save(hisppopestRaw, file = "inputs/hisppopestRaw.RData")
write_csv(hisppopestRaw %>% arrange(place, year), file = "temp/pep23_hispanic.csv")


popestVars <- c("POP", "DATE_DESC", "DATE_CODE", "GEO_ID", "HISP", "RACE")
popestVars2000 <- c("POP", "DATE_DESC", "DATE_", "GEONAME", "HISP")
# listCensusMetadata("pep/int_charagegroups", vintage = 2019, type = "variables")
blackpopestRaw <- getCensus(
    name = "pep/charagegroups", # most recent
    vintage = 2019,
    key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
    vars = popestVars,
    region = "county: 071",
    regionin = "state:22"
) %>%
    rename(DATE = DATE_CODE) %>%
    bind_rows(getCensus(
        name = "pep/int_charagegroups", # Intercensal estimates
        vintage = 2000,
        key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
        vars = popestVars2000,
        region = "county:071",
        regionin = "state:22"
    )) %>%
    mutate(
        place = "Orleans",
        year = str_sub(DATE_DESC, 5, 8)
    ) %>% # Clean July 1 from every year
    select(-GEONAME, -DATE) %>%
    filter(HISP == 1) %>%
    filter(RACE == 2) %>%
    filter(DATE_DESC == "4/1/2010 Census population" | year == 2006 | year == 2007 | year == 2008 | year == 2009 | year == 2011 | year == 2012 | year == 2013 | year == 2014 | year == 2015 | year == 2016 | year == 2017 | year == 2018 | year == 2019) %>%
    select(-GEO_ID, -DATE_)

blackpopest20 <- allparishesRaw2020 %>%
    filter(race == "Black or African American alone" & place == "Orleans Parish" & age == "Total" & sex == "Total") %>%
    select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>%
    mutate(state = 22, county = 071, year = 2020) %>%
    select(state, county, POP, DATE_DESC, HISP, RACE, place, year)
blackpopest21 <- allparishesRaw2022 %>%
    filter(race == "Black or African American alone" & place == "Orleans Parish" & age == "Total" & sex == "Total") %>%
    select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>%
    filter(RACE == "Black or African American alone" & place == "Orleans Parish") %>%
    mutate(state = 22, county = 071, year = 2022) %>%
    select(state, county, POP, DATE_DESC, HISP, RACE, place, year)
blackpopest22 <- allparishesRaw2022 %>%
    filter(race == "Black or African American alone" & place == "Orleans Parish" & age == "Total" & sex == "Total") %>%
    select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>%
    filter(RACE == "Black or African American alone" & place == "Orleans Parish") %>%
    mutate(state = 22, county = 071, year = 2022) %>%
    select(state, county, POP, DATE_DESC, HISP, RACE, place, year)
blackpopest23 <- allparishesRaw2023 %>%
    filter(race == "Black or African American alone" & place == "Orleans Parish" & age == "Total" & sex == "Total") %>%
    select(POP = population, DATE_DESC = date, HISP = hisp, RACE = race, place) %>%
    filter(RACE == "Black or African American alone" & place == "Orleans Parish") %>%
    mutate(state = 22, county = 071, year = 2023) %>%
    select(state, county, POP, DATE_DESC, HISP, RACE, place, year)
blackpopestRaw <- rbind(blackpopestRaw, blackpopest20, blackpopest21, blackpopest22, blackpopest23)

save(blackpopestRaw, file = "inputs/blackpopestRaw.RData")


###### HT adding new code to get all races retrospectively July 2024

popestVars <- c("POP", "DATE_DESC", "DATE_CODE", "GEO_ID", "HISP", "RACE")
popestVars2000 <- c("POP", "DATE_DESC", "DATE_", "GEONAME", "HISP", "RACE")
# listCensusMetadata("pep/int_charagegroups", vintage = 2019, type = "variables")
Historic_popestRaw <- getCensus(
    name = "pep/charagegroups", # most recent
    vintage = 2019,
    key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
    vars = popestVars,
    region = "county:071,051,075,087,089,093,095,103",
    regionin = "state:22"
) %>%
    rename(DATE = DATE_CODE) %>%
    bind_rows(getCensus(
        name = "pep/int_charagegroups", # Intercensal estimates
        vintage = 2000,
        key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
        vars = popestVars2000,
        region = "county:071,051,075,087,089,093,095,103",
        regionin = "state:22"
    )) %>%
    mutate(place = case_when(
        county == "071" ~ "Orleans",
        county == "051" ~ "Jefferson",
        county == "075" ~ "Plaquemines",
        county == "087" ~ "St. Bernard",
        county == "089" ~ "St. Charles",
        county == "093" ~ "St. James",
        county == "095" ~ "St. John the Baptist",
        county == "103" ~ "St. Tammany"
    )) %>%
    mutate(year = str_sub(DATE_DESC, 5, 8)) %>% # Clean July 1 from every year
    select(-GEONAME, -DATE) %>%
    mutate(race = factor(case_when(
        RACE == 0 & HISP == 0 ~ "Total",
        RACE == 1 & HISP == 1 ~ "White",
        RACE == 2 & HISP == 1 ~ "Black",
        RACE == 4 & HISP == 1 ~ "Asian",
        HISP == 2 & RACE == 0 ~ "Hispanic"
    ), levels = c("White", "Black", "Hispanic", "Asian"))) %>%
    filter((DATE_DESC == "4/1/2010 Census population" | DATE_DESC == "4/1/2000 population estimates base") &
        race %in% c("White", "Black", "Hispanic", "Asian")) %>%
    select(-GEO_ID, -DATE_) %>%
    mutate(
        state = "",
        county = ""
    ) %>%
    select(state, county, POP, DATE_DESC, race, place, year)

Racepopest20 <- allparishesRaw2020 %>%
    filter(age == "Total" & sex == "Total" & raceSimple != "Total") %>%
    mutate(place = PlaceName) %>%
    select(POP = population, DATE_DESC = date, race = raceSimple, place) %>%
    mutate(
        state = "",
        county = "",
        year = 2020
    ) %>%
    select(state, county, POP, DATE_DESC, race, place, year)

Racepopest23 <- allparishesRaw2023 %>%
    filter(age == "Total" & sex == "Total" & raceSimple != "Total") %>%
    mutate(place = PlaceName) %>%
    select(POP = population, DATE_DESC = date, race = raceSimple, place) %>%
    mutate(state = "", county = "", year = 2023) %>%
    select(state, county, POP, DATE_DESC, race, place, year)


RacepopestRaw <- rbind(Historic_popestRaw, Racepopest20, Racepopest23) %>%
    filter(place != "United States") %>%
    group_by(year, race, DATE_DESC) %>%
    summarize(POP = sum(POP)) %>%
    mutate(place = "Metro")

save(RacepopestRaw, file = "inputs/RacepopestRaw.RData")

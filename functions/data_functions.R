############################################
# # PULLING DATA # #
############################################

## pull parish, metro, and US numbers with census api
## input: census api variable names, human-readable names, and vintage
## output: dataframe in same format as Who Lives data tables excel sheet

# Variable Declaration
readRenviron(".env.test")
CENSUS_KEY <- Sys.getenv("TEST_CENSUS_KEY")


wholivesdatapull <- function(variables, names = variables, year = YEAR, censusname = "acs/acs1") {
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

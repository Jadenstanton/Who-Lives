############################################
# # PEP # #
############################################

##############################################

# To analyze, filter to:
# whatever combination of demographics you need: race, age, sex, hisp
# whatever geography you need: place
# whatever year of estimate that you need: date

orderDemo <- c(
  "Orleans Parish", "Jefferson Parish", "Plaquemines Parish", "St. Bernard Parish", "St. Charles Parish",
  "St. James Parish", "St. John the Baptist Parish", "St. Tammany Parish", "Metro", "United States"
)
# allparishesRaw <- load("inputs/allparishesRaw.RData")

# Table 1
load(paste0("inputs/allparishesRaw", YEAR_PEP, ".RData"))
AAWhiteHispan <- paste0("allparishesRaw", YEAR_PEP) %>%
  filter(place == "Orleans Parish") %>%
  filter(date == paste0("7/1/", YEAR_PEP, " population estimate")) %>%
  filter(age == "Total" & sex == "Total" & (raceSimple == "Black" | raceSimple == "White" | hisp == "Hispanic" | raceSimple == "Asian")) %>%
  mutate(race.fac = factor(.$raceSimple, levels = c("Black", "White", "Hispanic", "Asian"))) %>%
  arrange(race.fac) %>%
  mutate(est2000 = c(323392, 128871, 14826, 10919)) %>% # check order of races in data frame. Order is bottom up
  select(raceSimple, race.fac, population, est2000)


write.csv(AAWhiteHispan, "outputs/spreadsheets/AAWhiteHispan.csv")
storage_write_csv(AAWhiteHispan, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/AAWhiteHispan.csv"))



# Tables 2


# Remove Louisiana and Us to be able to combine 8 parish estimates for each race/ethnicity to create Metro

# HT : I just switched these around so it would make Metro last... The dataframe is not ordered by the levels of placename factor
# that's the order that the 2000 numbers are going in.
ParishDemo2 <- paste0("allparishesRaw", YEAR_PEP) %>%
  filter(PlaceName %in% c(
    "Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
    "St. James", "St. John the Baptist", "St. Tammany"
  )) %>%
  filter(paste0(date == "7/1/", YEAR_PEP, " population estimate")) %>%
  filter(age == "Total" & sex == "Total") %>%
  group_by(raceSimple) %>%
  unique() %>%
  summarise(population = sum(population)) %>%
  mutate(PlaceName = "Metro") %>%
  select(PlaceName, population, raceSimple)

ParishDemo3 <- paste0("allparishesRaw", YEAR_PEP) %>%
  filter(PlaceName == "United States") %>%
  filter(paste0(date == "7/1/", YEAR_PEP, " population estimate")) %>%
  filter(age == "Total" & sex == "Total") %>%
  select(PlaceName, population, raceSimple)

ParishDemo1 <- paste0("allparishesRaw", YEAR_PEP) %>%
  filter(PlaceName %in% c(
    "Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
    "St. James", "St. John the Baptist", "St. Tammany"
  )) %>%
  arrange((PlaceName)) %>%
  filter(paste0(date == "7/1/", YEAR_PEP, " population estimate")) %>%
  filter(age == "Total" & sex == "Total") %>%
  select(PlaceName, population, raceSimple) %>%
  bind_rows(., ParishDemo2, ParishDemo3)



# reshape data from long to wide for easy analysis
ParishDemo <- pivot_wider(ParishDemo1, names_from = raceSimple, values_from = population) %>%
  mutate(PlaceName = factor(PlaceName, levels = c(
    "Orleans", "Jefferson", "Plaquemines",
    "St. Bernard", "St. Charles", "St. James",
    "St. John the Baptist", "St. Tammany", "Metro", "United States"
  ))) %>%
  mutate(
    pctwhite = as.numeric(White) / as.numeric(Total),
    pctblack = as.numeric(Black) / as.numeric(Total),
    pctasian = as.numeric(Asian) / as.numeric(Total),
    pcthisp = as.numeric(Hispanic) / as.numeric(Total),
    white2000 = c(.266, .645, .688, .844, .705, .497, .51, .853, .547, .691),
    black2000 = c(.667, .227, .233, .076, .251, .492, .446, .098, .373, .121),
    asian2000 = c(.023, .031, .026, .013, .006, 0, .005, .008, .021, .037),
    hispanic2000 = c(.031, .071, .016, .051, .028, .006, .029, .025, .044, .125)
  ) # %>%
# .[-2,]

orleansdemo_csv <- ParishDemo %>%
  filter(PlaceName == "Orleans") %>%
  select(PlaceName, Total:Hispanic) %>%
  pivot_longer(cols = -PlaceName, names_to = "race", values_to = paste0("est", YEAR_PEP))
write.csv(orleansdemo_csv, "outputs/spreadsheets/orleansdemo.csv")
storage_write_csv(orleansdemo_csv, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/orleansdemo.csv"))

parishdemo_csv <- ParishDemo %>%
  select(-c(Total:Hispanic)) %>%
  pivot_longer(cols = -PlaceName, names_to = c("race", "year"), names_sep = "2", values_to = "val") %>%
  mutate(
    year = case_when(
      year == "000" ~ "2000",
      is.na(year) ~ as.character(YEAR_PEP),
      T ~ year
    ),
    race = factor(case_when(
      grepl("white", race) ~ "White, non-Hispanic",
      grepl("black", race) ~ "Black",
      grepl("hisp", race) ~ "Hispanic, any race",
      grepl("asian", race) ~ "Asian"
    ), levels = c("White, non-Hispanic", "Black", "Hispanic, any race", "Asian"))
  ) %>%
  arrange(PlaceName, year) %>%
  pivot_wider(names_from = c(PlaceName, year), values_from = val) %>%
  arrange(race)
write.csv(parishdemo_csv, "outputs/spreadsheets/ParishDemo.csv")
storage_write_csv(parishdemo_csv, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/ParishDemo.csv"))



# Table 3 African American Population, New Orleans, 2000-current

# Pulling population estimates for 2010-current
load("inputs/blackpopestRaw.RData")
AAhistorical <- blackpopestRaw %>%
  select(year, POP) %>%
  arrange(year) %>%
  # .[-(2:3),] %>% #Remove 2010 estimates we don't need. We use Census Population for 2010 so we can delete 2010 Population estimate
  bind_rows(data.frame(POP = c(323392, 0, 0, 0, 0, 0, 133015, 159887, 181882, 197337), row.names = (NULL)), .) %>%
  select(POP) %>%
  bind_cols(data.frame(year = as.factor(c(2000:YEAR_PEP))), .)

write.csv(AAhistorical, "outputs/spreadsheets/AAHistorical.csv")
storage_write_csv(AAhistorical, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/AAHistorical.csv"))

####### AA historical part 2
BlackPopyears <- paste0("allparishesRaw", YEAR_PEP) %>%
  filter(age == "Total" & sex == "Total") %>%
  filter(raceSimple == "Black") %>%
  filter(place %in% c(
    "Orleans Parish", "Jefferson Parish", "Plaquemines Parish", "St. Bernard Parish", "St. Charles Parish",
    "St. James Parish", "St. John the Baptist Parish", "St. Tammany Parish"
  ))

BlackpopM <- blackpopestRaw %>%
  add_row(year = "2000", place = "Orleans", POP = 323392) %>%
  select(year, POP)

write.csv(BlackpopM, "outputs/spreadsheets/BlackpopM.csv")
storage_write_csv(BlackpopM, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/BlackpopM.csv"))

# Table 4 Hispanic population change by population

HispanicPop <- paste0("allparishesRaw", YEAR_PEP) %>%
  filter(PlaceName %in% c(
    "Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
    "St. James", "St. John the Baptist", "St. Tammany"
  )) %>%
  filter(age == "Total" & sex == "Total") %>%
  filter(raceSimple == "Hispanic") %>%
  select(PlaceName, population) %>%
  mutate(
    est2000 = 0,
    est2000 = ifelse(PlaceName == "St. Tammany", 4737, est2000),
    est2000 = ifelse(PlaceName == "St. John the Baptist", 1230, est2000),
    est2000 = ifelse(PlaceName == "St. James", 130, est2000),
    est2000 = ifelse(PlaceName == "St. Charles", 1346, est2000),
    est2000 = ifelse(PlaceName == "St. Bernard", 3425, est2000),
    est2000 = ifelse(PlaceName == "Plaquemines", 433, est2000),
    est2000 = ifelse(PlaceName == "Orleans", 14826, est2000),
    est2000 = ifelse(PlaceName == "Jefferson", 32418, est2000)
  )

# Table 5 Hispanic population for parishes in metro by year

HispanicPopyears <- paste0("allparishesRaw", YEAR_PEP) %>%
  filter(age == "Total" & sex == "Total") %>%
  filter(raceSimple == "Hispanic") %>%
  filter(PlaceName %in% c(
    "Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
    "St. James", "St. John the Baptist", "St. Tammany"
  ))

HispanicPopyears <- HispanicPopyears %>%
  select(PlaceName, date, population) %>%
  pivot_wider(id_cols = date, names_from = PlaceName, values_from = population)
write.csv(HispanicPopyears, "outputs/spreadsheets/HispanicPopyears.csv")
storage_write_csv(HispanicPopyears, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/HispanicPopyears.csv"))

load("inputs/hisppopestRaw.RData")
HISPpopM <- hisppopestRaw %>%
  mutate(year = as.numeric(year)) %>%
  filter(place %in% c(
    "Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
    "St. James", "St. John the Baptist", "St. Tammany"
  )) %>%
  add_row(year = 2000, place = "Orleans", POP = 14826) %>%
  add_row(year = 2000, place = "Jefferson", POP = 32418) %>%
  add_row(year = 2000, place = "St. Tammany", POP = 4737) %>%
  add_row(year = 2000, place = "Plaquemines", POP = 433) %>%
  add_row(year = 2000, place = "St. Bernard", POP = 3425) %>%
  add_row(year = 2000, place = "St. Charles", POP = 1346) %>%
  add_row(year = 2000, place = "St. James", POP = 130) %>%
  add_row(year = 2000, place = "St. John the Baptist", POP = 1230) %>%
  select(place, year, POP) %>%
  unique()
HISPpopM_CSV <- HISPpopM %>%
  pivot_wider(names_from = "place", values_from = POP) %>%
  select(year, Orleans, Jefferson, Plaquemines, `St. Bernard`, `St. Charles`, `St. James`, `St. John the Baptist`, `St. Tammany`) %>%
  arrange(year)
write.csv(HISPpopM_CSV, "outputs/spreadsheets/HISPpopM.csv")
storage_write_csv(HISPpopM_CSV, cont_proj, psate0("who_lives/", CURRENT_YEAR, "/outputs/HISPpopM_CSV.csv"))

# For excel
# HISPpopSheet1 <- HISPpopM %>%
#   select(year, place, POP) %>%
#   mutate(POP = as.numeric(POP))
#
# HiSPpopSheet <- spread(HISPpopSheet1, place, POP)

# I thought about calculating age in this way, but in the end, ACS metro-level estimate makes more sense to use -- this is kinda weird to do the weighted mean/median. Leaving this here to remember that I tried it! -- Jenna, 7/13/2020
# medAge <- as.data.frame(matrix(data = c(POPESTIMATE	= c(432493, 390144, 23197	, 47244	, 53100	, 21096	, 42837	, 260419), MEDIAN_AGE_TOT = c(39.6, 37.7, 37.1, 35.1, 38.4, 39.5, 37.8, 40.5)), ncol = 2)) %>%
#   summarise(med = mean(x = V2, wt = V1))
load("inputs/medageRaw.RData")


# Table 5 Population by age group

orderAge <- c(
  rep("Jefferson", 18), rep("Orleans", 18), rep("Plaquemines", 18),
  rep("St. Bernard", 18), rep("St. Charles", 18), rep("St. James", 18),
  rep("St. John The Baptist", 18), rep("St. Tammany", 18)
)
Agepop <- paste0("allparishesRaw", YEAR_PEP) %>%
  filter(age == "Under 5 years" | age == "5 to 9" | age == "10 to 14" | age == "15 to 19" |
    age == "20 to 24" | age == "25 to 29" | age == "30 to 34" | age == "35 to 39" | age == "40 to 44" |
    age == "45 to 49" | age == "50 to 54" | age == "55 to 59" | age == "60 to 64" | age == "65 to 69" |
    age == "70 to 74" | age == "75 to 79" | age == "80 to 84" | age == "85 plus") %>%
  filter(raceSimple == "Total") %>%
  filter(sex == "Total") %>%
  filter(date == paste0("7/1/", YEAR_PEP, " population estimate")) %>%
  filter(PlaceName %in% c(
    "Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
    "St. James", "St. John the Baptist", "St. Tammany"
  )) %>%
  arrange(factor(PlaceName, levels = c(
    "Jefferson", "Orleans", "Plaquemines",
    "St. Bernard", "St. Charles", "St. James",
    "St. John the Baptist", "St. Tammany"
  ))) %>%
  select(age, PlaceName, population) %>%
  mutate(est2000 = c(
    30226, 31811, 32657, 32436, 29793, 31838, 32713, 36367, 36834, 34166, 30658, 23741, 17911, 15034, 14991, 11973, 6942, 5375, 33496,
    37133, 36769, 38312, 38932, 36416, 34050, 35053, 36444, 34562, 29128, 21068, 16658, 14648, 14301, 12458, 7838, 7408, 1977, 2183, 2241,
    2197, 1668, 1621, 2024, 2271, 2247, 1855, 1554, 1272, 1034, 854, 769, 524, 259, 207, 4242, 4639, 4996, 5021, 4257, 4196, 4584, 5327, 5530,
    4939, 4398, 3241, 2597, 2569, 2714, 2148, 1051, 780, 3511, 3994, 4352, 4063, 2649, 2662, 3440, 4407, 4569, 3732, 2872, 2025, 1488, 1345,
    1244, 859, 462, 398, 1483, 1711, 1863, 1936, 1346, 1142, 1439, 1671, 1713, 1496, 1220, 918, 916, 736, 616, 448, 275, 287, 3463, 3692, 3874,
    3837, 2721, 2699, 3118, 3612, 3588, 3240, 2503, 1907, 1434, 1006, 925, 663, 416, 346, 13556, 15029, 16147, 14672, 9045, 10257, 12729, 16457,
    17655, 16062, 13641, 9733, 7125, 5825, 5168, 4033, 2296, 1838
  ))
Agepop_csv <- Agepop %>%
  mutate(
    est22 = population,
    est00 = est2000
  ) %>%
  pivot_wider(id_cols = age, names_from = PlaceName, values_from = c(est00, est22)) %>%
  select(
    est00_Orleans, est22_Orleans,
    est00_Jefferson, est22_Jefferson,
    est00_Plaquemines, est22_Plaquemines,
    `est00_St. Bernard`, `est22_St. Bernard`,
    `est00_St. Charles`, `est22_St. Charles`,
    `est00_St. James`, `est22_St. James`,
    `est00_St. John the Baptist`, `est22_St. John the Baptist`,
    `est00_St. Tammany`, `est22_St. Tammany`
  )
write.csv(Agepop_csv, "outputs/spreadsheets/Agepop.csv")
storage_write_csv(Agepop_csv, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/Agepop.csv"))


# Table 6 Under 18 population
# Different than estimates from google sheets but aligns with American fact finder


under18pars <- paste0("allparishesRaw", YEAR_PEP) %>%
  filter(age == "18 years and over" | age == "Total") %>%
  filter(raceSimple == "Total") %>%
  filter(sex == "Total") %>%
  filter(date == paste0("7/1/", YEAR_PEP, " population estimate")) %>%
  filter(PlaceName %in% c(
    "Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
    "St. James", "St. John the Baptist", "St. Tammany"
  )) %>%
  select(age, PlaceName, population)

# Creating metro
under18metro <- under18pars %>%
  group_by(age) %>%
  summarise(population = sum(population))
under18metro$PlaceName <- "Metro"

# stack back with other under 18
popunder18 <- bind_rows(under18pars, under18metro) %>%
  spread(., age, population) %>%
  mutate(under18 = Total - `18 years and over`) %>%
  filter(PlaceName == "Orleans" | PlaceName == "Jefferson" | PlaceName == "St. Tammany" | PlaceName == "Metro") %>%
  select(PlaceName, under18) %>%
  mutate(est2000 = c(115255, 358092, 129408, 54399))

popunder18CSV <- popunder18 %>%
  select(PlaceName, est2000, under18) %>%
  pivot_longer(-c("PlaceName"), names_to = "under18", values_to = "Value") %>%
  mutate(
    name = paste(PlaceName, under18, sep = "-"),
    year = YEAR_PEP
  ) %>%
  select(-PlaceName) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value") %>%
  select(`Orleans-est2000`, `Orleans-under18`, `Jefferson-est2000`, `Jefferson-under18`, `St. Tammany-est2000`, `St. Tammany-under18`, `Metro-est2000`, `Metro-under18`)
write.csv(popunder18CSV, "outputs/spreadsheets/under18.csv")
storage_write_csv(popunder18CSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/under18.csv"))


###########################
# Jenna's analysis expanded
###########################

childPovProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/childPov.csv")
homeownershipProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/homeownership.csv")
educationalAttainmentProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/educationalAttainment.csv")
medHHincProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/medHHinc.csv")

### Median household income ###
###
load("inputs/medhhRaw_exp.RData")
medhh_exp <- medhhRaw_exp %>%
  mutate(
    placenames = NA,
    placenames = ifelse(place == "103", "St. Tammany", placenames),
    placenames = ifelse(place == "051", "Jefferson", placenames),
    placenames = ifelse(place == "071", "Orleans", placenames),
    placenames = ifelse(place == "35380", "Metro", placenames),
    placenames = ifelse(place == "1", "U.S.", placenames)
  ) %>%
  mutate(place.fac = factor(.$placenames, levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
  select(-place, -placenames, -placename, -contains("MOE")) %>%
  pivot_longer(-place.fac, names_to = "var", values_to = "val")


### Across geos median hh income bar chart ###
medhh.totals <- medhh_exp %>%
  filter(var == "MedianHHIncome") %>%
  mutate(var.fac = factor(.$var, levels = c("Black", "White,\nnon-Hispanic", "Asian", "Hispanic,\nany race")))

medhh.race <- medhh_exp %>%
  filter(var != "MedianHHIncome") %>%
  mutate(
    var = ifelse(grepl("asian", var), "Asian", var),
    var = ifelse(grepl("blk", var), "Black", var),
    var = ifelse(grepl("hisp", var), "Hispanic,\nany race", var),
    var = ifelse(grepl("wht", var), "White,\nnon-Hispanic", var)
  ) %>%
  mutate(var.fac = factor(.$var, levels = c("Black", "White,\nnon-Hispanic", "Asian", "Hispanic,\nany race")))


### Historical median hh income line chart ###

## median hh income adjusted to 2022 dollars ###

# using https://data.bls.gov/cgi-bin/cpicalc.pl from january to january
# 1979 to 2022 = 3.83
# 1989 to 2022 = 2.16
# 1999 to 2022 = 1.59
# 2010 to 2022 = 1.21
# 2016 to 2022 = 1.10
load("inputs/medhh_unadjusted.RData")
medhhinc_adjusted21 <- medhh_unadjusted %>% mutate(
  value = as.numeric(value),
  inc_adj21 = case_when(
    Year == 1979 ~ value * 3.83,
    Year == 1989 ~ value * 2.16,
    Year == 1999 ~ value * 1.59,
    Year == 2010 ~ value * 1.21,
    Year == 2016 ~ value * 1.10
  )
)
# write_csv(medhhinc_adjusted21, "inputs/medHHinc_exp.csv")

medhh.hist <- medhhinc_adjusted21 %>%
  select(-value) %>%
  rename(val = inc_adj21) %>%
  mutate(
    Year = as.numeric(Year),
    var = ifelse(var == "Overall", "All", var),
    var = ifelse(grepl("Bla", var), "Black", var),
    var = ifelse(grepl("Hispanic,", var), "Hispanic,\nany race", var),
    var = ifelse(grepl("Whi", var), "White,\nnon-Hispanic", var)
  ) %>%
  bind_rows(medhh_exp %>%
    filter(
      place.fac == "Orleans",
      !grepl("asian", var)
    ) %>% ## remove asian numbers
    select(-place.fac) %>%
    mutate(
      Year = 2022, ## for the drafts, this is the wrong year, but will make the x axis the correct length for when we update the numbers
      var = ifelse(var == "MedianHHIncome", "All", var),
      var = ifelse(grepl("blk", var), "Black", var),
      var = ifelse(grepl("hisp", var), "Hispanic,\nany race", var),
      var = ifelse(grepl("wht", var), "White,\nnon-Hispanic", var)
    )) %>%
  mutate(var.fac = factor(.$var, levels = c("All", "Black", "White,\nnon-Hispanic", "Hispanic,\nany race")))


### stat testing ###
medhh.race_stattest <- medhhRaw_exp %>%
  mutate(
    sig_wht_blk = stattest(x = MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y = MedianHHIncome_blk, moey = MedianHHIncomeMOE_blk),
    sig_wht_hisp = stattest(x = MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y = MedianHHIncome_hisp, moey = MedianHHIncomeMOE_hisp),
    sig_wht_asian = stattest(x = MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y = MedianHHIncome_asian, moey = MedianHHIncomeMOE_asian),
    sigall_wht = stattest(x = MedianHHIncome_wht, moex = MedianHHIncomeMOE_wht, y = MedianHHIncome, moey = MedianHHIncomeMOE),
    sig_blk_hisp = stattest(x = MedianHHIncome_blk, moex = MedianHHIncomeMOE_blk, y = MedianHHIncome_hisp, moey = MedianHHIncomeMOE_hisp),
    sig_blk_asian = stattest(x = MedianHHIncome_blk, moex = MedianHHIncomeMOE_blk, y = MedianHHIncome_asian, moey = MedianHHIncomeMOE_asian),
    sigall_blk = stattest(x = MedianHHIncome_blk, moex = MedianHHIncomeMOE_blk, y = MedianHHIncome, moey = MedianHHIncomeMOE),
    sig_hisp_asian = stattest(x = MedianHHIncome_hisp, moex = MedianHHIncomeMOE_hisp, y = MedianHHIncome_asian, moey = MedianHHIncomeMOE_asian),
    sigall_hisp = stattest(x = MedianHHIncome_hisp, moex = MedianHHIncomeMOE_hisp, y = MedianHHIncome, moey = MedianHHIncomeMOE),
    sigall_asian = stattest(x = MedianHHIncome_asian, moex = MedianHHIncomeMOE_asian, y = MedianHHIncome, moey = MedianHHIncomeMOE)
  )

medhh_stat_all <- medhh.race_stattest %>%
  select(place, placename, (contains("sigall")), (contains("MedianHHIncome") & !contains("MOE"))) %>%
  pivot_longer(cols = c(-place, -placename, -contains("sig")), names_to = "race", values_to = "val") %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_all") %>%
  group_by(place, placename) %>%
  mutate(
    race = case_when(
      race == "MedianHHIncome" ~ "All",
      grepl("asian", race) & grepl("asian", var) ~ "Asian",
      grepl("blk", race) & grepl("blk", var) ~ "Black",
      grepl("hisp", race) & grepl("hisp", var) ~ "Hispanic, any race",
      grepl("wht", race) & grepl("wht", var) ~ "White, non-Hispanic"
    ),
    placename = case_when(
      placename == "New Orleans Metro Area" ~ "Metro",
      T ~ placename
    )
  ) %>%
  na.omit()
medhh_stat_all$stat_all[medhh_stat_all$race == "All"] <- "yes"
medhh_stat_all <- medhh_stat_all %>%
  group_by(place, placename, race) %>%
  mutate(val_lab = paste0("$", comma(val))) %>%
  select(-var, -placename) %>%
  unique()


medhh_stat_race <- medhh.race_stattest %>%
  select(place, placename, (contains("sig") & !contains("all"))) %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_race") %>%
  select(-var) %>%
  group_by(place, placename) %>%
  mutate(
    placename = case_when(
      placename == "New Orleans Metro Area" ~ "Metro",
      T ~ placename
    ),
    placename = case_when(
      "no" %in% stat_race ~ paste0(placename, "*"),
      T ~ placename
    )
  ) %>%
  select(-stat_race) %>%
  unique()
medhh_with_stats <- medhh_stat_all %>%
  left_join(medhh_stat_race, by = "place") %>%
  unique() %>%
  filter(race != "All") %>%
  mutate(
    placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "St. Tammany*", "Metro*", "United States")),
    var.fac = factor(race, levels = c("Black", "White, non-Hispanic", "Asian", "Hispanic, any race"))
  )


### Across geos pov bar chart ###
medhh.totals <- medhh_stat_all %>%
  left_join(medhh_stat_race, by = "place") %>%
  unique() %>%
  filter(race == "All") %>%
  mutate(
    var.fac = factor(race, levels = c("Black", "White, non-Hispanic", "Asian", "Hispanic, any race")),
    placename.fac = factor(placename.y, levels = c("Orleans ◊", "Jefferson ◊", "St. Tammany ◊", "Metro ◊", "United States"))
  )

medhh.totals_CSV <- medhh.totals %>%
  ungroup() %>%
  select(placename.x, race, val) %>%
  mutate(placename.x = ifelse(placename.x == "United States", "U.S.", placename.x)) %>%
  pivot_wider(names_from = placename.x, values_from = val)

medhh.race_CSV <- medhh.race %>%
  select(race = var, place.fac, val) %>%
  pivot_wider(names_from = place.fac, values_from = val) %>%
  rbind(medhh.totals_CSV)
write.csv(medhh.race_CSV, "outputs/spreadsheets/medhhrace.csv")
storage_write_csv(medhh.race_CSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/medhhrace.csv"))

medhh.hist_stattest.EST <- medhhRaw_exp %>%
  filter(place == "071") %>%
  select(-place, -placename, -contains("MOE")) %>%
  pivot_longer(everything(), names_to = "var", values_to = "val2022") %>%
  filter(var != "MedianHHIncome_asian") %>%
  mutate(race = case_when(
    var == "MedianHHIncome" ~ "Overall",
    var == "MedianHHIncome_blk" ~ "Black",
    var == "MedianHHIncome_wht" ~ "White,\nnon-Hispanic",
    var == "MedianHHIncome_hisp" ~ "Hispanic,\nany race"
  )) %>%
  select(race, val2022)

medhh.hist_stattest.MOE <- medhhRaw_exp %>%
  filter(place == "071") %>%
  select(contains("MOE")) %>%
  pivot_longer(everything(), names_to = "var", values_to = "moe2022") %>%
  filter(var != "MedianHHIncomeMOE_asian") %>%
  mutate(race = case_when(
    var == "MedianHHIncomeMOE" ~ "Overall",
    var == "MedianHHIncomeMOE_blk" ~ "Black",
    var == "MedianHHIncomeMOE_wht" ~ "White,\nnon-Hispanic",
    var == "MedianHHIncomeMOE_hisp" ~ "Hispanic,\nany race"
  )) %>%
  select(race, moe2022)

medhh_unadjusted <- read_xlsx("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/Copy_MedianInc.xlsx", range = "A1:H7") %>%
  transmute(
    "var" = `...1`,
    `1979` = as.character(`1979 (1979$)`),
    `1989` = as.character(`1989 (1989$)`),
    `1999` = as.character(`1999 (1999$)`),
    `2010` = `2010 (2010$)...5`,
    `2016` = `2016 (2016$)...7`,
    `2010MOE` = `2010 (2010$)...6`,
    `2016MOE` = `2016 (2016$)...8`,
    var = case_when(
      var == "White, Alone" ~ "White,\nnon-Hispanic",
      var == "Hispanic, Any Race" ~ "Hispanic,\nany race",
      T ~ var
    )
  ) %>%
  na.omit()

meddhh.hist_withmoe <- medhh_unadjusted %>%
  select(var, `1999`, `2010`, `2010MOE`) %>%
  mutate(adj1999 = as.numeric(`1999`) * 1.59, adj2010 = as.numeric(`2010`) * 1.21, adj2010moe = as.numeric(`2010MOE`) * 1.21) %>%
  mutate(adj1999moe = moe2000(adj1999, 215091, designfac = 1.2))

medhh.hist_stattest <- left_join(medhh.hist_stattest.EST, medhh.hist_stattest.MOE) %>%
  left_join(meddhh.hist_withmoe, by = c("race" = "var")) %>%
  mutate(
    sig_99_10 = stattest(x = adj1999, moex = adj1999moe, y = adj2010, moey = adj2010moe),
    sig_99_21 = stattest(x = adj1999, moex = adj1999moe, y = val2022, moey = moe2022),
    sig_10_21 = stattest(x = adj2010, moex = adj2010moe, y = val2022, moey = moe2022)
  ) %>%
  select(race, contains("sig"))

medhh.hist <- medhh.hist %>%
  left_join(medhh.hist_stattest, by = c("var" = "race")) %>%
  filter(var != "All") %>%
  mutate(val_lab = case_when(
    (sig_99_10 == "no" | sig_99_21 == "no" | sig_10_21 == "no") ~ "*",
    T ~ " "
  ))
medhh.hist_csv <- medhh.hist %>%
  select(Year, race = var, val) %>%
  filter(Year != 2016) %>%
  pivot_wider(names_from = Year, values_from = val)
write.csv(medhh.hist_csv, "outputs/spreadsheets/medhhhist.csv")
storage_write_csv(medhh.hist_csv, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/medhhhist.csv"))

### Educational attainment ###
###
load("inputs/bachRaw_exp.RData")
bach_exp <- bachRaw_exp %>%
  mutate(
    totbach = MaleBach + FemaleBach + MaleGradProf + FemaleGradProf,
    totbach_blk = MaleBach_blk + FemaleBach_blk,
    totbach_wht = MaleBach_wht + FemaleBach_wht,
    totbach_hisp = MaleBach_hisp + FemaleBach_hisp,
    totbach_asian = MaleBach_asian + FemaleBach_asian,
    pctbach = totbach / Total,
    pctbach_blk = totbach_blk / Total_blk,
    pctbach_wht = totbach_wht / Total_wht,
    pctbach_hisp = totbach_hisp / Total_hisp,
    pctbach_asian = totbach_asian / Total_asian,
    moeagg = moeagg(cbind(MaleBachMOE, FemaleBachMOE, MaleGradProfMOE, FemaleGradProfMOE)),
    moeagg_blk = moeagg(cbind(MaleBachMOE_blk, FemaleBachMOE_blk)),
    moeagg_wht = moeagg(cbind(MaleBachMOE_wht, FemaleBachMOE_wht)),
    moeagg_hisp = moeagg(cbind(MaleBachMOE_hisp, FemaleBachMOE_hisp)),
    moeagg_asian = moeagg(cbind(MaleBachMOE_asian, FemaleBachMOE_asian)),
    moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctbach),
    moeprop_blk = moeprop(y = Total_blk, moex = moeagg_blk, moey = TotalMOE_blk, p = pctbach_blk),
    moeprop_wht = moeprop(y = Total_wht, moex = moeagg_wht, moey = TotalMOE_wht, p = pctbach_wht),
    # moeprop_asian = moeprop(y = Total_asian, moex = moeagg_asian, moey = TotalMOE_asian, p = pctbach_asian),
    moeprop_hisp = moeprop(y = Total_hisp, moex = moeagg_hisp, moey = TotalMOE_hisp, p = pctbach_hisp)
  ) %>%
  select(place, contains("pct")) %>%
  mutate(
    placenames = NA,
    placenames = ifelse(place == "103", "St. Tammany", placenames),
    placenames = ifelse(place == "051", "Jefferson", placenames),
    placenames = ifelse(place == "071", "Orleans", placenames),
    placenames = ifelse(place == "35380", "Metro", placenames),
    placenames = ifelse(place == "1", "U.S.", placenames)
  ) %>%
  mutate(place.fac = factor(.$placenames, levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
  select(-place, -placenames) %>%
  pivot_longer(-place.fac, names_to = "var", values_to = "val")

bach.race_stattest.data <- bachRaw_exp %>%
  mutate(
    totbach = MaleBach + FemaleBach + MaleGradProf + FemaleGradProf,
    totbach_blk = MaleBach_blk + FemaleBach_blk,
    totbach_wht = MaleBach_wht + FemaleBach_wht,
    totbach_hisp = MaleBach_hisp + FemaleBach_hisp,
    totbach_asian = MaleBach_asian + FemaleBach_asian,
    pctbach = totbach / Total,
    pctbach_blk = totbach_blk / Total_blk,
    pctbach_wht = totbach_wht / Total_wht,
    pctbach_hisp = totbach_hisp / Total_hisp,
    pctbach_asian = totbach_asian / Total_asian,
    moeagg = moeagg(cbind(MaleBachMOE, FemaleBachMOE, MaleGradProfMOE, FemaleGradProfMOE)),
    moeagg_blk = moeagg(cbind(MaleBachMOE_blk, FemaleBachMOE_blk)),
    moeagg_wht = moeagg(cbind(MaleBachMOE_wht, FemaleBachMOE_wht)),
    moeagg_hisp = moeagg(cbind(MaleBachMOE_hisp, FemaleBachMOE_hisp)),
    moeagg_asian = moeagg(cbind(MaleBachMOE_asian, FemaleBachMOE_asian)),
    moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctbach),
    moeprop_blk = moeprop(y = Total_blk, moex = moeagg_blk, moey = TotalMOE_blk, p = pctbach_blk),
    moeprop_wht = moeprop(y = Total_wht, moex = moeagg_wht, moey = TotalMOE_wht, p = pctbach_wht),
    moeprop_asian = moeprop(y = Total_asian, moex = moeagg_asian, moey = TotalMOE_asian, p = pctbach_asian),
    moeprop_hisp = moeprop(y = Total_hisp, moex = moeagg_hisp, moey = TotalMOE_hisp, p = pctbach_hisp)
  )
bach.race_stattest <- bach.race_stattest.data %>%
  mutate(
    sig_wht_blk = stattest(x = pctbach_wht, moex = moeprop_wht, y = pctbach_blk, moey = moeprop_blk),
    sig_wht_hisp = stattest(x = pctbach_wht, moex = moeprop_wht, y = pctbach_hisp, moey = moeprop_hisp),
    sig_wht_asian = stattest(x = pctbach_wht, moex = moeprop_wht, y = pctbach_asian, moey = moeprop_asian),
    sigall_wht = stattest(x = pctbach_wht, moex = moeprop_wht, y = pctbach, moey = moeprop),
    sig_blk_hisp = stattest(x = pctbach_blk, moex = moeprop_blk, y = pctbach_hisp, moey = moeprop_hisp),
    sig_blk_asian = stattest(x = pctbach_blk, moex = moeprop_blk, y = pctbach_asian, moey = moeprop_asian),
    sigall_blk = stattest(x = pctbach_blk, moex = moeprop_blk, y = pctbach, moey = moeprop),
    sig_hisp_asian = stattest(x = pctbach_hisp, moex = moeprop_hisp, y = pctbach_asian, moey = moeprop_asian),
    sigall_hisp = stattest(x = pctbach_hisp, moex = moeprop_hisp, y = pctbach, moey = moeprop),
    sigall_asian = stattest(x = pctbach_asian, moex = moeprop_asian, y = pctbach, moey = moeprop)
  )

bach_stat_all <- bach.race_stattest %>%
  select(place, placename, (contains("sig") & contains("all")), contains("pct")) %>%
  pivot_longer(cols = contains("pct"), names_to = "race", values_to = "val") %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_all") %>%
  group_by(place, placename) %>%
  mutate(
    race = case_when(
      race == "pctbach" ~ "All",
      grepl("asian", race) & grepl("asian", var) ~ "Asian",
      grepl("blk", race) & grepl("blk", var) ~ "Black",
      grepl("hisp", race) & grepl("hisp", var) ~ "Hispanic, any race",
      grepl("wht", race) & grepl("wht", var) ~ "White, non-Hispanic"
    ),
    placename = case_when(
      placename == "New Orleans Metro Area" ~ "Metro",
      T ~ placename
    )
  ) %>%
  na.omit()
bach_stat_all$stat_all[bach_stat_all$race == "All"] <- "yes"
bach_stat_all <- bach_stat_all %>%
  group_by(place, placename, race) %>%
  mutate(val_lab = paste0(round.off(val * 100), "%")) %>%
  select(-var, -placename) %>%
  unique()

bach_stat_race <- bach.race_stattest %>%
  select(place, placename, (contains("sig") & !contains("all"))) %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_race") %>%
  select(-var) %>%
  group_by(place, placename) %>%
  mutate(
    placename = case_when(
      placename == "New Orleans Metro Area" ~ "Metro",
      T ~ placename
    ),
    placename = case_when(
      "no" %in% stat_race ~ paste0(placename, "*"),
      T ~ placename
    )
  ) %>%
  select(-stat_race) %>%
  unique()
bach_with_stats <- bach_stat_all %>%
  left_join(bach_stat_race, by = "place") %>%
  unique() %>%
  filter(race != "All") %>%
  mutate(
    placename.fac = factor(placename.y, levels = c("Orleans", "Jefferson*", "St. Tammany*", "Metro*", "United States")),
    var.fac = factor(race, levels = c("Black", "White, non-Hispanic", "Asian", "Hispanic, any race"))
  )


bach.totals <- bach_stat_all %>%
  left_join(bach_stat_race, by = "place") %>%
  unique() %>%
  filter(race == "All") %>%
  mutate(
    var.fac = factor(race, levels = c("Black", "White,\nnon-Hispanic", "Asian", "Hispanic,\nany race")),
    placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "St. Tammany*", "Metro", "United States"))
  )

bach.race <- bach_exp %>%
  filter(var != "pctbach") %>%
  mutate(
    var = ifelse(grepl("asian", var), "Asian", var),
    var = ifelse(grepl("blk", var), "Black", var),
    var = ifelse(grepl("hisp", var), "Hispanic,\nany race", var),
    var = ifelse(grepl("wht", var), "White,\nnon-Hispanic", var)
  ) %>%
  mutate(var.fac = factor(.$var, levels = c("Black", "White,\nnon-Hispanic", "Asian", "Hispanic,\nany race")))

bach.totals_CSV <- bach.totals %>%
  ungroup() %>%
  select(placename.x, race, val) %>%
  mutate(placename.x = ifelse(placename.x == "United States", "U.S.", placename.x)) %>%
  pivot_wider(names_from = placename.x, values_from = val)

bach.race_CSV <- bach.race %>%
  select(race = var, place.fac, val) %>%
  pivot_wider(names_from = place.fac, values_from = val) %>%
  rbind(bach.totals_CSV)
write.csv(bach.race_CSV, "outputs/spreadsheets/bachrace.csv")
storage_write_csv(bach.race_CSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/bachrace.csv"))


# Historical Educational Attainment Line Chart

EduAtt <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/educationalAttainment_byrace.csv")
EduAtt.hist <- EduAtt %>%
  mutate(var = ifelse(var == "White,\r\nnon-Hispanic", "White,\nnon-Hispanic", ifelse(var == "Hispanic,\r\nany race", "Hispanic,\nany race", var))) %>%
  mutate(var.fac = factor(.$var, levels = c("All", "Black", "White,\nnon-Hispanic", "Hispanic,\nany race"))) %>%
  bind_rows(bach.race %>%
    filter(place.fac == "Orleans", var != "Asian") %>% select(-place.fac) %>% mutate(year = 2022))

bachvars10 <- c(
  "C15002_001E", "C15002_001M", "C15002_008E", "C15002_008M", "C15002_009E", "C15002_009M", "C15002_016E", "C15002_016M",
  "C15002_017E", "C15002_017M",
  "C15002B_001E", "C15002B_001M", "C15002B_006E", "C15002B_006M", "C15002B_011E", "C15002B_011M",
  "C15002D_001E", "C15002D_001M", "C15002D_006E", "C15002D_006M", "C15002D_011E", "C15002D_011M",
  "C15002H_001E", "C15002H_001M", "C15002H_006E", "C15002H_006M", "C15002H_011E", "C15002H_011M",
  "C15002I_001E", "C15002I_001M", "C15002I_006E", "C15002I_006M", "C15002I_011E", "C15002I_011M"
)
bachnames10 <- c(
  "Total", "TotalMOE", "MaleBach", "MaleBachMOE", "MaleGradProf", "MaleGradProfMOE", "FemaleBach",
  "FemaleBachMOE", "FemaleGradProf", "FemaleGradProfMOE",
  "Total_blk", "TotalMOE_blk", "MaleBach_blk", "MaleBachMOE_blk", "FemaleBach_blk",
  "FemaleBachMOE_blk",
  "Total_asian", "TotalMOE_asian", "MaleBach_asian", "MaleBachMOE_asian", "FemaleBach_asian",
  "FemaleBachMOE_asian",
  "Total_wht", "TotalMOE_wht", "MaleBach_wht", "MaleBachMOE_wht", "FemaleBach_wht",
  "FemaleBachMOE_wht",
  "Total_hisp", "TotalMOE_hisp", "MaleBach_hisp", "MaleBachMOE_hisp", "FemaleBach_hisp",
  "FemaleBachMOE_hisp"
)
Bach10_raw <- wholivesdatapull(bachvars10, bachnames10, year = 2010)
Bach10MOE <- Bach10_raw %>%
  filter(place == "071") %>%
  mutate(
    year = 2010,
    pctTotalBach = (MaleBach + FemaleBach + MaleGradProf + FemaleGradProf) / Total,
    pctWhiteBach = (MaleBach_wht + FemaleBach_wht) / Total_wht,
    pctBlackBach = (MaleBach_blk + FemaleBach_blk) / Total_blk,
    pctHispBach = (MaleBach_hisp + FemaleBach_hisp) / Total_hisp,
    TotalBachmoeagg = moeagg(cbind(MaleBachMOE, FemaleBachMOE, MaleGradProfMOE, FemaleGradProfMOE)),
    Whitemoeagg = moeagg(cbind(MaleBachMOE_wht, FemaleBachMOE_wht)),
    Blackmoeagg = moeagg(cbind(MaleBachMOE_blk, FemaleBachMOE_blk)),
    Hispmoeagg = moeagg(cbind(MaleBachMOE_hisp, FemaleBachMOE_hisp)),
    Totalmoeprop = moeprop(y = Total, moex = TotalBachmoeagg, moey = TotalMOE, p = pctTotalBach),
    Whitemoeprop = moeprop(y = Total_wht, moex = Whitemoeagg, moey = TotalMOE_wht, p = pctWhiteBach),
    Blackmoeprop = moeprop(y = Total_blk, moex = Blackmoeagg, moey = TotalMOE_blk, p = pctBlackBach),
    Hispmoeprop = moeprop(y = Total_hisp, moex = Hispmoeagg, moey = TotalMOE_hisp, p = pctHispBach)
  ) %>%
  select(contains("moeprop")) %>%
  pivot_longer(everything(), names_to = "var", values_to = "moe2010") %>%
  mutate(race = case_when(
    var == "Totalmoeprop" ~ "All",
    var == "Blackmoeprop" ~ "Black",
    var == "Whitemoeprop" ~ "White,\nnon-Hispanic",
    var == "Hispmoeprop" ~ "Hispanic,\nany race"
  )) %>%
  select(-var)

Bach00Raw <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds151_2000_county.csv")
Bach00Wht <- wholivesdatapull(
  variables = c("P001001", "P148I001", "P148I008", "P148I009", "P148I016", "P148I017"),
  names = c("Totalpop2000", "TotalWhite2000", "MaleBach", "MaleGradProf", "FemaleBach", "FemaleGradProf"),
  censusname = "dec/sf3",
  year = 2000
) %>%
  mutate(
    WhiteBach = MaleBach + MaleGradProf + FemaleBach + FemaleGradProf,
    WhiteBachMOE = moe2000(WhiteBach, Totalpop2000, designfac = 1.2),
    TotalWhiteMOE = moe2000(TotalWhite2000, Totalpop2000, designfac = 1.2), # is this the correct way to do this for whites 25+? Do I use race/eth designfac? But it's for educational attainment pop.
    pctWhiteBach = WhiteBach / TotalWhite2000,
    WhiteBachmoeprop = moeprop(y = TotalWhite2000, moex = WhiteBachMOE, moey = TotalWhiteMOE, p = pctWhiteBach)
  ) %>%
  filter(place == "071") %>%
  select(pctWhiteBach, WhiteBachmoeprop)

Bach00MOE <- Bach00Raw %>%
  filter(STATEA == "22" & COUNTYA == "071") %>% # this one is by sex
  transmute(
    year = 2000,
    totBach = GRW006 + GRW007 + GRW013 + GRW014 + GRW020 + GRW021 + GRW027 + GRW028 + GRW034 + GRW035 + GRW041 + GRW042 + GRW048 + GRW049 + GRW055 + GRW056 + GRW062 + GRW063 + GRW069 + GRW070 + GRW076 + GRW077 + GRW083 + GRW084 + GRW090 + GRW091 + GRW097 + GRW098,
    totpop = sum(c_across(GRW001:GRW098), na.rm = T),
    totBachMOE = moe2000(est = totBach, n = 484674, designfac = 1.2),
    totpopMOE = moe2000(est = totpop, n = 484674, designfac = 1.2),
    # WhiteBach = GRW006 + GRW007 + GRW013 + GRW014,
    # WhiteBachMOE = moe2000(est = WhiteBach, n = 484674, designfac = 1.2),
    # totWhite = sum(c_across(GRW001:GRW014), na.rm = T), #adding all White adults 25+
    # totWhiteMOE = moe2000(est = totWhite, n = 484674, designfac = 1.2),
    BlackBach = GRW020 + GRW021 + GRW027 + GRW028,
    BlackBachMOE = moe2000(est = BlackBach, n = 484674, designfac = 1.2),
    totBlack = sum(c_across(GRW015:GRW028), na.rm = T),
    totBlackMOE = moe2000(est = totBlack, n = 484674, designfac = 1.2),
    HispBach = GRZ006 + GRZ007 + GRZ013 + GRZ014,
    HispBachMOE = moe2000(est = HispBach, n = 484674, designfac = 1.2),
    totHisp = sum(c_across(GRZ001:GRZ014), na.rm = T),
    totHispMOE = moe2000(est = totHisp, n = 484674, designfac = 1.2),
    pctTotalBach = totBach / totpop,
    # pctWhiteBach = WhiteBach / totWhite,
    pctBlackBach = BlackBach / totBlack,
    pctHispBach = HispBach / totHisp,
    Totalmoeprop = moeprop(totpop, totBachMOE, totpopMOE, pctTotalBach),
    # WhiteBachmoeprop = moeprop(totWhite,WhiteBachMOE,totWhiteMOE,pctWhiteBach),
    BlackBachmoeprop = moeprop(totBlack, BlackBachMOE, totBlackMOE, pctBlackBach),
    HispBachmoeprop = moeprop(totHisp, HispBachMOE, totHispMOE, pctHispBach)
  ) %>%
  cbind(Bach00Wht) %>%
  select(contains("moeprop")) %>%
  pivot_longer(everything(), names_to = "var", values_to = "moe2000") %>%
  mutate(race = case_when(
    var == "Totalmoeprop" ~ "All",
    var == "BlackBachmoeprop" ~ "Black",
    var == "WhiteBachmoeprop" ~ "White,\nnon-Hispanic",
    var == "HispBachmoeprop" ~ "Hispanic,\nany race"
  )) %>%
  select(-var)

bach.hist_stattest <- left_join(Bach10MOE, Bach00MOE) %>%
  left_join(EduAtt.hist %>% filter(year == 2000) %>% transmute(est2000 = val, race = var)) %>%
  left_join(EduAtt.hist %>% filter(year == 2010) %>% transmute(est2010 = val, race = var)) %>%
  left_join(bach.race_stattest.data %>% filter(place == "071") %>% select(moeprop, moeprop_blk, moeprop_wht, moeprop_hisp) %>%
    pivot_longer(everything(), names_to = "var", values_to = "moe2022") %>%
    mutate(race = case_when(
      var == "moeprop" ~ "All",
      var == "moeprop_blk" ~ "Black",
      var == "moeprop_wht" ~ "White,\nnon-Hispanic",
      var == "moeprop_hisp" ~ "Hispanic,\nany race"
    )) %>%
    select(-var)) %>%
  left_join(bach.race_stattest.data %>% filter(place == "071") %>% select(pctbach, pctbach_blk, pctbach_wht, pctbach_hisp) %>%
    pivot_longer(everything(), names_to = "var", values_to = "est2022") %>%
    mutate(race = case_when(
      var == "pctbach" ~ "All",
      var == "pctbach_blk" ~ "Black",
      var == "pctbach_wht" ~ "White,\nnon-Hispanic",
      var == "pctbach_hisp" ~ "Hispanic,\nany race"
    )) %>%
    select(-var)) %>%
  mutate(
    sig_00_10 = stattest(est2000, moe2000, est2010, moe2010),
    sig_00_21 = stattest(est2000, moe2000, est2022, moe2022),
    sig_10_21 = stattest(est2010, moe2010, est2022, moe2022)
  ) %>%
  select(race, contains("sig"))
EduAtt.hist <- EduAtt.hist %>%
  left_join(bach.hist_stattest, by = c("var" = "race")) %>%
  mutate(val_lab = case_when(
    (sig_00_10 == "no" | sig_00_21 == "no" | sig_10_21 == "no") ~ "*",
    T ~ " "
  ))
EduAtt.hist_csv <- EduAtt.hist %>%
  select(year, race = var, val) %>%
  filter(year != 2016) %>%
  pivot_wider(names_from = year, values_from = val)
write.csv(EduAtt.hist_csv, "outputs/spreadsheets/EduAtthist.csv")
storage_write_csv(EduAtt.hist_csv, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/EduAtthist.csv"))

### stat test ###

bach.race_stattest.data <- bachRaw_exp %>%
  mutate(
    totbach = MaleBach + FemaleBach + MaleGradProf + FemaleGradProf,
    totbach_blk = MaleBach_blk + FemaleBach_blk,
    totbach_wht = MaleBach_wht + FemaleBach_wht,
    totbach_hisp = MaleBach_hisp + FemaleBach_hisp,
    totbach_asian = MaleBach_asian + FemaleBach_asian,
    pctbach = totbach / Total,
    pctbach_blk = totbach_blk / Total_blk,
    pctbach_wht = totbach_wht / Total_wht,
    pctbach_hisp = totbach_hisp / Total_hisp,
    pctbach_asian = totbach_asian / Total_asian,
    moeagg = moeagg(cbind(MaleBachMOE, FemaleBachMOE, MaleGradProfMOE, FemaleGradProfMOE)),
    moeagg_blk = moeagg(cbind(MaleBachMOE_blk, FemaleBachMOE_blk)),
    moeagg_wht = moeagg(cbind(MaleBachMOE_wht, FemaleBachMOE_wht)),
    moeagg_hisp = moeagg(cbind(MaleBachMOE_hisp, FemaleBachMOE_hisp)),
    moeagg_asian = moeagg(cbind(MaleBachMOE_asian, FemaleBachMOE_asian)),
    moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctbach),
    moeprop_blk = moeprop(y = Total_blk, moex = moeagg_blk, moey = TotalMOE_blk, p = pctbach_blk),
    moeprop_wht = moeprop(y = Total_wht, moex = moeagg_wht, moey = TotalMOE_wht, p = pctbach_wht),
    # moeprop_asian = moeprop(y = Total_asian, moex = moeagg_asian, moey = TotalMOE_asian, p = pctbach_asian),
    moeprop_hisp = moeprop(y = Total_hisp, moex = moeagg_hisp, moey = TotalMOE_hisp, p = pctbach_hisp)
  )
bach.race_stattest.race <- bach.race_stattest.data %>%
  mutate(sig_wht_blk = stattest(x = pctbach_wht, moex = moeprop_wht, y = pctbach_blk, moey = moeprop_blk))
bach.race_stattest.geoEST <- bach.race_stattest.data %>%
  select(place, pctbach_blk, pctbach_wht) %>%
  pivot_longer(-place, names_to = "var", values_to = "val")
bach.race_stattest.geoMOE <- bach.race_stattest.data %>%
  select(place, moeprop_blk, moeprop_wht) %>%
  pivot_longer(-place, names_to = "var", values_to = "moe")
bach.race_stattest.geo <- left_join(bach.race_stattest.geoEST, bach.race_stattest.geoMOE, by = "place") %>%
  pivot_wider(names_from = place, values_from = c(val, moe)) %>%
  mutate(
    sig_no_us = stattest(x = val_071, moex = moe_071, y = val_1, moey = moe_1),
    sig_no_metro = stattest(x = val_071, moex = moe_071, y = val_35380, moey = moe_35380),
    sig_no_stt = stattest(x = val_071, moex = moe_071, y = val_103, moey = moe_103),
    sig_no_jeff = stattest(x = val_071, moex = moe_071, y = val_051, moey = moe_051)
  )


# Poverty

load("inputs/povRaw_exp.RData")
pov_exp <- povRaw_exp %>%
  mutate(
    pctpov = BelowPov / Total,
    pctpov_blk = BelowPov_blk / Total_blk,
    pctpov_wht = BelowPov_wht / Total_wht,
    pctpov_hisp = BelowPov_hisp / Total_hisp,
    pctpov_asian = BelowPov_asian / Total_asian,
    moeprop = moeprop(y = Total, moex = BelowPovMOE, moey = TotalMOE, p = pctpov),
    moeprop_blk = moeprop(y = Total_blk, moex = BelowPovMOE_blk, moey = TotalMOE_blk, p = pctpov_blk),
    moeprop_wht = moeprop(y = Total_wht, moex = BelowPovMOE_wht, moey = TotalMOE_wht, p = pctpov_wht),
    moeprop_hisp = moeprop(y = Total_hisp, moex = BelowPovMOE_hisp, moey = TotalMOE_hisp, p = pctpov_hisp),
    # moeprop_asian = moeprop(y = Total_asian, moex = BelowPovMOE_asian, moey = TotalMOE_asian, p = pctpov_asian)
  ) %>%
  select(place, contains("pct")) %>%
  mutate(
    placenames = NA,
    placenames = ifelse(place == "103", "St. Tammany", placenames),
    placenames = ifelse(place == "051", "Jefferson", placenames),
    placenames = ifelse(place == "071", "Orleans", placenames),
    placenames = ifelse(place == "35380", "Metro", placenames),
    placenames = ifelse(place == "1", "U.S.", placenames)
  ) %>%
  mutate(place.fac = factor(.$placenames, levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
  select(-place, -placenames) %>%
  pivot_longer(-place.fac, names_to = "var", values_to = "val")

pov_stattest.data <- povRaw_exp %>%
  replace(is.na(.), 0) %>%
  transmute(
    place = place,
    placename = placename,
    pctpov = BelowPov / Total,
    pctpov_blk = BelowPov_blk / Total_blk,
    pctpov_wht = BelowPov_wht / Total_wht,
    pctpov_hisp = BelowPov_hisp / Total_hisp,
    pctpov_asian = ifelse(!is.nan(BelowPov_asian / Total_asian), BelowPov_asian / Total_asian, 0),
    moeprop = moeprop(y = Total, moex = BelowPovMOE, moey = TotalMOE, p = pctpov),
    moeprop_blk = moeprop(y = Total_blk, moex = BelowPovMOE_blk, moey = TotalMOE_blk, p = pctpov_blk),
    moeprop_wht = moeprop(y = Total_wht, moex = BelowPovMOE_wht, moey = TotalMOE_wht, p = pctpov_wht),
    moeprop_hisp = moeprop(y = Total_hisp, moex = BelowPovMOE_hisp, moey = TotalMOE_hisp, p = pctpov_hisp),
    moeprop_asian = moeprop(y = Total_asian, moex = BelowPovMOE_asian, moey = TotalMOE_asian, p = pctpov_asian)
  )

pov_stattest <- povRaw_exp %>%
  replace(is.na(.), 0) %>%
  transmute(
    place = place,
    placename = placename,
    pctpov = BelowPov / Total,
    pctpov_blk = BelowPov_blk / Total_blk,
    pctpov_wht = BelowPov_wht / Total_wht,
    pctpov_hisp = BelowPov_hisp / Total_hisp,
    pctpov_asian = ifelse(!is.nan(BelowPov_asian / Total_asian), BelowPov_asian / Total_asian, 0),
    moeprop = moeprop(y = Total, moex = BelowPovMOE, moey = TotalMOE, p = pctpov),
    moeprop_blk = moeprop(y = Total_blk, moex = BelowPovMOE_blk, moey = TotalMOE_blk, p = pctpov_blk),
    moeprop_wht = moeprop(y = Total_wht, moex = BelowPovMOE_wht, moey = TotalMOE_wht, p = pctpov_wht),
    moeprop_hisp = moeprop(y = Total_hisp, moex = BelowPovMOE_hisp, moey = TotalMOE_hisp, p = pctpov_hisp),
    moeprop_asian = moeprop(y = Total_asian, moex = BelowPovMOE_asian, moey = TotalMOE_asian, p = pctpov_asian)
  ) %>%
  mutate(
    sig_wht_blk = stattest(x = pctpov_wht, moex = moeprop_wht, y = pctpov_blk, moey = moeprop_blk),
    sig_wht_hisp = stattest(x = pctpov_wht, moex = moeprop_wht, y = pctpov_hisp, moey = moeprop_hisp),
    sig_wht_asian = stattest(x = pctpov_wht, moex = moeprop_wht, y = pctpov_asian, moey = moeprop_asian),
    sigall_wht = stattest(x = pctpov_wht, moex = moeprop_wht, y = pctpov, moey = moeprop),
    sig_blk_hisp = stattest(x = pctpov_blk, moex = moeprop_blk, y = pctpov_hisp, moey = moeprop_hisp),
    sig_blk_asian = stattest(x = pctpov_blk, moex = moeprop_blk, y = pctpov_asian, moey = moeprop_asian),
    sigall_blk = stattest(x = pctpov_blk, moex = moeprop_blk, y = pctpov, moey = moeprop),
    sig_hisp_asian = stattest(x = pctpov_hisp, moex = moeprop_hisp, y = pctpov_asian, moey = moeprop_asian),
    sigall_hisp = stattest(x = pctpov_hisp, moex = moeprop_hisp, y = pctpov, moey = moeprop),
    sigall_asian = stattest(x = pctpov_asian, moex = moeprop_asian, y = pctpov, moey = moeprop)
  )

pov_stat_all <- pov_stattest %>%
  select(place, placename, (contains("sig") & contains("all")), contains("pct")) %>%
  pivot_longer(cols = contains("pct"), names_to = "race", values_to = "val") %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_all") %>%
  group_by(place, placename) %>%
  mutate(
    race = case_when(
      race == "pctpov" ~ "All",
      grepl("asian", race) & grepl("asian", var) ~ "Asian",
      grepl("blk", race) & grepl("blk", var) ~ "Black",
      grepl("hisp", race) & grepl("hisp", var) ~ "Hispanic, any race",
      grepl("wht", race) & grepl("wht", var) ~ "White, non-Hispanic"
    ),
    placename = case_when(
      placename == "New Orleans Metro Area" ~ "Metro",
      T ~ placename
    )
  ) %>%
  na.omit()
pov_stat_all$stat_all[pov_stat_all$race == "All"] <- "yes"
pov_stat_all <- pov_stat_all %>%
  group_by(place, placename, race) %>%
  mutate(val_lab = paste0(round.off(val * 100), "%")) %>%
  select(-var, -placename) %>%
  unique()


pov_stat_race <- pov_stattest %>%
  select(place, placename, (contains("sig") & !contains("all"))) %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_race") %>%
  select(-var) %>%
  group_by(place, placename) %>%
  mutate(
    placename = case_when(
      placename == "New Orleans Metro Area" ~ "Metro",
      T ~ placename
    ),
    placename = case_when(
      "no" %in% stat_race ~ paste0(placename, "*"),
      T ~ placename
    )
  ) %>%
  select(-stat_race) %>%
  unique()
pov_with_stats <- pov_stat_all %>%
  left_join(pov_stat_race, by = "place") %>%
  unique() %>%
  filter(race != "All") %>%
  mutate(
    placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "St. Tammany*", "Metro*", "United States")),
    var.fac = factor(race, levels = c("Black", "White, non-Hispanic", "Asian", "Hispanic, any race"))
  )


### Across geos pov bar chart ###
pov.totals <- pov_stat_all %>%
  left_join(pov_stat_race, by = "place") %>%
  unique() %>%
  filter(race == "All") %>%
  mutate(
    var.fac = factor(race, levels = c("Black", "White,\nnon-Hispanic", "Asian", "Hispanic,\nany race")),
    placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "St. Tammany*", "Metro*", "United States"))
  )

pov.race <- pov_exp %>%
  filter(var != "pctpov") %>%
  mutate(
    var = ifelse(grepl("asian", var), "Asian", var),
    var = ifelse(grepl("blk", var), "Black", var),
    var = ifelse(grepl("hisp", var), "Hispanic,\nany race", var),
    var = ifelse(grepl("wht", var), "White,\nnon-Hispanic", var)
  ) %>%
  mutate(var.fac = factor(.$var, levels = c("Black", "White,\nnon-Hispanic", "Asian", "Hispanic,\nany race")))

pov.totals_CSV <- pov.totals %>%
  ungroup() %>%
  select(placename.x, race, val) %>%
  mutate(placename.x = ifelse(placename.x == "United States", "U.S.", placename.x)) %>%
  pivot_wider(names_from = placename.x, values_from = val)

pov.race_CSV <- pov.race %>%
  select(race = var, place.fac, val) %>%
  pivot_wider(names_from = place.fac, values_from = val) %>%
  rbind(pov.totals_CSV)
write.csv(pov.race_CSV, "outputs/spreadsheets/povrace.csv")
storage_write_csv(pov.race_CSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/povrace.csv"))


totalPov <- read_csv("inputs/hist_pov.csv")
totalPov.hist <- totalPov %>%
  mutate(year = ifelse(year != 2010, year - 1, year)) %>%
  mutate(var = case_when(
    var == "White,\r\nnon-Hispanic" ~ "White,\nnon-Hispanic",
    var == "Hispanic,\r\nany race" ~ "Hispanic,\nany race",
    T ~ var
  )) %>%
  mutate(var.fac = factor(.$var, levels = c("All", "Black", "White,\nnon-Hispanic", "Hispanic,\nany race"))) %>%
  bind_rows(pov.race %>%
    filter(place.fac == "Orleans", var != "Asian") %>% select(-place.fac) %>% mutate(year = 2022))
pov00Wht <- wholivesdatapull(
  variables = c("P001001", "P148I001", "P159I002"),
  names = c("TotalPop2000", "TotalWhitepop", "Whitepov"),
  censusname = "dec/sf3",
  year = 2000
) %>%
  filter(place == "071") %>%
  mutate(
    pctWhitepov = Whitepov / TotalWhitepop,
    WhitepovMOE = moe2000(Whitepov, TotalPop2000, designfac = 1.5),
    TotalWhiteMOE = moe2000(TotalWhitepop, TotalPop2000, designfac = 2),
    Whitemoeprop = moeprop(y = TotalWhitepop, moex = WhitepovMOE, moey = TotalWhiteMOE, p = pctWhitepov)
  ) %>%
  select(pctWhitepov, Whitemoeprop)


pov00Raw <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0092_ds151_2000_county.csv")
pov00MOE <- pov00Raw %>%
  filter(STATEA == "22" & COUNTYA == "071") %>%
  transmute(
    year = 2000,
    totpov = GTV001 + GTV003 + GTV005 + GTV007 + GTV009 + GTV011 + GTV013,
    totpop = sum(c_across(GTV001:GTV014), na.rm = T),
    totpovMOE = moe2000(est = totpov, n = totpop, designfac = 1.3),
    totpopMOE = moe2000(est = totpop, n = totpop, designfac = 1.3),
    # Whitepov = GTV001,
    # totWhitepop = GTV001 + GTV002,
    # WhitepovMOE = moe2000(est = Whitepov, n = totpop, designfac = 1.3),
    # totWhitepopMOE = moe2000(est = totWhitepop, n = totpop, designfac = 1.3),
    Blackpov = GTV003,
    totBlackpop = GTV003 + GTV004,
    BlackpovMOE = moe2000(est = Blackpov, n = totpop, designfac = 1.3),
    totBlackpopMOE = moe2000(est = totBlackpop, n = totpop, designfac = 1.3),
    Hisppov = GTY001,
    totHisppop = GTY001 + GTY002,
    HisppovMOE = moe2000(est = Hisppov, n = totpop, designfac = 1.3),
    totHisppopMOE = moe2000(est = totHisppop, n = totpop, designfac = 1.3),
    pctTotalpov = totpov / totpop,
    # pctWhitepov = Whitepov / totWhitepop,
    pctBlackpov = Blackpov / totBlackpop,
    pctHisppov = Hisppov / totHisppop,
    Totalmoeprop = moeprop(y = totpop, moex = totpovMOE, moey = totpopMOE, p = pctTotalpov),
    # Whitemoeprop = moeprop(y = totWhitepop, moex = WhitepovMOE, moey = totWhitepopMOE, p = pctWhitepov),
    Blackmoeprop = moeprop(y = totBlackpop, moex = BlackpovMOE, moey = totBlackpopMOE, p = pctBlackpov),
    Hispmoeprop = moeprop(y = totHisppop, moex = HisppovMOE, moey = totHisppopMOE, p = pctHisppov)
  ) %>%
  cbind(pov00Wht) %>%
  select(contains("moeprop")) %>%
  pivot_longer(everything(), names_to = "var", values_to = "moe2000") %>%
  mutate(race = case_when(
    var == "Totalmoeprop" ~ "All",
    var == "Blackmoeprop" ~ "Black",
    var == "Whitemoeprop" ~ "White,\nnon-Hispanic",
    var == "Hispmoeprop" ~ "Hispanic,\nany race"
  )) %>%
  select(-var)
# getting 2010 and 2016 from ACS1
## View(load_variables(year = 2016, dataset = "acs1"))

povvars10 <- c(
  "C17001_001E", "C17001_001M", "C17001_002E", "C17001_002M",
  "C17001B_001E", "C17001B_001M", "C17001B_002E", "C17001B_002M",
  "C17001D_001E", "C17001D_001M", "C17001D_002E", "C17001D_002M",
  "C17001H_001E", "C17001H_001M", "C17001H_002E", "C17001H_002M",
  "C17001I_001E", "C17001I_001M", "C17001I_002E", "C17001I_002M"
)
povnames10 <- c(
  "Total", "TotalMOE", "BelowPov", "BelowPovMOE",
  "Total_blk", "TotalMOE_blk", "BelowPov_blk", "BelowPovMOE_blk",
  "Total_asian", "TotalMOE_asian", "BelowPov_asian", "BelowPovMOE_asian",
  "Total_wht", "TotalMOE_wht", "BelowPov_wht", "BelowPovMOE_wht",
  "Total_hisp", "TotalMOE_hisp", "BelowPov_hisp", "BelowPovMOE_hisp"
)
pov10_raw <- wholivesdatapull(povvars10, povnames10, year = 2010)

pov10 <- pov10_raw %>%
  filter(place == "071") %>%
  transmute(
    place = place,
    year = 2010,
    pctTotalpov = BelowPov / Total,
    pctWhitepov = BelowPov_wht / Total_wht,
    pctBlackpov = BelowPov_blk / Total_blk,
    pctHisppov = BelowPov_hisp / Total_hisp
  ) %>%
  pivot_longer(cols = pctTotalpov:pctHisppov, values_to = "val") %>%
  select(place, year, val, name)

pov10MOE <- pov10_raw %>%
  filter(place == "071") %>%
  transmute(
    year = 2010,
    place = place,
    pctTotalpov = BelowPov / Total,
    pctWhitepov = BelowPov_wht / Total_wht,
    pctBlackpov = BelowPov_blk / Total_blk,
    pctHisppov = BelowPov_hisp / Total_hisp,
    moeprop = moeprop(Total, BelowPovMOE, TotalMOE, pctTotalpov),
    Blackmoeprop = moeprop(Total_blk, BelowPovMOE_blk, TotalMOE_blk, pctBlackpov),
    Whitemoeprop = moeprop(Total_wht, BelowPovMOE_wht, TotalMOE_wht, pctWhitepov),
    Hispmoeprop = moeprop(Total_hisp, BelowPovMOE_hisp, TotalMOE_hisp, pctHisppov)
  ) %>%
  select(contains("moeprop")) %>%
  pivot_longer(everything(), names_to = "var", values_to = "moe2010") %>%
  mutate(race = case_when(
    var == "moeprop" ~ "All",
    var == "Blackmoeprop" ~ "Black",
    var == "Whitemoeprop" ~ "White,\nnon-Hispanic",
    var == "Hispmoeprop" ~ "Hispanic,\nany race"
  )) %>%
  select(-var)


pov.hist_stattest <- left_join(pov10MOE, pov00MOE) %>%
  left_join(totalPov.hist %>% filter(year == 1999) %>% transmute(est2000 = val, race = var)) %>%
  left_join(totalPov.hist %>% filter(year == 2010) %>% transmute(est2010 = val, race = var)) %>%
  left_join(totalPov.hist %>% filter(year == 2022) %>% transmute(est2022 = val, race = var)) %>%
  left_join(pov_stattest.data %>% filter(placename == "Orleans") %>% select(moeprop, moeprop_blk, moeprop_wht, moeprop_hisp) %>%
    pivot_longer(everything(), names_to = "var", values_to = "moe2022") %>%
    mutate(race = case_when(
      var == "moeprop" ~ "All",
      var == "moeprop_blk" ~ "Black",
      var == "moeprop_wht" ~ "White,\nnon-Hispanic",
      var == "moeprop_hisp" ~ "Hispanic,\nany race"
    )) %>%
    select(-var)) %>%
  mutate(
    sig_00_10 = stattest(est2000, moe2000, est2010, moe2010),
    sig_00_21 = stattest(est2000, moe2000, est2022, moe2022),
    sig_10_21 = stattest(est2010, moe2010, est2022, moe2022)
  ) %>%
  select(race, contains("sig")) %>%
  filter(race != "All")

totalPov.hist <- totalPov.hist %>%
  left_join(pov.hist_stattest, by = c("var" = "race")) %>%
  mutate(val_lab = case_when(
    (sig_00_10 == "no" | sig_00_21 == "no" | sig_10_21 == "no") ~ "*",
    T ~ " "
  ))
totalpov.hist_csv <- totalPov.hist %>%
  select(year, race = var, val) %>%
  filter(year != 2016) %>%
  pivot_wider(names_from = year, values_from = val)
write.csv(totalpov.hist_csv, "outputs/spreadsheets/totalpovhist.csv")
storage_write_csv(totalpov.hist_csv, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/totalpovhist.csv"))


### Child poverty ###
###
load("inputs/childpovRaw_exp.RData")
childpov_exp <- childpovRaw_exp %>%
  mutate(
    TotChildPov = BelowPovFemaleChild + BelowPovMaleChild + AbovePovFemaleChild + AbovePovMaleChild,
    moeaggtot = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE, AbovePovFemaleChildMOE, AbovePovMaleChildMOE)),
    TotBelowChildPov = BelowPovFemaleChild + BelowPovMaleChild,
    moeaggbelow = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE)),
    pctBelowChildPov = TotBelowChildPov / TotChildPov,
    moeprop = moeprop(y = TotChildPov, moex = moeaggbelow, moey = moeaggtot, p = pctBelowChildPov),
    TotChildPov_blk = BelowPovFemaleChild_blk + BelowPovMaleChild_blk + AbovePovFemaleChild_blk + AbovePovMaleChild_blk,
    moeaggtot_blk = moeagg(cbind(BelowPovFemaleChildMOE_blk, BelowPovMaleChildMOE_blk, AbovePovFemaleChildMOE_blk, AbovePovMaleChildMOE_blk)),
    TotBelowChildPov_blk = BelowPovFemaleChild_blk + BelowPovMaleChild_blk,
    moeaggbelow_blk = moeagg(cbind(BelowPovFemaleChildMOE_blk, BelowPovMaleChildMOE_blk)),
    pctBelowChildPov_blk = TotBelowChildPov_blk / TotChildPov_blk,
    moeprop_blk = moeprop(y = TotChildPov_blk, moex = moeaggbelow_blk, moey = moeaggtot_blk, p = pctBelowChildPov_blk),
    TotChildPov_wht = BelowPovFemaleChild_wht + BelowPovMaleChild_wht + AbovePovFemaleChild_wht + AbovePovMaleChild_wht,
    moeaggtot_wht = moeagg(cbind(BelowPovFemaleChildMOE_wht, BelowPovMaleChildMOE_wht, AbovePovFemaleChildMOE_wht, AbovePovMaleChildMOE_wht)),
    TotBelowChildPov_wht = BelowPovFemaleChild_wht + BelowPovMaleChild_wht,
    moeaggbelow_wht = moeagg(cbind(BelowPovFemaleChildMOE_wht, BelowPovMaleChildMOE_wht)),
    pctBelowChildPov_wht = TotBelowChildPov_wht / TotChildPov_wht,
    moeprop_wht = moeprop(y = TotChildPov_wht, moex = moeaggbelow_wht, moey = moeaggtot_wht, p = pctBelowChildPov_wht),
    TotChildPov_hisp = BelowPovFemaleChild_hisp + BelowPovMaleChild_hisp + AbovePovFemaleChild_hisp + AbovePovMaleChild_hisp,
    moeaggtot_hisp = moeagg(cbind(BelowPovFemaleChildMOE_hisp, BelowPovMaleChildMOE_hisp, AbovePovFemaleChildMOE_hisp, AbovePovMaleChildMOE_hisp)),
    TotBelowChildPov_hisp = BelowPovFemaleChild_hisp + BelowPovMaleChild_hisp,
    moeaggbelow_hisp = moeagg(cbind(BelowPovFemaleChildMOE_hisp, BelowPovMaleChildMOE_hisp)),
    pctBelowChildPov_hisp = TotBelowChildPov_hisp / TotChildPov_hisp,
    moeprop_hisp = moeprop(y = TotChildPov_hisp, moex = moeaggbelow_hisp, moey = moeaggtot_hisp, p = pctBelowChildPov_hisp),
    TotChildPov_asian = BelowPovFemaleChild_asian + BelowPovMaleChild_asian + AbovePovFemaleChild_asian + AbovePovMaleChild_asian,
    moeaggtot_asian = moeagg(cbind(BelowPovFemaleChildMOE_asian, BelowPovMaleChildMOE_asian, AbovePovFemaleChildMOE_asian, AbovePovMaleChildMOE_asian)),
    TotBelowChildPov_asian = BelowPovFemaleChild_asian + BelowPovMaleChild_asian,
    moeaggbelow_asian = moeagg(cbind(BelowPovFemaleChildMOE_asian, BelowPovMaleChildMOE_asian)),
    pctBelowChildPov_asian = TotBelowChildPov_asian / TotChildPov_asian,
    # moeprop_asian = moeprop(y=TotChildPov_asian, moex = moeaggbelow_asian, moey = moeaggtot_asian, p=pctBelowChildPov_asian)
  ) %>%
  select(place, contains("pct")) %>%
  mutate(
    placenames = NA,
    placenames = ifelse(place == "103", "St. Tammany", placenames),
    placenames = ifelse(place == "051", "Jefferson", placenames),
    placenames = ifelse(place == "071", "Orleans", placenames),
    placenames = ifelse(place == "35380", "Metro", placenames),
    placenames = ifelse(place == "1", "U.S.", placenames)
  ) %>%
  mutate(place.fac = factor(.$placenames, levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
  select(-place, -placenames) %>%
  pivot_longer(-place.fac, names_to = "var", values_to = "val")

### stat test ###


childpov_stattest.data <- childpovRaw_exp %>%
  replace(is.na(.), 0) %>%
  transmute(
    place = place,
    placename = placename,
    TotChildPov = BelowPovFemaleChild + BelowPovMaleChild + AbovePovFemaleChild + AbovePovMaleChild,
    moeaggtot = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE, AbovePovFemaleChildMOE, AbovePovMaleChildMOE)),
    TotBelowChildPov = BelowPovFemaleChild + BelowPovMaleChild,
    moeaggbelow = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE)),
    pctBelowChildPov = TotBelowChildPov / TotChildPov,
    moeprop = moeprop(y = TotChildPov, moex = moeaggbelow, moey = moeaggtot, p = pctBelowChildPov),
    TotChildPov_blk = BelowPovFemaleChild_blk + BelowPovMaleChild_blk + AbovePovFemaleChild_blk + AbovePovMaleChild_blk,
    moeaggtot_blk = moeagg(cbind(BelowPovFemaleChildMOE_blk, BelowPovMaleChildMOE_blk, AbovePovFemaleChildMOE_blk, AbovePovMaleChildMOE_blk)),
    TotBelowChildPov_blk = BelowPovFemaleChild_blk + BelowPovMaleChild_blk,
    moeaggbelow_blk = moeagg(cbind(BelowPovFemaleChildMOE_blk, BelowPovMaleChildMOE_blk)),
    pctBelowChildPov_blk = TotBelowChildPov_blk / TotChildPov_blk,
    moeprop_blk = moeprop(y = TotChildPov_blk, moex = moeaggbelow_blk, moey = moeaggtot_blk, p = pctBelowChildPov_blk),
    TotChildPov_wht = BelowPovFemaleChild_wht + BelowPovMaleChild_wht + AbovePovFemaleChild_wht + AbovePovMaleChild_wht,
    moeaggtot_wht = moeagg(cbind(BelowPovFemaleChildMOE_wht, BelowPovMaleChildMOE_wht, AbovePovFemaleChildMOE_wht, AbovePovMaleChildMOE_wht)),
    TotBelowChildPov_wht = BelowPovFemaleChild_wht + BelowPovMaleChild_wht,
    moeaggbelow_wht = moeagg(cbind(BelowPovFemaleChildMOE_wht, BelowPovMaleChildMOE_wht)),
    pctBelowChildPov_wht = TotBelowChildPov_wht / TotChildPov_wht,
    moeprop_wht = moeprop(y = TotChildPov_wht, moex = moeaggbelow_wht, moey = moeaggtot_wht, p = pctBelowChildPov_wht),
    TotChildPov_hisp = BelowPovFemaleChild_hisp + BelowPovMaleChild_hisp + AbovePovFemaleChild_hisp + AbovePovMaleChild_hisp,
    moeaggtot_hisp = moeagg(cbind(BelowPovFemaleChildMOE_hisp, BelowPovMaleChildMOE_hisp, AbovePovFemaleChildMOE_hisp, AbovePovMaleChildMOE_hisp)),
    TotBelowChildPov_hisp = BelowPovFemaleChild_hisp + BelowPovMaleChild_hisp,
    moeaggbelow_hisp = moeagg(cbind(BelowPovFemaleChildMOE_hisp, BelowPovMaleChildMOE_hisp)),
    pctBelowChildPov_hisp = TotBelowChildPov_hisp / TotChildPov_hisp,
    moeprop_hisp = moeprop(y = TotChildPov_hisp, moex = moeaggbelow_hisp, moey = moeaggtot_hisp, p = pctBelowChildPov_hisp),
    TotChildPov_asian = BelowPovFemaleChild_asian + BelowPovMaleChild_asian + AbovePovFemaleChild_asian + AbovePovMaleChild_asian,
    moeaggtot_asian = moeagg(cbind(BelowPovFemaleChildMOE_asian, BelowPovMaleChildMOE_asian, AbovePovFemaleChildMOE_asian, AbovePovMaleChildMOE_asian)),
    TotBelowChildPov_asian = BelowPovFemaleChild_asian + BelowPovMaleChild_asian,
    moeaggbelow_asian = moeagg(cbind(BelowPovFemaleChildMOE_asian, BelowPovMaleChildMOE_asian)),
    pctBelowChildPov_asian = ifelse(!is.nan(TotBelowChildPov_asian / TotChildPov_asian), TotBelowChildPov_asian / TotChildPov_asian, 0),
    moeprop_asian = moeprop(y = TotChildPov_asian, moex = moeaggbelow_asian, moey = moeaggtot_asian, p = pctBelowChildPov_asian)
  )
childpov_stattest <- childpov_stattest.data %>%
  mutate(
    sig_wht_blk = stattest(x = pctBelowChildPov_wht, moex = moeprop_wht, y = pctBelowChildPov_blk, moey = moeprop_blk),
    sig_wht_hisp = stattest(x = pctBelowChildPov_wht, moex = moeprop_wht, y = pctBelowChildPov_hisp, moey = moeprop_hisp),
    sig_wht_asian = stattest(x = pctBelowChildPov_wht, moex = moeprop_wht, y = pctBelowChildPov_asian, moey = moeprop_asian),
    sigall_wht = stattest(x = pctBelowChildPov_wht, moex = moeprop_wht, y = pctBelowChildPov, moey = moeprop),
    sig_blk_hisp = stattest(x = pctBelowChildPov_blk, moex = moeprop_blk, y = pctBelowChildPov_hisp, moey = moeprop_hisp),
    sig_blk_asian = stattest(x = pctBelowChildPov_blk, moex = moeprop_blk, y = pctBelowChildPov_asian, moey = moeprop_asian),
    sigall_blk = stattest(x = pctBelowChildPov_blk, moex = moeprop_blk, y = pctBelowChildPov, moey = moeprop),
    sig_hisp_asian = stattest(x = pctBelowChildPov_hisp, moex = moeprop_hisp, y = pctBelowChildPov_asian, moey = moeprop_asian),
    sigall_hisp = stattest(x = pctBelowChildPov_hisp, moex = moeprop_hisp, y = pctBelowChildPov, moey = moeprop),
    sigall_asian = stattest(x = pctBelowChildPov_asian, moex = moeprop_asian, y = pctBelowChildPov, moey = moeprop)
  )

###
childpov_stat_all <- childpov_stattest %>%
  select(place, placename, (contains("sig") & contains("all")), contains("pct")) %>%
  pivot_longer(cols = contains("pct"), names_to = "race", values_to = "val") %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_all") %>%
  group_by(place, placename) %>%
  mutate(
    race = case_when(
      race == "pctBelowChildPov" ~ "All",
      grepl("asian", race) & grepl("asian", var) ~ "Asian",
      grepl("blk", race) & grepl("blk", var) ~ "Black",
      grepl("hisp", race) & grepl("hisp", var) ~ "Hispanic, any race",
      grepl("wht", race) & grepl("wht", var) ~ "White, non-Hispanic"
    ),
    placename = case_when(
      placename == "New Orleans Metro Area" ~ "Metro",
      T ~ placename
    )
  ) %>%
  na.omit()
childpov_stat_all$stat_all[childpov_stat_all$race == "All"] <- "yes"
childpov_stat_all <- childpov_stat_all %>%
  group_by(place, placename, race) %>%
  mutate(val_lab = paste0(round.off(val * 100), "%")) %>%
  select(-var, -placename) %>%
  unique()


childpov_stat_race <- childpov_stattest %>%
  select(place, placename, (contains("sig") & !contains("all"))) %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_race") %>%
  select(-var) %>%
  group_by(place, placename) %>%
  mutate(
    placename = case_when(
      placename == "New Orleans Metro Area" ~ "Metro",
      T ~ placename
    ),
    placename = case_when(
      "no" %in% stat_race ~ paste0(placename, "*"),
      T ~ placename
    )
  ) %>%
  select(-stat_race) %>%
  unique()
childpov_with_stats <- childpov_stat_all %>%
  left_join(childpov_stat_race, by = "place") %>%
  unique() %>%
  filter(race != "All") %>%
  mutate(
    placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "St. Tammany*", "Metro*", "United States*")),
    var.fac = factor(race, levels = c("Black", "White, non-Hispanic", "Asian", "Hispanic, any race"))
  )

###

### Across geos child pov bar chart ###
childpov.totals <- childpov_stat_all %>%
  left_join(childpov_stat_race, by = "place") %>%
  unique() %>%
  filter(race == "All") %>%
  mutate(
    var.fac = factor(race, levels = c("Black", "White,\nnon-Hispanic", "Asian", "Hispanic,\nany race")),
    placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "St. Tammany*", "Metro*", "United States*"))
  )

childpov.race <- childpov_exp %>%
  filter(var != "pctBelowChildPov") %>%
  mutate(
    var = ifelse(grepl("asian", var), "Asian", var),
    var = ifelse(grepl("blk", var), "Black", var),
    var = ifelse(grepl("hisp", var), "Hispanic,\nany race", var),
    var = ifelse(grepl("wht", var), "White,\nnon-Hispanic", var)
  ) %>%
  mutate(var.fac = factor(.$var, levels = c("Black", "White,\nnon-Hispanic", "Asian", "Hispanic,\nany race")))

childpov.totals_CSV <- childpov.totals %>%
  ungroup() %>%
  select(placename.x, race, val) %>%
  mutate(placename.x = ifelse(placename.x == "United States", "U.S.", placename.x)) %>%
  pivot_wider(names_from = placename.x, values_from = val)

childpov.race_CSV <- childpov.race %>%
  select(race = var, place.fac, val) %>%
  pivot_wider(names_from = place.fac, values_from = val) %>%
  rbind(childpov.totals_CSV)
write.csv(childpov.race_CSV, "outputs/spreadsheets/childpovrace.csv")
storage_write_csv(childpov.race_CSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/childpovrace"))


### Historical child pov line chart ###
childPov.hist <- childPovProspInd %>%
  mutate(year = ifelse(Year != 2010, Year - 1, Year)) %>%
  rename(val = childPov) %>%
  mutate(
    var = Race,
    var = ifelse(grepl("Bla", var), "Black", var),
    var = ifelse(grepl("Lat", var), "Hispanic,\nany race", var),
    var = ifelse(grepl("Whi", var), "White,\nnon-Hispanic", var)
  ) %>%
  select(-Geography, -Race) %>% ## some of the tables include US in the geography, so you'll have to filter those
  bind_rows(childpov_exp %>%
    filter(
      place.fac == "Orleans",
      !grepl("asian", var)
    ) %>% ## remove asian numbers
    select(-place.fac) %>%
    mutate(
      Year = 2022, ## for the drafts, this is the wrong year, but will make the x axis the correct length for when we update the numbers
      var = ifelse(var == "pctBelowChildPov", "All", var),
      var = ifelse(grepl("blk", var), "Black", var),
      var = ifelse(grepl("hisp", var), "Hispanic,\nany race", var),
      var = ifelse(grepl("wht", var), "White,\nnon-Hispanic", var)
    )) %>%
  mutate(var.fac = factor(.$var, levels = c("All", "Black", "White,\nnon-Hispanic", "Hispanic,\nany race")))

childPov.histMOE <- data.frame(
  race = c("Black", "White,\nnon-Hispanic", "Hispanic,\nany race", "All"),
  se2010 = c(0.023372212, 0.016043476, 0.104259483, 0.021288336),
  se2000 = c(0.005216453, 0.006067795, 0.026554044, 0.004610543)
) %>%
  mutate(
    moe2010 = se2010 * 1.645,
    moe2000 = se2000 * 1.645
  )

childpov.hist_stattest <- left_join(childPov.histMOE, (childPov.hist %>%
  filter(Year == 2000) %>%
  transmute(est2000 = val, race = var)), by = "race") %>%
  left_join(childPov.hist %>% filter(Year == 2010) %>% transmute(est2010 = val, race = var)) %>%
  left_join(childPov.hist %>% filter(Year == 2022) %>% transmute(est2022 = val, race = var)) %>%
  left_join((childpov_stattest.data %>%
    filter(placename == "Orleans") %>%
    select(moeprop, moeprop_blk, moeprop_wht, moeprop_hisp) %>%
    pivot_longer(everything(), names_to = "var", values_to = "moe2022") %>%
    mutate(race = case_when(
      var == "moeprop" ~ "All",
      var == "moeprop_blk" ~ "Black",
      var == "moeprop_wht" ~ "White,\nnon-Hispanic",
      var == "moeprop_hisp" ~ "Hispanic,\nany race"
    )))) %>%
  select(-var) %>%
  mutate(
    sig_00_10 = stattest(est2000, moe2000, est2010, moe2010),
    sig_00_21 = stattest(est2000, moe2000, est2022, moe2022),
    sig_10_21 = stattest(est2010, moe2010, est2022, moe2022)
  ) %>%
  select(race, contains("sig"))

childPov.hist <- childPov.hist %>%
  left_join(childpov.hist_stattest, by = c("var" = "race")) %>%
  filter(var != "All") %>%
  mutate(val_lab = case_when(
    (sig_00_10 == "no" | sig_00_21 == "no" | sig_10_21 == "no") ~ "*",
    T ~ " "
  ))

childpov.hist_csv <- childPov.hist %>%
  select(Year, race = var, val) %>%
  filter(Year != 2016) %>%
  pivot_wider(names_from = Year, values_from = val)
write.csv(childpov.hist_csv, "outputs/spreadsheets/childpovhist.csv")
storage_write_csv(childpov.hist_csv, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/childpovhist.csv"))


# Homeownership

load("inputs/hoRaw_exp.RData")
ho_exp <- hoRaw_exp %>%
  mutate(
    Ownerpct = Owner / Total,
    Ownermoeprop = moeprop(y = Total, moex = OwnerMOE, moey = TotalMOE, p = Ownerpct),
    Ownerpct_blk = Owner_blk / Total_blk,
    Ownermoeprop_blk = moeprop(y = Total_blk, moex = OwnerMOE_blk, moey = TotalMOE_blk, p = Ownerpct_blk),
    Ownerpct_wht = Owner_wht / Total_wht,
    Ownermoeprop_wht = moeprop(y = Total_wht, moex = OwnerMOE_wht, moey = TotalMOE_wht, p = Ownerpct_wht),
    Ownerpct_hisp = Owner_hisp / Total_hisp,
    Ownermoeprop_hisp = moeprop(y = Total_hisp, moex = OwnerMOE_hisp, moey = TotalMOE_hisp, p = Ownerpct_hisp),
    Ownerpct_asian = Owner_asian / Total_asian,
    # Ownermoeprop_asian = moeprop(y=Total_asian,moex = OwnerMOE_asian,moey = TotalMOE_asian,p=Ownerpct_asian)
  ) %>%
  select(place, contains("pct")) %>%
  mutate(
    placenames = NA,
    placenames = ifelse(place == "103", "St. Tammany", placenames),
    placenames = ifelse(place == "051", "Jefferson", placenames),
    placenames = ifelse(place == "071", "Orleans", placenames),
    placenames = ifelse(place == "35380", "Metro", placenames),
    placenames = ifelse(place == "1", "U.S.", placenames)
  ) %>%
  mutate(place.fac = factor(.$placenames, levels = c("Orleans", "Jefferson", "St. Tammany", "Metro", "U.S."))) %>%
  select(-place, -placenames) %>%
  pivot_longer(-place.fac, names_to = "var", values_to = "val")

### Across geos homeownership bar chart ###

ho.race <- ho_exp %>%
  filter(var != "Ownerpct") %>%
  mutate(
    var = ifelse(grepl("asian", var), "Asian", var),
    var = ifelse(grepl("blk", var), "Black", var),
    var = ifelse(grepl("hisp", var), "Hispanic,\nany race", var),
    var = ifelse(grepl("wht", var), "White,\nnon-Hispanic", var)
  ) %>%
  mutate(var.fac = factor(.$var, levels = c("Black", "White,\nnon-Hispanic", "Asian", "Hispanic,\nany race")))


### Historical homeownership line chart ###
homeownership.hist <- homeownershipProspInd %>%
  filter(Geography == "New Orleans") %>%
  rename(val = pctHomeownership) %>%
  mutate(
    var = Race,
    var = ifelse(grepl("Bla", var), "Black", var),
    var = ifelse(grepl("Lat", var), "Hispanic,\nany race", var),
    var = ifelse(grepl("Whi", var), "White,\nnon-Hispanic", var)
  ) %>%
  select(-Geography, -Race) %>% ## some of the tables include US in the geography, so you'll have to filter those
  bind_rows(ho_exp %>%
    filter(
      place.fac == "Orleans",
      !grepl("asian", var)
    ) %>% ## remove asian numbers
    select(-place.fac) %>%
    mutate(
      Year = 2022, ## for the drafts, this is the wrong year, but will make the x axis the correct length for when we update the numbers
      var = ifelse(var == "Ownerpct", "All", var),
      var = ifelse(grepl("blk", var), "Black", var),
      var = ifelse(grepl("hisp", var), "Hispanic,\nany race", var),
      var = ifelse(grepl("wht", var), "White,\nnon-Hispanic", var)
    )) %>%
  mutate(var.fac = factor(.$var, levels = c("All", "Black", "White,\nnon-Hispanic", "Hispanic,\nany race")))

ho.hist_csv <- homeownership.hist %>%
  select(Year, race = var, val) %>%
  filter(Year != 2016) %>%
  pivot_wider(names_from = Year, values_from = val)
write.csv(ho.hist_csv, "outputs/spreadsheets/hohist.csv")
storage_write_csv(ho.hist_csv, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/hohist.csv"))



### stat test

ho_stattest.data <- hoRaw_exp %>%
  replace(is.na(.), 0) %>%
  transmute(
    place = place,
    placename = placename,
    Ownerpct = Owner / Total,
    Ownermoeprop = moeprop(y = Total, moex = OwnerMOE, moey = TotalMOE, p = Ownerpct),
    Ownerpct_blk = Owner_blk / Total_blk,
    Ownermoeprop_blk = moeprop(y = Total_blk, moex = OwnerMOE_blk, moey = TotalMOE_blk, p = Ownerpct_blk),
    Ownerpct_wht = Owner_wht / Total_wht,
    Ownermoeprop_wht = moeprop(y = Total_wht, moex = OwnerMOE_wht, moey = TotalMOE_wht, p = Ownerpct_wht),
    Ownerpct_hisp = Owner_hisp / Total_hisp,
    Ownermoeprop_hisp = moeprop(y = Total_hisp, moex = OwnerMOE_hisp, moey = TotalMOE_hisp, p = Ownerpct_hisp),
    Ownerpct_asian = Owner_asian / Total_asian,
    Ownermoeprop_asian = moeprop(y = Total_asian, moex = OwnerMOE_asian, moey = TotalMOE_asian, p = Ownerpct_asian)
  )

ho_stattest <- ho_stattest.data %>%
  mutate(
    sig_wht_blk = stattest(x = Ownerpct_wht, moex = Ownermoeprop_wht, y = Ownerpct_blk, moey = Ownermoeprop_blk),
    sig_wht_hisp = stattest(x = Ownerpct_wht, moex = Ownermoeprop_wht, y = Ownerpct_hisp, moey = Ownermoeprop_hisp),
    sig_wht_asian = stattest(x = Ownerpct_wht, moex = Ownermoeprop_wht, y = Ownerpct_asian, moey = Ownermoeprop_asian),
    sigall_wht = stattest(x = Ownerpct_wht, moex = Ownermoeprop_wht, y = Ownerpct, moey = Ownermoeprop),
    sig_blk_hisp = stattest(x = Ownerpct_blk, moex = Ownermoeprop_blk, y = Ownerpct_hisp, moey = Ownermoeprop_hisp),
    sig_blk_asian = stattest(x = Ownerpct_blk, moex = Ownermoeprop_blk, y = Ownerpct_asian, moey = Ownermoeprop_asian),
    sigall_blk = stattest(x = Ownerpct_blk, moex = Ownermoeprop_blk, y = Ownerpct, moey = Ownermoeprop),
    sig_hisp_asian = stattest(x = Ownerpct_hisp, moex = Ownermoeprop_hisp, y = Ownerpct_asian, moey = Ownermoeprop_asian),
    sigall_hisp = stattest(x = Ownerpct_hisp, moex = Ownermoeprop_hisp, y = Ownerpct, moey = Ownermoeprop),
    sigall_asian = stattest(x = Ownerpct_asian, moex = Ownermoeprop_asian, y = Ownerpct, moey = Ownermoeprop)
  )

ho_stat_all <- ho_stattest %>%
  select(place, placename, (contains("sig") & contains("all")), contains("pct")) %>%
  pivot_longer(cols = contains("pct"), names_to = "race", values_to = "val") %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_all") %>%
  group_by(place, placename) %>%
  mutate(
    race = case_when(
      race == "Ownerpct" ~ "All",
      grepl("asian", race) & grepl("asian", var) ~ "Asian",
      grepl("blk", race) & grepl("blk", var) ~ "Black",
      grepl("hisp", race) & grepl("hisp", var) ~ "Hispanic, any race",
      grepl("wht", race) & grepl("wht", var) ~ "White, non-Hispanic"
    ),
    placename = case_when(
      placename == "New Orleans Metro Area" ~ "Metro",
      T ~ placename
    )
  ) %>%
  na.omit()
ho_stat_all$stat_all[ho_stat_all$race == "All"] <- "yes"
ho_stat_all <- ho_stat_all %>%
  group_by(place, placename, race) %>%
  mutate(val_lab = paste0(round.off(val * 100), "%")) %>%
  select(-var, -placename) %>%
  unique()


ho_stat_race <- ho_stattest %>%
  select(place, placename, (contains("sig") & !contains("all"))) %>%
  pivot_longer(cols = contains("sig"), names_to = "var", values_to = "stat_race") %>%
  select(-var) %>%
  group_by(place, placename) %>%
  mutate(
    placename = case_when(
      placename == "New Orleans Metro Area" ~ "Metro",
      T ~ placename
    ),
    placename = case_when(
      "no" %in% stat_race ~ paste0(placename, "*"),
      T ~ placename
    )
  ) %>%
  select(-stat_race) %>%
  unique()
ho_with_stats <- ho_stat_all %>%
  left_join(ho_stat_race, by = "place") %>%
  unique() %>%
  filter(race != "All") %>%
  mutate(
    placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "St. Tammany*", "Metro*", "United States")),
    var.fac = factor(race, levels = c("Black", "White, non-Hispanic", "Asian", "Hispanic, any race"))
  )

ho.totals <- ho_stat_all %>%
  left_join(ho_stat_race, by = "place") %>%
  unique() %>%
  filter(race == "All") %>%
  mutate(
    var.fac = factor(race, levels = c("Black", "White,\nnon-Hispanic", "Asian", "Hispanic,\nany race")),
    placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "St. Tammany*", "Metro", "United States"))
  )

ho.totals_CSV <- ho.totals %>%
  ungroup() %>%
  select(placename.x, race, val) %>%
  mutate(placename.x = ifelse(placename.x == "United States", "U.S.", placename.x)) %>%
  pivot_wider(names_from = placename.x, values_from = val)

ho.race_CSV <- ho.race %>%
  select(race = var, place.fac, val) %>%
  unique() %>%
  pivot_wider(names_from = place.fac, values_from = val) %>%
  rbind(ho.totals_CSV)
write.csv(ho.race_CSV, "outputs/spreadsheets/horace.csv")
storage_write_csv(ho.race_CSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/horace.csv"))

###

### Across geos HO bar chart ###
ho.totals <- ho_stat_all %>%
  left_join(ho_stat_race, by = "place") %>%
  unique() %>%
  filter(race == "All") %>%
  mutate(
    var.fac = factor(race, levels = c("Black", "White,\nnon-Hispanic", "Asian", "Hispanic,\nany race")),
    placename.fac = factor(placename.y, levels = c("Orleans*", "Jefferson*", "St. Tammany*", "Metro", "United States"))
  )

ho.hist_stattest <-
  left_join((homeownership.hist %>% filter(Year == 2000) %>% transmute(est2000 = val, race = var)), (homeownership.hist %>% filter(Year == 2010) %>% transmute(est2010 = val, race = var))) %>%
  left_join((homeownership.hist %>% filter(Year == 2022) %>% transmute(est2022 = val, race = var))) %>%
  left_join((ho_stattest.data %>%
    filter(placename == "Orleans") %>%
    select(Ownermoeprop, Ownermoeprop_blk, Ownermoeprop_wht, Ownermoeprop_hisp) %>%
    pivot_longer(everything(), names_to = "var", values_to = "moe2022") %>%
    mutate(race = case_when(
      var == "Ownermoeprop" ~ "All",
      var == "Ownermoeprop_blk" ~ "Black",
      var == "Ownermoeprop_wht" ~ "White,\nnon-Hispanic",
      var == "Ownermoeprop_hisp" ~ "Hispanic,\nany race"
    ))) %>%
    select(-var)) %>%
  mutate(
    sig_00_21 = stattest(x = est2000, y = est2022, moey = moe2022),
    sig_10_21 = stattest(x = est2010, y = est2022, moey = moe2022)
  ) %>%
  filter(race != "All")

homeownership.hist <- homeownership.hist %>%
  filter(var != "All") %>%
  left_join(ho.hist_stattest, by = c("var" = "race")) %>%
  mutate(val_lab = case_when(
    (sig_00_21 == "no" | sig_10_21 == "no") ~ "*",
    T ~ " "
  ))

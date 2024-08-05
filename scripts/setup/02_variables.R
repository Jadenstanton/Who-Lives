mycensuskey <- "b6844db29933c9dce9e13fa37f1d015281001b95"

# CPI numbers, calculated in the nbhd profiles google sheet 'Neighborhood Analysis," documentation in Box
cpi04 <- 1.52
cpi99 <- 1.71

# the current/most recent year - the year the data you're updating represents
# TODO: Change all occurences of 'year' to 'YEAR'
# Change all global variables to be in all caps

YEAR <- as.numeric(format(Sys.Date(), "%Y")) - 2
YEAR_PEP <- as.numeric(format(Sys.Date(), "%Y")) - 1

YEAR.char <- toString(as.numeric(format(Sys.Date(), "%Y")) - 2)
YEAR_PEP.char <- toString(as.numeric(format(Sys.Date(), "%Y")) - 1)

CURRENT_YEAR <- as.numeric(format(Sys.Date(), "%Y"))



windowsFonts("Asap" = windowsFont("Asap"))

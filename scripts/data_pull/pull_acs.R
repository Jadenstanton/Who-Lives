############################################
# # ACS # #
############################################


# Total population change
## test: using the total pop numbers from our site

totalpop_metro <- read_xlsx("inputs/TheDataCenter_PopulationbyParish.xlsx")
totalpop_metro <- totalpop_metro %>%
  rename(year = `...1`) %>%
  mutate(year = str_replace(year, "Census ", ""),
         year = str_replace(year, "Estimate ", ""),
         year = as.numeric(year),
         
         metro_pop = `Metro Area total`)

save(totalpop_metro, file = "inputs/totalpop_metro.RData")

#Hispanic Origin

hispanvars <-c("B03001_001E","B03001_001M","B03001_002E","B03001_002M","B03001_003E","B03001_003M","B03001_004E","B03001_004M","B03001_005E","B03001_005M","B03001_006E","B03001_006M","B03001_007E","B03001_007M","B03001_008E","B03001_008M","B03001_009E","B03001_009M","B03001_010E","B03001_010M","B03001_011E","B03001_011M","B03001_012E","B03001_012M","B03001_013E","B03001_013M","B03001_014E","B03001_014M","B03001_015E","B03001_015M","B03001_016E","B03001_016M","B03001_027E","B03001_027M")
hispannames <-c("Total","TotalMOE","TotalNotHIsporLat","TotalNotHIsporLatMOE","TotalHisporLat","TotalHisporLatMOE","TotMex","TotMexMOE","TotPR","TotPRMOE","TotCuba","TotCubaMOE","TotDomin","TotDominMOE","TotCentrAm","TotCentrAmMOE","TotCostaR","TotCostaRMOE","TotGuat","TotGuatMOE","TotHond","TotHondMOE","TotNicarag","TotNicaragMOE","TotPanama","TotPanamaMOE","TotSalva","TotSalvaMOE","TotOtherCA","TotOtherCAMOE","TotSA","TotSAMOE","TotOtherHisporLat","TotOtherHisporLatMOE")
hispanRaw <- wholivesdatapull(hispanvars,hispannames)[-3,]
save(hispanRaw, file = "inputs/hispanRaw.RData") # -3 removes St. Tammany because it is not included in this analysis

#Households with own children under 18
hwcvars <- c('B11001_001E','B11001_001M','B11003_003E','B11003_003M','B11003_010E','B11003_010M','B11003_016E','B11003_016M')
hwcnames <- c("TotalHH", "TotalHHMOE","Married", "MarriedMOE", "MaleHH", "MaleHHMOE", "FemaleHH" ,"FemaleHHMOE")
hwcRaw <- wholivesdatapull(hwcvars, hwcnames)
save(hwcRaw, file = "inputs/hwcRaw.RData")

## Median age calculation

medagevars <- c("B01002_001E")
medagenames <- c("medianage")
medageRaw <- getCensus("acs/acs1",
                       2022,
                       key = "530ce361defc2c476e5b5d5626d224d8354b9b9a",
                       vars = medagevars,
                       region = "metropolitan statistical area/micropolitan statistical area:35380")
names(medageRaw) <- c("metro", "medage")
save(medageRaw, file = "inputs/medageRaw.RData")

#One-person households

singvars <- c('B11001_001E','B11001_001M','B11001_008E','B11001_008M')
singnames <- c("TotalHH","TotalMOE","SingleHH","SingleHHMOE")
singRaw <- wholivesdatapull(singvars, singnames)
save(singRaw, file = "inputs/singRaw.RData")


#Less than a high school degree, adults 25 and older

hsvars <- c('C15002_001E','C15002_001M','C15002_003E','C15002_003M','C15002_004E','C15002_004M','C15002_011E','C15002_011M','C15002_012E','C15002_012M')
hsnames <- c("Total", "TotalMOE", "Male9", "Male9MOE", "Male9to12", "Male9to12MOE", "Female9", "Female9MOE", "Female9to12", "Female9to12MOE")
hsRaw <- wholivesdatapull(hsvars, hsnames)
save(hsRaw, file = "inputs/hsRaw.RData")

#Bachelor's degree or higher, adults 25 and older

bachvars <- c('C15002_001E','C15002_001M','C15002_008E','C15002_008M','C15002_009E','C15002_009M','C15002_016E','C15002_016M','C15002_017E','C15002_017M')
bachnames <- c("Total", "TotalMOE", "MaleBach", "MaleBachMOE", "MaleGradProf",  "MaleGradProfMOE", "FemaleBach", "FemaleBachMOE", "FemaleGradProf", "FemaleGradProfMOE")
bachRaw <- wholivesdatapull(bachvars, bachnames)
save(bachRaw, file = "inputs/bachRaw.RData")

#Median household income, inflation-adjusted dollars

medhhvars <- c('B19013_001E','B19013_001M')
medhhnames <- c("MedianHHIncome", "MedianHHIncomeMOE")
medhhRaw <- wholivesdatapull(medhhvars, medhhnames)
save(medhhRaw, file = "inputs/medhhRaw.RData")

#have to make MOEs slightly differently because it is a household universe.
medhhvars2000 <- c('H002001', 'H003001','HCT012001')
medhhnames2000 <- c("SampHousingUnits2000","TotHousingUnits2000", "MedianHHIncome")
medhhRaw2000 <- wholivesdatapull(medhhvars2000, medhhnames2000, censusname = "dec/sf3", year = 2000)
medhhRaw2000 <- medhhRaw2000 %>% mutate(PctinSamp = SampHousingUnits2000 / TotHousingUnits2000,
                                        MedhianHHIncomeMOE = moe2000(MedianHHIncome, TotHousingUnits2000, designfac = 1.2))

#Internet access

intavars <- c('B28002_001E','B28002_001M','B28002_006E','B28002_006M','B28002_012E','B28002_012M','B28002_013E','B28002_013M')
intanames <- c("Total","TotalMOE","CellOnly","CellOnlyMOE","NoSubscript","NoSubscriptMOE","NoAccess","NoAccessMOE")
intaRaw <- wholivesdatapull(intavars, intanames)
save(intaRaw, file = "inputs/intaRaw.RData")


#Poverty rate, population for whom poverty has been determined

povvars <- c('C17001_001E','C17001_001M','C17001_002E','C17001_002M')
povnames <- c("Total", "TotalMOE", "BelowPov", "BelowPovMOE")
povRaw <- wholivesdatapull(povvars, povnames)
save(povRaw, file = "inputs/povRaw.RData")

#Children in poverty, population for whom poverty has been determined	

childpovvars <- c('C17001_004E','C17001_004M','C17001_008E','C17001_008M','C17001_013E','C17001_013M','C17001_017E','C17001_017M')
childpovnames <- c( "BelowPovMaleChild", "BelowPovMaleChildMOE", "BelowPovFemaleChild", "BelowPovFemaleChildMOE", "AbovePovMaleChild", "AbovePovMaleChildMOE", "AbovePovFemaleChild", "AbovePovFemaleChildMOE")
childpovRaw <- wholivesdatapull(childpovvars, childpovnames)
save(childpovRaw, file = "inputs/childpovRaw.RData")


#Households without access to a vehicle

vehvars <- c('B08201_001E','B08201_001M','B08201_002E','B08201_002M')
vehnames <- c("Total","TotalMOE","NoVehAvail","NoVehAvailMOE")
vehRaw <- wholivesdatapull(vehvars, vehnames)
save(vehRaw, file = "inputs/vehRaw.RData")

#Population not U.S. citizens at birth

forborvars <- c('C05005_004E','C05005_004M','C05005_007E','C05005_007M','C05005_010E','C05005_010M','C05005_013E','C05005_013M','B01003_001E','B01003_001M')
forbornames <- c("TotForeign10on","TotForeign10onMOE","TotForeign00to09","TotForeign00to09MOE","TotForeign90to99","TotForeign90to99MOE","TotForeignPre90","TotForeignPre90MOE","TotalPop","TotalPopMOE")
forborRaw <- wholivesdatapull(forborvars, forbornames)
save(forborRaw, file = "inputs/forborRaw.RData")


#Population who moved in the past year

mobvars <- c('B07003_001E','B07003_001M','B07003_004E','B07003_004M','B07003_007E','B07003_007M','B07003_010E','B07003_010M','B07003_013E','B07003_013M','B07003_016E','B07003_016M')
mobnames <- c("Total","TotalMOE","TotSameHouse","TotSameHouseMOE","TotMovedinCty","TotMovedinCtyMOE","TotMovedinState","TotMovedinStateMOE","TotMovedbtwnStates","TotMovedbtwnStatesMOE","TotMovedfromAbroad","TotMovedfromAbroadMOE")
mobRaw <- wholivesdatapull(mobvars, mobnames)
save(mobRaw, file = "inputs/mobRaw.RData")

# https://www2.census.gov/acs2004/Core_Tables/
ACScounty_04 <- read_csv("inputs/ACS_data/ACS_2004_050.csv") 
ACSUS_04 <- read_csv("inputs/ACS_data/ACS_2004_010.csv")
ACSmetro_04 <- read_csv("inputs/ACS_data/ACS_2004_380.csv")
mob_04 <- ACScounty_04 %>% rbind(ACSUS_04, ACSmetro_04) %>%
  filter(grepl("22071", geoid) |
           grepl("22051", geoid) |
           grepl("22103", geoid) | #St. Tammany isn't in 2004 ACS for these.
           grepl("01000US", geoid) |
           grepl("38000US5560", geoid)) %>% 
  filter((tblid == "B07003" & (order == 1 | 
                                 order == 4 |
                                 order == 7 | 
                                 order == 10 | 
                                 order == 13 | 
                                 order == 16) )) %>%
  mutate(MOE = as.numeric(cest) - as.numeric(clb),
         placename = case_when(grepl("22071", geoid) ~ "Orleans",
                               grepl("22051", geoid) ~ "Jefferson",
                               grepl("01000US", geoid) ~ "United States",
                               grepl("38000US5560", geoid) ~ "New Orleans Metro Area"),
         var = case_when(tblid == "B07003" & (order == 1) ~ "Total",
                         tblid == "B07003" & (order == 4) ~ "TotSameHouse",
                         tblid == "B07003" & (order == 7) ~ "TotMovedinCty",
                         tblid == "B07003" & (order == 10) ~ "TotMovedinState",
                         tblid == "B07003" & (order == 13) ~ "TotMovedbtwnStates",
                         tblid == "B07003" & (order == 16) ~ "TotMovedfromAbroad"),
         cest = as.numeric(cest),
         clb = as.numeric(clb))  %>%
  select(placename, var, MOE, cest) %>%
  pivot_wider(names_from = var, values_from = c(MOE, cest)) %>%
  mutate(mobabroadpct = cest_TotMovedfromAbroad / cest_Total,
         mobabroadpctMOE = moeprop(y=cest_Total,moex = MOE_TotMovedfromAbroad,moey = MOE_Total,p=mobabroadpct),
         mobStatespct = cest_TotMovedbtwnStates / cest_Total,
         mobStatespctMOE = moeprop(y=cest_Total,moex = MOE_TotMovedbtwnStates,moey = MOE_Total,p=mobStatespct),
         difparishpct = cest_TotMovedinState / cest_Total,
         difparishpctMOE = moeprop(y=cest_Total,moex = MOE_TotMovedinState,moey = MOE_Total,p=difparishpct),
         withinparishpct = cest_TotMovedinCty / cest_Total,
         withinparishpctMOE = moeprop(y=cest_Total,moex = MOE_TotMovedinCty,moey = MOE_Total,p=withinparishpct),
         samehousepct = cest_TotSameHouse / cest_Total,
         samehousepctMOE = moeprop(y=cest_Total, moex = MOE_TotSameHouse,moey = MOE_Total,p=samehousepct)) %>%
  select(placename, 
         mobabroadpct, mobabroadpctMOE, 
         mobStatespct, mobStatespctMOE, 
         difparishpct, difparishpctMOE,
         withinparishpct, withinparishpctMOE,
         samehousepct, samehousepctMOE)


#Homeownership rates

hovars <- c('B25003_001E','B25003_001M','B25003_002E','B25003_002M')
honames <- c("Total","TotalMOE","Owner","OwnerMOE")
hoRaw <- wholivesdatapull(hovars, honames)
save(hoRaw, file = "inputs/hoRaw.RData")

hovars2000 <- c('H004001', 'H004002')
honames2000 <- c("Total2000", "Owner2000")
hoRaw2000 <- wholivesdatapull(hovars2000, honames2000, censusname = "dec/sf1", year = 2000) %>% select(-place)
save(hoRaw2000, file = "inputs/hoRaw2000.RData")

#Homeowners without a mortgage

honomovars <- c('B25081_001E','B25081_001M','B25081_002E','B25081_002M','B25081_009E','B25081_009M')
honomonames <- c("Total","TotalMOE","Mortgage","MortgageMOE","NoMortgage","NoMortgageMOE")
honomoRaw <- wholivesdatapull(honomovars, honomonames)
save(honomoRaw, file = "inputs/honomoRaw.RData")


#Renters with severe housing cost burdens

rentburvars <- c('B25070_001E','B25070_001M','B25070_010E','B25070_010M','B25070_011E','B25070_011M')
rentburnames <- c("Total","TotalMOE","50orMore","50orMoreMOE","NotComputed","NotComputedMOE")
rentburRaw <- wholivesdatapull(rentburvars, rentburnames)
save(rentburRaw, file = "inputs/rentburRaw.RData")


#Homeowners with severe housing cost burdens

hoburvars <- c('B25091_001E','B25091_001M','B25091_011E','B25091_011M','B25091_012E','B25091_012M','B25091_022E','B25091_022M','B25091_023E','B25091_023M')
hoburnames <- c("Total","TotalMOE","50orMoreMortgage","50orMoreMortgageMOE","NotComputedMortgage","NotComputedMortgageMOE","50orMoreNoMortgage","50orMoreNoMortgageMOE","NotComputedNoMortgage","NotComputedNoMortgageMOE")
hoburRaw <- wholivesdatapull(hoburvars, hoburnames)
save(hoburRaw, file = "inputs/hoburRaw.RData")

#### getting SEs for 2004 data:

housing <-  ACScounty_04 %>% rbind(ACSUS_04, ACSmetro_04) %>%
  filter(grepl("22071", geoid) |
           grepl("22051", geoid) |
           grepl("22103", geoid) | #St. Tammany isn't in 2004 ACS for these.
           grepl("01000US", geoid) |
           grepl("38000US5560", geoid)) %>% 
  filter((tblid == "B25064" & order == 1) |
           (tblid == "B25070" & (order == 1 | order == 10 | order == 11 ) |
              (tblid == "B25091" & (order == 1 | order == 11| order == 12| order == 22| order == 23)))) %>%
  mutate(MOE = as.numeric(cest) - as.numeric(clb),
         placename = case_when(grepl("22071", geoid) ~ "Orleans",
                               grepl("22051", geoid) ~ "Jefferson",
                               grepl("01000US", geoid) ~ "United States",
                               grepl("38000US5560", geoid) ~ "New Orleans Metro Area"),
         var = case_when(tblid == "B25064" & order == 1 ~ "medgrossrent",
                         tblid == "B25070" & order == 1 ~  "totrenters",
                         tblid == "B25070" & order == 10 ~ "rentcostburden",
                         tblid == "B25070" & order == 11 ~ "renters_notcomp",
                         tblid == "B25091" & order == 1 ~ "tothomeowners",
                         tblid == "B25091" & order == 11 ~ "hocostburden",
                         tblid == "B25091" & order == 12 ~ "ho_notcomp",
                         tblid == "B25091" & order == 22 ~ "hocostburden_nomort",
                         tblid == "B25091" & order == 23 ~ "ho_notcomp_nomort"),
         cest = as.numeric(cest),
         clb = as.numeric(clb)) %>%
  select(placename, var, MOE, cest) %>%
  pivot_wider(names_from = var, values_from = c(MOE, cest)) %>%
  mutate(rentburpct = (cest_rentcostburden) / (cest_totrenters - cest_renters_notcomp),
         MOE_rentersagg = moeagg(cbind(MOE_totrenters,MOE_renters_notcomp)),
         rentburpctMOE = moeprop(y = cest_totrenters- cest_renters_notcomp, moex = MOE_rentcostburden, moey = MOE_rentersagg, p = rentburpct),
         medgrossrent = cest_medgrossrent,
         medgrossrentMOE = MOE_medgrossrent,
         hoburpct = (cest_hocostburden+cest_hocostburden_nomort) / (cest_tothomeowners - (cest_ho_notcomp+cest_ho_notcomp_nomort)),
         MOE_hoburagg = moeagg(cbind(MOE_hocostburden,MOE_hocostburden_nomort)),
         MOE_hoagg = moeagg(cbind(MOE_tothomeowners,MOE_ho_notcomp,MOE_ho_notcomp_nomort)),
         hoburpctMOE = moeprop(y = (cest_tothomeowners- (cest_ho_notcomp+cest_ho_notcomp_nomort)), moex = MOE_hoburagg, moey = MOE_hoagg, p = hoburpct)) %>%
  select(placename, medgrossrent, medgrossrentMOE, rentburpct, rentburpctMOE, hoburpct, hoburpctMOE)




#Median gross rent, 201* inflation-adjusted dollars

medrentvars <- c('B25064_001E','B25064_001M')
medrentnames <- c("Rent","RentMOE")
medrentRaw <- wholivesdatapull(medrentvars, medrentnames)
save(medrentRaw, file = "inputs/medrentRaw.RData")


#Year structure built, 201* housing units

yrbuiltvars <- c('B25034_001E','B25034_001M','B25034_002E','B25034_002M','B25034_003E','B25034_003M','B25034_004E','B25034_004M','B25034_005E','B25034_005M','B25034_006E','B25034_006M','B25034_007E','B25034_007M','B25034_008E','B25034_008M','B25034_009E','B25034_009M','B25034_010E','B25034_010M','B25034_011E','B25034_011M')
yrbuiltnames <- c("Total","TotalMOE","2014","2014MOE","2010to2013","2010to2013MOE","2000to2009","2000to2009MOE","1990to1999","1990to1999MOE","1980to1989","1980to1989MOE","1970to1979","1970to1979MOE","1960to1969","1960to1969MOE","1950to1959","1950to1959MOE","1940to1949","1940to1949MOE","1939","1939MOE")
yrbuiltRaw <- wholivesdatapull(yrbuiltvars, yrbuiltnames)
save(yrbuiltRaw, file = "inputs/yrbuiltRaw.RData")


#Means of transportation to work, workers 16 and older

commutevars <- c('B08301_001E','B08301_001M','B08301_003E','B08301_003M','B08301_004E','B08301_004M','B08301_010E','B08301_010M','B08301_016E','B08301_016M','B08301_017E','B08301_017M','B08301_018E','B08301_018M','B08301_019E','B08301_019M','B08301_020E','B08301_020M','B08301_021E','B08301_021M')
commutenames <- c("Total","TotalMOE","DroveAlone","DroveAloneMOE","Carpool","CarpoolMOE","PublicTransit","PublicTransitMOE","Taxi","TaxiMOE","Motorcycle","MotorcycleMOE","Bike","BikeMOE","Walk","WalkMOE","Other","OtherMOE","Workhome","WorkhomeMOE")
commuteRaw <- wholivesdatapull(commutevars, commutenames)
save(commuteRaw, file = "inputs/commuteRaw.RData")

# 2000 - means of transportation to work, 16+
#P030

commutevars2000 <- c('P003001','P030001', 'P030003','P030004','P030005','P030011','P030012','P030013','P030014','P030015','P030016')
commutenames2000 <- c("Totalpop2000","Total2000","DroveAlone2000","Carpool2000","PublicTransit2000","Taxi2000","Motorcycle2000","Bike2000","Walk2000","Other2000","Workhome2000")
commuteRaw2000 <- wholivesdatapull(commutevars2000, commutenames2000, year = 2000, censusname = "dec/sf3")

#Design factor for this variable is 1.3 for parishes and metro, 1.1 for US
commuteRaw2000 <- commuteRaw2000 %>%
  mutate(TotalMOE2000 = moe2000(est = Total2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         DroveAloneMOE2000 = moe2000(est = DroveAlone2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         CarpoolMOE2000 = moe2000(est = Carpool2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         PublicTransitMOE2000 = moe2000(est = PublicTransit2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         TaxiMOE2000 = moe2000(est = Taxi2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         MotorcycleMOE2000 = moe2000(est = Motorcycle2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         BikeMOE2000 = moe2000(est = Bike2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)), 
         WalkMOE2000 = moe2000(est = Walk2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         OtherMOE2000 = moe2000(est = Other2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)),
         WorkhomeMOE2000 = moe2000(est = Workhome2000, Totalpop2000, designfac = ifelse(placename == "United States", 1.1, 1.3)))
save(commuteRaw2000, file = "inputs/commuteRaw2000.RData")


#################################################
# # Jenna's expanded data pull
#################################################

#Median household income, 201* inflation-adjusted dollars

medhhvars <- c('B19013_001E','B19013_001M',
               'B19013B_001E','B19013B_001M',
               'B19013D_001E','B19013D_001M',
               'B19013H_001E','B19013H_001M',
               'B19013I_001E','B19013I_001M')
medhhnames <- c("MedianHHIncome", "MedianHHIncomeMOE",
                "MedianHHIncome_blk", "MedianHHIncomeMOE_blk",
                "MedianHHIncome_asian", "MedianHHIncomeMOE_asian",
                "MedianHHIncome_wht", "MedianHHIncomeMOE_wht",
                "MedianHHIncome_hisp", "MedianHHIncomeMOE_hisp")
medhhRaw_exp <- wholivesdatapull(medhhvars, medhhnames, year = YEAR)
save(medhhRaw_exp, file = "inputs/medhhRaw_exp.RData")

medhh_unadjusted <- read_xlsx("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/Copy_MedianInc.xlsx", range = "A1:H7") %>%
  transmute("var" = `...1`,
            `1979` = as.character(`1979 (1979$)`),
            `1989`= as.character(`1989 (1989$)`),
            `1999` = as.character(`1999 (1999$)`),
            `2010` = `2010 (2010$)...5`,
            `2010MOE` = `2010 (2010$)...6`) %>% na.omit()
medhh_unadjusted <- medhh_unadjusted %>% select(-c(`2010MOE`)) %>% pivot_longer(cols = `1979`:`2010`, names_to = "Year")
save(medhh_unadjusted, file = "inputs/medhh_unadjusted.RData")
#Bachelor's degree or higher, adults 25 and older

bachvars <- c('C15002_001E','C15002_001M','C15002_008E','C15002_008M','C15002_009E','C15002_009M','C15002_016E','C15002_016M',
              'C15002_017E','C15002_017M',
              'C15002B_001E','C15002B_001M','C15002B_006E','C15002B_006M','C15002B_011E','C15002B_011M',
              'C15002D_001E','C15002D_001M','C15002D_006E','C15002D_006M','C15002D_011E','C15002D_011M',
              'C15002H_001E','C15002H_001M','C15002H_006E','C15002H_006M','C15002H_011E','C15002H_011M',
              'C15002I_001E','C15002I_001M','C15002I_006E','C15002I_006M','C15002I_011E','C15002I_011M')
bachnames <- c("Total", "TotalMOE", "MaleBach", "MaleBachMOE", "MaleGradProf",  "MaleGradProfMOE", "FemaleBach", 
               "FemaleBachMOE", "FemaleGradProf", "FemaleGradProfMOE",
               "Total_blk", "TotalMOE_blk", "MaleBach_blk", "MaleBachMOE_blk",  "FemaleBach_blk", 
               "FemaleBachMOE_blk",
               "Total_asian", "TotalMOE_asian", "MaleBach_asian", "MaleBachMOE_asian", "FemaleBach_asian", 
               "FemaleBachMOE_asian", 
               "Total_wht", "TotalMOE_wht", "MaleBach_wht", "MaleBachMOE_wht",  "FemaleBach_wht", 
               "FemaleBachMOE_wht", 
               "Total_hisp", "TotalMOE_hisp", "MaleBach_hisp", "MaleBachMOE_hisp", "FemaleBach_hisp", 
               "FemaleBachMOE_hisp")
bachRaw_exp <- wholivesdatapull(bachvars, bachnames, year = YEAR)
save(bachRaw_exp, file = "inputs/bachRaw_exp.RData")

#Historial Educational Attainment. 1980-2000 from NHGIS
Bach80 <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds107_1980_county.csv")
Bach90 <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds123_1990_county.csv")
Bach00 <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0095_csv/nhgis0095_ds151_2000_county.csv")

Bach80 <- Bach80 %>% filter(STATEA == "22" & COUNTYA == "071") %>% 
  transmute(year = 1980,
            totBach = DHN005 + DHN010 + DHN015 + DHN020 + DHN025,
            totpop = sum(c_across(DHN001:DHN025), na.rm = T),
            WhiteBach = DHN005,
            totWhite = DHN001 + DHN002 + DHN003 + DHN004 + DHN005,
            BlackBach = DHN010,
            totBlack = DHN006 + DHN007 + DHN008 + DHN009 + DHN010,
            HispBach = DHO005,
            totHisp = DHO001 + DHO002 + DHO003 + DHO004 + DHO005,
            pctTotalBach = totBach / totpop,
            pctWhiteBach = WhiteBach / totWhite,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp) %>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

Bach90 <- Bach90 %>% filter(STATEA == "22" & COUNTYA == "071") %>%
  transmute(year = 1990,
            totBach = E34006 + E34007 + E34013 + E34014 + E34020 + E34021 + E34027 + E34028 + E34034 + E34035,
            totpop = sum(c_across(E34001:E34035), na.rm = T),
            #WhiteBach = E34006 + E34007,
            #totWhite = E34001 + E34002 + E34003 + E34004 + E34005 + E34006 + E34007,
            BlackBach = E34013 + E34014,
            totBlack = E34008 + E34009 + E34010 + E34011 + E34012 + E34013 + E34014,
            HispBach = E35006 + E35007,
            totHisp = E35001 + E35002 + E35003 + E35004 + E35005 + E35006 + E35007,
            pctTotalBach = totBach / totpop,
            #pctWhiteBach = WhiteBach / totWhite,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp) %>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

Bach90Wht <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0009_ds125_1990_county.csv")
Bach90Wht <- Bach90Wht %>% filter(STATEA == "22" & COUNTYA == "071") %>%
  transmute(year = 1990, 
            WhiteBach = FF5ABR012 + FF5ABR013 + FF5ABR014 + FF5ABR015 + FF5ABR027 + FF5ABR028 + FF5ABR029 + FF5ABR030,
            totWhite = sum(c_across(FF5ABR001:FF5ABR030), na.rm = T),
            pctWhiteBach = WhiteBach / totWhite) %>% 
  pivot_longer(cols = pctWhiteBach, values_to = "val") %>%
  select(year, val, name)

Bach90 <- rbind(Bach90, Bach90Wht)

Bach00Wht <- wholivesdatapull(variables = c('P001001', "P148I001", 'P148I008', 'P148I009', 'P148I016', 'P148I017'),
                              names = c("Totalpop2000", "TotalWhite2000", "MaleBach", "MaleGradProf", "FemaleBach", "FemaleGradProf"),
                              censusname = "dec/sf3",
                              year = 2000) %>%
  mutate(WhiteBach = MaleBach + MaleGradProf + FemaleBach + FemaleGradProf,
         WhiteBachMOE = moe2000(WhiteBach, Totalpop2000, designfac = 1.2),
         TotalWhiteMOE = moe2000(TotalWhite2000, Totalpop2000, designfac = 1.2), #is this the correct way to do this for whites 25+? Do I use race/eth designfac? But it's for educational attainment pop.
         pctWhiteBach = WhiteBach / TotalWhite2000,
         WhiteBachmoeprop = moeprop(y = TotalWhite2000, moex = WhiteBachMOE, moey = TotalWhiteMOE, p = pctWhiteBach)) %>% filter(place == "071") %>% select(pctWhiteBach, WhiteBachmoeprop)


Bach00 <- Bach00 %>% filter(STATEA == "22" & COUNTYA == "071") %>% #this one is by sex
  transmute(year = 2000,
            totBach = GRW006 + GRW007 + GRW013 + GRW014 + GRW020 + GRW021 + GRW027 + GRW028 + GRW034 + GRW035 + GRW041 + GRW042 + GRW048 + GRW049 + GRW055 + GRW056 + GRW062 + GRW063 + GRW069 + GRW070 + GRW076 + GRW077 + GRW083 + GRW084 + GRW090 + GRW091 + GRW097 + GRW098,
            totpop = sum(c_across(GRW001:GRW098), na.rm = T),
            totBachMOE = moe2000(est = totBach, totpop, designfac = 1.3),
            totpopMOE = moe2000(est = totpop, totpop, designfac = 1.3),
            #WhiteBach = GRW006 + GRW007 + GRW013 + GRW014, 
            #WhiteBachMOE = moe2000(est = WhiteBach, totpop, designfac = 1.3),
            totWhite = sum(c_across(GRW001:GRW014), na.rm = T), #adding all White adults 25+
            totWhiteMOE = moe2000(est = totWhite, totpop, designfac = 1.3),
            BlackBach = GRW020 + GRW021 + GRW027 + GRW028,
            BlackBachMOE = moe2000(est = BlackBach, totpop, designfac = 1.3),
            totBlack = sum(c_across(GRW015:GRW028), na.rm = T),
            totBlackMOE = moe2000(est = totBlack, totpop, designfac = 1.3),
            HispBach = GRZ006 + GRZ007 + GRZ013 + GRZ014,
            HispBachMOE = moe2000(est = HispBach, totpop, designfac = 1.3),
            totHisp = sum(c_across(GRZ001:GRZ014), na.rm = T),
            totHispMOE = moe2000(est = totHisp, totpop, designfac = 1.3),
            pctTotalBach = totBach / totpop,
            #pctWhiteBach = WhiteBach / totWhite,
            pctBlackBach = BlackBach / totBlack,
            pctHispBach = HispBach / totHisp,
            
            Totalmoeprop = moeprop(totpop, totBachMOE, totpopMOE, pctTotalBach),
            #WhiteBachmoeprop = moeprop(totWhite,WhiteBachMOE,totWhiteMOE,pctWhiteBach),
            BlackBachmoeprop = moeprop(totBlack,BlackBachMOE,totBlackMOE,pctBlackBach),
            HispBachmoeprop = moeprop(totHisp,HispBachMOE,totHispMOE,pctHispBach)
  )%>% cbind(Bach00Wht) %>%
  pivot_longer(cols = c(pctTotalBach:pctHispBach, pctWhiteBach), values_to = "val") %>% 
  select(year, val, name)

#pulling ACS1 for 2010 data

# library(tidycensus)
# View(load_variables(year = 2016, dataset = "acs1"))


#Bachelor's degree or higher, adults 25 and older

bachvars10 <- c('C15002_001E','C15002_001M','C15002_008E','C15002_008M','C15002_009E','C15002_009M','C15002_016E','C15002_016M',
                'C15002_017E','C15002_017M',
                'C15002B_001E','C15002B_001M','C15002B_006E','C15002B_006M','C15002B_011E','C15002B_011M',
                'C15002D_001E','C15002D_001M','C15002D_006E','C15002D_006M','C15002D_011E','C15002D_011M',
                'C15002H_001E','C15002H_001M','C15002H_006E','C15002H_006M','C15002H_011E','C15002H_011M',
                'C15002I_001E','C15002I_001M','C15002I_006E','C15002I_006M','C15002I_011E','C15002I_011M')
bachnames10 <- c("Total", "TotalMOE", "MaleBach", "MaleBachMOE", "MaleGradProf",  "MaleGradProfMOE", "FemaleBach", 
                 "FemaleBachMOE", "FemaleGradProf", "FemaleGradProfMOE",
                 "Total_blk", "TotalMOE_blk", "MaleBach_blk", "MaleBachMOE_blk",  "FemaleBach_blk", 
                 "FemaleBachMOE_blk",
                 "Total_asian", "TotalMOE_asian", "MaleBach_asian", "MaleBachMOE_asian", "FemaleBach_asian", 
                 "FemaleBachMOE_asian", 
                 "Total_wht", "TotalMOE_wht", "MaleBach_wht", "MaleBachMOE_wht",  "FemaleBach_wht", 
                 "FemaleBachMOE_wht", 
                 "Total_hisp", "TotalMOE_hisp", "MaleBach_hisp", "MaleBachMOE_hisp", "FemaleBach_hisp", 
                 "FemaleBachMOE_hisp")
Bach10 <- wholivesdatapull(bachvars10, bachnames10, year = 2010)

Bach10 <- Bach10 %>%
  filter(place == "071") %>% 
  transmute(year = 2010,
            pctTotalBach = (MaleBach + FemaleBach + MaleGradProf + FemaleGradProf) / Total,
            pctWhiteBach = (MaleBach_wht + FemaleBach_wht) / Total_wht,
            pctBlackBach = (MaleBach_blk + FemaleBach_blk) / Total_blk,
            pctHispBach = (MaleBach_hisp + FemaleBach_hisp) / Total_hisp) %>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

# Pulling ACS1 for 2016 data

#Bachelor's degree or higher, adults 25 and older

bachvars16 <- c('C15002_001E','C15002_001M','C15002_008E','C15002_008M','C15002_009E','C15002_009M','C15002_016E','C15002_016M',
                'C15002_017E','C15002_017M',
                'C15002B_001E','C15002B_001M','C15002B_006E','C15002B_006M','C15002B_011E','C15002B_011M',
                'C15002D_001E','C15002D_001M','C15002D_006E','C15002D_006M','C15002D_011E','C15002D_011M',
                'C15002H_001E','C15002H_001M','C15002H_006E','C15002H_006M','C15002H_011E','C15002H_011M',
                'C15002I_001E','C15002I_001M','C15002I_006E','C15002I_006M','C15002I_011E','C15002I_011M')
bachnames16 <- c("Total", "TotalMOE", "MaleBach", "MaleBachMOE", "MaleGradProf",  "MaleGradProfMOE", "FemaleBach", 
                 "FemaleBachMOE", "FemaleGradProf", "FemaleGradProfMOE",
                 "Total_blk", "TotalMOE_blk", "MaleBach_blk", "MaleBachMOE_blk",  "FemaleBach_blk", 
                 "FemaleBachMOE_blk",
                 "Total_asian", "TotalMOE_asian", "MaleBach_asian", "MaleBachMOE_asian", "FemaleBach_asian", 
                 "FemaleBachMOE_asian", 
                 "Total_wht", "TotalMOE_wht", "MaleBach_wht", "MaleBachMOE_wht",  "FemaleBach_wht", 
                 "FemaleBachMOE_wht", 
                 "Total_hisp", "TotalMOE_hisp", "MaleBach_hisp", "MaleBachMOE_hisp", "FemaleBach_hisp", 
                 "FemaleBachMOE_hisp")
Bach16<- wholivesdatapull(bachvars16, bachnames16, year = 2016)

Bach16 <- Bach16 %>%
  filter(place == "071") %>% 
  transmute(year = 2016,
            pctTotalBach = (MaleBach + FemaleBach + MaleGradProf + FemaleGradProf) / Total,
            pctWhiteBach = (MaleBach_wht + FemaleBach_wht) / Total_wht,
            pctBlackBach = (MaleBach_blk + FemaleBach_blk) / Total_blk,
            pctHispBach = (MaleBach_hisp + FemaleBach_hisp) / Total_hisp) %>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

#most recent year

bachRaw_exp <- bachRaw_exp %>% ## something's going on here where this is names the same as bachRaw above and so can't be used in the analysis piece as it was before
  filter(place == "071") %>% 
  transmute(year = YEAR,
            pctTotalBach = (MaleBach + FemaleBach + MaleGradProf + FemaleGradProf) / Total,
            pctWhiteBach = (MaleBach_wht + FemaleBach_wht) / Total_wht,
            pctBlackBach = (MaleBach_blk + FemaleBach_blk) / Total_blk,
            pctHispBach = (MaleBach_hisp + FemaleBach_hisp) / Total_hisp) %>%
  pivot_longer(cols = pctTotalBach:pctHispBach, values_to = "val") %>% 
  select(year, val, name)

#joining the TS data
Bach_TS <- rbind(Bach80, Bach90, Bach00, Bach10, Bach16, bachRaw_exp) %>%
  mutate(var = case_when(name == "pctTotalBach" ~ "All",
                         name == "pctWhiteBach" ~ "White,\nnon-Hispanic",
                         name == "pctBlackBach" ~ "Black",
                         name == "pctHispBach" ~ "Hispanic,\nany race")) %>% select(-name)
write_csv(Bach_TS, "inputs/hist_educationalAttainment.csv")


#Poverty rate, population for whom poverty has been determined

povvars <- c('C17001_001E','C17001_001M','C17001_002E','C17001_002M',
             'C17001B_001E','C17001B_001M','C17001B_002E','C17001B_002M',
             'C17001D_001E','C17001D_001M','C17001D_002E','C17001D_002M',
             'C17001H_001E','C17001H_001M','C17001H_002E','C17001H_002M',
             'C17001I_001E','C17001I_001M','C17001I_002E','C17001I_002M')
povnames <- c("Total", "TotalMOE", "BelowPov", "BelowPovMOE",
              "Total_blk", "TotalMOE_blk", "BelowPov_blk", "BelowPovMOE_blk",
              "Total_asian", "TotalMOE_asian", "BelowPov_asian", "BelowPovMOE_asian",
              "Total_wht", "TotalMOE_wht", "BelowPov_wht", "BelowPovMOE_wht",
              "Total_hisp", "TotalMOE_hisp", "BelowPov_hisp", "BelowPovMOE_hisp")
povRaw_exp <- wholivesdatapull(povvars, povnames, year = YEAR)
save(povRaw_exp, file = "inputs/povRaw_exp.RData")

# Creating a time series for total poverty - had to get 1980-2000 from IPUMS NHGIS

pov80 <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0092_ds107_1980_county.csv")
hisppov80 <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0093_ds107_1980_county.csv")
pov90 <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0092_ds140_1990_county.csv")
pov00 <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/nhgis0092_csv/nhgis0092_ds151_2000_county.csv")

#filter to Orleans Parish

pov80 <- pov80 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 1980,
                                                                           totpov = DI9002 + DI9004 + DI9006 + DI9008 + DI9010,
                                                                           Whitepov = DI9002,
                                                                           Blackpov = DI9004,
                                                                           totpop = sum(c_across(DI9001:DI9010), na.rm = T),
                                                                           totWhitepop = DI9001 + DI9002,
                                                                           totBlackpop= DI9003 + DI9004,
                                                                           pctTotalpov = totpov / totpop,
                                                                           pctWhitepov = Whitepov / totWhitepop,
                                                                           pctBlackpov = Blackpov / totBlackpop) %>% pivot_longer(cols = pctTotalpov:pctBlackpov, values_to = "val") %>% select(year, val, name)
hisppov80 <- hisppov80 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 1980,
                                                                                   Hisppov = DJA002,
                                                                                   totHisppop = DJA001 + DJA002,
                                                                                   pctHisppov = Hisppov / totHisppop,
                                                                                   name = "pctHisppov") %>% select(year, val = pctHisppov, name)
pov90 <- pov90 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 1990,
                                                                           totpov = sum(c_across(EKT011:EKT020), na.rm = T),
                                                                           Whitepov = EKT011,
                                                                           Blackpov = EKT012 + EKT017,
                                                                           Hisppov = EKT016  + EKT018 + EKT019 + EKT020,
                                                                           totpop = sum(c_across(EKT001:EKT020), na.rm = T),
                                                                           totWhitepop = EKT001 + EKT011,
                                                                           totBlackpop = EKT002 + EKT012 + EKT007 + EKT017,
                                                                           totHisppop = EKT006 + EKT008 + EKT009 + EKT010 + EKT016 + EKT018 + EKT019 + EKT020,
                                                                           pctTotalpov = totpov / totpop,
                                                                           pctWhitepov = Whitepov / totWhitepop,
                                                                           pctBlackpov = Blackpov / totBlackpop,
                                                                           pctHisppov = Hisppov / totHisppop) %>% pivot_longer(cols = pctTotalpov:pctHisppov, values_to = "val") %>% select(year, val, name)
pov00Wht <- wholivesdatapull(variables = c('P001001', 'P148I001', 'P159I002'),
                             names = c('TotalPop2000', "TotalWhitepop", "Whitepov"),
                             censusname = "dec/sf3",
                             year = 2000) %>%
  filter(place == "071") %>%
  mutate(pctWhitepov = Whitepov / TotalWhitepop,
         WhitepovMOE = moe2000(Whitepov, TotalPop2000, designfac = 1.5),
         TotalWhiteMOE = moe2000(TotalWhitepop, TotalPop2000, designfac = 2),
         Whitemoeprop = moeprop(y = TotalWhitepop, moex = WhitepovMOE, moey = TotalWhiteMOE, p = pctWhitepov)) %>% select(pctWhitepov, Whitemoeprop)


pov00 <- pov00 %>% filter(STATEA == "22" & COUNTYA == "071") %>% transmute(year = 2000,
                                                                           totpov = GTV001 + GTV003 + GTV005 + GTV007 + GTV009 + GTV011 + GTV013,
                                                                           totpop = sum(c_across(GTV001:GTV014),na.rm = T),
                                                                           #Whitepov = GTV001,
                                                                           #totWhitepop = GTV001 + GTV002,
                                                                           Blackpov = GTV003,
                                                                           totBlackpop = GTV003 + GTV004,
                                                                           Hisppov = GTY001,
                                                                           totHisppop = GTY001 + GTY002,
                                                                           pctTotalpov = totpov / totpop,
                                                                           #pctWhitepov = Whitepov / totWhitepop,
                                                                           pctBlackpov = Blackpov / totBlackpop,
                                                                           pctHisppov = Hisppov / totHisppop) %>% cbind(pov00Wht) %>%
  pivot_longer(cols = c(pctTotalpov:pctHisppov, pctWhitepov), values_to = "val") %>% select(year, val, name)
# getting 2010 and 2016 from ACS1
##View(load_variables(year = 2016, dataset = "acs1"))

povvars10 <- c('C17001_001E','C17001_001M','C17001_002E','C17001_002M',
               'C17001B_001E','C17001B_001M','C17001B_002E','C17001B_002M',
               'C17001D_001E','C17001D_001M','C17001D_002E','C17001D_002M',
               'C17001H_001E','C17001H_001M','C17001H_002E','C17001H_002M',
               'C17001I_001E','C17001I_001M','C17001I_002E','C17001I_002M')
povnames10 <- c("Total", "TotalMOE", "BelowPov", "BelowPovMOE",
                "Total_blk", "TotalMOE_blk", "BelowPov_blk", "BelowPovMOE_blk",
                "Total_asian", "TotalMOE_asian", "BelowPov_asian", "BelowPovMOE_asian",
                "Total_wht", "TotalMOE_wht", "BelowPov_wht", "BelowPovMOE_wht",
                "Total_hisp", "TotalMOE_hisp", "BelowPov_hisp", "BelowPovMOE_hisp")
pov10 <- wholivesdatapull(povvars10, povnames10, year = 2010)

pov10 <- pov10 %>%
  filter(place == "071") %>% 
  transmute(year = 2010,
            pctTotalpov = BelowPov / Total,
            pctWhitepov = BelowPov_wht / Total_wht,
            pctBlackpov = BelowPov_blk / Total_blk,
            pctHisppov = BelowPov_hisp / Total_hisp) %>%
  pivot_longer(cols = pctTotalpov:pctHisppov, values_to = "val") %>% 
  select(year, val, name)

povvars16 <- c('C17001_001E','C17001_001M','C17001_002E','C17001_002M',
               'C17001B_001E','C17001B_001M','C17001B_002E','C17001B_002M',
               'C17001D_001E','C17001D_001M','C17001D_002E','C17001D_002M',
               'C17001H_001E','C17001H_001M','C17001H_002E','C17001H_002M',
               'C17001I_001E','C17001I_001M','C17001I_002E','C17001I_002M')
povnames16 <- c("Total", "TotalMOE", "BelowPov", "BelowPovMOE",
                "Total_blk", "TotalMOE_blk", "BelowPov_blk", "BelowPovMOE_blk",
                "Total_asian", "TotalMOE_asian", "BelowPov_asian", "BelowPovMOE_asian",
                "Total_wht", "TotalMOE_wht", "BelowPov_wht", "BelowPovMOE_wht",
                "Total_hisp", "TotalMOE_hisp", "BelowPov_hisp", "BelowPovMOE_hisp")

pov16 <- wholivesdatapull(povvars16, povnames16, year = 2016)

pov16 <- pov16 %>%
  filter(place == "071") %>% 
  transmute(year = 2016,
            pctTotalpov = BelowPov / Total,
            pctWhitepov = BelowPov_wht / Total_wht,
            pctBlackpov = BelowPov_blk / Total_blk,
            pctHisppov = BelowPov_hisp / Total_hisp) %>%
  pivot_longer(cols = pctTotalpov:pctHisppov, values_to = "val") %>% 
  select(year, val, name)

#current year
povraw_exp <- povRaw_exp %>%
  filter(place == "071") %>% 
  transmute(year = YEAR, 
            pctTotalpov = BelowPov / Total,
            pctWhitepov = BelowPov_wht / Total_wht,
            pctBlackpov = BelowPov_blk / Total_blk,
            pctHisppov = BelowPov_hisp / Total_hisp) %>%
  pivot_longer(cols = pctTotalpov:pctHisppov, values_to = "val") %>% 
  select(year, val, name)


pov_TS <- rbind(pov80, hisppov80, pov90, pov00, pov10, pov16, povraw_exp) %>% mutate(var = case_when(name == "pctTotalpov" ~ "All",
                                                                                                 name == "pctWhitepov" ~ "White,\nnon-Hispanic",
                                                                                                 name == "pctBlackpov" ~ "Black",
                                                                                                 name == "pctHisppov" ~ "Hispanic,\nany race")) %>% select(-name)


write_csv(pov_TS, "inputs/hist_pov.csv")


#Children in poverty, population for whom poverty has been determined	

childpovvars <- c('C17001_004E','C17001_004M','C17001_008E','C17001_008M','C17001_013E','C17001_013M','C17001_017E','C17001_017M',
                  'C17001B_004E','C17001B_004M','C17001B_008E','C17001B_008M','C17001B_013E','C17001B_013M','C17001B_017E','C17001B_017M',
                  'C17001D_004E','C17001D_004M','C17001D_008E','C17001D_008M','C17001D_013E','C17001D_013M','C17001D_017E','C17001D_017M',
                  'C17001H_004E','C17001H_004M','C17001H_008E','C17001H_008M','C17001H_013E','C17001H_013M','C17001H_017E','C17001H_017M',
                  'C17001I_004E','C17001I_004M','C17001I_008E','C17001I_008M','C17001I_013E','C17001I_013M','C17001I_017E','C17001I_017M')
childpovnames <- c("BelowPovMaleChild", "BelowPovMaleChildMOE", "BelowPovFemaleChild", "BelowPovFemaleChildMOE", "AbovePovMaleChild", 
                   "AbovePovMaleChildMOE", "AbovePovFemaleChild", "AbovePovFemaleChildMOE",
                   "BelowPovMaleChild_blk", "BelowPovMaleChildMOE_blk", "BelowPovFemaleChild_blk", "BelowPovFemaleChildMOE_blk", "AbovePovMaleChild_blk", 
                   "AbovePovMaleChildMOE_blk", "AbovePovFemaleChild_blk", "AbovePovFemaleChildMOE_blk",
                   "BelowPovMaleChild_asian", "BelowPovMaleChildMOE_asian", "BelowPovFemaleChild_asian", "BelowPovFemaleChildMOE_asian", "AbovePovMaleChild_asian", 
                   "AbovePovMaleChildMOE_asian", "AbovePovFemaleChild_asian", "AbovePovFemaleChildMOE_asian",
                   "BelowPovMaleChild_wht", "BelowPovMaleChildMOE_wht", "BelowPovFemaleChild_wht", "BelowPovFemaleChildMOE_wht", "AbovePovMaleChild_wht", 
                   "AbovePovMaleChildMOE_wht", "AbovePovFemaleChild_wht", "AbovePovFemaleChildMOE_wht",
                   "BelowPovMaleChild_hisp", "BelowPovMaleChildMOE_hisp", "BelowPovFemaleChild_hisp", "BelowPovFemaleChildMOE_hisp", "AbovePovMaleChild_hisp", 
                   "AbovePovMaleChildMOE_hisp", "AbovePovFemaleChild_hisp", "AbovePovFemaleChildMOE_hisp")
childpovRaw_exp <- wholivesdatapull(childpovvars, childpovnames, year = YEAR)
save(childpovRaw_exp, file = "inputs/childpovRaw_exp.RData")

#Homeownership rates

hovars <- c('B25003_001E','B25003_001M','B25003_002E','B25003_002M',
            'B25003B_001E','B25003B_001M','B25003B_002E','B25003B_002M',
            'B25003D_001E','B25003D_001M','B25003D_002E','B25003D_002M',
            'B25003H_001E','B25003H_001M','B25003H_002E','B25003H_002M',
            'B25003I_001E','B25003I_001M','B25003I_002E','B25003I_002M')
honames <- c("Total","TotalMOE","Owner","OwnerMOE",
             "Total_blk","TotalMOE_blk","Owner_blk","OwnerMOE_blk",
             "Total_asian","TotalMOE_asian","Owner_asian","OwnerMOE_asian",
             "Total_wht","TotalMOE_wht","Owner_wht","OwnerMOE_wht",
             "Total_hisp","TotalMOE_hisp","Owner_hisp","OwnerMOE_hisp")
hoRaw_exp <- wholivesdatapull(hovars, honames)
save(hoRaw_exp, file = "inputs/hoRaw_exp.RData")

childPovProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/childPov.csv")
homeownershipProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/homeownership.csv")
educationalAttainmentProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/educationalAttainment.csv")
medHHincProspInd <- read_csv("inputs/indicator expansion drafts/ProspInd_tables_WhoLives2022/medHHinc.csv")
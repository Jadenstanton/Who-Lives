############################################
# # ACS # #
############################################
order <- c("Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States")
orderHisp <- c("Orleans", "Jefferson", "New Orleans Metro Area", "United States")

load("inputs/totalpop_metro.RData")
load("inputs/hispanRaw.RData")
load("inputs/RacepopestRaw.RData")

hispanRaw[hispanRaw == -555555555] <- 0
# Hispanic Origin
hispan <- hispanRaw %>%
  filter(placename %in% c("Orleans", "New Orleans Metro Area", "United States")) %>%
  # slice(match(orderHisp, place)) %>%
  mutate(
    Cubanpct = TotCuba / TotalHisporLat,
    Dominicanpct = TotDomin / TotalHisporLat,
    Mexicanpct = TotMex / TotalHisporLat,
    PuertoRicanpct = TotPR / TotalHisporLat,
    Honduranpct = TotHond / TotalHisporLat,
    Guatemalanpct = TotGuat / TotalHisporLat,
    Nicaraguanpct = TotNicarag / TotalHisporLat,
    Salvadoranpct = TotSalva / TotalHisporLat,
    OtherCApct = TotOtherCA / TotalHisporLat,
    SouthAmericanpct = TotSA / TotalHisporLat,
    Otherpct = TotOtherHisporLat / TotalHisporLat,

    # for US/other geo sig testing
    CubanUS = rep(Cubanpct[3], 3),
    DominicanUS = rep(Dominicanpct[3], 3),
    MexicanUS = rep(Mexicanpct[3], 3),
    PuertoRicanUS = rep(PuertoRicanpct[3], 3),
    HonduranUS = rep(Honduranpct[3], 3),
    GuatemalanUS = rep(Guatemalanpct[3], 3),
    NicaraguanUS = rep(Nicaraguanpct[3], 3),
    SalvadoranUS = rep(Salvadoranpct[3], 3),
    OtherCAUS = rep(OtherCApct[3], 3),
    SouthAmericanUS = rep(SouthAmericanpct[3], 3),
    OtherUS = rep(Otherpct[3], 3),
    CubanMoeProp = moeprop(y = TotalHisporLat, moex = TotCubaMOE, moey = TotalHisporLatMOE, p = Cubanpct),
    DominicanMoeProp = moeprop(y = TotalHisporLat, moex = TotDominMOE, moey = TotalHisporLatMOE, p = Dominicanpct),
    MexicanMoeProp = moeprop(y = TotalHisporLat, moex = TotMexMOE, moey = TotalHisporLatMOE, p = Mexicanpct),
    PuertoRicanMoeProp = moeprop(y = TotalHisporLat, moex = TotPRMOE, moey = TotalHisporLatMOE, p = PuertoRicanpct),
    HonduranMoeProp = moeprop(y = TotalHisporLat, moex = TotHondMOE, moey = TotalHisporLatMOE, p = Honduranpct),
    GuatemalanMoeProp = moeprop(y = TotalHisporLat, moex = TotGuatMOE, moey = TotalHisporLatMOE, p = Guatemalanpct),
    NicaraguanMoeProp = moeprop(y = TotalHisporLat, moex = TotNicaragMOE, moey = TotalHisporLatMOE, p = Nicaraguanpct),
    SalvadoranMoeProp = moeprop(y = TotalHisporLat, moex = TotSalvaMOE, moey = TotalHisporLatMOE, p = Salvadoranpct),
    OtherCAMoeProp = moeprop(y = TotalHisporLat, moex = TotOtherCAMOE, moey = TotalHisporLatMOE, p = OtherCApct),
    SouthAmericanMoeProp = moeprop(y = TotalHisporLat, moex = TotSAMOE, moey = TotalHisporLatMOE, p = SouthAmericanpct),
    OtherMoeProp = moeprop(y = TotalHisporLat, moex = TotOtherHisporLatMOE, moey = TotalHisporLatMOE, p = Otherpct),

    ### adding US MOEs to the stat test###
    CubanUSMOE = rep(CubanMoeProp[3], 3),
    DominicanUSMOE = rep(DominicanMoeProp[3], 3),
    MexicanUSMOE = rep(MexicanMoeProp[3], 3),
    PuertoRicanUSMOE = rep(PuertoRicanMoeProp[3], 3),
    HonduranUSMOE = rep(HonduranMoeProp[3], 3),
    GuatemalanUSMOE = rep(GuatemalanMoeProp[3], 3),
    NicaraguanUSMOE = rep(NicaraguanMoeProp[3], 3),
    SalvadoranUSMOE = rep(SalvadoranMoeProp[3], 3),
    OtherCAUSMOE = rep(OtherCAMoeProp[3], 3),
    SouthAmericanUSMOE = rep(SouthAmericanMoeProp[3], 3),
    OtherUSMOE = rep(OtherMoeProp[3], 3),
    ######

    CubanSIG = stattest(x = CubanUS, moex = CubanUSMOE, y = Cubanpct, moey = CubanMoeProp),
    DominicanSIG = stattest(x = DominicanUS, moex = DominicanUSMOE, y = Dominicanpct, moey = DominicanMoeProp),
    MexicanSIG = stattest(x = MexicanUS, moex = MexicanUSMOE, y = Mexicanpct, moey = MexicanMoeProp),
    PuertoRicanSIG = stattest(x = PuertoRicanUS, moex = PuertoRicanUSMOE, y = PuertoRicanpct, moey = PuertoRicanMoeProp),
    HonduranSIG = stattest(x = HonduranUS, moex = HonduranUSMOE, y = Honduranpct, moey = PuertoRicanMoeProp),
    GuatemalanSIG = stattest(x = GuatemalanUS, moex = GuatemalanUSMOE, y = Guatemalanpct, moey = GuatemalanMoeProp),
    NicaraguanSIG = stattest(x = NicaraguanUS, moex = NicaraguanUSMOE, y = Nicaraguanpct, moey = NicaraguanMoeProp),
    SalvadoranSIG = stattest(x = SalvadoranUS, moex = SalvadoranUSMOE, y = Salvadoranpct, moey = SalvadoranMoeProp),
    OtherCASIG = stattest(x = OtherCAUS, moex = OtherCAUSMOE, y = OtherCApct, moey = OtherCAMoeProp),
    SouthAmericanSIG = stattest(x = SouthAmericanUS, moex = SouthAmericanUSMOE, y = SouthAmericanpct, moey = SouthAmericanMoeProp),
    OtherSIG = stattest(x = OtherUS, moex = OtherUSMOE, y = Otherpct, moey = OtherMoeProp)
  )

hispanCSV <- hispan %>%
  select(place, (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "Hispanic origin", values_to = "Value") %>%
  pivot_wider(id_cols = c("Hispanic origin"), names_from = "place", values_from = "Value")

write.csv(hispanCSV, "outputs/spreadsheets/hispan.csv")
storage_write_csv(hispanCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/hispan.csv"))

# Households with own children under 18
load("inputs/hwcRaw.RData")
hwc <- hwcRaw %>%
  mutate(
    census2000 = c(0.3007, 0.3251, 0.397, 0.3353, 0.3339),
    census2000SE = c(0.00259888, 0.002743002, 0.004572234, 0.001643334, 9.24E-05),
    tothwc = Married + MaleHH + FemaleHH,
    pcthwc = tothwc / TotalHH,
    moeagg = moeagg(cbind(MarriedMOE, MaleHHMOE, FemaleHHMOE)),
    # moeagg2000 = moeagg(cbind(MarriedMOE, MaleHHMOE, FemaleHHMOE)),
    moeprop = moeprop(y = TotalHH, moex = moeagg, moey = TotalHHMOE, p = pcthwc),
    # moeprop2000 = moeprop(y = TotalHH2000, moex = moeagg2000, moey = TotalHHMOE2000, p = pcthwc2000),
    significant = stattest(x = census2000, moex = census2000SE * 1.645, y = pcthwc, moey = moeprop)
  )

hwcCSV <- hwc %>%
  select(place, census2000, (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "hwc", values_to = "Value") %>%
  mutate(
    name = paste(place, hwc, sep = "-"),
    year = YEAR_ACS
  ) %>%
  select(-place) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")

write.csv(hwcCSV, "outputs/spreadsheets/hwc.csv")
storage_write_csv(hwcCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/hwc.csv"))

# One-person households
load("inputs/singRaw.RData")
sing <- singRaw %>%
  mutate(
    census2000 = c(0.331, 0.2665, 0.1968, 0.2707, 0.2578),
    census2000SE = c(0.002666846, 0.00258897, 0.003715052, 0.001549686, 8.57E-05),
    pctsing = SingleHH / TotalHH,
    moeprop = moeprop(y = TotalHH, moex = SingleHHMOE, moey = TotalMOE, p = pctsing),
    significant = stattest(x = census2000, moex = census2000SE * 1.645, y = pctsing, moey = moeprop)
  )

singCSV <- sing %>%
  select(place, census2000, (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "sing", values_to = "Value") %>%
  mutate(
    name = paste(place, sing, sep = "-"),
    year = YEAR_ACS
  ) %>%
  select(-place) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(singCSV, "outputs/spreadsheets/sing.csv")
storage_write_csv(singCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/sing.csv"))

# Less than a high school degree, adults 25 and older
load("inputs/hsRaw.RData")
hs <- hsRaw %>%
  mutate(
    census2000 = c(0.2531, 0.2073, 0.1613, 0.1536, 0.196),
    census2000SE = c(0.002128079, 0.001990165, 0.002814633, 0.001219893, 6.58E-05),
    totless = Male9 + Male9to12 + Female9 + Female9to12,
    pctless = totless / Total,
    moeagg = moeagg(cbind(Male9MOE, Male9to12MOE, Female9MOE, Female9to12MOE)),
    moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctless),
    significant = stattest(x = census2000, moex = census2000SE * 1.645, y = pctless, moey = moeprop)
  )

hsCSV <- hs %>%
  select(place, census2000, (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "hs", values_to = "Value") %>%
  mutate(
    name = paste(place, hs, sep = "-"),
    year = YEAR_ACS
  ) %>%
  select(-place) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(hsCSV, "outputs/spreadsheets/hs.csv")
storage_write_csv(hsCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/hs.csv"))

# Bachelor's degree or higher, adults 25 and older
load("inputs/bachRaw.RData")
bach <- bachRaw %>%
  mutate(
    census2000 = c(0.2575, 0.2149, 0.2832, 0.2256, 0.244), # are we sure about MSA number
    census2000SE = c(0.002140185, 0.002016578, 0.003447718, 0.001228988, 7.11E-05),
    totbach = MaleBach + MaleGradProf + FemaleBach + FemaleGradProf,
    pctbach = totbach / Total,
    moeagg = moeagg(cbind(MaleBachMOE, MaleGradProfMOE, FemaleBachMOE, FemaleGradProfMOE)),
    moeprop = moeprop(y = Total, moex = moeagg, moey = TotalMOE, p = pctbach),
    significant = stattest(x = census2000, moex = census2000SE * 1.645, y = pctbach, moey = moeprop)
  )

bachCSV <- bach %>%
  select(place, census2000, (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "bach", values_to = "Value") %>%
  mutate(
    name = paste(place, bach, sep = "-"),
    year = YEAR_ACS
  ) %>%
  select(-place) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(bachCSV, "outputs/spreadsheets/bach.csv")
storage_write_csv(bachCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/bach.csv"))

# Median household income, 201* inflation-adjusted dollars
#*************** NEED MOE FOR 2000 DATA**********************
census2000 <- data.frame( # census2000 = cpi99*c(27133,38435,47883,35317,41994)) #old numbers
  census2000 = cpi99 * c(27129, 38239, 47453, 35183, 41851),
  census2000MOE = cpi99 * c(679.63, 770.33, 585.37, 801.32, 902.83)
)
load("inputs/medhhRaw.RData")
medhh <- medhhRaw %>%
  bind_cols(., census2000) %>%
  mutate(significant = stattest(x = census2000, moex = census2000MOE, y = MedianHHIncome, moey = MedianHHIncomeMOE))

medhhCSV <- medhh %>%
  select(place, census2000, MedianHHIncome) %>%
  pivot_longer(-c("place"), names_to = "medhh", values_to = "Value") %>%
  mutate(
    name = paste(place, medhh, sep = "-"),
    year = YEAR_ACS
  ) %>%
  select(-place) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(medhhCSV, "outputs/spreadsheets/medhh.csv")
storage_write_csv(medhhCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/medhh.csv"))

# Internet access
load("inputs/intaRaw.RData")
inta <- intaRaw %>%
  mutate(
    cellonlypct = CellOnly / Total,
    cellmoeprop = moeprop(y = Total, moex = CellOnlyMOE, moey = TotalMOE, p = cellonlypct),
    nosubpct = NoSubscript / Total,
    nosubmoeprop = moeprop(y = Total, moex = NoSubscriptMOE, moey = TotalMOE, p = nosubpct),
    noaccpct = NoAccess / Total,
    noaccmoeprop = moeprop(y = Total, moex = NoAccessMOE, moey = TotalMOE, p = noaccpct),
    broadband = Total - (CellOnly + NoSubscript + NoAccess),
    broadbandMOE = moeagg(cbind(TotalMOE, CellOnlyMOE, NoSubscriptMOE, NoAccessMOE)),
    broadbandpct = broadband / Total,
    broadbandmoeprop = moeprop(y = Total, moex = broadbandMOE, moey = TotalMOE, p = broadbandpct),
    cellonlyUS = rep(cellonlypct[5], 5),
    nosubUS = rep(nosubpct[5], 5),
    noaccUS = rep(noaccpct[5], 5),
    broadbandUS = rep(broadbandpct[5], 5),
    cellonlyUSMOE = rep(cellmoeprop[5], 5),
    nosubUSMOE = rep(nosubmoeprop[5], 5),
    noaccUSMOE = rep(noaccmoeprop[5], 5),
    broadbandUSMOE = rep(broadbandmoeprop[5], 5),
    cellonlySIG = stattest(x = cellonlyUS, moex = cellonlyUSMOE, y = cellonlypct, moey = cellmoeprop),
    nosubSIG = stattest(x = nosubUS, moex = nosubUSMOE, y = nosubpct, moey = nosubmoeprop),
    noaccSIG = stattest(x = noaccUS, moex = noaccUSMOE, y = noaccpct, moey = noaccmoeprop),
    broadbandSIG = stattest(x = broadbandUS, moex = broadbandUSMOE, y = broadbandpct, moey = broadbandmoeprop)
  )


intaCSV <- inta %>%
  select(place, (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "inta", values_to = "Value") %>%
  pivot_wider(id_cols = c("inta"), names_from = "place", values_from = "Value")
write.csv(intaCSV, "outputs/spreadsheets/inta.csv")
storage_write_csv(intaCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/inta.csv"))

# Poverty rate, population for whom poverty has been determined
load("inputs/povRaw.RData")
pov <- povRaw %>%
  mutate(
    sf1999 = c(0.2794, 0.1365, 0.0972, 0.1838, 0.1238),
    sf1999SE = c(0.002198943, 0.001714384, 0.002287413, 0.001142575, 5.78E-05),
    pctpov = BelowPov / Total,
    moeprop = moeprop(y = Total, moex = BelowPovMOE, moey = TotalMOE, p = pctpov),
    significant = stattest(x = sf1999, moex = sf1999SE * 1.645, y = pctpov, moey = moeprop)
  )


povCSV <- pov %>%
  select(place, sf1999, (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "pov", values_to = "Value") %>%
  mutate(
    name = paste(place, pov, sep = "-"),
    year = YEAR_ACS
  ) %>%
  select(-place) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(povCSV, "outputs/spreadsheets/pov.csv")
storage_write_csv(povCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/pov.csv"))

# Children in poverty, population for whom poverty has been determined
load("inputs/childpovRaw.RData")
childpov <- childpovRaw %>%
  mutate(
    sf1999 = c(0.4053, 0.2034, 0.123, 0.2623, 0.1656),
    sf1999SE = c(0.004610543, 0.00401331, 0.004760158, 0.002505787, 1.28E-04),
    TotChildPov = BelowPovFemaleChild + BelowPovMaleChild + AbovePovFemaleChild + AbovePovMaleChild,
    moeaggtot = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE, AbovePovFemaleChildMOE, AbovePovMaleChildMOE)),
    TotBelowChildPov = BelowPovFemaleChild + BelowPovMaleChild,
    moeaggbelow = moeagg(cbind(BelowPovFemaleChildMOE, BelowPovMaleChildMOE)),
    pctBelowChildPov = TotBelowChildPov / TotChildPov,
    moeprop = moeprop(y = TotChildPov, moex = moeaggbelow, moey = moeaggtot, p = pctBelowChildPov),
    significant = stattest(x = sf1999, moex = sf1999SE * 1.645, y = pctBelowChildPov, moey = moeprop)
  )


childpovCSV <- childpov %>%
  select(place, sf1999, (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "childpov", values_to = "Value") %>%
  mutate(
    name = paste(place, childpov, sep = "-"),
    year = YEAR_ACS
  ) %>%
  select(-place) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(childpovCSV, "outputs/spreadsheets/childpov.csv")
storage_write_csv(childpovCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/childpov.csv"))

# Households without access to a vehicle
load("inputs/vehRaw.RData")
veh <- vehRaw %>%
  mutate(
    census2000 = c(0.2732, 0.0930, 0.0442, 0.1532, 0.1030),
    census2000SE = c(0.002755866, 0.001856238, 0.002095766, 0.001371385, 6.62E-05),
    vehpct = NoVehAvail / Total,
    moeprop = moeprop(y = Total, moex = NoVehAvailMOE, moey = TotalMOE, p = vehpct),
    significant = stattest(x = census2000, moex = census2000SE * 1.645, y = vehpct, moey = moeprop)
  )

vehCSV <- veh %>%
  select(place, census2000, (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "veh", values_to = "Value") %>%
  mutate(
    name = paste(place, veh, sep = "-"),
    year = YEAR_ACS
  ) %>%
  select(-place) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(vehCSV, "outputs/spreadsheets/veh.csv")
storage_write_csv(vehCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/veh.csv"))

# Population not U.S. citizens at birth
load("inputs/forborRaw.RData")
forbor <- forborRaw %>%
  mutate(TotalPopMOE = ifelse(TotalPopMOE < 0, 0, TotalPopMOE)) %>%
  mutate(
    census2000 = c(0.0425, 0.0748, 0.0237, 0.048, 0.1105),
    census2000SE = c(0.001036254, 0.001394458, 0.001243694, 0.000671199, 5.85E-05),
    forborpct = (TotForeign00to09 + TotForeign90to99 + TotForeignPre90 + TotForeign10on) / TotalPop,
    forbormoeagg = moeagg(cbind(TotForeign00to09MOE, TotForeign90to99MOE, TotForeignPre90MOE, TotForeign10onMOE)),
    forbormoeprop = moeprop(y = TotalPop, moex = forbormoeagg, moey = TotalPopMOE, p = forborpct),
    significant = stattest(x = census2000, moex = census2000SE * 1.645, y = forborpct, moey = forbormoeprop)
  )

forborCSV <- forbor %>%
  select(place, census2000, (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "forbor", values_to = "Value") %>%
  mutate(
    name = paste(place, forbor, sep = "-"),
    year = YEAR_ACS
  ) %>%
  select(-place) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(forborCSV, "outputs/spreadsheets/forbor.csv")
storage_write_csv(forborCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/forbor.csv"))

# Population who moved in the past year
load("inputs/mobRaw.RData")
mob <- mobRaw %>%
  mutate(
    sf2004mobabroad = c(0.0013, 0.0044, 0.00, 0.00, 0.006), # zeroes previously filled in for both metro and St. Tammany 2004 because of missing data, HT is filling in 2004 Metro.
    sf2004mobabroadMOE = c(0.0013025893, 0.0033903959, 0, 0, 0.0002322461),
    sf2004states = c(0.0206, 0.02, 0.00, 0.00, 0.0235),
    sf2004statesMOE = c(0.0085199087, 0.0115190278, 0, 0, 0.0006242538),
    sf2004difparish = c(0.0085, 0.03, 0.00, 0.00, 0.0302),
    sf2004difparishMOE = c(0.0037592895, 0.0119890315, 0, 0, 0.0005295007),
    sf2004withinparish = c(0.1131, 0.0958, 0.00, 0.00, 0.0973),
    sf2004withinparishMOE = c(0.023344033, 0.022923510, 0, 0, 0.001341028),
    sf2004samehouse = c(0.8565, 0.8498, 0.00, 0.00, 0.8430),
    sf2004samehouseMOE = c(0.026188401, 0.028107571, 0, 0, 0.001767759),
    mobabroadpct = TotMovedfromAbroad / Total,
    mobStatespct = TotMovedbtwnStates / Total,
    difparishpct = TotMovedinState / Total,
    withinparishpct = TotMovedinCty / Total,
    samehousepct = TotSameHouse / Total,
    mobabroadmoeprop = moeprop(y = Total, moex = TotMovedfromAbroadMOE, moey = TotalMOE, p = mobabroadpct),
    mobStatesmoeprop = moeprop(y = Total, moex = TotMovedbtwnStatesMOE, moey = TotalMOE, p = mobStatespct),
    difparishmoeprop = moeprop(y = Total, moex = TotMovedinStateMOE, moey = TotalMOE, p = difparishpct),
    withinparishmoeprop = moeprop(y = Total, moex = TotMovedinCtyMOE, moey = TotalMOE, p = withinparishpct),
    samehousemoeprop = moeprop(y = Total, moex = TotSameHouseMOE, moey = TotalMOE, p = samehousepct),
    abroadSIG = stattest(x = sf2004mobabroad, moex = sf2004mobabroadMOE, y = mobabroadpct, moey = mobabroadmoeprop),
    statesSIG = stattest(x = sf2004states, moex = sf2004statesMOE, y = mobStatespct, moey = mobStatesmoeprop),
    difparishSIG = stattest(x = sf2004difparish, moex = sf2004difparishMOE, y = difparishpct, moey = difparishmoeprop),
    withinparishSIG = stattest(x = sf2004withinparish, moex = sf2004withinparishMOE, y = withinparishpct, moey = withinparishmoeprop),
    samhouseSIG = stattest(x = sf2004samehouse, moex = sf2004samehouseMOE, y = samehousepct, moey = samehousemoeprop)
  )

mobCSV <- mob %>%
  select(place, (contains("sf2004") & !contains("MOE")), (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "mob", values_to = "Value") %>%
  mutate(
    # TODO: Ask Haleigh about the hardcoded 2022
    year = ifelse(grepl("2004", mob), "2004", "2022"),
    header = paste(place, year, sep = "-"),
    mobfinal = ifelse(grepl("abroad", mob), "abroad", mob),
    mobfinal = ifelse(grepl("tates", mob), "tates", mobfinal),
    mobfinal = ifelse(grepl("difparish", mob), "difparish", mobfinal),
    mobfinal = ifelse(grepl("withinparish", mob), "withinparish", mobfinal),
    mobfinal = ifelse(grepl("samehouse", mob), "samehouse", mobfinal),
    mobfinal = factor(mobfinal, levels = c("samehouse", "withinparish", "difparish", "tates", "abroad"))
  ) %>%
  arrange(mobfinal) %>%
  pivot_wider(id_cols = c("mobfinal"), names_from = "header", values_from = "Value")
write.csv(mobCSV, "outputs/spreadsheets/mob.csv")
storage_write_csv(mobCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/mob.csv"))

## mobility sig testing for written analysis

OPmoeagg <- moeagg(cbind(mob$TotMovedinStateMOE[1], mob$TotMovedbtwnStatesMOE[1], mob$TotMovedfromAbroadMOE[1]))
OPpct <- (mob$TotMovedinState[1] + mob$TotMovedbtwnStates[1] + mob$TotMovedfromAbroad[1]) / mob$Total[1]
OPmoeprop <- moeprop(y = mob$Total[1], moex = OPmoeagg, moey = mob$TotalMOE[1], p = OPpct)
OPsig <- stattest(x = (mob$sf2004mobabroad[1] + mob$sf2004states[1] + mob$sf2004difparish[1]), y = OPpct, moey = OPmoeprop)

jeffmoeagg <- moeagg(cbind(mob$TotMovedinStateMOE[2], mob$TotMovedbtwnStatesMOE[2], mob$TotMovedfromAbroadMOE[2]))
jeffpct <- (mob$TotMovedinState[2] + mob$TotMovedbtwnStates[2] + mob$TotMovedfromAbroad[2]) / mob$Total[2]
jeffmoeprop <- moeprop(y = mob$Total[2], moex = jeffmoeagg, moey = mob$TotalMOE[2], p = jeffpct)
jeffsig <- stattest(x = (mob$sf2004mobabroad[2] + mob$sf2004states[2] + mob$sf2004difparish[2]), y = jeffpct, moey = jeffmoeprop)


# Homeownership rates
load("inputs/hoRaw.RData")
load("inputs/hoRaw2000.RData")
ho <- hoRaw %>%
  left_join(hoRaw2000, by = "placename") %>%
  mutate( # census2000=c(0.465,0.6385,0.8048,0.6183,0.6619), #got this from SF1 for no SEs
    # census2000SE = c(0.00282756, 0.002814847, 0.0037049, 0.001695053, 1.03E-04), #using SF1 to be consistent and no MOEs
    Ownerpct = Owner / Total,
    Ownerpct2000 = Owner2000 / Total2000,
    Ownermoeprop = moeprop(y = Total, moex = OwnerMOE, moey = TotalMOE, p = Ownerpct),
    significant = stattest(x = Ownerpct2000, y = Ownerpct, moey = Ownermoeprop)
  )

hoCSV <- ho %>%
  select(place, (contains("pct"))) %>%
  select(place, Ownerpct2000, Ownerpct) %>%
  pivot_longer(-c("place"), names_to = "ho", values_to = "Value") %>%
  mutate(
    name = paste(place, ho, sep = "-"),
    year = YEAR_ACS
  ) %>%
  select(-place) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(hoCSV, "outputs/spreadsheets/ho.csv")
storage_write_csv(hoCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/ho.csv"))


# Homeowners without a mortgage
load("inputs/honomoRaw.RData")
honomo <- honomoRaw %>%
  mutate(
    census2000 = c(0.3298, 0.3458, 0.2967, 0.3476, 0.3259),
    census2000SE = c(0.004263821, 0.003804423, 0.005191947, 0.00230056781162641, 1.25E-04),
    honomopct = NoMortgage / Total,
    moeprop = moeprop(y = Total, moex = NoMortgageMOE, moey = TotalMOE, p = honomopct),
    significant = stattest(x = census2000, moex = census2000SE * 1.645, y = honomopct, moey = moeprop)
  )

honomoCSV <- honomo %>%
  select(place, census2000, (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "honomo", values_to = "Value") %>%
  mutate(
    name = paste(place, honomo, sep = "-"),
    year = YEAR_ACS
  ) %>%
  select(-place) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(honomoCSV, "outputs/spreadsheets/honomo.csv")
storage_write_csv(honomoCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/honomo.csv"))

# Renters with severe housing cost burdens
load("inputs/rentburRaw.RData")
rentbur <- rentburRaw %>%
  mutate( # sf2004=c(0.2432,0.2167,0,0.2161,0.2384),#0 for St. tammany missing value ### ****HT commenting out old values, adding ones I pulled from 2004 ACS
    # order is "Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States"
    sf2004 = c(0.2304765, 0.2019471, 0, 0, 0.2196190),
    sf2004MOE = c(0.044544178, 0.052252324, 0, 0, 0.003006082),
    rentburpct = `50orMore` / (Total - NotComputed),
    moeagg = moeagg(cbind(TotalMOE, NotComputedMOE)),
    moeprop = moeprop(y = (Total - NotComputed), moex = `50orMoreMOE`, moey = moeagg, p = rentburpct),
    significant = stattest(x = sf2004, moex = sf2004MOE, y = rentburpct, moey = moeprop)
  )

rentburCSV <- rentbur %>%
  select(place, (contains("2004")) & (!contains("MOE")), (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "rentbur", values_to = "Value") %>%
  mutate(
    name = paste(place, rentbur, sep = "-"),
    year = YEAR_ACS
  ) %>%
  select(-place) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(rentburCSV, "outputs/spreadsheets/rentbur.csv")
storage_write_csv(rentburCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/rentbur.csv"))

# Homeowners with severe housing cost burdens
load("inputs/hoburRaw.RData")
hobur <- hoburRaw %>%
  mutate( # sf2004=c(0.1620,0.0891,0,0.1134,0.0988), #0 for St. tammany missing value, ### ****HT commenting out old values, adding ones I pulled from 2004 ACS
    # order is "Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States"
    sf2004 = c(0.13226868, 0.06815970, 0, 0, 0.07895178),
    sf2004MOE = c(0.030880769, 0.025995416, 0, 0, 0.000911579), # getting these numbers from line 154 of data-pull.R
    hoburpct = (`50orMoreMortgage` + `50orMoreNoMortgage`) / (Total - NotComputedMortgage - NotComputedNoMortgage),
    moexagg = moeagg(cbind(`50orMoreMortgageMOE`, `50orMoreNoMortgageMOE`)),
    moeyagg = moeagg(cbind(TotalMOE, NotComputedMortgageMOE, NotComputedNoMortgageMOE)),
    moeprop = moeprop(y = Total, moex = moexagg, moey = moeyagg, p = hoburpct),
    significant = stattest(x = sf2004, moex = sf2004MOE, y = hoburpct, moey = moeprop)
  )

hoburCSV <- hobur %>%
  select(place, (contains("2004")) & (!contains("MOE")), (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "hobur", values_to = "Value") %>%
  mutate(
    name = paste(place, hobur, sep = "-"),
    year = YEAR_ACS
  ) %>%
  select(-place) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(hoburCSV, "outputs/spreadsheets/hobur.csv")
storage_write_csv(hoburCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/hobur.csv"))


# Median gross rent, 201* inflation-adjusted dollars
load("inputs/medrentRaw.RData")
# sf2004 <- data.frame(sf2004 = cpi04*c(566,654,0,616,694))### ****HT commenting out old values, adding ones I pulled from 2004 ACS
# order is "Orleans", "Jefferson", "St. Tammany", "New Orleans Metro Area", "United States"

sf2004 <- data.frame(
  sf2004 = cpi04 * c(566, 654, 0, 0, 694), # note that these are the exact same as what we already had, just adjusting to 2022 dollars and noting that I have the same estimates for med rent, but not the rent/hoburden percentages.)
  sf2004MOE = cpi04 * c(29, 48, 0, 0, 2)
) # also noting that I am inflation adjusting the MOES the same way

medrent <- medrentRaw %>%
  bind_cols(., sf2004) %>%
  mutate(significant = stattest(x = sf2004, moex = sf2004MOE, y = Rent, moey = RentMOE))

medrentCSV <- medrent %>%
  select(place, sf2004, Rent) %>%
  pivot_longer(-c("place"), names_to = "medrent", values_to = "Value") %>%
  mutate(
    name = paste(place, medrent, sep = "-"),
    year = YEAR_ACS
  ) %>%
  select(-place) %>%
  pivot_wider(id_cols = c("year"), names_from = "name", values_from = "Value")
write.csv(medrentCSV, "outputs/spreadsheets/medrent.csv")
storage_write_csv(medrentCSV, cont_proj, paste0("who_lives/", CURRENT_YEAR, "/outputs/medrent.csv"))


# Year structure built, 201* housing units
load("inputs/yrbuiltRaw.RData")
yrbuilt <- yrbuiltRaw %>%
  mutate(
    orLater1990pct = (`1990to1999` + `2000to2009` + `2010to2013` + `2014`) / Total,
    mid1950to1989pct = (`1950to1959` + `1960to1969` + `1970to1979` + `1980to1989`) / Total,
    orbefore1949pct = (`1940to1949` + `1939`) / Total,
    orlater1990moeagg = moeagg(cbind(`1990to1999MOE`, `2000to2009MOE`, `2010to2013MOE`, `2014MOE`)),
    mid1950to1989moeagg = moeagg(cbind(`1950to1959MOE`, `1960to1969MOE`, `1970to1979MOE`, `1980to1989MOE`)),
    orbefore1949moeagg = moeagg(cbind(`1940to1949MOE`, `1939MOE`)),
    orlater1990moeprop = moeprop(y = Total, moex = orlater1990moeagg, moey = TotalMOE, p = orLater1990pct),
    mid1950to1989moeprop = moeprop(y = Total, moex = mid1950to1989moeagg, moey = TotalMOE, p = mid1950to1989pct),
    orbefore1949moeprop = moeprop(y = Total, moex = orbefore1949moeagg, moey = TotalMOE, p = orbefore1949pct),
    orLater1990US = rep(orLater1990pct[5], 5),
    mid1950to1989US = rep(mid1950to1989pct[5], 5),
    orbefore1949US = rep(orbefore1949pct[5], 5),
    orLater1990SIG = stattest(x = orLater1990US, y = orLater1990pct, moey = orlater1990moeprop),
    mid1950to1989SIG = stattest(x = mid1950to1989US, y = mid1950to1989pct, moey = mid1950to1989moeprop),
    orbefore1949SIG = stattest(x = orbefore1949US, y = orbefore1949pct, moey = orbefore1949moeprop)
  )


yrbuiltCSV <- yrbuilt %>%
  select(place, (contains("pct"))) %>%
  pivot_longer(-c("place"), names_to = "yrbuilt", values_to = "Value") %>%
  pivot_wider(id_cols = c("yrbuilt"), names_from = "place", values_from = "Value")
write.csv(yrbuiltCSV, "outputs/spreadsheets/yrbuilt.csv")
storage_write_csv(yrbuiltCSV, cont_proj, paste("who_lives/", CURRENT_YEAR, "/outputs/yrbuilt.csv"))



# Means of transportation to work,
load("inputs/commuteRaw.RData")
load("inputs/commuteRaw2000.RData")
commute <- commuteRaw %>%
  left_join(commuteRaw2000, by = "placename") %>%
  filter(placename %in% c("Orleans", "Jefferson", "New Orleans Metro Area", "United States")) %>%
  mutate(
    census2000drive = c(0.6028, 0.7855, 0.7301, 0.7570),
    census2000carpool = c(0.1614, 0.1372, 0.1465, 0.1219),
    census2000publictransit = c(0.1322, 0.0023, 0.0530, 0.0457),
    census2000bike = c(0.0116, 0.0032, 0.0059, 0.0038),
    census2000walk = c(0.0521, 0.0174, 0.0272, 0.0293),
    census2000workhome = c(0.0266, 0.0368, 0.0241, 0.0326),
    census2000other = c(0.0133, 0.0143, 0.0133, 0.0097),
    Drivepct = DroveAlone / Total,
    Carpoolpct = Carpool / Total,
    PublicTransitpct = PublicTransit / Total,
    bikepct = Bike / Total,
    Walkpct = Walk / Total,
    Workhomepct = Workhome / Total,
    Otherpct = Other / Total,
    Drivepct2000 = DroveAlone2000 / Total2000,
    Carpoolpct2000 = Carpool2000 / Total2000,
    PublicTransitpct2000 = PublicTransit2000 / Total2000,
    bikepct2000 = Bike2000 / Total2000,
    Walkpct2000 = Walk2000 / Total2000,
    Workhomepct2000 = Workhome2000 / Total2000,
    Otherpct2000 = Other2000 / Total2000,
    Drivemoeprop = moeprop(y = Total, moex = DroveAloneMOE, moey = TotalMOE, p = Drivepct),
    carpoolmoeprop = moeprop(y = Total, moex = CarpoolMOE, moey = TotalMOE, p = Carpoolpct),
    PublicTransitmoeprop = moeprop(y = Total, moex = PublicTransitMOE, moey = TotalMOE, p = PublicTransitpct),
    bikemoeprop = moeprop(y = Total, moex = BikeMOE, moey = TotalMOE, p = bikepct),
    Walkmoeprop = moeprop(y = Total, moex = WalkMOE, moey = TotalMOE, p = Walkpct),
    workhomemoeprop = moeprop(y = Total, moex = WorkhomeMOE, moey = TotalMOE, p = Workhomepct),
    othermoeprop = moeprop(y = Total, moex = OtherMOE, moey = TotalMOE, p = Otherpct),
    Drivemoeprop2000 = moeprop(y = Total2000, moex = DroveAloneMOE2000, moey = TotalMOE2000, p = Drivepct2000),
    carpoolmoeprop2000 = moeprop(y = Total2000, moex = CarpoolMOE2000, moey = TotalMOE2000, p = Carpoolpct2000),
    PublicTransitmoeprop2000 = moeprop(y = Total2000, moex = PublicTransitMOE2000, moey = TotalMOE2000, p = PublicTransitpct2000),
    bikemoeprop2000 = moeprop(y = Total2000, moex = BikeMOE2000, moey = TotalMOE2000, p = bikepct2000),
    Walkmoeprop2000 = moeprop(y = Total2000, moex = WalkMOE2000, moey = TotalMOE2000, p = Walkpct2000),
    workhomemoeprop2000 = moeprop(y = Total2000, moex = WorkhomeMOE2000, moey = TotalMOE2000, p = Workhomepct2000),
    othermoeprop2000 = moeprop(y = Total2000, moex = OtherMOE2000, moey = TotalMOE2000, p = Otherpct2000),
    DriveSIG = stattest(x = census2000drive, moex = Drivemoeprop2000, y = Drivepct, moey = Drivemoeprop),
    carpoolSIG = stattest(x = census2000carpool, moex = carpoolmoeprop2000, y = Carpoolpct, moey = carpoolmoeprop),
    PublicTransitSIG = stattest(x = census2000publictransit, moex = , y = PublicTransitpct, moey = PublicTransitmoeprop),
    bikeSIG = stattest(x = census2000bike, moex = PublicTransitmoeprop2000, y = bikepct, moey = bikemoeprop),
    walkSIG = stattest(x = census2000walk, moex = Walkmoeprop2000, y = Walkpct, moey = Walkmoeprop),
    workhomeSIG = stattest(x = census2000workhome, moex = workhomemoeprop2000, y = Workhomepct, moey = workhomemoeprop),
    otherSIG = stattest(x = census2000other, moex = othermoeprop2000, y = Otherpct, moey = othermoeprop)
  )

commuteCSV <- commute %>%
  select(placename, (contains("pct"))) %>%
  pivot_longer(-c("placename"), names_to = "commute", values_to = "Value") %>%
  mutate(
    year = ifelse(grepl("2000", commute), 2000, 2022),
    header = paste(placename, year, sep = "-"),
    commutefinal = ifelse(grepl("ive", commute), "DroveAlone", commute),
    commutefinal = ifelse(grepl("arpool", commute), "Carpool", commutefinal),
    commutefinal = ifelse(grepl("ublic", commute), "PublicTransit", commutefinal),
    commutefinal = ifelse(grepl("Motorcycle", commute), "Motorcycle", commutefinal),
    commutefinal = ifelse(grepl("ike", commute), "Bike", commutefinal),
    commutefinal = ifelse(grepl("alk", commute), "Walk", commutefinal),
    commutefinal = ifelse(grepl("home", commute), "Workhome", commutefinal),
    commutefinal = ifelse(grepl("ther", commute), "Other", commutefinal)
  ) %>%
  pivot_wider(id_cols = c("commutefinal"), names_from = "header", values_from = "Value") %>%
  select(commutefinal, `Orleans-2000`, `Orleans-2022`, `Jefferson-2000`, `Jefferson-2022`, `New Orleans Metro Area-2000`, `New Orleans Metro Area-2022`, `United States-2000`, `United States-2022`)
write.csv(commuteCSV, "outputs/spreadsheets/commute.csv")
storage_write_csv(commuteCSV, cont_proj, paste("who_lives/", CURRENT_YEAR, "/outputs/commute.csv"))

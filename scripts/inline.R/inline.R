
#library(dplyr)

#Year to be changed manually in 02_variables_SQL
YEAR_PEP <- data_frame("year" = YEAR_PEP)
YEAR_PEP <- YEAR_PEP %>%
   select(year) %>%
  pull()
#for 2nd update later in year -- aligns with ACS year
year <- data_frame("yearu2" = year)
year <- year %>%
  select(yearu2) %>%
  pull()

allparishes <- allparishesRaw2023 %>% 
  filter(PlaceName %in% c("Orleans", "Jefferson", "Plaquemines", "St. Bernard", "St. Charles",
                          "St. James", "St. John the Baptist", "St. Tammany", "United States")) 


Overallpop <- allparishes %>%
  filter(date =="7/1/2023 population estimate") %>% ## PEP update
  filter(sex =="Total") %>%
  filter(hisp =="Total") %>%
  filter(raceSimple =="Total") %>%
  filter(age =="Total")

#The U.S. Census Bureau estimates that 1,275,762 residents were living in metro New Orleans as of July 2018
getvalue.pop.no.current <- Overallpop %>%
  filter(PlaceName == "Orleans") %>%
  select(population) %>%
  pull()
getvalue.pop.jeff.current <- Overallpop %>%
  filter(PlaceName == "Jefferson") %>%
  select(population) %>%
  pull()
getvalue.pop.plaq.current <- Overallpop %>%
  filter(PlaceName == "Plaquemines") %>%
  select(population) %>%
  pull()
getvalue.pop.bernard.current <- Overallpop %>%
  filter(PlaceName == "St. Bernard") %>%
  select(population) %>%
  pull()
getvalue.pop.charles.current <- Overallpop %>%
  filter(PlaceName == "St. Charles") %>%
  select(population) %>%
  pull()
getvalue.pop.james.current <- Overallpop %>%
  filter(PlaceName == "St. James") %>%
  select(population) %>%
  pull()
getvalue.pop.john.current <- Overallpop %>%
  filter(PlaceName == "St. John the Baptist") %>%
  select(population) %>%
  pull()

getvalue.pop.tam.current <- Overallpop %>%
  filter(PlaceName == "St. Tammany") %>%
  select(population) %>%
  pull()

metropop.current <-(getvalue.pop.no.current + getvalue.pop.jeff.current  + getvalue.pop.plaq.current  + getvalue.pop.bernard.current + getvalue.pop.charles.current + getvalue.pop.james.current + getvalue.pop.john.current + getvalue.pop.tam.current)


#According to the U.S. Census Bureau’s 2018 population estimates, there are now 91,274 fewer African Americans living in New Orleans (Orleans)
load("inputs/allparishes_retro.RData")
Overallpop2010 <- allparishes_retro %>%
  filter(date =="4/1/2010 Census population") %>%
  filter(sex =="Total") %>%
  filter(hisp =="Total") %>%
  filter(raceSimple =="Total") 

#The U.S. Census Bureau estimates that 1,275,762 residents were living in metro New Orleans as of July 2018
getvalue.pop.no.2010 <- Overallpop2010 %>%
  filter(PlaceName == "Orleans") %>%
  select(population) %>%
  pull()

getvalue.pop.jeff.2010 <- Overallpop2010 %>%
  filter(PlaceName == "Jefferson") %>%
  select(population) %>%
  pull()
getvalue.pop.plaq.2010 <- Overallpop2010 %>%
  filter(PlaceName == "Plaquemines") %>%
  select(population) %>%
  pull()

getvalue.pop.bernard.2010 <- Overallpop2010 %>%
  filter(PlaceName == "St. Bernard") %>%
  select(population) %>%
  pull()
getvalue.pop.charles.2010 <- Overallpop2010 %>%
  filter(PlaceName == "St. Charles") %>%
  select(population) %>%
  pull()
getvalue.pop.james.2010 <- Overallpop2010 %>%
  filter(PlaceName == "St. James") %>%
  select(population) %>%
  pull()
getvalue.pop.john.2010 <- Overallpop2010 %>%
  filter(PlaceName == "St. John the Baptist") %>%
  select(population) %>%
  pull()
getvalue.pop.tam.2010 <- Overallpop2010 %>%
  filter(PlaceName == "St. Tammany") %>%
  select(population) %>%
  pull()


metro2010pop <- (getvalue.pop.no.2010 + getvalue.pop.jeff.2010  + getvalue.pop.plaq.2010  + getvalue.pop.bernard.2010 + getvalue.pop.charles.2010 + getvalue.pop.james.2010 + getvalue.pop.john.2010 + getvalue.pop.tam.2010)

pctincrease.metro.current.2010 <- ((metropop.current - metro2010pop)/metro2010pop)


## Percent increase in the metro since 2020.
metro2020pop <- totalpop_metro$`Metro Area total`[totalpop_metro$year == 2020]

pctincrease.metro.current.2020 <- ((metropop.current - metro2020pop)/metro2020pop)


#The number of African Americans living In New Orleans grew every year post-Katrina (from 2006 to 2018)
#but decreased for the first time post-Katrina from 232,118 in 2018 to 231,147 In 2018.

AA2017 <- AAhistorical %>%   ### PEP update 
  filter(year == 2017) %>%
  select(POP) %>%
  pull()

AA2018 <- AAhistorical %>%
  filter(year == 2018) %>%
  select(POP) %>%
  pull()

AA2019 <- AAhistorical %>%
  filter(year == 2019) %>%
  select(POP) %>%
  pull()

AA2020 <- AAhistorical %>%
  filter(year == 2020) %>%
  select(POP) %>%
  pull()

AA2021 <- AAhistorical %>%
  filter(year == 2021) %>%
  select(POP) %>%
  pull()

AA2022 <- AAhistorical %>%
  filter(year == 2022) %>%
  select(POP) %>%
  pull()

AA2023 <- AAhistorical %>%
  filter(year == 2023) %>%
  select(POP) %>%
  pull()


getvalue.AA.no.current <- AAWhiteHispan %>%  ### PEP update
  filter(raceSimple == "Black") %>%
  select(population) %>%
  pull()

getvalue.AA.no.2000 <- AAWhiteHispan %>%
  filter(raceSimple == "Black") %>%
  select(est2000) %>%
  pull()


#getvalue.AA.no.2000-getvalue.AA.no.2018

#compared to 2000, but there are also 7,945 fewer whites. Meanwhile, the number of Hispanics grew by 7,498.[2]


getvalue.white.no.current <- AAWhiteHispan %>%  ### PEP update
  filter(raceSimple == "White") %>%
  select(population) %>%
  pull()

getvalue.white.no.2000 <- AAWhiteHispan %>%
  filter(raceSimple == "White") %>%
  select(est2000) %>%
  pull()

#getvalue.white.no.2000 - getvalue.white.no.2018

#the number of Hispanics grew by 7,498.[2]
getvalue.Hispan.no.current <- AAWhiteHispan %>% ### PEP update
  filter(raceSimple == "Hispanic") %>%
  select(population) %>%
  pull()

getvalue.Hispan.no.2000 <- AAWhiteHispan %>%
  filter(raceSimple == "Hispanic") %>%
  select(est2000) %>%
  pull()

#In Orleans, the share of the 2018 population that is African American — while lower than in 2000 when it was 66.7 percent —

getvalue.AApct.no.2000 <-ParishDemo %>%
  filter(PlaceName == "Orleans") %>%
  select(black2000) %>%
  pull()

#continues to represent the majority of city residents at 59.0 percent.

getvalue.AApct.no.current <-ParishDemo %>% ### PEP update
  filter(PlaceName == "Orleans") %>%
  select(pctblack) %>%
  pull()

#The share of Hispanics in the city increased from 3.1 percent in 2000 to 5.7 percent in 2018;

getvalue.Hispanpct.no.current <-ParishDemo %>% ### PEP update
  filter(PlaceName == "Orleans") %>%
  select(pcthisp) %>%
  pull()

getvalue.Hispanpct.no.2000 <-ParishDemo %>%
  filter(PlaceName == "Orleans") %>%
  select(hispanic2000) %>%
  pull()

#the share of Asians increased from 2.3 percent to 3.0 percent;
getvalue.Asianpct.no.current <-ParishDemo %>% ### PEP update
  filter(PlaceName == "Orleans") %>%
  select(pctasian) %>%
  pull()

getvalue.Asianpct.no.2000 <-ParishDemo %>%
  filter(PlaceName == "Orleans") %>%
  select(asian2000) %>%
  pull()
#and the share of whites increased from 26.6 percent to 30.7 percent.

getvalue.whitepct.no.current <-ParishDemo %>%  ### PEP update
  filter(PlaceName == "Orleans") %>%
  select(pctwhite) %>%
  pull()

getvalue.whitepct.no.2000 <-ParishDemo %>%
  filter(PlaceName == "Orleans") %>%
  select(white2000) %>%
  pull()

#Meanwhile, Hispanic, Asian, and African American populations increased as a share of the total population in Jefferson, St. Bernard, St. Charles, St. John the Baptist, and St. Tammanyes, each.
#In fact, the number and share of Hispanics have increased in all eightes in the metro area.


#Between 2000 and 2018, the number of Hispanics in Jefferson increased by 33,163

getvalue.hispan.jeff.2000 <- HispanicPop %>%
  filter(PlaceName == "Jefferson") %>%
  select(est2000) %>%
  pull()

getvalue.hispan.jeff.current <- HispanicPop %>% ### PEP update
  filter(PlaceName == "Jefferson") %>%
  select(population) %>%
  pull()
#getvalue.hispan.jeff.2018-getvalue.hispan.jeff.2000

#reaching over 14.9 percent of the total population.

getvalue.Hispanpct.jeff.current <-ParishDemo %>% ### PEP update
  filter(PlaceName == "Jefferson") %>%
  select(pcthisp) %>%
  pull()


#Orleans and St. Tammany gained 7,498 and 10,021 Hispanics, respectively,

#getvalue.Hispan.no.2018 - getvalue.Hispan.no.2000

getvalue.hispan.tam.2000 <- HispanicPop %>%
  filter(PlaceName == "St. Tammany") %>%
  select(est2000) %>%
  pull()

getvalue.hispan.tam.current <- HispanicPop %>%  ### PEP update
  filter(PlaceName == "St. Tammany") %>%
  select(population) %>%
  pull()

#such that the Hispanic share of the population was 5.7 percent in Orleans and 5.8 percent in St. Tammany by 2018.

#getvalue.Hispanpct.no.2018

getvalue.Hispanpct.tam.current <-ParishDemo %>%  ### PEP update
  filter(PlaceName == "St. Tammany") %>%
  select(pcthisp) %>%
  pull()



#As of July 2018, there were 115,396 Hispanics in the metro area,
getvalue.Hispan.metro.current <-ParishDemo %>%  ### PEP update
  filter(grepl("Metro",PlaceName)) %>%
  select(Hispanic) %>%
  pull()


#representing 9.0 percent of the metro population.

getvalue.Hispanpct.metro.current <-ParishDemo %>%  ### PEP update
  filter(grepl("Metro",PlaceName)) %>%
  select(pcthisp) %>%
  pull()

#This is up from 2000 when there were 58,545,

getvalue.Hispanpop.jeff.current <- HispanicPop %>%   ### PEP update
  filter(PlaceName == "Jefferson") %>%
  select(population) %>%
  pull()
getvalue.Hispanpop.no.current <- HispanicPop %>%
  filter(PlaceName == "Orleans") %>%
  select(population) %>%
  pull()
getvalue.Hispanpop.plaq.current <- HispanicPop %>%
  filter(PlaceName == "Plaquemines") %>%
  select(population) %>%
  pull()
getvalue.Hispanpop.bern.current <- HispanicPop %>%
  filter(PlaceName == "St. Bernard") %>%
  select(population) %>%
  pull()
getvalue.Hispanpop.char.current <- HispanicPop %>%
  filter(PlaceName == "St. Charles") %>%
  select(population) %>%
  pull()
getvalue.Hispanpop.jam.current <- HispanicPop %>%
  filter(PlaceName == "St. James") %>%
  select(population) %>%
  pull()
getvalue.Hispanpop.john.current <- HispanicPop %>%
  filter(PlaceName == "St. John the Baptist") %>%
  select(population) %>%
  pull()
getvalue.Hispanpop.tam.current <- HispanicPop %>%
  filter(PlaceName == "St. Tammany") %>%
  select(population) %>%
  pull()

getvalue.Hispanpop.jeff.2000 <- HispanicPop %>%   ### PEP update
  filter(PlaceName == "Jefferson") %>%
  select(est2000) %>%
  pull()
getvalue.Hispanpop.no.2000 <- HispanicPop %>%
  filter(PlaceName == "Orleans") %>%
  select(est2000) %>%
  pull()
getvalue.Hispanpop.plaq.2000 <- HispanicPop %>%
  filter(PlaceName == "Plaquemines") %>%
  select(est2000) %>%
  pull()
getvalue.Hispanpop.bern.2000 <- HispanicPop %>%
  filter(PlaceName == "St. Bernard") %>%
  select(est2000) %>%
  pull()
getvalue.Hispanpop.char.2000 <- HispanicPop %>%
  filter(PlaceName == "St. Charles") %>%
  select(est2000) %>%
  pull()
getvalue.Hispanpop.jam.2000 <- HispanicPop %>%
  filter(PlaceName == "St. James") %>%
  select(est2000) %>%
  pull()
getvalue.Hispanpop.john.2000 <- HispanicPop %>%
  filter(PlaceName == "St. John the Baptist") %>%
  select(est2000) %>%
  pull()
getvalue.Hispanpop.tam.2000 <- HispanicPop %>%
  filter(PlaceName == "St. Tammany") %>%
  select(est2000) %>%
  pull()

getvalue.hispanpop.metro.current <- (getvalue.Hispanpop.jeff.current + 
                                    getvalue.Hispanpop.no.current + 
                                    getvalue.Hispanpop.plaq.current + 
                                    getvalue.Hispanpop.bern.current + 
                                    getvalue.Hispanpop.char.current + 
                                    getvalue.Hispanpop.jam.current + 
                                    getvalue.Hispanpop.john.current + 
                                    getvalue.Hispanpop.tam.current)


getvalue.hispanpop.metro.2000 <- (getvalue.Hispanpop.jeff.2000 + 
                                     getvalue.Hispanpop.no.2000 + 
                                     getvalue.Hispanpop.plaq.2000 + 
                                     getvalue.Hispanpop.bern.2000 + 
                                     getvalue.Hispanpop.char.2000 + 
                                     getvalue.Hispanpop.jam.2000 + 
                                     getvalue.Hispanpop.john.2000 + 
                                     getvalue.Hispanpop.tam.2000)
# representing 4.4 percent of the metro population.
getvalue.Hispanpct.metro.2000 <-ParishDemo %>%
  filter(grepl("Metro",PlaceName)) %>%
  select(hispanic2000) %>%
  pull()


#Despite these recent gains, the Hispanic share of the population in metro areaes is far below the average for the United States,
#which has grown from 12.5 percent to 18.1 percent of the total U.S. population over these 17 years.

getvalue.Hispanpct.us.2000 <-ParishDemo %>%
  filter(PlaceName == "United States") %>%
  select(hispanic2000) %>%
  pull()

getvalue.Hispanpct.us.current <-ParishDemo %>%  ### PEP update
  filter(PlaceName == "United States") %>%
  select(pcthisp) %>%
  pull()




#The number of Hispanics in New Orleans metro has grown every year since 2006.
#Indeed while the overall metro population has grown 7% since 2010,
# pctincrease.metro.2018.2010

#the Hispanic population has grown 20%

getvalue.Hispgrowth.metro.2010 <-HISPpopM %>%
  filter(year ==2010) %>%
  mutate(POP = as.numeric(POP)) %>%
  select(POP)%>%
  pull()
# sum(getvalue.Hispgrowth.metro.2010)

getvalue.Hispgrowth.metro.2020 <-HISPpopM %>%  ### PEP update
  filter(year == 2020) %>%
  mutate(POP = as.numeric(POP)) %>%
  select(POP)%>%
  pull()

getvalue.Hispgrowth.metro.current <-HISPpopM %>%  ### PEP update
  filter(year == YEAR_PEP) %>%
  mutate(POP = as.numeric(POP)) %>%
  select(POP)%>%
  pull()
#sum(getvalue.Hispgrowth.metro.2018)


getvalue.Hispgrowth.metro.2010.current <- ((sum(getvalue.Hispgrowth.metro.current) - sum(getvalue.Hispgrowth.metro.2010)) / sum(getvalue.Hispgrowth.metro.2010))

getvalue.Hispgrowth.metro.2020.current <- ((sum(getvalue.Hispgrowth.metro.current) - sum(getvalue.Hispgrowth.metro.2020)) / sum(getvalue.Hispgrowth.metro.2020))


#such that Hispanics account for more than one-quarter of the metro's population growth since 2010.
getvalue.hispangrowthCOUNT.metro.2010.current <- (sum(getvalue.Hispgrowth.metro.current) - sum(getvalue.Hispgrowth.metro.2010))
getvalue.growthCOUNT.metro.2010.current <-(metropop.current - metro2010pop)

getvalue.hispangrowthCOUNT.metro.2010.2020 <- (sum(getvalue.Hispgrowth.metro.2020) - sum(getvalue.Hispgrowth.metro.2010))
getvalue.growthCOUNT.metro.2010.2020 <-(metro2020pop - metro2010pop)


getvalue.hispangrowthCOUNT.metro.2020.current <- (sum(getvalue.Hispgrowth.metro.current) - sum(getvalue.Hispgrowth.metro.2020))
getvalue.growthCOUNT.metro.2020.current <-(metropop.current - metro2020pop)


getvalue.growthpcthispanic.metro.2010.current <- (getvalue.hispangrowthCOUNT.metro.2010.current)/ getvalue.growthCOUNT.metro.2010.current

getvalue.growthpcthispanic.metro.2020.current <- (getvalue.hispangrowthCOUNT.metro.2020.current)/ getvalue.growthCOUNT.metro.2020.current



#This increase was part of a larger trend that has pushed the Central American population of the metro to -- percent of the Hispanic metro population.
getvalue.Hon.2018 <- hispan %>%
 filter(placename == "New Orleans Metro Area") %>%
 select(Honduranpct) %>%
  pull()
getvalue.sal.2018 <- hispan %>%
  filter(placename == "New Orleans Metro Area") %>%
  select(Salvadoranpct) %>%
  pull()
getvalue.guat.2018 <- hispan %>%
  filter(placename == "New Orleans Metro Area") %>%
  select(Guatemalanpct) %>%
  pull()
getvalue.nic.2018 <- hispan %>%
  filter(placename == "New Orleans Metro Area") %>%
  select(Nicaraguanpct) %>%
  pull()
getvalue.otherCA.2018 <- hispan %>%
  filter(placename == "New Orleans Metro Area") %>%
  select(OtherCApct) %>%
  pull()

getvalue.CA.metro.2018 <- (getvalue.Hon.2018 + getvalue.sal.2018 + getvalue.guat.2018 + getvalue.nic.2018 + getvalue.otherCA.2018)



#In comparison, Central Americans represent only 9 percent of the national Hispanic population.
getvalue.Hon.us.2018 <- hispan %>%
  filter(placename == "United States") %>%
  select(Honduranpct) %>%
  pull()
getvalue.sal.us.2018 <- hispan %>%
#  mutate(value = as.numeric(value)) %>%
  filter(placename == "United States") %>%
  select(Salvadoranpct) %>%
  pull()
getvalue.guat.us.2018 <- hispan %>%
  filter(placename == "United States") %>%
  select(Guatemalanpct) %>%
  pull()
getvalue.nic.us.2018 <- hispan %>%
  filter(placename == "United States") %>%
  select(Nicaraguanpct) %>%
  pull()
getvalue.otherCA.us.2018 <- hispan %>%
  filter(placename == "United States") %>%
  select(OtherCApct) %>%
  pull()

getvalue.CA.us.2018 <- (getvalue.Hon.us.2018 + getvalue.sal.us.2018 + getvalue.guat.us.2018 + getvalue.nic.us.2018 + getvalue.otherCA.us.2018)


# Mexicans are now solidly the second largest group behind Hondurans in metro New Orleans, representing - percent of the Hispanic population.
getvalue.Mex.2018 <- hispan %>%
  filter(placename == "New Orleans Metro Area") %>%
  select(Mexicanpct) %>%
  pull()

# Mexican population is much less prominent in the metro than nationally, where it represents - percent of the Hispanic population.

getvalue.MexUS.2018 <- hispan %>%
  filter(placename == "United States") %>%
  select(Mexicanpct) %>%
  pull()

#The progression of the baby boomers through the age groups, along with falling birth rates, have brought massive changes to the metro —
#and indeed the whole country — with many more changes yet to come.[4] Looking at the total population in the metro by five-year age groups for 2000 and 2018,
#the baby boomers are like a ParishDemographic tidal wave. Consequently, the median age of the metro has risen to 37.7 in 2016 from 34.8 in 2000.

getvalue.medage.current <- medageRaw$medage




#As of 2016, 26 percent of households in metro New Orleans included children

getvalue.hwc.2018 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(grepl("Metro",placename)) %>%
  select(pcthwc) %>%
pull()

getvalue.hwc.2000 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(grepl("Metro",placename)) %>%
  select(census2000) %>%
pull()

# . Between 2000 and 2016, the percent of St. Tammany households with children declined from 40 percent to 29 percent;

getvalue.hwc.StTam.2000 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(placename == "St. Tammany") %>%
  select(census2000) %>%
pull()

getvalue.hwc.StTam.2018 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(placename == "St. Tammany") %>%
  select(pcthwc) %>%
pull()

#the percent of Jefferson households with children declined from 33 percent to 26 percent;


getvalue.hwc.Jeff.2000 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(placename == "Jefferson") %>%
  select(census2000) %>%
pull()

getvalue.hwc.Jeff.2018 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(placename == "Jefferson") %>%
  select(pcthwc) %>%
pull()


#and the percent of Orleans households with children declined from 30 percent to 20 percent.

getvalue.hwc.no.2000 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(placename == "Orleans") %>%
  select(census2000) %>%
pull()

getvalue.hwc.no.2018 <- hwc %>%
  select(-significant, -contains("moeprop")) %>%
  filter(placename == "Orleans") %>%
  select(pcthwc) %>%
pull()



# The metro area share of individuals living alone grew from 27 percent in 2000 to - percent in 2016


getvalue.sing.2000 <- sing %>%
  select(-significant, -contains("moeprop")) %>%
  filter(grepl("Metro",placename)) %>%
  select(census2000) %>%
  pull()

  getvalue.sing.2018 <- sing %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",placename)) %>%
    select(pctsing) %>%
  pull()


  #similar to the trend for Jefferson where the share of households living alone grew from
  
  getvalue.singJeff.2000 <- sing %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Jefferson",placename)) %>%
    select(census2000) %>%
    pull()
  
  getvalue.singJeff.2018 <- sing %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Jefferson",placename)) %>%
    select(pctsing) %>%
    pull()
  
  
  #The increase was larger in Orleans, which jumped from 33 to 43 percent.
  getvalue.singNO.2000 <- sing %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(census2000) %>%
  pull()

  getvalue.singNO.2018 <- sing %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(pctsing) %>%
  pull()


  #While the metro has regained much of the post-Katrina population losses, youth population is substantially lower than pre-Katrina levels.
  #The metro had 358,092 children under 18 years in 2000 and only 284,231 in 2018.

 getvalue.under18.metro.2000 <- popunder18 %>%  ### PEP update
   filter(grepl("Metro",PlaceName)) %>%
   select(est2000) %>%
   pull()
 # 
  getvalue.under18.metro.current <- popunder18 %>%
    filter(grepl("Metro",PlaceName)) %>%
    select(under18) %>%
    pull()
 # 
 #  #Much of this loss was driven by Orleans, where the under 18 population declined to 79,264 from 129,408. The under 18 population is now 21 percent of the metro population, down from 27 percent in 2000.
 # 
  getvalue.under18.no.2000 <- popunder18 %>%
    filter(PlaceName == "Orleans") %>%
    select(est2000) %>%
    pull()

  getvalue.under18.no.current <- popunder18 %>%
    filter(PlaceName == "Orleans") %>%
    select(under18) %>%
    pull()
 # 
 #The under 18 population is now 21 percent of the metro population, down from 27 percent in 2000.
  getvalue.over18.metro.current <- under18metro %>%
    filter(age == "18 years and over") %>%
    select(population) %>%
    pull()
 # 
  getvalue.under18totalpop.metro.current <- under18metro %>%
    filter(age == "Total") %>%
    select(population) %>%
    pull()
 # 
  getvalue.pct.under18.metro.current <- ( (getvalue.under18totalpop.metro.current - getvalue.over18.metro.current) / getvalue.under18totalpop.metro.current)

  #The proportion of adults 25 years and older with less than a high school education declined across all three of the largestes,
  #leading to a metrowide decrease from 15 percent in 2000 to 14 percent in 2016.


  getvalue.hs.metro.2000 <- hs %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",placename)) %>%
    select(census2000) %>%
  pull()

  getvalue.hs.metro.2018 <- hs %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",placename)) %>%
    select(pctless) %>%
  pull()

  #New Orleans, the share of adults with less than a high school degree fell from 25 percent to 14 percent but is still higher than the U.S. average of 13 percent.

  getvalue.hs.no.2000 <- hs %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(census2000) %>%
  pull()

  getvalue.hs.no.2018 <- hs %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(pctless) %>%
  pull()

  getvalue.hs.us.2018 <- hs %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(pctless) %>%
  pull()

  # In New Orleans, 38 percent of adults 25 and older had a college degree in 2016
  getvalue.bach.no.2018 <- bach %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(pctbach) %>%
    pull()

  #higher than the U.S. average of 31 percent
  getvalue.bach.us.2018 <- bach %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(pctbach) %>%
    pull()

  # up from 26 percent in 2000.
  getvalue.bach.no.2000 <- bach %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(census2000) %>%
    pull()

  # The overall metro area share of adults with a bachelor’s degree grew from 23 to 29 percent
  getvalue.bach.metro.2000 <- bach %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",placename)) %>%
    select(census2000) %>%
    pull()

  getvalue.bach.metro.2018 <- bach %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",placename)) %>%
    select(pctbach) %>%
    pull()
  
  #
  
  getvalue.bachdeg.nopct <- bach %>% filter(placename == "Orleans") %>% select(pctbach) %>% as.numeric()
  getvalue.bachdeg.USpct <- bach %>% filter(placename == "United States") %>% select(pctbach) %>% as.numeric()
  getvalue.bachdeg.metropct <- bach %>% filter(placename == "New Orleans Metro Area") %>% select(pctbach)
  
  getvalue.bachdeg.white.nopct <- bach.race %>% filter(place.fac == "Orleans" & var.fac == "White,\nnon-Hispanic") %>% select(val) %>% as.numeric()
  getvalue.bachdeg.black.nopct <- bach.race %>% filter(place.fac == "Orleans" & var.fac == "Black") %>% select(val) %>% as.numeric()
  
  getvalue.bachdeg.white.maxotherpct <- bach.race %>% filter(place.fac != "Orleans" & var.fac == "White,\nnon-Hispanic") %>% select(val) %>% max() %>% as.numeric()
  
  getvalue.bachdeg.black.jeffpct <- bach.race %>% filter(place.fac == "Jefferson" & var.fac == "Black") %>% select(val)
  getvalue.bachdeg.black.stTampct <- bach.race %>% filter(place.fac == "St. Tammany" & var.fac == "Black") %>% select(val)

  getvalue.noHSdeg.metropct <- hs %>% filter(placename == "New Orleans Metro Area") %>% select(pctless)
  getvalue.noHSdeg.nopct <- hs %>% filter(placename == "Orleans") %>% select(pctless)
  
  
  # pushed household income down 5 percent in the nation between 1999 and 2016
  getvalue.medhh.us.2000 <- medhh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(census2000) %>%
    pull()

  getvalue.medhh.us.2018 <- medhh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(MedianHHIncome) %>%
    pull()

  #and 4 percent across the metro
  getvalue.medhh.metro.2000 <- medhh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",placename)) %>%
    select(census2000) %>%
    pull()

  getvalue.medhh.metro.2018 <- medhh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",placename)) %>%
    select(MedianHHIncome) %>%
    pull()

  getvalue.medhh.no.2018 <- medhh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(MedianHHIncome) %>%
    pull()

  #In Jefferson, median household income declined 10 percent between 1999 and 2016, falling to $49,678
  getvalue.medhh.jeff.2000 <- medhh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(census2000) %>%
    pull()

  getvalue.medhh.jeff.2018 <- medhh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(MedianHHIncome) %>%
    pull()
  
  getvalue.medhh.black.no <- medhh.race %>% filter(place.fac == "Orleans" & var.fac == "Black") %>% select(val) %>% as.numeric()
  getvalue.medhh.white.no <- medhh.race %>% filter(place.fac == "Orleans" & var.fac == "White,\nnon-Hispanic") %>% select(val) %>% as.numeric()
  getvalue.medhh.hisp.no <- medhh.race %>% filter(place.fac == "Orleans" & var.fac == "Hispanic,\nany race") %>% select(val) %>% as.numeric()
  
  getvalue.medhh.blkwht.no.pctdiff <- (getvalue.medhh.white.no - getvalue.medhh.black.no) / getvalue.medhh.white.no
  
  getvalue.medhh.black.stTam <- medhh.race %>% filter(place.fac == "St. Tammany" & var.fac == "Black") %>% select(val) %>% as.numeric()
  getvalue.medhh.white.stTam <- medhh.race %>% filter(place.fac == "St. Tammany" & var.fac == "White,\nnon-Hispanic") %>% select(val) %>% as.numeric()
  
  getvalue.medhh.blkwht.stTam.pctdiff <- (getvalue.medhh.white.stTam - getvalue.medhh.black.stTam) / getvalue.medhh.white.stTam
  
  
  getvalue.medhh.hisp.metro <- medhh.race %>% filter(place.fac == "Metro" & var.fac == "Hispanic,\nany race") %>% select(val) %>% as.numeric()
  
  getvalue.medhh.black.jeff <- medhh.race %>% filter(place.fac == "Jefferson" & var.fac == "Black") %>% select(val) %>% as.numeric()
  getvalue.medhh.white.jeff <- medhh.race %>% filter(place.fac == "Jefferson" & var.fac == "White,\nnon-Hispanic") %>% select(val) %>% as.numeric()
  
  getvalue.medhh.blkwht.jeff.pctdiff <- (getvalue.medhh.white.jeff - getvalue.medhh.black.jeff) / getvalue.medhh.white.jeff
  
  
  #Internet access is an important indicator of access to information. Studies have shown that without broadband, computer access, and
  # encompassing technology training services, workers and students are at a disadvantage in the job market and education system.
  #[6] Only 60 percent of households in Orleans and only 67 percent of households in Jefferson are connected to the

  getvalue.inta.no.2018 <- inta %>%
    select( -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(broadbandpct) %>%
    pull()

  getvalue.inta.jeff.2018 <- inta %>%
    select( -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(broadbandpct) %>%
    pull()


  #Internet through a home-based internet service, such as broadband (cable, DSL, or fiber), satellite, or dial-up service,
  #compared to 71 percent nationwide.

  getvalue.inta.us.2018 <- inta %>%
    select( -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(broadbandpct) %>%
    pull()

  #St. Tammany is above the national average at 75 percent of households connected to the
  getvalue.inta.tam.2018 <- inta %>%
    select( -contains("moeprop")) %>%
    filter(placename == "St. Tammany") %>%
    select(broadbandpct) %>%
    pull()
  #Internet by a home-based service internet connection. Internet access without a subscription refers to households who only
  #have access through group access locations such as school, work, a library, or coffee shop.

  #An increasingly common way to access the Internet is through a smartphone or some other cellular device. While,
  #in general, smartphone access contributes positively to lessening the Digital Divide, having access only through a
  #smartphone restricts ability to fully leverage the Internet to complete common tasks such as writing and researching
  #a resume, registering your kids for school, analyzing data about your neighborhood, or creating content for an internet
  #business.

  #13 percent of households in Orleans and 8 percent of households in Jefferson only have access
  #through a smartphone. This is compared to 10 percent nationwide.

  getvalue.intacell.no.2018 <- inta %>%
    select( -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(cellonlypct) %>%
    pull()

  getvalue.intacell.jeff.2018 <- inta %>%
    select( -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(cellonlypct) %>%
    pull()

  getvalue.intacell.us.2018 <- inta %>%
    select( -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(cellonlypct) %>%
    pull()


 #Individuals living below the poverty level indicate the economy is not providing all residents with the ability to meet
  #their most basic needs, including food, housing, and transportation.

  #The poverty rate in New Orleans declined from 28 percent in 1999 to 24 percent in 2016.
  getvalue.pov.no.1999 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(sf1999) %>%
    pull()

  getvalue.pov.no.2018 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(pctpov) %>%
    pull()

  #Meanwhile, the Jefferson poverty rate increased from 14 to 16 percent between
  #1999 and 2016.

  getvalue.pov.jeff.1999 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(sf1999) %>%
    pull()

  getvalue.pov.jeff.2018 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(pctpov) %>%
    pull()

  #St. Tammany pov

  getvalue.pov.tam.1999 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "St. Tammany") %>%
    select(sf1999) %>%
    pull()

  getvalue.pov.tam.2018 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "St. Tammany") %>%
    select(pctpov) %>%
    pull()


  #Across the U.S., the poverty rate increased from 12 to 14 percent between 1999 and 2016.

  getvalue.pov.us.1999 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(sf1999) %>%
    pull()

  getvalue.pov.us.2018 <- pov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(pctpov) %>%
    pull()
  
  getvalue.pov.black.metropct <- pov.race %>% filter(place.fac == "Metro" & var.fac == "Black") %>% select(val) %>% as.numeric()
  getvalue.pov.hisp.metropct <- pov.race %>% filter(place.fac == "Metro" & var.fac == "Hispanic,\nany race") %>% select(val) %>% as.numeric()
  getvalue.pov.white.metropct <- pov.race %>% filter(place.fac == "Metro" & var.fac == "White,\nnon-Hispanic") %>% select(val) %>% as.numeric()
  getvalue.pov.asian.metropct <- pov.race %>% filter(place.fac == "Metro" & var.fac == "Asian") %>% select(val) %>% as.numeric()
  


  #Like the overall poverty rate, the child poverty rate in New Orleans has fallen, while child poverty has increased in Jefferson and across the U.S.
  #The New Orleans child poverty rate fell from 41 percent in 1999 to 33 percent in 2016.

  getvalue.childpov.no.1999 <- childpov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(sf1999) %>%
    pull()

  getvalue.childpov.no.2018 <- childpov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(pctBelowChildPov) %>%
    pull()

  #In Jefferson, the child poverty rate jumped from 20 percent in 1999 to 26 percent in 2016 —
  getvalue.childpov.jeff.1999 <- childpov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(sf1999) %>%
    pull()

  getvalue.childpov.jeff.2018 <- childpov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(pctBelowChildPov) %>%
    pull()


  #greatly surpassing the U.S. child poverty rate, which rose from 17 to 19 percent between 1999 and 2016.
  getvalue.childpov.us.1999 <- childpov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(sf1999) %>%
    pull()

  getvalue.childpov.us.2018 <- childpov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(pctBelowChildPov) %>%
    pull()
  
  getvalue.childpov.metro.2018 <- childpov %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Metro") %>%
    select(pctBelowChildPov) %>%
    pull()
  
  getvalue.childpov.white.nopct <- childpov.race %>% filter(place.fac == "Orleans" & var.fac == "White,\nnon-Hispanic") %>% select(val) %>% as.numeric()
  getvalue.childpov.hisp.nopct <- childpov.race %>% filter(place.fac == "Orleans" & var.fac == "Hispanic,\nany race") %>% select(val) %>% as.numeric()
  getvalue.childpov.hisp.metropct <- childpov.race %>% filter(place.fac == "Metro" & var.fac == "Hispanic,\nany race") %>% select(val) %>% as.numeric()
  
  # Post-Katrina, the share of New Orleans households without access to a vehicle
  #dropped from 27 percent in 2000 to 20 percent in 2016. Nonetheless, at 20 percent,
  #New Orleans’ share is more than twice as high as in neighboringes and the nation,
  #indicating the importance of a robust public transportation system and comprehensive evacuation plan.

  getvalue.veh.no.2000 <- veh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(census2000) %>%
    pull()

  getvalue.veh.no.2018 <- veh %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(vehpct) %>%
    pull()

  #A rising foreign-born share of the population may reflect expanding economic opportunities for both high-skilled and low-skilled workers.[7]
  #That share of the population has grown in all three of the most populous metroes since 2000,
  #led by a 5 percentage point gain in Jefferson.
  #By 2016, fully 13 percent of Jefferson population was foreign-born

  getvalue.forbor.jeff.2000 <- forbor %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(census2000) %>%
    pull()

  getvalue.forbor.jeff.2018 <- forbor %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(forborpct) %>%
    pull()


  #statistically the same as the U.S. share.
  #Need to test if this is true********************************

  #In Orleans and St. Tammany, the foreign-born share of the population increased by 2 percentage point between
  #2000 and 2016 each.
  getvalue.forbor.no.2000 <- forbor %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(census2000) %>%
    pull()

  getvalue.forbor.no.2018 <- forbor %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(forborpct) %>%
    pull()

  getvalue.forbor.tam.2000 <- forbor %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "St. Tammany") %>%
    select(census2000) %>%
    pull()

  getvalue.forbor.tam.2018 <- forbor %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "St. Tammany") %>%
    select(forborpct) %>%
    pull()

  #Like the foreign-born population, a rising share of the population who moved into Orleans in the past year may
  #reflect expanding economic opportunities. The most frequent reason people move long distances, such as from one state
  #to another state, is for job opportunities.[8] In addition, the young and well-educated are more likely than others to
  #move long distances.[9]

  #In 2016, 7 percent of the population in Orleans had moved into the in the past year,

  getvalue.mobabroadpct.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(mobabroadpct) %>%
    pull()

  getvalue.mobStatespct.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(mobStatespct) %>%
    pull()

  getvalue.difparishpct.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(difparishpct) %>%
    pull()

  #up from 3 percent in 2004.
  getvalue.mobabroadpct.no.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(sf2004mobabroad) %>%
    pull()

  getvalue.mobStatespct.no.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(sf2004states) %>%
    pull()

  getvalue.difparishpct.no.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(sf2004difparish) %>%
    pull()







  getvalue.mobsamehouse.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(samehousepct) %>%
    pull()

  getvalue.mobbtwnparish.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(withinparishpct) %>%
    pull()

  getvalue.mobsamehouse.no.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(sf2004samehouse) %>%
    pull()

  getvalue.mobbtwnparish.no.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(sf2004withinparish) %>%
    pull()



  #Over half of the new movers into Orleans came from outside the state of Louisiana.

  getvalue.mobTotMovedbtwnStates.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(TotMovedbtwnStates) %>%
    pull()

  getvalue.mobTotMovedfromAbroad.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(TotMovedfromAbroad) %>%
    pull()

  getvalue.mobTotal.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(Total) %>%
    pull()

  getvalue.mobTotSameHouse.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(TotSameHouse) %>%
    pull()


  getvalue.mobTotMovedInCity.no.2018 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(TotMovedinCty) %>%
    pull()

getvalue.outsidestatepct.no.2018 <- (getvalue.mobTotMovedbtwnStates.no.2018 + getvalue.mobTotMovedfromAbroad.no.2018)/(getvalue.mobTotal.no.2018-getvalue.mobTotSameHouse.no.2018 -  getvalue.mobTotMovedInCity.no.2018)

  #In Jefferson, the share of the population who were new movers into the was 5 percent in 2004,
  #and has not significantly changed for 2016.

  getvalue.abroad.jeff.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(sf2004mobabroad) %>%
    pull()

  getvalue.states.jeff.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(sf2004states) %>%
    pull()

  getvalue.difparish.jeff.2004 <- mob %>%
    select(-abroadSIG, -statesSIG, -difparishSIG, -withinparishSIG, -samhouseSIG, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(sf2004difparish) %>%
    pull()

getvalue.moboutofparish.jeff.2004 <- (getvalue.abroad.jeff.2004  + getvalue.states.jeff.2004 + getvalue.difparish.jeff.2004)

#and has not significantly changed for 2016.



  #Homeownership rates across the U.S. have fallen since 2000 from 66 to 63 percent in 2016.

  getvalue.ho.us.2000 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(Ownerpct2000) %>%
    pull()

  getvalue.ho.us.2018 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(Ownerpct) %>%
    pull()

#Homeownership rates have stayed the same in St. Tammany at about 79 percent
  #*************Need to test signifigance********
  getvalue.ho.tam.2000 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "St. Tammany") %>%
    select(Ownerpct2000) %>%
    pull()

  getvalue.ho.tam.2018 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "St. Tammany") %>%
    select(Ownerpct) %>%
    pull()

  #and more dramatically dropped in Jefferson from 64 to 59 percent between 2000 and 2016.
  getvalue.ho.jeff.2000 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(Ownerpct2000) %>%
    pull()

  getvalue.ho.jeff.2018 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(Ownerpct) %>%
    pull()

  #In contrast, homeownership rates in New Orleans have held steady, but at a much lower 46 percent.

  getvalue.ho.no.2000 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(Ownerpct2000) %>%
    pull()

  getvalue.ho.no.2018 <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(Ownerpct) %>%
    pull()
  
  getvalue.homeowner.metropct <- ho %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "New Orleans Metro Area") %>%
    select(Ownerpct) %>%
    pull()

  getvalue.homeowner.white.metropct <- ho.race %>% filter(place.fac == "Metro" & var.fac == "White,\nnon-Hispanic") %>% select(val) %>% as.numeric()
  getvalue.homeowner.black.metropct <- ho.race %>% filter(place.fac == "Metro" & var.fac == "Black") %>% select(val) %>% as.numeric()
  
  getvalue.homeowner.black.stTam <- ho.race %>% filter(place.fac == "St. Tammany" & var.fac == "Black") %>% select(val) %>% as.numeric()
  getvalue.homeowner.white.stTam <- ho.race %>% filter(place.fac == "St. Tammany" & var.fac == "White,\nnon-Hispanic") %>% select(val) %>% as.numeric()
  getvalue.homeowner.hisp.stTam <- ho.race %>% filter(place.fac == "St. Tammany" & var.fac == "Hispanic,\nany race") %>% select(val) %>% as.numeric()
  
  getvalue.homeowner.white.nopct <- ho.race %>% filter(place.fac == "Orleans" & var.fac == "White,\nnon-Hispanic") %>% select(val) %>% as.numeric()
  getvalue.homeowner.black.nopct <- ho.race %>% filter(place.fac == "Orleans" & var.fac == "Black") %>% select(val) %>% as.numeric()
  

  #Homeowners without a mortgage own their homes free and clear of any type of loan. A high share of such homeowners usually indicates residents living in the same house for long periods of time, and helps shield neighborhoods from foreclosures.
  #The proportion of metro area homeowners without a mortgage has increased from 35 to 42 percent between 2000 and 2016,


  getvalue.honomo.metro.2018 <- honomo %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",placename)) %>%
    select(honomopct) %>%
    pull()

  getvalue.honomo.metro.2000 <- honomo %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",placename)) %>%
    select(census2000) %>%
    pull()


  #driven by changes in Orleans and Jefferson. The share of homeowners without a mortgage jumped from 33 to 43 percent in Orleans

  getvalue.honomo.no.2018 <- honomo %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(honomopct) %>%
    pull()

  getvalue.honomo.no.2000 <- honomo %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(census2000) %>%
    pull()

  #and from 35 to 42 percent in Jefferson. One reason for the surge may be that homeowners who returned after Katrina used insurance or Road Home proceeds to pay off their mortgage principal. In fact, Orleans and Jefferson received the first and second largest number of Road Home Option 1 grants among all Louisianaes.[10]

  getvalue.honomo.jeff.2018 <- honomo %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(honomopct) %>%
    pull()

  getvalue.honomo.jeff.2000 <- honomo %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(census2000) %>%
    pull()



  #High housing costs can limit a region’s ability to attract and retain the workforce essential for a healthy economy.[11] Severe housing cost burdens of more than 50 percent of household income indicate a serious problem in housing affordability.
  #In 2004, the share of severely cost-burdened renters in New Orleans and the U.S. was 24 percent.

  getvalue.rentbur.no.2004 <- rentbur %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(sf2004) %>%
    pull()

  getvalue.rentbur.us.2004 <- rentbur %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(sf2004) %>%
    pull()

  #In the 12 years since, that share has spiked to 35 percent in Orleans while rising to only 25 percent nationally.

  getvalue.rentburpct.no.2018 <- rentbur %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(rentburpct) %>%
    pull()

  getvalue.rentburpct.us.2018 <- rentbur %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(rentburpct) %>%
    pull()

  #In Jefferson, the share of renters paying more than 50 percent of household income on housing and utilities
  #is 29 percent in 2016, on par with the nation.

  getvalue.rentbur.jeff.2018 <- rentbur %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(rentburpct) %>%
    pull()

  #The share of homeowners paying more than 50 percent of household income on their mortgage, taxes, utilities, and insurance is virtually unchanged
  #in metro areaes and the nation since 2004.
  #There is a clear gap between the rate of housing cost burden for renters vs. homeowners, and that gap has widened.

  getvalue.hobur.metro.2004 <- hobur %>% 
    filter(placename == "New Orleans Metro Area") %>% 
    select(sf2004) %>% 
    pull()
  
  getvalue.hobur.metro.2018 <- hobur %>% 
    filter(placename == "New Orleans Metro Area") %>% 
    select(hoburpct) %>% 
    pull()
  
  getvalue.hobur.metro.20042018dif = (getvalue.hobur.metro.2004 - getvalue.hobur.metro.2018 )
  
  #The surge in the share of severely cost-burdened renters in New Orleans is reflective of the surge in the median gross rent (rent plus utilities) in the city.
  #From 2004 to 2016, monthly rent plus utilities rose from $724 to $934 in New Orleans,

  getvalue.medrent.no.2004 <- medrent %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(sf2004) %>%
    pull()

  getvalue.medrent.no.2018 <- medrent %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(Rent) %>%
    pull()


  #a 29 percent increase.

 #((getvalue.rentbur.no.2018 - getvalue.rentbur.no.2004) / getvalue.rentbur.no.2004)

  # Meanwhile, median gross rents increased 18 percent metrowide
  getvalue.medrent.metro.2004 <- medrent %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",placename)) %>%
    select(sf2004) %>%
    pull()

  getvalue.medrent.metro.2018 <- medrent %>%
    select(-significant, -contains("moeprop")) %>%
    filter(grepl("Metro",placename)) %>%
    select(Rent) %>%
    pull()
  #((getvalue.medrent.metro.2018 - getvalue.medrent.metro.2004) / getvalue.medrent.metro.2004)

  #compared to only 10 percent nationwide.
  getvalue.medrent.us.2004 <- medrent %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(sf2004) %>%
    pull()

  getvalue.medrent.us.2018 <- medrent %>%
    select(-significant, -contains("moeprop")) %>%
    filter(placename == "United States") %>%
    select(Rent) %>%
    pull()
  #((getvalue.medrent.us.2018 - getvalue.medrent.us.2004) / getvalue.medrent.us.2004)



  #In Orleans, fully 42 percent of all housing units are in pre-1950 structures.


  getvalue.yrbuilt1950.nola.2018 <- yrbuilt %>%
    select( -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(orbefore1949pct) %>%
    pull()



  #Meanwhile, in Jefferson, 77 percent of the housing stock was built in the 1950s, 1960s, 1970s, and 1980s,

  getvalue.yrbuilt1950to1989.jeff.2018 <- yrbuilt %>%
    select( -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(mid1950to1989pct) %>%
    pull()

  #and just 16 percent of housing stock has been built since 1990.
  getvalue.yrbuilt1990orlater.jeff.2018 <- yrbuilt %>%
    select( -contains("moeprop")) %>%
    filter(placename == "Jefferson") %>%
    select(orLater1990pct) %>%
    pull()


  #In contrast, in St. Tammany, the majority of housing units are in structures that have been built since 1990.

  getvalue.yrbuilt1990orlater.tam.2018 <- yrbuilt %>%
    select( -contains("moeprop")) %>%
    filter(placename == "St. Tammany") %>%
    select(orLater1990pct) %>%
    pull()

  #The share of commuters in New Orleans using public transportation declined sharply from 13 percent in 2000 to 7 percent in 2016,

  getvalue.publictransit.no.2000 <- commute %>%
    select(-contains("SIG"), -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(census2000publictransit) %>%
    pull()


   getvalue.publictransit.no.2018 <- commute %>%
    select(-contains("SIG"), -contains("moeprop")) %>%
    filter(placename == "Orleans") %>%
    select(PublicTransitpct) %>%
    pull()



  #while the share in Jefferson is stastically the same as 2000, with 1 percent of commuters using public transportation in 2016.

   getvalue.publictransit.jeff.2000 <- commute %>%
     select(-contains("SIG"), -contains("moeprop")) %>%
     filter(placename == "Jefferson") %>%
     select(census2000publictransit) %>%
     pull()


   getvalue.publictransit.jeff.2018 <- commute %>%
     select(-contains("SIG"), -contains("moeprop")) %>%
     filter(placename == "Jefferson") %>%
     select(PublicTransitpct) %>%
     pull()

  #But the share of bike commuters in New Orleans rose to 3 percent, giving the city the sixth highest share of bike commuting of the largest 70 cities nationwide.[15]

   getvalue.bike.no.2018 <- commute %>%
     select(-contains("SIG"), -contains("moeprop")) %>%
     filter(placename == "Orleans") %>%
     select(bikepct) %>%
     pull()

  #Meanwhile, the metro share of carpoolers fell from 15 percent in 2000 to 11 percent in 2016, as did the Orleans

   getvalue.carpool.metro.2000 <- commute %>%
     select(-contains("SIG"), -contains("moeprop")) %>%
     filter(grepl("Metro",placename)) %>%
     select(census2000carpool) %>%
     pull()

    getvalue.carpool.metro.2018 <- commute %>%
     select(-contains("SIG"), -contains("moeprop")) %>%
     filter(grepl("Metro",placename)) %>%
     select(Carpoolpct) %>%
     pull()

  #as did the Orleans share from 16 percent to 10 percent.

    getvalue.carpool.no.2000 <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(placename == "Orleans") %>%
      select(census2000carpool) %>%
      pull()

    getvalue.carpool.no.2018 <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(placename == "Orleans") %>%
      select(Carpoolpct) %>%
      pull()

    #The percentage of workers who commute by driving alone has increased within the metro region since 2000 from 73 percent to 77 percent,

    getvalue.drivealone.metro.2000 <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(grepl("Metro",placename)) %>%
      select(census2000drive) %>%
      pull()

    getvalue.drivealone.metro.2018 <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(grepl("Metro",placename)) %>%
      select(Drivepct) %>%
      pull()

    #driven by a 60 to 67 percent rise in Orleans.

    getvalue.drivealone.no.2000 <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(placename == "Orleans") %>%
      select(census2000drive) %>%
      pull()

    getvalue.drivealone.no.2018 <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(placename == "Orleans") %>%
      select(Drivepct) %>%
      pull()
    #This goes against the national trend, where the share in driving alone remained steady between 2000 and 2016
    #and where public transit use has also remained steady.

    getvalue.wfh.no.2000 <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(placename == "Orleans") %>%
      select(Workhomepct2000) %>%
      pull()
    
    getvalue.wfh.no.current <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(placename == "Orleans") %>%
      select(Workhomepct) %>%
      pull()
    
    getvalue.wfh.jeff.current <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(placename == "Jefferson") %>%
      select(Workhomepct) %>%
      pull()
    
    getvalue.wfh.stTam.current <- commute %>%
      select(-contains("SIG"), -contains("moeprop")) %>%
      filter(placename == "St. Tammany") %>%
      select(Workhomepct) %>%
      pull()




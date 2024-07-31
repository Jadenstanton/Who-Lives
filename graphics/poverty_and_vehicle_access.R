source(here("inputs/datacenter_colors.R"))

############################################
# # POVERTY AND ACCESS TO VEHICLES # #
############################################


#### 16 - Poverty rate, population for whom poverty has been determined

povGraphic <- dodgedBar(pov,
    quo(pctpov),
    "Poverty rate, population for whom poverty has been determined",
    yscale = c(0, .35),
    colors = c(DCcolor.p2teal50, DCcolor.p1mediumblue),
    comparisonyear = "1999"
)

#### 17 - Children in poverty, population for whom poverty has been determined

childpovGraphic <- dodgedBar(childpov,
    quo(pctBelowChildPov),
    "Children in poverty, population for whom poverty has been determined",
    colors = c(DCcolor.p2teal50, DCcolor.p1mediumblue),
    comparisonyear = "1999"
)

#### 18 - Households without access to a vehicle

vehGraphic <- dodgedBar(veh,
    quo(vehpct),
    "Households without access to a vehicle",
    yscale = c(0, .35)
)

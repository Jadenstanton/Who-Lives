source(here("inputs/datacenter_colors.R"))

############################################
# # HOMEOWNERSHIP # #
############################################

#### 21 - Homeownership rates

hoGraphic <- dodgedBar(ho,
    quo(Ownerpct),
    "Homeownership rates",
    yscale = c(0, .85)
)

#### 22 - Homeowners without a mortgage

honomoGraphic <- dodgedBar(honomo,
    quo(honomopct),
    "Homeowners without a mortgage",
    yscale = c(0, .55)
)

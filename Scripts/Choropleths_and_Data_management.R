rm(list=ls())
setwd("X:/PROJECTS/SEGREGATION METRICS TOOLKIT/")
source("Segregation Metrics.R")


# Get shapefiles




# Using question on religion from 2011 census

# KS209SC2b

# Minority if Roman Catholic

dta <- read.csv("E:/Census 2011/2ablk_2astd/2ablk/SNS Data Zone/KS209SCb.csv", header=T)

names(dta)[1] <- "areal.unit"
dta.nation <- dta[1,]
dta.dz <- dta[-1,]

dta.dz[,-1] <- apply(dta.dz[,-1], 2, as.numeric)

for (i in 2:dim(dta.dz)[2]){
  x <- dta.dz[,i]
  x[is.na(x)] <- 0
  x <- x+ 0.5
  dta.dz[,i] <- x
}

n <- dta.dz[,"Roman.Catholic"]
N <- dta.dz[,"All.people"]

minority.proportion <- n/N


# Shapefiles downloaded from http://saspac.org/data/2011-census/scotland-2011/

# Lowest level: output areas (42 604 areas)
#OA_shp <- readShapePoly("E:/SNS_Data/All-Scottish-Census-boundaries(shp)/OA_2011_EoR_Scotland.shp")

# Datazones : 6505 areas 
#DZ_shp <- readShapePoly("E:/SNS_Data/All-Scottish-Census-boundaries(shp)/DZ_2011_EoR_Scotland.shp")
DZ_shp <- readShapePoly("E:/SNS/ShapeFiles/SNS_Geography_14_3_2013/DataZone_2001_bdry.shp")

# Shape file location 
shape_file_dir <- "E:/SNS_Data/All-Scottish-Census-boundaries(shp)/"

# Calculating centroids of each polygons
require(rgeos)
centroids <- gCentroid(DZ_shp, byid=T)

# calculating area of each polygon
areas <- gArea(DZ_shp, byid=T)



dz.unevenness <- CalcUnevenness(DZ_shp)

# 
# Spatial Data location 
spatial_data_dir <- "E:/SNS/Data/SNS_FullData_CSV_14_3_2013/"


# Splits: 
# 1) Find those ending with ".csv"
# 2) Find first "_" : symbols to the left are category
# 3) 4 digit code to the right of first "-" is numeric code
# 4) Code from first to second "_" is specific name
# 5) 4 digit code from 2nd  to 3rd "_" is year
# 6) 2 digit code after "_" after year is areal unit type


    
Files_DF <- ExtractMetadata(spatial_data_dir)




# want something of the following format:

#areal unit type 
#areal unit value
# year
#variable 
# value




Data_ZN_2011 <- ExtractSpatialData(
    Files_DF=Files_DF, 
    years.ofinterest="2010",
    arealunits.ofinterest="ZN",
    verbose=F
)


DZ_shp <- readShapePoly(
    paste(
        shape_file_dir,
        "DZ_2011_EoR_Scotland.shp",
        sep=""
    )
)


DZ_map <- fortify(DZ_shp)

# Save things at this point

save(DZ_map, DZ_shp, Data_ZN_2011, Files_DF, file="Objects.RData")



ggplot(
    DZ_map, 
    aes(
        x=long, 
        y=lat, 
        group=group
    )
) + geom_path() + theme_minimal()

ggplot(
  DZ_map, 
  aes(
    x=long, 
    y=lat, 
    group=group  )
) + geom_polygon( alpha=0.5) + theme_minimal() + geom_path()


# Choropleth using a specific variable...
# merge using arealunit.code on Data_ZN_2011 and 
# id on DZ_map

# for now just pick one variable to merge

variable.names <- unique(Data_ZN_2011$variable.names)

Data_short <- subset(
    Data_ZN_2011, 
    subset=Data_ZN_2011$variable.names==variable.names[1], 
    select=c("arealunit.code", "variable.value")
    )

Data_Merged_2011 <- merge(
    DZ_map, 
    Data_short, 
    by.x="id", 
    by.y="arealunit.code"
)

Data_Merged_2011 <-subset(Data_Merged_2011, drop="group")

names(Data_Merged_2011)[1] <- "group"

# put in order
require(plyr)
Data_Merged_2011 <- arrange(Data_Merged_2011, group, order)


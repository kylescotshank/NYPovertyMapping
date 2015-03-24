# Library Load
library(ggplot2)
library(rgdal)
library(scales)
library(ggmap)
library(dplyr)
library(Cairo)

#-------------------------------------------------------------------------------------
# Read in Shapefile -- Census Tract Layer / General Outline
tract<-readOGR(dsn="c:/Users/kyle.shank/Documents/Random Data Things/NY Poverty Project", layer="gz_2010_36_140_00_500k")
# Fortify turns file into useable data.frame
tract<-fortify(tract,region="GEO_ID")

# Read in Shapefile - County Line Layer
county<-readOGR(dsn="c:/Users/kyle.shank/Documents/Random Data Things/NY Poverty Project", layer="gz_2010_36_140_00_500k")
county<-fortify(county, region="COUNTY")

# Read in Shapefile - Electric Utility Operating Area 
Elec<-readOGR(dsn="c:/Users/kyle.shank/Documents/Random Data Things/GIS East Files", layer="e_ServTerr")
# Transform projection of this data to conform to Census Tract 
Elec<-spTransform(Elec, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 "))
Elec<-fortify(Elec)

# read in Shapefile - Gas Utility Operating Area
Gas<-readOGR(dsn="c:/Users/kyle.shank/Documents/Random Data Things/GIS East Files", layer="g_ServTerr")
Gas<-spTransform(Gas, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 "))
Gas<-fortify(Gas)
#-------------------------------------------------------------------------------------

# Read in CAS Data - NY State only / Below Poverty Level 
mapData <- read.csv(file = "c:/Users/kyle.shank/Documents/Random Data Things/NY Poverty Project/ACS_13_5YR_S1701_with_ann.csv", stringsAsFactors=FALSE)
# Read in CSV file containing Lat, Long for participating contractors listed. 
cities <- read.csv(file = "c:/Users/kyle.shank/Documents/Random Data Things/NY Poverty Project/contractorcities.csv", stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------------

# HC03_EST_VC01 is the Column Name for All People in Census Tract Below FPL
Below100Data <- mapData[,c("GEO.id2", "HC03_EST_VC01")]
# Change Names
colnames(Below100Data) <- c("id", "percent")
# Confirm Character Class
Below100Data$id <-as.character(Below100Data$id)
# Confirm Numeric Class
Below100Data$percent <-as.numeric(Below100Data$percent)
# Set to Pecentage
Below100Data$percent <-Below100Data$percent/100
# Confirm ID validity with Shapefile
Below100Data$id <- paste("1400000US", Below100Data$id, sep = "")
# Suppress Bad Census Trac (1400000US36045980000), row 1095 -- this tract is mistakenly labeled as 1, even though this is a
# low population density zone
Below100Data[1095,]$percent<-NA

#-------------------------------------------------------------------------------------

# Estimates are missing from the data set re: % of population below 200$ of the FPL, so we will create them.
TotalPop<-as.numeric(mapData$HC01_EST_VC01)
TotalBelow200 <- as.numeric(mapData$HC01_EST_VC52)
TotalPrcntBelow200 <- TotalBelow200/TotalPop
Mapping <- as.character(mapData$GEO.id2)


Below200Data <- data.frame(Mapping, TotalPrcntBelow200)
colnames(Below200Data) <- c("id", "percent")
Below200Data$id <-as.character(Below200Data$id)
Below200Data$id <- paste("1400000US", Below200Data$id, sep = "")
# Set the same Bad Census Tract to NA
Below200Data[1095,]$percent<-NA

#-------------------------------------------------------------------------------------
# Coordinates of Cities with Participating Contractors
cityPoints <- SpatialPoints(cities)

plotData1 <- left_join(tract, Below100Data)
plotData2 <- left_join(tract, Below200Data)

#-------------------------------------------------------------------------------------
# Plot 1: Percentage of Population below The Poverty Line - Census Tract Granularity, County Lines

p <- ggplot() +
  geom_polygon(data = plotData1, aes(x = long, y = lat, group = group,
                                    fill = percent)) +
  geom_polygon(data = county, aes(x = long, y = lat, group = group),
               fill = NA, color = "grey3", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Set2", labels = percent,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE, title="% Below Poverty Line")) +
  geom_point(aes(lon, lat,colour=Locations), size = 2, colour="Black", data=cities) +
  theme_nothing(legend = TRUE) +
  labs(title = "Percentage of Population below\nThe Poverty Line")
ggsave(p, file = "NY_Below_100_Prcnt_FPL.png", width = 8, height = 8, type = "cairo-png")


#-------------------------------------------------------------------------------------
# Plot 2: Percentage of Population below 200% of the Poverty Line - Census Tract Granularity, County Lines

q <- ggplot() +
  geom_polygon(data = plotData2, aes(x = long, y = lat, group = group,
                                     fill = percent)) +
  geom_polygon(data = county, aes(x = long, y = lat, group = group),
               fill = NA, color = "grey3", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Set2", labels = percent,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE, title="% Below 200% of Poverty Line")) +
  geom_point(aes(lon, lat,colour=Locations), size = 2.5, colour="Black", data=cities) +
  theme_nothing(legend = TRUE) +
  labs(title = "Estimated Percentage of Population below\n 200% of The Poverty Line")
ggsave(q, file = "NY_Below_200_Prcnt_FPL.png", width = 8, height = 8, type = "cairo-png")

#-------------------------------------------------------------------------------------
# Plot 3: Percentage of Population below the Poverty Line - Census Tract Granularity, Electrical Utility Operating Area

r <- ggplot() +
  geom_polygon(data = plotData1, aes(x = long, y = lat, group = group,
                                     fill = percent)) +
  geom_polygon(data = Elec, aes(x = long, y = lat, group = group),
               fill = NA, color = "grey3", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Set2", labels = percent,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE, title="% Below Poverty Line")) +
  geom_point(aes(lon, lat,colour=Locations), size = 2.5, colour="Black", data=cities) +
  theme_nothing(legend = TRUE) +
  labs(title = "Percentage of Population below The Poverty Line \nby Electrical Utility Service Area")
ggsave(r, file = "NY_Below_100_Prcnt_FPL_byElecUtil.png", width = 8, height = 8, type = "cairo-png")

#-------------------------------------------------------------------------------------
# Plot 4: Percentage of Population below 200% of the Poverty Line - Census Tract Granularity, Electrical Utility Operating Area

s <- ggplot() +
  geom_polygon(data = plotData2, aes(x = long, y = lat, group = group,
                                     fill = percent)) +
  geom_polygon(data = Elec, aes(x = long, y = lat, group = group),
               fill = NA, color = "grey3", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Set2", labels = percent,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE, title="% Below 200% of Poverty Line")) +
  geom_point(aes(lon, lat,colour=Locations), size = 2.5, colour="Black", data=cities) +
  theme_nothing(legend = TRUE) +
  labs(title = "Estimaed Percentage of Population \nbelow 200% of The Poverty Line \nby Electrical Utility Service Area")
ggsave(s, file = "NY_Below_200_Prcnt_FPL_byElecUtil.png", width = 8, height = 8, type = "cairo-png")
s

#-------------------------------------------------------------------------------------
# Plot 5: Percentage of Population below the Poverty Line - Census Tract Granularity, Gas Utility Operating Area

t <- ggplot() +
  geom_polygon(data = plotData1, aes(x = long, y = lat, group = group,
                                     fill = percent)) +
  geom_polygon(data = Gas, aes(x = long, y = lat, group = group),
               fill = NA, color = "grey3", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Set2", labels = percent,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE, title="% Below Poverty Line")) +
  geom_point(aes(lon, lat,colour=Locations), size = 2.5, colour="Black", data=cities) +
  theme_nothing(legend = TRUE) +
  labs(title = "Percentage of Population below The Poverty Line \nby Gas Utility Service Area")
ggsave(t, file = "NY_Below_100_Prcnt_FPL_byGasUtil.png", width = 8, height = 8, type = "cairo-png")


#-------------------------------------------------------------------------------------
# Plot 5: Percentage of Population below the Poverty Line - Census Tract Granularity, Gas Utility Operating Area

u <- ggplot() +
  geom_polygon(data = plotData2, aes(x = long, y = lat, group = group,
                                     fill = percent)) +
  geom_polygon(data = Gas, aes(x = long, y = lat, group = group),
               fill = NA, color = "grey3", size = 0.25) +
  coord_map() +
  scale_fill_distiller(palette = "Set2", labels = percent,
                       breaks = pretty_breaks(n = 10)) +
  guides(fill = guide_legend(reverse = TRUE, title="% Below 200% of Poverty Line")) +
  geom_point(aes(lon, lat,colour=Locations), size = 2.5, colour="Black", data=cities) +
  theme_nothing(legend = TRUE) +
  labs(title = "Estimated Percentage of Population \nbelow 200% of The Poverty Line \nby Gas Utility Service Area")
ggsave(u, file = "NY_Below_200_Prcnt_FPL_byGasUtil.png", width = 8, height = 8, type = "cairo-png")

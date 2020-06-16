#Libraries
require(tidyverse)
require(rgdal)
require(sp)
require(sf)
require(maptools)
require(ggthemes)
require(ggplot2) 
require(viridis)
theme_set(theme_bw())

#Setting unique paths
Andrea <- '/Users/andreaquevedo/'
Lutz <- '/Users/ls1389/'
User <- Andrea

#### LOADING DATA ####

### US SHAPE FILE ###
US_path<- paste(User,"Box/POWERPLANTS/Shapefiles/US_shapefiles/LS_clipped/", sep="")
US_shape <-"US_counties_clipped.shp"
US_file <- paste(US_path, US_shape, sep="")
US_shp <- rgdal::readOGR(US_file)

### PUDL ###
PUDL <-read_csv(paste(User,'/Box/POWERPLANTS/Output/CSVs/pudl_generation_metadata.csv', sep=""))


#inspecting missing values
colSums(is.na(PUDL))


#### MANIPULATING DATA ####

### US SHAPE FILE ###

## Projecting to desired CRS 
US_shp <- spTransform(US_shp, CRS("+proj=longlat +datum=WGS84")) 

## Converting shapefile to dataframe 
US_shp_df <- fortify(US_shp, region="GEOID")
US_shp_df <- US_shp_df%>% filter(hole == FALSE) 
US_shp_df <- US_shp_df[order(US_shp_df$order),]
summary(US_shp_df)


### PUDL ### 

#Keeping only contiguous states
PUDL_ALL_cont<- PUDL %>% 
  filter(latitude < 100)%>% 
  filter(between(longitude, -125, -50))

colSums(is.na(PUDL_ALL_cont))


##Creating year subsets
PUDL_18 <- PUDL_ALL_cont[ which(PUDL_ALL_cont$year=="2018"), ]
PUDL_17 <- PUDL_ALL_cont[ which(PUDL_ALL_cont$year=="2017"), ]
PUDL_16 <- PUDL_ALL_cont[ which(PUDL_ALL_cont$year=="2016"), ]
PUDL_15 <- PUDL_ALL_cont[ which(PUDL_ALL_cont$year=="2015"), ]
PUDL_14 <- PUDL_ALL_cont[ which(PUDL_ALL_cont$year=="2014"), ]
PUDL_13 <- PUDL_ALL_cont[ which(PUDL_ALL_cont$year=="2013"), ]
PUDL_12 <- PUDL_ALL_cont[ which(PUDL_ALL_cont$year=="2012"), ]
PUDL_11 <- PUDL_ALL_cont[ which(PUDL_ALL_cont$year=="2011"), ]
PUDL_10 <- PUDL_ALL_cont[ which(PUDL_ALL_cont$year=="2010"), ]
PUDL_09 <- PUDL_ALL_cont[ which(PUDL_ALL_cont$year=="2009"), ]

#count of fuel by year
PUDL_18 %>% group_by(primary_fuel) %>% tally()

### MAPS PRIMARY FUEL ###

cPalette <- c("#636363", "#ec7014", "#2b8cbe", "#000000", 
              "#54278f", "#dfc27d", "#d73027", "#CC79A7","#35978f")

#2018
map_usa_18 <- ggplot() +
  geom_sf() +
  coord_sf(crs= "+proj=longlat +datum=WGS84", expand = FALSE)+
  geom_path(data = US_shp_df, 
            aes(x = long, y = lat, group = group),
            color = 'black', size = .10)+
  geom_point(data=PUDL_18 , aes(x=longitude, y=latitude, color= primary_fuel),
             size=.3, alpha=0.8, inherit.aes = FALSE)+
  labs(title = "Power Plants by Fuel Type in USA",
       subtitle = "County Level \nYear: 2018",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  guides(color = guide_legend(override.aes = list(size=4)))+
  scale_color_manual(values= cPalette)+
  ggsave("PP_MAP_PUDL_18.png",path='../Output/Figures', width = 12, height = 7, dpi=320)

map_usa_18


#2017
map_usa_17 <- ggplot() +
  geom_sf() +
  coord_sf(crs= "+proj=longlat +datum=WGS84", expand = FALSE)+
  geom_path(data = US_shp_df, 
            aes(x = long, y = lat, group = group),
            color = 'black', size = .10)+
  geom_point(data=PUDL_17 , aes(x=longitude, y=latitude, color= primary_fuel),
             size=.3, alpha=0.8, inherit.aes = FALSE)+
  labs(title = "Power Plants by Fuel Type in USA",
       subtitle = "County Level \nYear: 2017",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  guides(color = guide_legend(override.aes = list(size=4)))+
  scale_color_manual(values= cPalette)+
  ggsave("PP_MAP_PUDL_17.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

map_usa_17


#2016
map_usa_16 <- ggplot() +
  geom_sf() +
  coord_sf(crs= "+proj=longlat +datum=WGS84", expand = FALSE)+
  geom_path(data = US_shp_df, 
            aes(x = long, y = lat, group = group),
            color = 'black', size = .10)+
  geom_point(data=PUDL_16 , aes(x=longitude, y=latitude, color= primary_fuel),
             size=.3, alpha=0.8, inherit.aes = FALSE)+
  labs(title = "Power Plants by Fuel Type in USA",
       subtitle = "County Level \nYear: 2016",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  guides(color = guide_legend(override.aes = list(size=4)))+
  scale_color_manual(values= cPalette)+
  ggsave("PP_MAP_PUDL_16.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

map_usa_16


#2015
map_usa_15 <- ggplot() +
  geom_sf() +
  coord_sf(crs= "+proj=longlat +datum=WGS84", expand = FALSE)+
  geom_path(data = US_shp_df, 
            aes(x = long, y = lat, group = group),
            color = 'black', size = .10)+
  geom_point(data=PUDL_15 , aes(x=longitude, y=latitude, color= primary_fuel),
             size=.3, alpha=0.8, inherit.aes = FALSE)+
  labs(title = "Power Plants by Fuel Type in USA",
       subtitle = "County Level \nYear: 2015",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  guides(color = guide_legend(override.aes = list(size=4)))+
  scale_color_manual(values= cPalette)+
  ggsave("PP_MAP_PUDL_15.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

map_usa_15


#2014
map_usa_14 <- ggplot() +
  geom_sf() +
  coord_sf(crs= "+proj=longlat +datum=WGS84", expand = FALSE)+
  geom_path(data = US_shp_df, 
            aes(x = long, y = lat, group = group),
            color = 'black', size = .10)+
  geom_point(data=PUDL_14 , aes(x=longitude, y=latitude, color= primary_fuel),
             size=.3, alpha=0.8, inherit.aes = FALSE)+
  labs(title = "Power Plants by Fuel Type in USA",
       subtitle = "County Level \nYear: 2014",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  guides(color = guide_legend(override.aes = list(size=4)))+
  scale_color_manual(values= cPalette)+
  ggsave("PP_MAP_PUDL_14.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

map_usa_14


#2013
map_usa_13 <- ggplot() +
  geom_sf() +
  coord_sf(crs= "+proj=longlat +datum=WGS84", expand = FALSE)+
  geom_path(data = US_shp_df, 
            aes(x = long, y = lat, group = group),
            color = 'black', size = .10)+
  geom_point(data=PUDL_13 , aes(x=longitude, y=latitude, color= primary_fuel),
             size=.3, alpha=0.8, inherit.aes = FALSE)+
  labs(title = "Power Plants by Fuel Type in USA",
       subtitle = "County Level \nYear: 2013",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  guides(color = guide_legend(override.aes = list(size=4)))+
  scale_color_manual(values= cPalette)+
  ggsave("PP_MAP_PUDL_13.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

map_usa_13


#2012
map_usa_12 <- ggplot() +
  geom_sf() +
  coord_sf(crs= "+proj=longlat +datum=WGS84", expand = FALSE)+
  geom_path(data = US_shp_df, 
            aes(x = long, y = lat, group = group),
            color = 'black', size = .10)+
  geom_point(data=PUDL_12 , aes(x=longitude, y=latitude, color= primary_fuel),
             size=.3, alpha=0.8, inherit.aes = FALSE)+
  labs(title = "Power Plants by Fuel Type in USA",
       subtitle = "County Level \nYear: 2012",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  guides(color = guide_legend(override.aes = list(size=4)))+
  scale_color_manual(values= cPalette)+
  ggsave("PP_MAP_PUDL_12.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

map_usa_12


#2011
map_usa_11 <- ggplot() +
  geom_sf() +
  coord_sf(crs= "+proj=longlat +datum=WGS84", expand = FALSE)+
  geom_path(data = US_shp_df, 
            aes(x = long, y = lat, group = group),
            color = 'black', size = .10)+
  geom_point(data=PUDL_11 , aes(x=longitude, y=latitude, color= primary_fuel),
             size=.3, alpha=0.8, inherit.aes = FALSE)+
  labs(title = "Power Plants by Fuel Type in USA",
       subtitle = "County Level \nYear: 2011",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  guides(color = guide_legend(override.aes = list(size=4)))+
  scale_color_manual(values= cPalette)+
  ggsave("PP_MAP_PUDL_11.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

map_usa_11


#2010
map_usa_10 <- ggplot() +
  geom_sf() +
  coord_sf(crs= "+proj=longlat +datum=WGS84", expand = FALSE)+
  geom_path(data = US_shp_df, 
            aes(x = long, y = lat, group = group),
            color = 'black', size = .10)+
  geom_point(data=PUDL_10 , aes(x=longitude, y=latitude, color= primary_fuel),
             size=.3, alpha=0.8, inherit.aes = FALSE)+
  labs(title = "Power Plants by Fuel Type in USA",
       subtitle = "County Level \nYear: 2010",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  guides(color = guide_legend(override.aes = list(size=4)))+
  scale_color_manual(values= cPalette)+
  ggsave("PP_MAP_PUDL_10.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

map_usa_10


#2009
map_usa_09 <- ggplot() +
  geom_sf() +
  coord_sf(crs= "+proj=longlat +datum=WGS84", expand = FALSE)+
  geom_path(data = US_shp_df, 
            aes(x = long, y = lat, group = group),
            color = 'black', size = .10)+
  geom_point(data=PUDL_09 , aes(x=longitude, y=latitude, color= primary_fuel),
             size=.3, alpha=0.8, inherit.aes = FALSE)+
  labs(title = "Power Plants by Fuel Type in USA",
       subtitle = "County Level \nYear: 2009",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  guides(color = guide_legend(override.aes = list(size=4)))+
  scale_color_manual(values= cPalette)+
  ggsave("PP_MAP_PUDL_09.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

map_usa_09


#### FINDING WHICH COUNTY EACH POWER PLANT IS IN ####

# Creating data frame of only longitude and latitude values
coords <- select(PUDL_ALL_cont, longitude, latitude)

# Creating SpatialPoints object with coords and CRS
points_sp <- SpatialPoints(coords = coords,
                           proj4string = CRS("+proj=longlat +datum=WGS84"))


ppn_over <- over(points_sp,US_shp)
PUDL_ALL_cont$GEOID <- ppn_over$GEOID

colSums(is.na(PUDL_ALL_cont))


#Exporting CSV
write_csv(PUDL_ALL_cont,'../Output/CSVs/pudl_generation_geoid.csv')


#### GENERATION BY YEAR AND GEOID ####

GEOID_YEAR<- PUDL_ALL_cont %>% filter(!is.na(GEOID))%>% 
  group_by(GEOID, year) %>% 
  select(GEOID, year,coal_generation_mwh, gas_generation_mwh, oil_generation_mwh,
         hydro_generation_mwh,  nuclear_generation_mwh,solar_generation_mwh, 
         wind_generation_mwh, waste_generation_mwh, other_generation_mwh, 
         total_net_generation_mwh)%>%
  summarize_all(funs(sum), na.rm = TRUE)

#percentage of carbon generation of total generation
GEOID_YEAR<- GEOID_YEAR%>% 
  mutate(coal_percentage_gen=coal_generation_mwh/total_net_generation_mwh*100)

#creating new column assigning >100 and <0 standardized values 
GEOID_YEAR<- GEOID_YEAR%>%
  mutate(coal_percentage_gen_RESTRICT = if_else(coal_percentage_gen > 100, 100, coal_percentage_gen))%>%
  mutate(coal_percentage_gen_RESTRICT = if_else(coal_percentage_gen < 0, 0, coal_percentage_gen_RESTRICT))

#Exporting CSV
write_csv(GEOID_YEAR,'../Output/CSVs/GEOID_carbon_percentage.csv')

##Creating year subsets
GEOID_18 <- GEOID_YEAR[ which(GEOID_YEAR$year=="2018"), ]
GEOID_17 <- GEOID_YEAR[ which(GEOID_YEAR$year=="2017"), ]
GEOID_16 <- GEOID_YEAR[ which(GEOID_YEAR$year=="2016"), ]
GEOID_15 <- GEOID_YEAR[ which(GEOID_YEAR$year=="2015"), ]
GEOID_14 <- GEOID_YEAR[ which(GEOID_YEAR$year=="2014"), ]
GEOID_13 <- GEOID_YEAR[ which(GEOID_YEAR$year=="2013"), ]
GEOID_12 <- GEOID_YEAR[ which(GEOID_YEAR$year=="2012"), ]
GEOID_11 <- GEOID_YEAR[ which(GEOID_YEAR$year=="2011"), ]
GEOID_10 <- GEOID_YEAR[ which(GEOID_YEAR$year=="2010"), ]
GEOID_09 <- GEOID_YEAR[ which(GEOID_YEAR$year=="2009"), ]


### MAPS COAL GENERATION PERCENTAGE ###

#Reading sf shapefile and projecting crs
us_gdf <- st_read(US_file)
us_gdf<- us_gdf %>% select(GEOID)
us_gdf <- st_set_crs(us_gdf, "+proj=longlat +datum=WGS84")

#Setting discrete intervals
pretty_breaks <- c(20,40,60,80)
minVal <- min(GEOID_YEAR$coal_percentage_gen_RESTRICT, na.rm = T)
maxVal <- max(GEOID_YEAR$coal_percentage_gen_RESTRICT, na.rm = T)

labels <- c()
brks <- c(minVal, pretty_breaks, maxVal)
for(idx in 1:length(brks)){
  labels <- c(labels,round(brks[idx + 1], 2))
}
labels <- labels[1:length(labels)-1]


#2018
us_geo_18 <- left_join(us_gdf,GEOID_18, by="GEOID")
us_geo_18 <- st_set_geometry(us_geo_18, "geometry")

us_geo_18$brks <- cut(us_geo_18$coal_percentage_gen_RESTRICT, 
                      breaks = brks, 
                      include.lowest = TRUE, 
                      labels = labels)

brks_scale <- levels(us_geo_18$brks)
labels_scale <- rev(brks_scale)

coal_18<- ggplot(data=us_geo_18) + 
  geom_sf(aes(fill=brks),
          color = 'black', size = .10) +
  labs(title = "Percentage of Coal Generation",
       subtitle = "County Level \nYear: 2018",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  scale_fill_manual(
    values = rev(magma(7)[1:6]),
    breaks = rev(brks_scale),
    name = "Percentage of Total Generation",
    drop = FALSE,
    labels = labels_scale,
    na.value="gray90",
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"))+
  ggsave("COAL_MAP_PUDL_18.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

coal_18


#2017
us_geo_17 <- left_join(us_gdf,GEOID_17, by="GEOID")
us_geo_17 <- st_set_geometry(us_geo_17, "geometry")

us_geo_17$brks <- cut(us_geo_17$coal_percentage_gen_RESTRICT, 
                      breaks = brks, 
                      include.lowest = TRUE, 
                      labels = labels)

brks_scale <- levels(us_geo_17$brks)
labels_scale <- rev(brks_scale)

coal_17<- ggplot(data=us_geo_17) + 
  geom_sf(aes(fill=brks),
          color = 'black', size = .10) +
  labs(title = "Percentage of Coal Generation",
       subtitle = "County Level \nYear: 2017",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  scale_fill_manual(
    values = rev(magma(7)[1:6]),
    breaks = rev(brks_scale),
    name = "Percentage of Total Generation",
    drop = FALSE,
    labels = labels_scale,
    na.value="gray90",
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"))+
  ggsave("COAL_MAP_PUDL_17.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

coal_17


#2016
us_geo_16 <- left_join(us_gdf,GEOID_16, by="GEOID")
us_geo_16 <- st_set_geometry(us_geo_16, "geometry")

us_geo_16$brks <- cut(us_geo_16$coal_percentage_gen_RESTRICT, 
                      breaks = brks, 
                      include.lowest = TRUE, 
                      labels = labels)

brks_scale <- levels(us_geo_16$brks)
labels_scale <- rev(brks_scale)

coal_16<- ggplot(data=us_geo_16) + 
  geom_sf(aes(fill=brks),
          color = 'black', size = .10) +
  labs(title = "Percentage of Coal Generation",
       subtitle = "County Level \nYear: 2016",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  scale_fill_manual(
    values = rev(magma(7)[1:6]),
    breaks = rev(brks_scale),
    name = "Percentage of Total Generation",
    drop = FALSE,
    labels = labels_scale,
    na.value="gray90",
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"))+
  ggsave("COAL_MAP_PUDL_16.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

coal_16


#2015
us_geo_15 <- left_join(us_gdf,GEOID_15, by="GEOID")
us_geo_15 <- st_set_geometry(us_geo_15, "geometry")

us_geo_15$brks <- cut(us_geo_15$coal_percentage_gen_RESTRICT, 
                      breaks = brks, 
                      include.lowest = TRUE, 
                      labels = labels)

brks_scale <- levels(us_geo_15$brks)
labels_scale <- rev(brks_scale)

coal_15<- ggplot(data=us_geo_15) + 
  geom_sf(aes(fill=brks),
          color = 'black', size = .10) +
  labs(title = "Percentage of Coal Generation",
       subtitle = "County Level \nYear: 2015",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  scale_fill_manual(
    values = rev(magma(7)[1:6]),
    breaks = rev(brks_scale),
    name = "Percentage of Total Generation",
    drop = FALSE,
    labels = labels_scale,
    na.value="gray90",
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"))+
  ggsave("COAL_MAP_PUDL_15.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

coal_15


#2014
us_geo_14 <- left_join(us_gdf,GEOID_14, by="GEOID")
us_geo_14 <- st_set_geometry(us_geo_14, "geometry")

us_geo_14$brks <- cut(us_geo_14$coal_percentage_gen_RESTRICT, 
                      breaks = brks, 
                      include.lowest = TRUE, 
                      labels = labels)

brks_scale <- levels(us_geo_14$brks)
labels_scale <- rev(brks_scale)

coal_14<- ggplot(data=us_geo_14) + 
  geom_sf(aes(fill=brks),
          color = 'black', size = .10) +
  labs(title = "Percentage of Coal Generation",
       subtitle = "County Level \nYear: 2014",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  scale_fill_manual(
    values = rev(magma(7)[1:6]),
    breaks = rev(brks_scale),
    name = "Percentage of Total Generation",
    drop = FALSE,
    labels = labels_scale,
    na.value="gray90",
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"))+
  ggsave("COAL_MAP_PUDL_14.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

coal_14



#2013
us_geo_13 <- left_join(us_gdf,GEOID_13, by="GEOID")
us_geo_13 <- st_set_geometry(us_geo_13, "geometry")

us_geo_13$brks <- cut(us_geo_13$coal_percentage_gen_RESTRICT, 
                      breaks = brks, 
                      include.lowest = TRUE, 
                      labels = labels)

brks_scale <- levels(us_geo_13$brks)
labels_scale <- rev(brks_scale)

coal_13<- ggplot(data=us_geo_13) + 
  geom_sf(aes(fill=brks),
          color = 'black', size = .10) +
  labs(title = "Percentage of Coal Generation",
       subtitle = "County Level \nYear: 2013",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  scale_fill_manual(
    values = rev(magma(7)[1:6]),
    breaks = rev(brks_scale),
    name = "Percentage of Total Generation",
    drop = FALSE,
    labels = labels_scale,
    na.value="gray90",
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"))+
  ggsave("COAL_MAP_PUDL_13.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

coal_13


#2012
us_geo_12 <- left_join(us_gdf,GEOID_12, by="GEOID")
us_geo_12 <- st_set_geometry(us_geo_12, "geometry")

us_geo_12$brks <- cut(us_geo_12$coal_percentage_gen_RESTRICT, 
                      breaks = brks, 
                      include.lowest = TRUE, 
                      labels = labels)

brks_scale <- levels(us_geo_12$brks)
labels_scale <- rev(brks_scale)

coal_12<- ggplot(data=us_geo_12) + 
  geom_sf(aes(fill=brks),
          color = 'black', size = .10) +
  labs(title = "Percentage of Coal Generation",
       subtitle = "County Level \nYear: 2012",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  scale_fill_manual(
    values = rev(magma(7)[1:6]),
    breaks = rev(brks_scale),
    name = "Percentage of Total Generation",
    drop = FALSE,
    labels = labels_scale,
    na.value="gray90",
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"))+
  ggsave("COAL_MAP_PUDL_12.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

coal_12


#2011
us_geo_11 <- left_join(us_gdf,GEOID_11, by="GEOID")
us_geo_11 <- st_set_geometry(us_geo_11, "geometry")

us_geo_11$brks <- cut(us_geo_11$coal_percentage_gen_RESTRICT, 
                      breaks = brks, 
                      include.lowest = TRUE, 
                      labels = labels)

brks_scale <- levels(us_geo_11$brks)
labels_scale <- rev(brks_scale)

coal_11<- ggplot(data=us_geo_11) + 
  geom_sf(aes(fill=brks),
          color = 'black', size = .10) +
  labs(title = "Percentage of Coal Generation",
       subtitle = "County Level \nYear: 2011",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  scale_fill_manual(
    values = rev(magma(7)[1:6]),
    breaks = rev(brks_scale),
    name = "Percentage of Total Generation",
    drop = FALSE,
    labels = labels_scale,
    na.value="gray90",
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"))+
  ggsave("COAL_MAP_PUDL_11.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

coal_11


#2010
us_geo_10 <- left_join(us_gdf,GEOID_10, by="GEOID")
us_geo_10 <- st_set_geometry(us_geo_10, "geometry")

us_geo_10$brks <- cut(us_geo_10$coal_percentage_gen_RESTRICT, 
                      breaks = brks, 
                      include.lowest = TRUE, 
                      labels = labels)

brks_scale <- levels(us_geo_10$brks)
labels_scale <- rev(brks_scale)

coal_10<- ggplot(data=us_geo_10) + 
  geom_sf(aes(fill=brks),
          color = 'black', size = .10) +
  labs(title = "Percentage of Coal Generation",
       subtitle = "County Level \nYear: 2010",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  scale_fill_manual(
    values = rev(magma(7)[1:6]),
    breaks = rev(brks_scale),
    name = "Percentage of Total Generation",
    drop = FALSE,
    labels = labels_scale,
    na.value="gray90",
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"))+
  ggsave("COAL_MAP_PUDL_10.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

coal_10


#2009
us_geo_09 <- left_join(us_gdf,GEOID_09, by="GEOID")
us_geo_09 <- st_set_geometry(us_geo_09, "geometry")

us_geo_09$brks <- cut(us_geo_09$coal_percentage_gen_RESTRICT, 
                      breaks = brks, 
                      include.lowest = TRUE, 
                      labels = labels)

brks_scale <- levels(us_geo_09$brks)
labels_scale <- rev(brks_scale)

coal_09<- ggplot(data=us_geo_09) + 
  geom_sf(aes(fill=brks),
          color = 'black', size = .10) +
  labs(title = "Percentage of Coal Generation",
       subtitle = "County Level \nYear: 2009",
       caption = "Source: PUDL")+
  theme_map()+
  theme(legend.position = "top",
        plot.title = element_text(face="bold"))+
  scale_fill_manual(
    values = rev(magma(7)[1:6]),
    breaks = rev(brks_scale),
    name = "Percentage of Total Generation",
    drop = FALSE,
    labels = labels_scale,
    na.value="gray90",
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(70 / length(labels), units = "mm"),
      title.position = 'top',
      title.hjust = 0.5,
      label.hjust = 1,
      nrow = 1,
      byrow = T,
      reverse = T,
      label.position = "bottom"))+
  ggsave("COAL_MAP_PUDL_09.png", path='../Output/Figures', width = 12, height = 7, dpi=320)

coal_09

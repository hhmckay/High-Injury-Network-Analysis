# HIN

library(dplyr)
library(sf)
library(mapview)
library(tigris)
library(stplanr)
library(ggplot2)
library(SpatialKDE)
library(spatstat)
library(sfhotspot)
library(ggspatial)
library(prettymapr)

# Read boundary
boundary <- read_sf("/Users/henrymckay/Downloads/HIN/GRID/POLYGON.shp") %>%
  st_transform("epsg:4326")

# read crashes
TIMS <- read.csv("/Users/henrymckay/Downloads/HIN/SAC_TIMS.csv") %>%
  filter(!is.na(POINT_X) | !is.na(POINT_Y))

TIMS <- st_as_sf(TIMS, coords = c("POINT_X","POINT_Y"), crs = 4326) %>%
  st_intersection(boundary)

TIMS_test <- TIMS %>%
  mutate(FSC = COUNT_PED_KILLED + COUNT_PED_INJURED + COUNT_BICYCLIST_KILLED + COUNT_BICYCLIST_INJURED) %>%
  filter(FSC > 0) %>%
  select(FSC) %>%
  st_transform("epsg:3310")

crash_clip <- TIMS_test %>%
  hotspot_kde(bandwidth = 150, cell_size = 50) %>%
  st_transform("epsg:4326")
st_intersection(boundary)

roads <- tigris::roads(state = 06, county = 067) %>%
  st_transform("epsg:4326") %>%
  st_intersection(boundary)

roads2 <- roads  %>%
  st_intersection(crash_clip) %>%
  mutate(count = row_number()) %>%
  mutate(segment_id = paste0(LINEARID, "_", count)) %>%
  select(kde, segment_id) %>%
  mutate(length = as.numeric(st_length(roads2)))

ROAD_test <- roads2

TIMS_test <- TIMS_test %>%
  st_transform("epsg:4326")

crash_location <- st_join(TIMS_test, ROAD_test, join = st_nearest_feature) %>%
  select(FSC, segment_id) %>%
  st_drop_geometry() %>%
  group_by(segment_id) %>%
  summarise(crash_count = sum(FSC))

merged_crashes <- merge(ROAD_test,
                        crash_location,
                        by = "segment_id",
                        all.x = T)

crash_segments <- merged_crashes %>%
  mutate(crash_count = ifelse(is.na(crash_count), 0, crash_count)) %>%
  arrange(-kde, -crash_count) %>%
  mutate(cum_crashes = cumsum(crash_count)) %>%
  mutate(cum_pct = cum_crashes / sum(TIMS_test$FSC)) %>%
  mutate(dist_cum = cumsum(length)) %>%
  mutate(dist_pct = dist_cum / sum(merged_crashes$length))

cutoff <- 1

crash_segments_map <- crash_segments %>%
  filter(cum_pct <= cutoff)

# Create ggplot map object
map <- ggplot(boundary) +
  geom_sf(aes(), color = "grey65") +
  geom_sf(data = roads, color = "grey66", linewidth = 1) +
  geom_sf(data = crash_segments_map, color = "red", linewidth = 2) +
  #coord_sf(xlim = c(-121.9, -121), ylim = c(38, 38.75)) +
  labs(title = "Sac Grid High Injury Network",
       subtitle = paste0("% of all FSC: ", cutoff, "\n", "% of road miles: ", round(sum(crash_segments_map$length) / sum(crash_segments$length), digits = 2))) +
  theme_void() 

# Adjust font, color, and size of map text
map <- map + theme(
  plot.title = element_text(color = "grey23", size = 30, face = "bold"),
  plot.subtitle = element_text(color = "grey23", size = 25),
  legend.title=element_text(color = "grey23", size=15, face = "bold")
)

# Set filepath to map output
filepath = paste0("/Users/henrymckay/Downloads/HIN/Maps/Map", cutoff, ".png")

# Save map as png
ggsave(filepath, 
       plot = map,
       dpi = 600,
       height = 12,
       width = 12,
       bg = 'white')















map1 <- ggplot() +
  #annotation_map_tile(type = "cartolight") +
  geom_sf(
    aes(fill = kde),
    data = crash_clip,
    alpha = .50,
    colour = NA
  ) +
  theme_void()

filepath = paste0("/Users/henrymckay/Downloads/HIN/Maps/Map_Crashes4.png")

# Save map as png
ggsave(filepath, 
       plot = map1,
       dpi = 200,
       height = 12,
       width = 12,
       bg = 'white')




3310

st_set

TIMS_FATAL <- TIMS %>%
  filter(COLLISION_SEVERITY == 1)

roads <- tigris::roads(state = 06, county = 067)
st_crs(roads) <- 4326

TIMS_test <- TIMS %>%
  mutate(FSC = COUNT_PED_KILLED + COUNT_PED_INJURED + COUNT_BICYCLIST_KILLED + COUNT_BICYCLIST_INJURED) %>%
  filter(FSC > 0) %>%
  select(FSC) %>%
  st_transform("epsg:3310")


ROAD_test <- roads

crash_location <- st_join(TIMS_test, ROAD_test, join = st_nearest_feature) %>%
  select(FSC, LINEARID) %>%
  st_drop_geometry() %>%
  group_by(LINEARID) %>%
  summarise(crash_count = sum(FSC))

merged_crashes <- merge(ROAD_test,
                        crash_location,
                        by = "LINEARID",
                        all.x = T)


### Divide Roads
roads <- roads %>%
  filter(FULLNAME == "Freeport Blvd")

#dissolve

raods_divide <- line_segment(roads, segment_length = 500)
mapview(raods_divide)
raods_divide$length2 = as.numeric(st_length(raods_divide))

roads_divide <- raods_divide %>%
  mutate(count = row_number()) %>%
  mutate(segment_id = paste0(LINEARID, "_", count)) %>%
  select(length2, segment_id)


crash_location <- st_join(TIMS_test, roads_divide, join = st_nearest_feature) %>%
  select(FSC, length2, segment_id) %>%
  st_drop_geometry() %>%
  group_by(segment_id) %>%
  summarise(crash_count = sum(FSC))

crash_segments <- merge(roads_divide,
                        crash_location,
                        by = "segment_id",
                        all.x = T) %>%
  mutate(normalized_crashes = crash_count / length2) %>%
  filter(normalized_crashes > 0)

crash_segments <- crash_segments %>%
  arrange(desc(normalized_crashes)) %>%
  mutate(cum_crashes = cumsum(crash_count)) %>%
  mutate(cum_pct = cum_crashes / sum(TIMS_test$FSC))

county <- counties(state = 06)
county <- county %>%
  filter(GEOID == "06067") %>%
  st_transform("epsg:4326")

cutoff <- 1

crash_segments_map <- crash_segments %>%
  filter(cum_pct <= cutoff)

# Create ggplot map object
map <- ggplot(county) +
  geom_sf(aes(), color = "grey65") +
  geom_sf(data = roads, color = "grey66", size = .1) +
  geom_sf(data = crash_segments_map, color = "red", size = .5) +
  coord_sf(xlim = c(-121.9, -121), ylim = c(38, 38.75)) +
  labs(title = "Sacramento County High Injury Network",
       subtitle = paste0(cutoff, " of all FSC")) +
  theme_void() 

# Adjust font, color, and size of map text
map <- map + theme(
  plot.title = element_text(color = "grey23", size = 30, face = "bold"),
  plot.subtitle = element_text(color = "grey23", size = 25),
  legend.title=element_text(color = "grey23", size=15, face = "bold")
)

# Set filepath to map output
filepath = paste0("/Users/henrymckay/Downloads/HIN/Maps/Map", cutoff, ".png")

# Save map as png
ggsave(filepath, 
       plot = map,
       dpi = 600,
       height = 12,
       width = 12,
       bg = 'white')


# KDE Estimation
crash_clip <- TIMS_test %>%
  hotspot_kde(bandwidth = 250, cell_size = 100) %>%
  st_transform("epsg:4326")
  st_intersection(county)
  
map1 <- ggplot() +
  annotation_map_tile(type = "cartolight", zoomin = 0) +
  geom_sf(
    aes(fill = kde),
    data = crash_clip,
    alpha = .50,
    colour = NA
  )

filepath = paste0("/Users/henrymckay/Downloads/HIN/Maps/Map_Crashes2.png")

# Save map as png
ggsave(filepath, 
       plot = map1,
       dpi = 600,
       height = 12,
       width = 12,
       bg = 'white')

map1 <- ggplot(county) +
  geom_sf(
    aes(fill = kde),
        data = crash_clip,
        alpha = 0.75,
        color = NA
        )





kde

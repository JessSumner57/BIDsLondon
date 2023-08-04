library(tidyverse)
library(sf)
library(tmap)
library(janitor)
library(spatstat)
library (osmdata)
library (rgdal)
library(dplyr)


library(sf)


camden_points_df <- read.csv("/Users/jessicasumner/Desktop/Dissertation/BID/business_improvement_districts/Camdendata.csv")
camden_points_sf <- st_as_sf(camden_points_df, wkt = "geometry")

BID_sf <- st_read("/Users/jessicasumner/Desktop/Dissertation/BID/business_improvement_districts/business_improvement_districts.shp")
london <- st_read ("/Users/jessicasumner/Desktop/Dissertation/BID/business_improvement_districts/London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")
camden_points_transformed <- st_transform(camden_points_sf, crs = 27700)
st_crs(camden_points_sf) <- 4326
camden_points_transformed <- st_transform(camden_points_sf, crs = 27700)


BID_sf <- st_transform(BID_sf, crs = 27700)

library(sf)
library(ggplot2)

ggplot() +
  geom_sf(data = london, fill = "white", color = "black") +
  geom_sf(data = BID_sf, fill = "blue", alpha = 0.5) + # Change "blue" to the desired fill color for BIDs
  coord_sf(crs = st_crs(27700)) +
  theme_minimal()


ggplot() +
  geom_sf(data = london, fill = "white", color = "black") +
  geom_sf(data = BID_sf, fill = "blue", alpha = 0.5) + # Change "blue" to the desired fill color for BIDs
  geom_sf(data = camden_points_transformed, color = "red", size = .03) + # Change "red" to the desired color for Camden points
  coord_sf(crs = st_crs(27700)) +
  theme_minimal()
specific_polygon <- BID_sf[BID_sf$borough == "Camden", ]


specific_polygon <- st_set_crs(specific_polygon, 27700)


camden_points_within_polygon <- st_intersection(camden_points_transformed, specific_polygon)
camden_points_without_polygon <- st_difference(camden_points_transformed, specific_polygon)

camden_points_within_polygon$Location <- "Within BID"
camden_points_without_polygon$Location <- "Outside BID"

combined_data <- rbind(camden_points_within_polygon, camden_points_without_polygon)

ggplot() +
  geom_sf(data = london, fill = "white", color = "black") +
  geom_sf(data = BID_sf, fill = "blue", alpha = 0.5) +
  geom_sf(data = combined_data, aes(color = Location), size = 0.03) +
  scale_color_manual(values = c("red", "green")) + # Specify colors for Within and Outside
  coord_sf(crs = st_crs(27700)) +
  theme_minimal()
t_test_result <- t.test(camden_points_within_polygon$rentalValuation, camden_points_without_polygon$rentalValuation)

print(t_test_result)

ggplot() +
  geom_boxplot(data = camden_points_within_polygon, aes(x = "Within Polygon", y = floorArea)) +
  geom_boxplot(data = camden_points_without_polygon, aes(x = "Without Polygon", y = floorArea)) +
  labs(title = "Comparative Analysis of Floor Area",
       x = "Data Source",
       y = "Floor Area") +
  theme_minimal()

summary_within <- summary(camden_points_within_polygon[, c("floorArea", "rentalValuation", "ratesPaid")])

# Summary statistics for points without the polygon
summary_without <- summary(camden_points_without_polygon[, c("floorArea", "rentalValuation", "ratesPaid")])
print (summary_without)
print(summary_within)

gpkg_file <- "/Users/jessicasumner/Downloads/ListedBuildings/ListedBuildings_24May2023.shp"

buildings <- st_read(gpkg_file)

print(buildings)
print(camden_points_within_polygon)

library(dplyr)



useCategory_freq <- table(camden_points_within_polygon$useCategory)

useCategory_freq_df <- as.data.frame(useCategory_freq)

colnames(useCategory_freq_df) <- c("useCategory", "Frequency")

useCategory_freq_df <- useCategory_freq_df %>%
  arrange(desc(Frequency))

top_10_common <- head(useCategory_freq_df, 10)
print(top_10_common)


useCategory_freq <- table(camden_points_without_polygon$useCategory)

useCategory_freq_df <- as.data.frame(useCategory_freq)


colnames(useCategory_freq_df) <- c("useCategory", "Frequency")

useCategory_freq_df <- useCategory_freq_df %>%
  arrange(desc(Frequency))

top_10_common <- head(useCategory_freq_df, 10)
print(top_10_common)
print
shops_stores_data <- camden_points_within_polygon[camden_points_within_polygon$useCategory %in% c("Shops", "Stores"), ]



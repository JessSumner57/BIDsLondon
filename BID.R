library(tidyverse)
library(sf)
library(tmap)
library(janitor)
library(spatstat)
library (osmdata)
library (rgdal)
library(dplyr)


library(sf)


# Read the Camden points data from CSV file and create the geometry column
camden_points_df <- read.csv("/Users/jessicasumner/Desktop/Dissertation/BID/business_improvement_districts/Camdendata.csv")
camden_points_sf <- st_as_sf(camden_points_df, wkt = "geometry")

BID_sf <- st_read("/Users/jessicasumner/Desktop/Dissertation/BID/business_improvement_districts/business_improvement_districts.shp")
# Assuming you have loaded the required libraries and read the data
london <- st_read ("/Users/jessicasumner/Desktop/Dissertation/BID/business_improvement_districts/London-wards-2018/London-wards-2018_ESRI/London_Ward.shp")
# Set the CRS for Camden points to WGS 84 (EPSG:4326) since it is in latitude and longitude
camden_points_transformed <- st_transform(camden_points_sf, crs = 27700)
st_crs(camden_points_sf) <- 4326
# Transform Camden points to British National Grid (EPSG:27700)
camden_points_transformed <- st_transform(camden_points_sf, crs = 27700)

# Transform BID data to British National Grid (EPSG:27700)
BID_sf <- st_transform(BID_sf, crs = 27700)
# Now, both datasets have the same CRS (EPSG:27700)
# Plot the London map with BID boundaries
library(sf)
library(ggplot2)

ggplot() +
  geom_sf(data = london, fill = "white", color = "black") +
  geom_sf(data = BID_sf, fill = "blue", alpha = 0.5) + # Change "blue" to the desired fill color for BIDs
  coord_sf(crs = st_crs(27700)) +
  theme_minimal()

# Add Camden points to the map
ggplot() +
  geom_sf(data = london, fill = "white", color = "black") +
  geom_sf(data = BID_sf, fill = "blue", alpha = 0.5) + # Change "blue" to the desired fill color for BIDs
  geom_sf(data = camden_points_transformed, color = "red", size = .03) + # Change "red" to the desired color for Camden points
  coord_sf(crs = st_crs(27700)) +
  theme_minimal()
# Perform spatial join between Camden points and BID polygons
specific_polygon <- BID_sf[BID_sf$borough == "Camden", ]

# Set the CRS for specific_polygon to British National Grid (EPSG:27700) if not already in that CRS
specific_polygon <- st_set_crs(specific_polygon, 27700)

# Filter Camden points to include only those within the specific_polygon
camden_points_within_polygon <- st_intersection(camden_points_transformed, specific_polygon)
camden_points_without_polygon <- st_difference(camden_points_transformed, specific_polygon)

camden_points_within_polygon$Location <- "Within BID"
camden_points_without_polygon$Location <- "Outside BID"

# Combine the two datasets
combined_data <- rbind(camden_points_within_polygon, camden_points_without_polygon)

ggplot() +
  geom_sf(data = london, fill = "white", color = "black") +
  geom_sf(data = BID_sf, fill = "blue", alpha = 0.5) +
  geom_sf(data = combined_data, aes(color = Location), size = 0.03) +
  scale_color_manual(values = c("red", "green")) + # Specify colors for Within and Outside
  coord_sf(crs = st_crs(27700)) +
  theme_minimal()
t_test_result <- t.test(camden_points_within_polygon$rentalValuation, camden_points_without_polygon$rentalValuation)

# Print the p-value and test result
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

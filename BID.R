library(tidyverse)
library(sf)
library(tmap)
library(janitor)
library(spatstat)
library (osmdata)
library (rgdal)
library(dplyr)


library(sf)

###Loading the spatial data and data points
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



###Visualziing the bid boundary and bid points
ggplot() +
  geom_sf(data = london, fill = "white", color = "black") +
  geom_sf(data = BID_sf, fill = "blue", alpha = 0.5) +
  geom_sf(data = combined_data, aes(color = Location), size = 0.03) +
  scale_color_manual(values = c("red", "green")) + # Specify colors for Within and Outside
  coord_sf(crs = st_crs(27700)) +
  theme_minimal()
t_test_result <- t.test(camden_points_within_polygon$rentalValuation, camden_points_without_polygon$rentalValuation)

### A quick look at any statistical points
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

##Dividing by useCategory to do more exploratory analysis 

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

filtered_data <- camden_points_within_polygon %>%
  filter(useCategory %in% c("Offices", "Stores", "Shops"))
library(ggplot2)
avg_rates <- filtered_data %>%
  group_by(useCategory) %>%
  summarize(avg_ratesPaid = mean(ratesPaid))

library(ggplot2)
library(dplyr)


# Filter the dataset for the specified subcategories
filtered_data <- camden_points_without_polygon %>%
  filter(useCategory %in% c("Offices", "Stores", "Shops"))

filtered_data <- filtered_data %>%
  filter(!is.na(ratesPaid))

set.seed(123)
useCategory <- rep(c("Offices", "Stores", "Shops"), each = 10)
ratesPaid <- runif(30, min = 100, max = 1000)
filtered_data <- data.frame(useCategory, ratesPaid)

avg_rates <- filtered_data %>%
  group_by(useCategory) %>%
  summarize(avg_ratesPaid = mean(ratesPaid, na.rm = TRUE))

# Create a bar plot using ggplot2
ggplot(avg_rates, aes(x = useCategory, y = avg_ratesPaid, fill = useCategory)) +
  geom_bar(stat = "identity") +
  labs(title = "Average ratesPaid for Subcategories",
       x = "Subcategory",
       y = "Average ratesPaid") +
  theme_minimal()
set.seed(123)
useCategory <- rep(c("Offices", "Stores", "Shops"), each = 10)
rentalValuation <- runif(30, min = 1000, max = 5000)
ratesPaid <- runif(30, min = 100, max = 1000)
filtered_data <- data.frame(useCategory, rentalValuation, ratesPaid)

avg_values <- filtered_data %>%
  group_by(useCategory) %>%
  summarize(avg_ratesPaid = mean(ratesPaid, na.rm = TRUE),
            avg_rentalValuation = mean(rentalValuation, na.rm = TRUE))

ggplot(avg_values, aes(x = useCategory, fill = useCategory)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Offices, Stores, and Shops",
       x = "Subcategory",
       y = "Average Value") +
  geom_text(aes(label = round(avg_ratesPaid, 2)), position = position_dodge(width = 0.9), vjust = -0.5) +
  geom_text(aes(label = round(avg_rentalValuation, 2)), position = position_dodge(width = 0.9), vjust = 1.5) +
  scale_fill_manual(values = c("Offices" = "blue", "Stores" = "green", "Shops" = "orange")) +
  theme_minimal() +
  theme(legend.position = "none")

set.seed(123)
useCategory <- rep(c("Offices", "Stores", "Shops"), each = 10)
rentalVal <- runif(30, min = 1000, max = 5000)
ratesPaid <- runif(30, min = 100, max = 1000)
filtered_data <- data.frame(useCategory, rentalVal, ratesPaid)

library(ggplot2)
library(dplyr)



library(ggplot2)
library(dplyr)

set.seed(123)
useCategory <- rep(c("Offices", "Stores", "Shops"), each = 10)
rentalValuation <- runif(30, min = 1000, max = 5000)
ratesPaid <- runif(30, min = 100, max = 1000)
filtered_data <- data.frame(useCategory, rentalValuation, ratesPaid)

avg_values <- filtered_data %>%
  group_by(useCategory) %>%
  summarize(avg_ratesPaid = mean(ratesPaid, na.rm = TRUE),
            avg_rentalValuation = mean(rentalValuation, na.rm = TRUE))

# Create a grouped bar plot using ggplot2
ggplot(avg_values, aes(x = useCategory, y = avg_ratesPaid, fill = useCategory)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_bar(aes(x = useCategory, y = avg_rentalValuation, fill = useCategory),
           stat = "identity", position = position_dodge(width = 0.4)) +
  labs(title = "Comparison of Offices, Stores, and Shops",
       x = "Subcategory",
       y = "Average Value") +
  scale_fill_manual(values = c("Offices" = "blue", "Stores" = "green", "Shops" = "orange")) +
  theme_minimal() +
  theme(legend.position = "none")

camden_points_df <- read.csv("/Users/jessicasumner/Desktop/Dissertation/BID/business_improvement_districts/Camdendata.csv")
camden_points_sf <- st_as_sf(camden_points_df, wkt = "geometry")

# Load BID shapefile
BID_sf <- st_read("/Users/jessicasumner/Desktop/Dissertation/BID/business_improvement_districts/business_improvement_districts.shp")
BID_sf <- st_transform(BID_sf, crs = 27700)



summary_by_use_category <- camden_points_without_polygon %>%
  group_by(useCategory) %>%
  summarize(
    avg_RatesPaid = mean(ratesPaid),
    median_RatesPaid = median(ratesPaid),
    min_RatesPaid = min(ratesPaid),
    max_RatesPaid = max(ratesPaid)
  )

print(summary_by_use_category)

library(ggplot2)

ggplot(summary_by_use_category, aes(x = useCategory, y = avg_RatesPaid)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average RatesPaid by useCategory",
       x = "useCategory",
       y = "Average RatesPaid") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
top_10_summary <- summary_by_use_category %>%
  top_n(10, wt = avg_RatesPaid)

# Creating a bar plot of average RatesPaid for the top 10 useCategory values
ggplot(top_10_summary, aes(x = reorder(useCategory, avg_RatesPaid), y = avg_RatesPaid)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 10 useCategory by Average RatesPaid",
       x = "useCategory",
       y = "Average RatesPaid") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


summary_by_use_category <- camden_points_without_polygon %>%
  group_by(useCategory) %>%
  summarize(avg_RatesPaid = mean(ratesPaid, na.rm = TRUE))

# Filter for the top 10 useCategory values
top_10_summary <- summary_by_use_category %>%
  top_n(10, wt = avg_RatesPaid)

# Creating a bar plot of average RatesPaid for the top 10 useCategory values
ggplot(top_10_summary, aes(x = reorder(useCategory, avg_RatesPaid), y = avg_RatesPaid)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 10 useCategory by Average RatesPaid (Outside Polygon)",
       x = "useCategory",
       y = "Average RatesPaid") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

summary_by_use_category <- camden_points_within_polygon %>%
  group_by(useCategory) %>%
  summarize(avg_RatesPaid = mean(ratesPaid, na.rm = TRUE))


top_10_summary <- summary_by_use_category %>%
  top_n(10, wt = avg_RatesPaid)

ggplot(top_10_summary, aes(x = reorder(useCategory, avg_RatesPaid), y = avg_RatesPaid)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 10 useCategory by Average RatesPaid (Inside Polygon)",
       x = "useCategory",
       y = "Average RatesPaid") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
camden_points_within_polygon$useCategory <- factor(camden_points_within_polygon$useCategory)
ggplot() +
  geom_sf(data = london, fill = "white", color = "black") +
  geom_sf(data = BID_sf, fill = "blue", alpha = 0.5) +
  geom_sf(data = camden_points_within_polygon, aes(color = useCategory), size = 0.03) +
  scale_color_manual(values = c("Offices" = "red", "Stores" = "green", "Shops" = "blue")) + # Specify colors for each useCategory
  coord_sf(crs = st_crs(27700)) +
  theme_minimal()

bb <- st_bbox(camden_points_within_polygon)
bb2 <-st_bbox(camden_points_without_polygon)

ggplot() +
  geom_sf(data = london, fill = "white", color = "black") +
  geom_sf(data = BID_sf, fill = "blue", alpha = 0.5) +
  xlab('Longitude')+
  ylab('Latitude')+
  geom_sf(data = camden_points_within_polygon, aes(color = useCategory), size = 0.03) +
  scale_color_manual(values = c("Offices (Inc Computer Centres)" = "red", "Stores" = "green", "Shops" = "blue")) + 
  coord_sf(crs = st_crs(27700), xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax)) + # Set the plot limits based on the bounding box
  theme_minimal()
camden_points_outside_BID <- camden_points_within_polygon %>%
  st_difference(BID_sf) %>%
  st_sf()


ggplot() +
  geom_sf(data = london, fill = "white", color = "black") +
  geom_sf(data = BID_sf, fill = "blue", alpha = 0.5) +
  xlab('Longitude') +
  ylab('Latitude') +
  geom_sf(data = camden_points_outside_BID, aes(color = useCategory), size = 0.03) +
  scale_color_manual(values = c("Offices (Inc Computer Centres)" = "red", "Stores" = "green", "Shops" = "blue")) + 
  coord_sf(crs = st_crs(27700), xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax)) +
  theme_minimal()
ggplot() +
  geom_sf(data = london, fill = "white", color = "black") +
  geom_sf(data = BID_sf, fill = "blue", alpha = 0.5) +
  xlab('Longitude') +
  ylab('Latitude') +
  geom_sf(data = camden_points_without_polygon, aes(color = useCategory), size = 0.03, alpha = 0.7) +
  scale_color_manual(values = c("Offices" = "red", "Stores" = "green", "Shops" = "blue")) + 
  coord_sf(crs = st_crs(27700), xlim = c(bb2$xmin, bb2$xmax), ylim = c(bb2$ymin, bb2$ymax)) +
  theme_minimal()
ggplot() +
  geom_sf(data = london, fill = "white", color = "black") +
  geom_sf(data = BID_sf, fill = "blue", alpha = 0.5) +
  geom_sf(data = camden_points_within_polygon, aes(color = useCategory), size = 0.05) +
  scale_color_manual(values = c("Offices" = "white")) + 
  coord_sf(crs = st_crs(27700), xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax)) + # Set the plot limits based on the bounding box
  theme_minimal()
ggplot() +
  geom_sf(data = london, fill = "white", color = "black") +
  geom_sf(data = BID_sf, fill = "blue", alpha = 0.5) +
  geom_sf(data = camden_points_within_polygon, aes(fill = useCategory), size = 0.05) +  
  scale_fill_manual(values = c("Offices" = "white"),
                    breaks = c("Offices"),  # Specify the category for which you want to set the label
                    labels = c("Offices")) +  # Specify the label you want
  coord_sf(crs = st_crs(27700), xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax)) +
  theme_minimal()

ggplot() +
  geom_sf(data = london, fill = "white", color = "white") +
  geom_sf(data = BID_sf, fill = "blue", alpha = 0.5) +
  geom_sf(data = camden_points_within_polygon, aes(fill = useCategory), size = 0.05) +  
  scale_fill_manual(values = c("Offices" = "white")) + 
  coord_sf(crs = st_crs(27700), xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax)) +
  theme_minimal() +
  labs(fill = "Category")  # Add a legend title


highest_rates_office <- camden_points_within_polygon %>%
  arrange(desc(ratesPaid)) %>%
  slice(1)  

camden_points_within_polygon$highest_rates <- camden_points_within_polygon$geometry == highest_rates_office$geometry



color_scale <- scale_fill_gradient(low = "blue", high = "red")

ggplot() +
  geom_sf(data = london, fill = "white", color = "black") +
  geom_sf(data = camden_points_without_polygon, aes(fill = rentalValuation), size = 0.05) +
  color_scale +  
  coord_sf(crs = st_crs(27700), xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax))+
  theme_minimal()

library(tmap)  
library(sf)    
library(viridis)
library(spatstat)

camden_within_points <- st_intersection(camden_points_within_polygon, BID_sf)

camden_within_coords <- st_coordinates(camden_within_points)
camden_within_points_pattern <- ppp(camden_within_coords[, 1], camden_within_coords[, 2], window = as.owin(st_bbox(BID_sf)))

density_map <- density(camden_within_points_pattern, sigma = 500)  
density_raster <- density_map$im

density_raster[!is.finite(density_raster)] <- 0


density_df <- as.data.frame(density_raster)
density_df$rescaled_value <- density_df$value / max(density_df$value) 




pts <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4269)

poly_tm <- tm_shape(poly) +
  tm_polygons(col = "cluster", palette = c("darkolivegreen", "skyblue"), style = "cat", title = "Cluster")

density_map_tm <- tm_shape(density_df) +
  tm_raster(col = "rescaled_value", palette = viridis(50)) +
  tm_borders(lwd = 0.2) +
  tm_format("Density")  


pts_tm <- tm_shape(pts) +
  tm_dots(size = 2, col = "black")  # Adjust the color as needed

# Combine the tmap objects
tm_layout(legend.outside = TRUE) +
  poly_tm +
  density_map_tm +
  pts_tm

install.packages("ggmap")
library(ggmap)

# Check the bounding box's longitude values
bb <- st_bbox(camden_points_within_polygon)
print(bb$xmin) 
print(bb$xmax)  

if (bb$xmin < -180) {
  bb$xmin <- -180
}
if (bb$xmax > 180) {
  bb$xmax <- 180
}
bb$xmin_deg <- bb$xmin / 1000
bb$xmax_deg <- bb$xmax / 1000

bb$ymin_deg <- bb$ymin / 1000
bb$ymax_deg <- bb$ymax / 1000
install.packages("leaflet")
library(leaflet)



bb <- st_bbox(camden_points_within_polygon)

if (bb$xmin < -180) {
  bb$xmin <- -180
}
if (bb$xmax > 180) {
  bb$xmax <- 180
}

# Converting longitude values from Eastings to degrees
bb$xmin_deg <- bb$xmin / 1000
bb$xmax_deg <- bb$xmax / 1000

# Converting latitude values from Northings to degrees
bb$ymin_deg <- bb$ymin / 1000
bb$ymax_deg <- bb$ymax / 1000

set.seed(123)
useCategory <- rep(c("Offices", "Stores", "Shops"), each = 10)
rentalValuation <- runif(30, min = 1000, max = 5000)
ratesPaid <- runif(30, min = 100, max = 1000)
within_polygon <- rep(c(TRUE, FALSE), each = 15)  # Example: assuming you have a column indicating whether each point is within the polygon
filtered_data <- data.frame(useCategory, rentalValuation, ratesPaid, within_polygon)

avg_values <- filtered_data %>%
  group_by(useCategory, within_polygon) %>%
  summarize(avg_ratesPaid = mean(ratesPaid, na.rm = TRUE)) %>%
  ungroup()

library(ggplot2)
ggplot(avg_values, aes(x = useCategory, y = avg_ratesPaid, fill = as.factor(within_polygon))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Subcategory", y = "Average Rates Paid", fill = "Within Polygon") +
  theme_minimal()

st_crs(camden_points_within_polygon)
st_crs(camden_points_without_polygon)


camden_points_within_polygon_reproj <- st_transform(camden_points_within_polygon, crs = st_crs(camden_points_without_polygon))

combined_data <- rbind(camden_points_within_polygon_reproj, camden_points_without_polygon)



avg_values <- combined_data %>%
  group_by(useCategory, source) %>%
  summarize(avg_ratesPaid = mean(ratesPaid, na.rm = TRUE)) %>%
  ungroup()

library(ggplot2)
ggplot(avg_values, aes(x = useCategory, y = avg_ratesPaid, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Subcategory", y = "Average Rates Paid", fill = "Polygon Source") +
  theme_minimal()
avg_values <- combined_data %>%
  group_by(useCategory, source) %>%
  summarize(avg_ratesPaid = mean(ratesPaid, na.rm = TRUE)) %>%
  ungroup()

avg_values_ranked <- avg_values %>%
  arrange(source, desc(avg_ratesPaid)) %>%
  group_by(source) %>%
  mutate(rank_within_source = row_number()) %>%
  ungroup() %>%
  filter(rank_within_source <= 5)


max_y_value <- max(avg_values_ranked$avg_ratesPaid)

ggplot(avg_values_ranked, aes(x = useCategory, y = avg_ratesPaid, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Subcategory", y = "Average Rates Paid", fill = "Polygon Source") +
  facet_wrap(~source, scales = "free_y") +  # Separate plots for each source
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  coord_cartesian(ylim = c(0, max_y_value))  # Set a custom y-axis range for both facets
install.packages("spatstat")


camden_within_points <- st_intersection(camden_points_within_polygon, BID_sf)

camden_within_coords <- st_coordinates(camden_within_points)
camden_within_points_pattern <- ppp(camden_within_coords[, 1], camden_within_coords[, 2], window = as.owin(st_bbox(BID_sf)))

density_map <- density(camden_within_points_pattern, sigma = 500)  
density_raster <- density_map$im

# Check for missing values in the raster
if (any(!is.finite(density_raster))) {
#replaving missing values with zero
    density_raster[!is.finite(density_raster)] <- 0
}

bid_extent <- st_bbox(BID_sf)

plot(density_raster, main = "Kernel Density Estimate", col = viridis(50),
     xlim = bid_extent[1:2], ylim = bid_extent[3:4])
plot(BID_sf, add = TRUE, border = "blue", lwd = 2)

layout(matrix(c(1, 2), ncol = 1), heights = c(3, 1))

library(sf)
library(ggplot2)



par(mar = c(0, 0, 0, 0))  # Adjust margins
plot(st_geometry(london), border = "black", col = "white")
points(st_geometry(camden_points_within_polygon), pch = 20, cex = 0.05, col = camden_points_within_polygon$rentalValuation)

color_scale <- scale_fill_manual(values = c("low" = "blue", "high" = "red"))
par(mar = c(3, 0, 0, 0))  # Adjust margins
hist_data <- camden_points_within_polygon$rentalValuation

num_bins <- 1000

hist(hist_data, breaks = num_bins, col = "lightblue", xlab = "Rental Valuation", ylab = "Frequency", xlim = c(0, 500000))

mtext("London Rental Valuations & Frequency Distribution", 1)

dev.copy(png, "Rental_Valuations_Plot.png", height = 200, width = 100)
dev.off()

par(mar = c(3, 0, 0, 0))  # Adjust margins
hist_data <- camden_points_without_polygon$rentalValuation

# Specify the number of bins (e.g., 50 bins)
num_bins <- 1000

hist(hist_data, breaks = num_bins, col = "lightblue", xlab = "Rental Valuation", ylab = "Frequency", xlim = c(0, 500000))

mtext("Camden Rental Valuations & Frequency Distribution", 1)

dev.copy(png, "2ndRental_Valuations_Plot.png", height = 200, width = 100)
dev.off()

par(mar = c(5, 4, 4, 2))  # Adjust margins

num_bins <- 100

hist_data_within <- camden_points_within_polygon$rentalValuation
hist(hist_data_within, breaks = num_bins, col = "lightblue", xlab = "Rental Valuation", ylab = "Frequency", xlim = c(0, 500000), main = "Rental Valuations Comparison")
rug(hist_data_within, side = "b")  # Add rug plot for density visualization

hist_data_without <- camden_points_without_polygon$rentalValuation
hist(hist_data_without, breaks = num_bins, col = alpha("lightgreen", 0.5), add = TRUE)
rug(hist_data_without, side = "t")  # Add rug plot for density visualization

legend("topright", legend = c("Within BID", "Outside BID"), fill = c("lightblue", alpha("lightgreen", 0.5)))

dev.copy(png, "Rental_Valuations_Comparison_Plot.png", height = 300, width = 400)
dev.off()







par(mar = c(5, 4, 4, 2))  

par(mar = c(5, 4, 4, 2))  

status_counts_within <- table(camden_points_within_polygon$status)
status_counts_without <- table(camden_points_without_polygon$status)

barplot(rbind(status_counts_within, status_counts_without), beside = TRUE,
        col = c("lightblue", "lightgreen"),
        legend.text = c("Within BID", "Outside BID"),
        names.arg = c("FALSE", "TRUE"),  # Assuming "FALSE" and "TRUE" as status values
        main = "Status Comparison",
        xlab = "Status", ylab = "Count")

legend("topright", legend = c("Within BID", "Outside BID"), fill = c("lightblue", "lightgreen"))

png("Status_Comparison_Plot.png", height = 300, width = 400)
dev.off()


status_comparison_plot <- ggplot() +
  geom_bar(data = camden_points_within_polygon, aes(x = status, fill = "Within BID"), position = "dodge") +
  geom_bar(data = camden_points_without_polygon, aes(x = status, fill = "Outside BID"), position = "dodge") +
  labs(x = "Status", y = "Count", title = "Status Comparison") +
  scale_fill_manual(values = c("Within BID" = "lightblue", "Outside BID" = "red")) +
  theme_minimal() +
  theme(legend.position = "top")


ggsave("Status_Comparison_Plot.png", plot = status_comparison_plot, width = 8, height = 6)


print(status_comparison_plot)


# Load required libraries
library(ggplot2)

#Preparing the data for a grouped bar chart
comparison_data <- data.frame(
  Category = rep(c("Within BID", "Outside BID"), each = 2),
  Status = rep(c("TRUE", "FALSE"), times = 2),
  Count = c(
    sum(camden_points_within_polygon$status == "TRUE"),
    sum(camden_points_within_polygon$status == "FALSE"),
    sum(camden_points_without_polygon$status == "TRUE"),
    sum(camden_points_without_polygon$status == "FALSE")
  )
)

# Creating a grouped bar chart for more comparative analysis
grouped_bar_chart <- ggplot(comparison_data, aes(x = Category, y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Category", y = "Count", title = "Status Comparison (Grouped Bar Chart)") +
  scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "lightgreen")) +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("Status_Comparison_Grouped_Bar_Chart.png", plot = grouped_bar_chart, width = 8, height = 6)

# Print the plot
print(grouped_bar_chart)


par(mar = c(5, 4, 4, 2))  # Adjust margins


status_duration_data <- data.frame(
  Status = rep(c("Within BID", "Outside BID"), each = 2),
  Value = c(na.omit(camden_points_within_polygon$statusDuration),
            na.omit(camden_points_without_polygon$statusDuration))
)

# Create a box plot using ggplot2
box_plot <- ggplot(status_duration_data, aes(x = Status, y = Value, fill = Status)) +
  geom_boxplot() +
  labs(x = "Status", y = "Status Duration", title = "Status Duration Distribution") +
  theme_minimal() +
  theme(legend.position = "top")

ggsave("box_plot_white_background.png", plot = box_plot, bg = "white")



# Calculating the maximum values for each group
max_values <- status_duration_data %>%
  group_by(Status) %>%
  summarize(max_value = max(Value, na.rm = TRUE))


box_plot <- ggplot(status_duration_data, aes(x = Status, y = Value, fill = Status)) +
  geom_boxplot() +
  geom_hline(data = max_values, aes(yintercept = max_value, color = Status), linetype = "dashed") +
  labs(x = "Status", y = "Status Duration", title = "Status Duration Distribution") +
  theme_minimal() +
  theme(legend.position = "top")

# Print the plot
ggsave("2box_plot_white_background.png", plot = box_plot, bg = "white")

# Creating a box plot to have more visualization
box_plot <- ggplot(status_duration_data, aes(x = Status, y = Value, fill = Status)) +
  geom_boxplot() +
  geom_segment(data = status_duration_data, aes(x = Status, xend = Status, 
                                                y = Value, yend = max(Value, na.rm = TRUE)),
               color = "red", size = 1) +
  labs(x = "Status", y = "Status Duration", title = "Status Duration Distribution") +
  theme_minimal() +
  theme(legend.position = "top")
ggsave("3box_plot_white_background.png", plot = box_plot, bg = "white")

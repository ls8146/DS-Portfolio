# Load necessary libraries
library(tidyverse)  # For data manipulation
library(sf)         # For working with shapefiles (spatial data)
library(readr)      # For reading CSV files
library(RColorBrewer) # For color palettes

# Step 1: Load the Precinct Shapefile
# Replace with the correct path to your shapefile
map2020 <- st_zm(st_read("shp/precincts_2020.shp"))
map2022 <- st_zm(st_read("shp/precincts_2022.shp"))
map2023 <- st_zm(st_read("shp/precincts_2023.shp"))

# Step 2: Load the Voter Data CSV
# Replace with the correct path to your CSV
voter_data <- read_csv("VoterListExport-20241024-no.csv")

# Step 3: Filter and calculate early in-person voting based on 2023, 2022, and 2020 columns

# Assuming the columns '2023 General Election', 'GENERAL_NOV_2022', and 'GENERAL_NOV_2020' contain data
# on the total votes and that there are additional columns to identify early in-person voters

# Filter for early in-person votes for 2023, 2022, and 2020
early_voters_2023 <- voter_data %>%
  filter(`2023 General Election` == "E-U")  # You can add 'E-D' or 'E-R' if applicable for 2023

early_voters_2022 <- voter_data %>%
  filter(GENERAL_NOV_2022 == "E-U")  # Same for 2022, use 'E-D', 'E-R' as necessary

early_voters_2020 <- voter_data %>%
  filter(GENERAL_NOV_2020 == "E-U")  # Same for 2020

# Step 4: Calculate the number of early in-person voters per precinct for each year
early_voters_by_precinct_2023 <- early_voters_2023 %>%
  group_by(PrecinctNum) %>%
  summarize(early_voters_count_2023 = n())

early_voters_by_precinct_2022 <- early_voters_2022 %>%
  group_by(PrecinctNum) %>%
  summarize(early_voters_count_2022 = n())

early_voters_by_precinct_2020 <- early_voters_2020 %>%
  group_by(PrecinctNum) %>%
  summarize(early_voters_count_2020 = n())

# Step 5: Calculate the total number of voters per precinct for each year
total_voters_by_precinct_2023 <- voter_data %>%
  group_by(PrecinctNum) %>%
  summarize(total_voters_count_2023 = n())

total_voters_by_precinct_2022 <- voter_data %>%
  group_by(PrecinctNum) %>%
  summarize(total_voters_count_2022 = n())

total_voters_by_precinct_2020 <- voter_data %>%
  group_by(PrecinctNum) %>%
  summarize(total_voters_count_2020 = n())

# Step 6: Merge early voters and total voters data for each year
precinct_voter_stats_2023 <- left_join(early_voters_by_precinct_2023, total_voters_by_precinct_2023, by = "PrecinctNum")
precinct_voter_stats_2022 <- left_join(early_voters_by_precinct_2022, total_voters_by_precinct_2022, by = "PrecinctNum")
precinct_voter_stats_2020 <- left_join(early_voters_by_precinct_2020, total_voters_by_precinct_2020, by = "PrecinctNum")

# Step 7: Calculate the proportion of early voters in each precinct for each year
precinct_voter_stats_2023 <- precinct_voter_stats_2023 %>%
  mutate(early_voting_proportion_2023 = (early_voters_count_2023 / total_voters_count_2023) * 100)

precinct_voter_stats_2022 <- precinct_voter_stats_2022 %>%
  mutate(early_voting_proportion_2022 = (early_voters_count_2022 / total_voters_count_2022) * 100)

precinct_voter_stats_2020 <- precinct_voter_stats_2020 %>%
  mutate(early_voting_proportion_2020 = (early_voters_count_2020 / total_voters_count_2020) * 100)

# Convert PrecinctNum in precinct_voter_stats_* to character to match PRECINCT in map2020
precinct_voter_stats_2023 <- precinct_voter_stats_2023 %>%
  mutate(PrecinctNum = as.character(PrecinctNum))

precinct_voter_stats_2022 <- precinct_voter_stats_2022 %>%
  mutate(PrecinctNum = as.character(PrecinctNum))

precinct_voter_stats_2020 <- precinct_voter_stats_2020 %>%
  mutate(PrecinctNum = as.character(PrecinctNum))

# Pull the first four characters from map2020$PRECINCT and store them in a new column called PrecinctID
map2020 <- map2020 %>%
  mutate(PrecinctID = substr(PRECINCT, 1, 4))

map2022 <- map2022 %>%
  mutate(PrecinctID = substr(NAME, 1, 4))

map2023 <- map2023 %>%
  mutate(PrecinctID = substr(NAME, 1, 4))

precinct_voter_stats_2020 <- precinct_voter_stats_2020 %>%
  mutate(PrecinctID = sprintf("%04d", as.numeric(substr(PrecinctNum, 1, 4))))

precinct_voter_stats_2022 <- precinct_voter_stats_2022 %>%
  mutate(PrecinctID = sprintf("%04d", as.numeric(substr(PrecinctNum, 1, 4))))

precinct_voter_stats_2023 <- precinct_voter_stats_2023 %>%
  mutate(PrecinctID = sprintf("%04d", as.numeric(substr(PrecinctNum, 1, 4))))

# Step 8: Merge the voting stats back into the precinct shapefile
mapANDresults_2023 <- left_join(map2020, precinct_voter_stats_2023, by = c("PrecinctID" = "PrecinctID"))
mapANDresults_2022 <- left_join(map2022, precinct_voter_stats_2022, by = c("PrecinctID" = "PrecinctID"))
mapANDresults_2020 <- left_join(map2023, precinct_voter_stats_2020, by = c("PrecinctID" = "PrecinctID"))

##### Calculate the overall percentage of early voters

# Step 1: Summarize the number of early votes for each year across all precincts
total_early_voters_2023 <- precinct_voter_stats_2023 %>%
  summarize(total_early_voters = sum(early_voters_count_2023, na.rm = TRUE)) %>%
  pull(total_early_voters)

total_early_voters_2022 <- precinct_voter_stats_2022 %>%
  summarize(total_early_voters = sum(early_voters_count_2022, na.rm = TRUE)) %>%
  pull(total_early_voters)

total_early_voters_2020 <- precinct_voter_stats_2020 %>%
  summarize(total_early_voters = sum(early_voters_count_2020, na.rm = TRUE)) %>%
  pull(total_early_voters)

# Step 2: Summarize the total number of votes for each year across all precincts
total_voters_2023 <- precinct_voter_stats_2023 %>%
  summarize(total_voters = sum(total_voters_count_2023, na.rm = TRUE)) %>%
  pull(total_voters)

total_voters_2022 <- precinct_voter_stats_2022 %>%
  summarize(total_voters = sum(total_voters_count_2022, na.rm = TRUE)) %>%
  pull(total_voters)

total_voters_2020 <- precinct_voter_stats_2020 %>%
  summarize(total_voters = sum(total_voters_count_2020, na.rm = TRUE)) %>%
  pull(total_voters)

# Step 3: Calculate the overall percentage of early in-person votes for each year
early_voting_percentage_2023 <- (total_early_voters_2023 / total_voters_2023) * 100
early_voting_percentage_2022 <- (total_early_voters_2022 / total_voters_2022) * 100
early_voting_percentage_2020 <- (total_early_voters_2020 / total_voters_2020) * 100

# Print the results
cat("Overall Percentage of Early In-Person Votes in Hamilton County:\n")
cat("2023: ", round(early_voting_percentage_2023, 2), "%\n")
cat("2022: ", round(early_voting_percentage_2022, 2), "%\n")
cat("2020: ", round(early_voting_percentage_2020, 2), "%\n")



# Step 9: Create maps for each year

# Create a dataframe for the Board of Elections point
board_of_elections <- data.frame(
  name = "Board of Elections",
  latitude = 39.162080,
  longitude = -84.453870
) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)



# Updated map plot for 2023 with overall percentage added to subtitle
mapANDresults_2023 %>%
  ggplot(aes(fill = early_voting_proportion_2023)) +
  geom_sf() +
  labs(title = "2023 Early In-Person Voting by Precinct", 
       subtitle = paste("Percentage of Early Votes (Hamilton County):", round(early_voting_percentage_2023, 2), "%"),
       fill = "Early Voting (%)") +
  scale_fill_gradientn(colours = brewer.pal(n = 6, name = "Spectral"), na.value = "transparent",
                       breaks = c(0, 5, 10, 15, 20, 25, 30), labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%"),
                       limits = c(0, 30)) +
  geom_sf(data = board_of_elections, color = 'purple', size = 10, shape = 4, inherit.aes = FALSE) + # Adding the Board of Elections point
  geom_sf_label(data = board_of_elections, aes(label = name), cex = 3, nudge_y = 0.005, alpha = 0.5, inherit.aes = FALSE)+
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold"), # Make title larger and bold
    axis.title = element_blank(), # Remove axis titles but keep the tick labels
    axis.text = element_text(size = 12) # Keep axis text for better readability
  )

# Updated map plot for 2022 with overall percentage added to subtitle
mapANDresults_2022 %>%
  ggplot(aes(fill = early_voting_proportion_2022)) +
  geom_sf() +
  labs(title = "2022 Early In-Person Voting by Precinct", 
       subtitle = paste("Percentage of Early Votes (Hamilton County):", round(early_voting_percentage_2022, 2), "%"),
       fill = "Early Voting (%)") +
  scale_fill_gradientn(colours = brewer.pal(n = 6, name = "Spectral"), na.value = "transparent",
                       breaks = c(0, 5, 10, 15, 20, 25, 30), labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%"),
                       limits = c(0, 30)) +
  geom_sf(data = board_of_elections, color = 'purple', size = 10, shape = 4, inherit.aes = FALSE) + # Adding the Board of Elections point
  geom_sf_label(data = board_of_elections, aes(label = name), cex = 3, nudge_y = 0.005, alpha = 0.5, inherit.aes = FALSE) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold"), # Make title larger and bold
    axis.title = element_blank(), # Remove axis titles but keep the tick labels
    axis.text = element_text(size = 12) # Keep axis text for better readability
  )

# Updated map plot for 2020 with overall percentage added to subtitle
mapANDresults_2020 %>%
  ggplot(aes(fill = early_voting_proportion_2020)) +
  geom_sf() +
  labs(title = "2020 Early In-Person Voting by Precinct", 
       subtitle = paste("Percentage of Early Votes (Hamilton County):", round(early_voting_percentage_2020, 2), "%"),
       fill = "Early Voting (%)") +
  scale_fill_gradientn(colours = brewer.pal(n = 10, name = "Spectral"), na.value = "transparent",
                       breaks = c(0, 5, 10, 15, 20, 25, 30), labels = c("0%", "5%", "10%", "15%", "20%", "25%", "30%"),
                       limits = c(0, 30)) +
  geom_sf(data = board_of_elections, color = 'purple', size = 6, shape = 1, lwd = .1, inherit.aes = FALSE) + # Make Board of Elections marker thicker by adjusting the line width
  geom_sf_label(data = board_of_elections, aes(label = name), cex = 3.5, nudge_y = 0.007, alpha = 0.5, fontface = "bold", inherit.aes = FALSE) + # Adding bold text for the label
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold"), # Make title larger and bold
    axis.title = element_blank(), # Remove axis titles but keep the tick labels
    axis.text = element_text(size = 12) # Keep axis text for better readability
  )

library(mapview)

mapview(mapANDresults_2020,zcol = "early_voting_proportion_2020",alpha=.05)
mapview(mapANDresults_2022,zcol = "early_voting_proportion_2022",alpha=.05)
mapview(mapANDresults_2023,zcol = "early_voting_proportion_2023",alpha=.05)


geom_sf



##### Distance from BOE idea

# Create a dataframe for the Board of Elections point if not already created
board_of_elections <- data.frame(
  name = "Board of Elections",
  latitude = 39.162080,
  longitude = -84.453870
) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Step 1: Make geometries valid for each precinct
map2023 <- st_make_valid(map2023)

# Step 2: Transform all geometries to the same projected CRS (e.g., EPSG 26917 - UTM Zone 17N)
target_crs <- 26917  # UTM Zone 17N

map2023 <- st_transform(map2023, crs = target_crs)
board_of_elections <- st_transform(board_of_elections, crs = target_crs)

# Revised Step 3: Calculate the centroids of each precinct polygon, handling complex geometries

# Simplify geometry for multi-part polygons and make valid, then calculate centroids
precinct_centroids_2023 <- map2023 %>%
  st_cast("MULTIPOLYGON") %>% # Ensure all geometries are of type MULTIPOLYGON
  st_make_valid() %>% # Make geometries valid if they aren't already
  st_centroid(of_largest_polygon = TRUE) %>% # Take the centroid of the largest part if there are multiple parts
  st_as_sf()  # Convert back to an sf object if necessary

# Ensure PrecinctID is retained by adding it explicitly to the centroid data
precinct_centroids_2023 <- precinct_centroids_2023 %>%
  mutate(PrecinctID = map2023$PrecinctID)

# Step 4: Calculate distance from Board of Elections for each precinct centroid
# Selecting the first geometry of board_of_elections since there is only one point
board_of_elections_geom <- st_geometry(board_of_elections)[1]

# Calculate distance from Board of Elections for each precinct centroid
precinct_centroids_2023 <- precinct_centroids_2023 %>%
  mutate(distance_to_BOE = as.numeric(st_distance(geometry, board_of_elections_geom)))

# Step 5: Merge distance information back with the voting data
# Assuming 'PrecinctID' is the common identifier between 'precinct_centroids_2023' and 'precinct_voter_stats_2023'
precinct_voter_stats_2023 <- left_join(precinct_voter_stats_2023, precinct_centroids_2023 %>% select(PrecinctID, distance_to_BOE), by = "PrecinctID")

# Step 6: Create the scatter plot of distance to BOE vs early voting percentage
ggplot(precinct_voter_stats_2023, aes(x = distance_to_BOE, y = early_voting_proportion_2023)) +
  geom_point(alpha = 0.6, color = "darkgreen") + # Scatter plot points with some transparency
  geom_smooth(method = "lm", color = "purple2", se = TRUE) + # Add regression line with confidence interval
  labs(title = "Effect of Distance to Board of Elections on Early In-Person Voting 2023",
       subtitle = "2023 Hamilton County Precincts",
       x = "Distance to Board of Elections (meters)",
       y = "Percentage of Early In-Person Votes") +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold"), # Make title larger and bold
    plot.subtitle = element_text(size = 14), # Make subtitle larger
    axis.title = element_text(size = 18, face = "bold"), # Make axis labels larger and bold
    axis.text = element_text(size = 12) # Adjust tick labels size for better readability
  )


process_voter_data <- function(map_data, voter_stats, year) {
  # Step 1: Make geometries valid for each precinct
  map_data <- st_make_valid(map_data)
  
  # Step 2: Transform all geometries to the same projected CRS
  map_data <- st_transform(map_data, crs = target_crs)
  board_of_elections_transformed <- st_transform(board_of_elections, crs = target_crs)
  
  # Step 3: Calculate the centroids of each precinct polygon, handling complex geometries
  precinct_centroids <- map_data %>%
    st_cast("MULTIPOLYGON") %>% # Ensure all geometries are of type MULTIPOLYGON
    st_make_valid() %>% # Make geometries valid if they aren't already
    st_centroid(of_largest_polygon = TRUE) %>% # Take the centroid of the largest part if there are multiple parts
    st_as_sf()  # Convert back to an sf object if necessary
  
  # Ensure PrecinctID is retained by adding it explicitly to the centroid data
  precinct_centroids <- precinct_centroids %>%
    mutate(PrecinctID = map_data$PrecinctID)
  
  # Step 4: Calculate distance from Board of Elections for each precinct centroid
  board_of_elections_geom <- st_geometry(board_of_elections_transformed)[1]
  precinct_centroids <- precinct_centroids %>%
    mutate(distance_to_BOE = as.numeric(st_distance(geometry, board_of_elections_geom)))
  
  # Step 5: Merge distance information back with the voting data
  voter_stats <- left_join(voter_stats, precinct_centroids %>% select(PrecinctID, distance_to_BOE), by = "PrecinctID")
  
  # Step 6: Create the scatter plot of distance to BOE vs early voting percentage
  ggplot(voter_stats, aes(x = distance_to_BOE, y = get(paste0("early_voting_proportion_", year)))) +
    geom_point(alpha = 0.6, color = "darkgreen") + # Scatter plot points with some transparency
    geom_smooth(method = "lm", color = "purple2", se = TRUE) + # Add regression line with confidence interval
    labs(title = paste("Effect of Distance to Board of Elections on Early In-Person Voting", year),
         subtitle = paste(year, "Hamilton County Precincts"),
         x = "Distance to Board of Elections (meters)",
         y = "Percentage of Early In-Person Votes") +
    scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 5)) + # Set y-axis limits and breaks
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, face = "bold"), # Make title larger and bold
      plot.subtitle = element_text(size = 14), # Make subtitle larger
      axis.title = element_text(size = 18, face = "bold"), # Make axis labels larger and bold
      axis.text = element_text(size = 12) # Adjust tick labels size for better readability
    )
}

# Process and plot for 2020
process_voter_data(map2020, precinct_voter_stats_2020, "2020")

# Process and plot for 2022
process_voter_data(map2022, precinct_voter_stats_2022, "2022")

#######################################################################
#                                                                     #
#                           Import Data                               #
#                                                                     #
#######################################################################
library(readxl)
hist <- read_excel("C:/Users/bibbs/Downloads/KyotoFullFlower7.xls",
                   sheet = 1, range = "A2:F1222",
                   col_names = c("hyear", "hdoy", "hdate",
                                 "hsource", "htype", "hrefname"))
mod <- read_excel("C:/Users/bibbs/Downloads/KyotoFullFlower7.xls",
                  sheet = 2, range = "A1:G6574", 
                  col_names = TRUE)

####LOESS####
sum(!is.na(hist$hdoy))

#create sub-data set containing observations have response variable
hist1 <- hist[!is.na(hist$hdoy), ]

# summary statistics
print(summary(hist1$hdoy))
print(mean(hist1$hdoy))
print(sd(hist1$hdoy))

# histogram of hdoy
hist(hist1$hdoy, 
     main = "Histogram of Full-flowering Date (DOY) for Historical Series",
     xlab = "Full-flowering Date (DOY)")

# plot DOY against year in hist data with method = loess
hdoy_plot_loess <- ggplot(data = hist1) +
  geom_point(mapping = aes(x = hyear, y = hdoy)) +
  geom_smooth(mapping = aes(x = hyear, y = hdoy), method = "loess") +
  geom_vline(xintercept = 1800, linetype = "dashed", color = "red") +
  annotate("text", x = 1800, y = max(hist$hdoy, na.rm = TRUE) - 5,
           label = "1800", vjust = 25) +
  xlab("Year") +
  ylab("Full-flowering date (DOY)") + 
  ggtitle("Full-flowering Date (DOY) versus Year for Historical Series")
print(hdoy_plot_loess)
#######################################################################
#                                                                     #
#                          Research Goal 1                            #
#                                                                     #
#######################################################################
#Research Goal 1: Use modern Japanese data to create 99-by-69 matrix 
#of location-by-year
#################################################################
#                           Data Cleaning                       #
#################################################################
head(mod)
str(mod)

#show map of 103 locations
library(mapview)
mapview(mod, xcol = "long", ycol = "lat", crs = 4269, grid = FALSE)

# Define regions manually based on latitude and longitude
mod$region <- with(mod, ifelse(lat < 30, "South",
                               ifelse(lat > 40, "North", "Central")))

# Define a vector of colors for the regions
region_colors <- c("South" = "green", "Central" = "orange", "North" = "yellow")

# Create the map with different colors for different regions
map <- mapview(
  mod, 
  xcol = "long", 
  ycol = "lat", 
  crs = 4269, 
  grid = FALSE,
  zcol = "region", 
  col.regions = region_colors
)

# View the map
map

# Load necessary libraries
library(dplyr)
library(plotly)

# Summary statistics
num_years <- length(unique(mod$year))
num_locations <- length(unique(mod$Location))
num_latitudes <- length(unique(mod$lat))
num_longitudes <- length(unique(mod$long))

cat("Number of years:", num_years, "\n") # 69 years
cat("Number of unique locations:", num_locations, "\n") # 99 unique locations
cat("Number of unique latitudes:", num_latitudes, "\n") # 99 latitudes
cat("Number of unique longitudes:", num_longitudes, "\n") # 99 longitudes


# Find duplicate rows in the dataset
duplicates <- mod %>% 
  filter(duplicated(.))
print(duplicates, n = 100)

# Remove exact duplicates
mod <- mod %>% distinct()

# Remove specific locations with short series
short_series_locations <- c(
  "Japan/Yakushima",
  "Japan/Yonagunijima",
  "Japan/Nago",
  "Japan/Iriomotejima",
  "Japan/Izuhara",
  "Japan/Kochi",
  "Japan/Kushiro",
  "Japan/Muroran",
  "Japan/Nagoya",
  "Japan/Sendai",
  "Japan/Tottori"
)

mod <- mod %>% 
  filter(
    !(Location %in% short_series_locations &
        (Location != "Japan/Izuhara" | alt != 130.00) &
        (Location != "Japan/Kochi" | alt != 3.00) &
        (Location != "Japan/Kushiro" | alt != 14.05) &
        (Location != "Japan/Muroran" | alt != 39.89) &
        (Location != "Japan/Nagoya" | lat != 35.1683333333333) &
        (Location != "Japan/Sendai" | alt != 38.85) &
        (Location != "Japan/Tottori" | alt != 7.10)
    )
  )

# Final data set glance
num_final_locations <- length(unique(mod$Location))
cat("Number of unique locations after cleaning:", num_final_locations,
    "\n") # 99 locations
cat("Number of rows after cleaning:", nrow(mod), "\n")
# Adjusted number of rows

# Structure of the final dataset
str(mod)


#create required data set - matrix of 99 locations across 69 years
library(dplyr)
library(tidyr)
mod <- mod %>% mutate(year = as.factor(year))


mod_matrix <- mod %>%
  select(Location, year, Fbloom_doy) %>%
  group_by(Location, year) %>%
  summarize(Fbloom_doy = mean(Fbloom_doy, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = year, values_from = Fbloom_doy) %>%
  ungroup() %>%
  mutate(across(everything(), ~ replace_na(., NA)))

print(mod_matrix)

#library(writexl) #matrix in excel file
#file_path <- "C:/Users/bibbs/Documents/table.xlsx"
#write_xlsx(mod_matrix, file_path)

#################################################################
#                    Examine Missing Values                     #
#################################################################


library(ggplot2)
library(mice)

# Function to count missing values by column and row
missing_by_col <- colSums(is.na(mod_matrix))
missing_by_row <- rowSums(is.na(mod_matrix))

# Function to count non-missing values by column and row
non_missing_by_col <- colSums(!is.na(mod_matrix))
non_missing_by_row <- rowSums(!is.na(mod_matrix[-1]))

# Table of non-missing values by row
table_non_missing_by_row <- table(non_missing_by_row)
cat("41 locations have complete data\n")

# Complete series by year
yr_complete_values <- colSums(!is.na(mod_matrix[-1]))
names(yr_complete_values) <- colnames(mod_matrix)[-1]
table_yr_complete_values <- table(yr_complete_values)
cat("11 years have complete data (entry from every location)\n")

# Sort years by longest series
longest_series_years <- head(sort(yr_complete_values, decreasing = TRUE), 20)
cat("Complete years from 1981-1992 except 1988 is missing one location\n")

# Sort years by shortest series
shortest_series_years <- head(sort(yr_complete_values, decreasing = FALSE), 20)
cat("2020, 2019, and 2021 have the shortest series with 56, 58,
    and 58 locations, respectively\n")

# Complete series by location
loc_complete_values <- rowSums(!is.na(mod_matrix[-1]))
names(loc_complete_values) <- mod_matrix$Location
table_loc_complete_values <- table(loc_complete_values)
cat("41 locations have complete data (entry from every year)\n")

# Sort locations by most missing data
most_missing_locations <- head(sort(loc_complete_values,
                                    decreasing = FALSE), 10)

# Year distribution plot
year_count <- colSums(!is.na(mod_matrix[, -1]))
df1 <- data.frame(year_count = year_count, year = names(year_count))

#################################################################
#                  Exploratory Data Analysis                    #
#################################################################

#histogram of hdoy
hist(mod$Fbloom_doy, 
     main = "Histogram of Full-flowering Date (DOY) for Modern Series",
     xlab = "Full-flowering Date (DOY)")

#summary of hdoy
summary(mod$Fbloom_doy)

#plot of hdoy with loess smoothing function
moddoy_plot <- ggplot(data = mod) +
  geom_point(mapping = aes(x = year, y = Fbloom_doy)) +
  geom_smooth(mapping = aes(x = year, y = Fbloom_doy), method = "loess") +
  xlab("Year") +
  ylab("Full-flowering date (DOY)") + 
  ggtitle("Full-flowering Date (DOY) versus Year")
print(moddoy_plot)

#get n for each location
location_n <- rowSums(!is.na(mod_matrix[-1]))
names(location_n) <- mod_matrix$Location
location_n
head(sort(location_n, decreasing = F), 10)

#find mean bloom doy for each location
location_means <- apply(mod_matrix[ , 2:70], 1, mean, na.rm = TRUE)
location_means

#find sd bloom doy for each location
location_sds <- apply(mod_matrix[ , 2:70], 1, sd, na.rm = TRUE)
location_sds

#create a table of locations with mean and sd of bloom doy
doy_summ <- data.frame(
  N = location_n,
  mean_DOY = round(location_means, 2),
  sd_DOY = round(location_sds, 2)
)

doy_summ

#decent summary for report
doy_summ[c(1:6,10,20,30,40,50,60,70,80,90,94:99), ]


#######################################################################
#                                                                     #
#                          Research Goal 2                            #
#                                                                     #
#######################################################################
#Research Goal 2: Determine the coefficients for location and year
# to analyze which laction/year were closest to Kyoto.

library(reshape2)

# Reshape the data
df2 <- melt(mod_matrix, id.vars = "Location", variable.name = "Year",
            value.name = "Fbloom_doy")
df2$Location <- factor(df2$Location)
df2$Year <- factor(df2$Year)

# Set "Japan/Kyoto" as the baseline for locations
ref <- "Japan/Kyoto"
df2$Location <- relevel(df2$Location, ref)

# Linear model
g <- lm(Fbloom_doy ~ Location + Year, data = df2)
summary(g) # Year of 1953 is already the baseline

# Plot the year coefficients against the years
coe <- g$coefficients
df3 <- data.frame(Year = names(coe)[grep("Year", names(coe))])
df3$Coef <- coe[grep("Year", names(coe))]
df3$Year <- as.numeric(gsub("Year", "", df3$Year))

plot3 <- ggplot(df3, aes(x = Year, y = Coef, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Year Coefficients Trend",
       x = "Year", y = "Year Coefficients")
print(plot3)

# Sort coefficients for the report
sorted_coefficients <- sort(g$coefficients, decreasing = TRUE)
print(sorted_coefficients)


#######################################################################
#                                                                     #
#                          Research Goal 3                            #
#                                                                     #
#######################################################################
#Research Goal 3: Use Kyoto time series to predict the day of 
#peak bloom for the next year (2022). Give a 95% Prediction Interval.
library(forecast)
df2$Year <- as.numeric(gsub("Year", "", df2$Year))
data_kyoto <- df2[df2$Location == "Japan/Kyoto", ]
ts_kyoto <- ts(data_kyoto$Fbloom_doy, start = 1953, frequency = 1)
g1 <- auto.arima(ts_kyoto)
summary(g1)
fx <- forecast(g1, h = 1)
summary(fx)
#[88.20257, 104.0644] prediction interval
#estimate 96.13347

plot6 <- plot(ts_kyoto, main =
                "Day of Peak Bloom for Japan/Kyoto from 1953 to 2022", 
              xlab = "Year", ylab = "Day of Peak Bloom")
points(x = 2022, y = 96.13347, col = "red", pch = 16)
segments(x0 = 2022, y0 = 96.13347, x1 = 2021, 
         y1 = ts_kyoto[length(ts_kyoto)], col = "green", lty = "dashed")
print(plot6)
---
title: "Miranda Final Project"
subtitle: "Mini-project 2 Updates"
output: html_notebook
---

# Cleaned up code for Mini-project 2

### Packages used
```{r}
library(sf)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(scales)
library(viridis)
```


### Loading files
```{r}
# Load and clean internet user data
national_counties <- read_sf("2023_TIGER_Line_National_Counties/tl_2023_us_county.shp")
texas_counties <- national_counties %>%
  filter(STATEFP == 48)

# Read in additional files
crosswalk <- read_excel("county_zip_tx.xlsx")
agi_2011 <- read_excel("iitr_2011_cleaned.xlsx")
agi_2016 <- read_excel("iitr_2016_cleaned.xlsx")
agi_2021 <- read_excel("iitr_2021_cleaned.xlsx")

```


### Data Wrangling
I have:

1. Shapefile that contains Texas counties that make a good map
- I need to make three maps (years 2011, 2016, 2021) that show AGI growth per county.
  
2. Three years of income tax data that has Zip codes
- Here I need to use only minimal data
- Here I need to extract each zip and AGI (see below)
  
3. A crosswalk of tract to zip code
- Use this to get the AGI onto the Shapefile. What links them?
- The GEOID in shapefile gives the county code which contains multiple zip codes. I need to put a few zip codes in each GEOID.
- Find unique list of all GEOIDs. Associate crosswalk zip codes. Find those zip codes in the tax data and average.

https://www.census.gov/cgi-bin/geo/shapefiles/index.php

https://www.huduser.gov/portal/datasets/usps_crosswalk.html

https://www.irs.gov/e-file-providers/definition-of-adjusted-gross-income

Adjusted Gross Income (AGI) = gross income – adjustments. Gross Income = Total income. Income from all sources of income. Adjustments = Expenses the taxpayer paid for with income that the government deems should not be taxed.
```{r}
# Get all unique GEOIDs
all_geoIDs <- unique(texas_counties$GEOID)

# Filter crosswalk data using all unique GEOIDs
zip_geoid_mapping <- crosswalk %>%          # Crosswalk dataset
  filter(COUNTY %in% all_geoIDs) %>%        # filter out any GEOID codes not listed in all_geoIDs
  select(COUNTY, ZIP) %>%                   # only grab COUNTY (GEOID)
  group_by(COUNTY) %>%                      # group unique GEOIDs
  summarise(ZIP = list(unique(ZIP))) %>%    # list all ZIPs c(zip1,zip2)
  unnest (ZIP)                              # makes each zip online with COUNTY

```

```{r}
# Extract ZIP totals.
# First column has zip codes so it cannot be blank.
# Second column has an "NA" for the total... that's the row I need that has the total AGI for that zip code.
total_AGI_2011 <- agi_2011 %>%
  filter(!is.na(`ZIP Code`) & `ZIP Code` != "" &
           is.na(`Size of AGI`)) %>%
  select(ZIP = `ZIP Code`, AGI = AGI)

# I forget how to use sapply() in this situation. Repeating for 2016.
total_AGI_2016 <- agi_2016 %>%
  filter(!is.na(`ZIP Code`) & `ZIP Code` != "" &
           is.na(`Size of AGI`)) %>%
  select(ZIP = `ZIP Code`, AGI = AGI)

# Repeating for 2021.
total_AGI_2021 <- agi_2021 %>%
  filter(!is.na(`ZIP Code`) & `ZIP Code` != "" &
           is.na(`Size of AGI`)) %>%
  select(ZIP = `ZIP Code`, AGI = AGI)

```

```{r}
# Merge my crosswalk now called zip_geoid_mapping with the agi data.
total_AGI_2011$ZIP <- as.character(total_AGI_2011$ZIP)
total_AGI_2016$ZIP <- as.character(total_AGI_2016$ZIP)
total_AGI_2021$ZIP <- as.character(total_AGI_2021$ZIP)

# Adding in all years
# I had this separate but it only kept the AGI_2021. The following worked! 
county_agi <- zip_geoid_mapping %>%
  left_join(total_AGI_2011, by = "ZIP") %>%
  rename(AGI_2011 = AGI) %>%
  left_join(total_AGI_2016, by = "ZIP") %>%
  rename(AGI_2016 = AGI) %>%
  left_join(total_AGI_2021, by = "ZIP") %>%
  rename(AGI_2021 = AGI)

```

```{r}
# Calculate average AGI by COUNTY (GEOID)
# I spent a bit of time trying to left join each year!!
id_avg_agi <- county_agi %>%
  group_by(COUNTY) %>%
  summarise(
    avg_AGI_2011 = mean(AGI_2011, na.rm = TRUE),
    avg_AGI_2016 = mean(AGI_2016, na.rm = TRUE),
    avg_AGI_2021 = mean(AGI_2021, na.rm = TRUE)
  )


# Integrate AGI into shapefile data
texas_counties_agi <- texas_counties %>%
  left_join(id_avg_agi, by = c("GEOID" = "COUNTY"))
```


### Data vizualization
```{r}
# Adding formatting for plot
texas_counties_agi$avg_AGI_2011 <- as.numeric(gsub("[^0-9.]", "", texas_counties_agi$avg_AGI_2011))
texas_counties_agi$avg_AGI_2016 <- as.numeric(gsub("[^0-9.]", "", texas_counties_agi$avg_AGI_2016))
texas_counties_agi$avg_AGI_2021 <- as.numeric(gsub("[^0-9.]", "", texas_counties_agi$avg_AGI_2021))

# Adjusting plot of AGI
agi_breaks <- c(0, 500000, 1000000, 1500000, 2000000, 2500000)
agi_limits <- c(0, 2500000)

# ggplot for 2011
gg_tca_2011 <- ggplot(data = texas_counties_agi) +
  geom_sf(aes(fill = avg_AGI_2011),
          alpha = .9,
          color = "white") +
  scale_fill_viridis_c(option = "magma", name = "Average AGI",breaks = agi_breaks, limits = agi_limits) +
  labs(title = "Texas Counties by AGI (2011)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

# ggplot for 2016
gg_tca_2016 <- ggplot(data = texas_counties_agi) +
  geom_sf(aes(fill = avg_AGI_2016),
          alpha = .9,
          color = "white") +
  scale_fill_viridis_c(option = "magma", name = "Average AGI",breaks = agi_breaks, limits = agi_limits) +
  labs(title = "Texas Counties by AGI (2016)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

# ggplot for 2021
gg_tca_2021 <- ggplot(data = texas_counties_agi) +
  geom_sf(aes(fill = avg_AGI_2021),
          alpha = .9,
          color = "white") +
  scale_fill_viridis_c(option = "magma", name = "Average AGI",breaks = agi_breaks, limits = agi_limits) +
  labs(title = "Texas Counties by AGI (2021)") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

# Show and save plots
gg_tca_2011
ggsave("gg_tca_2011.png")

gg_tca_2016
ggsave("gg_tca_2016.png")

gg_tca_2021
ggsave("gg_tca_2021.png")

```

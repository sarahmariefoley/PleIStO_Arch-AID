# Code for figures presented in the Pleistocene Archaeology - African Isotope Datasets: A Resource for Biogeochemical Research

# Preprint DOI: https://doi.org/10.21203/rs.3.rs-9556539/v1
# Faunal and hominin datasets used in these analyses are housed on the PleIStO-ArchAID repository page hosted by the Pandor Platform located here: https://www.doi.org/10.48493/b67d-jc56

setwd("C:file destination here")

# Libraries needed for figures
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(ggplot2)
library(readxl)
library(colorspace)

# --------------------------------------------
#  Figure 1B: Choropleth Map Total Samples
# --------------------------------------------

# Load excel data

keep_cols <- c("site_country")

df_fauna <- read_excel("C:file path here",   ##<---add downloaded PleIStO Arch-AID fauna file 
                       sheet     = "Fauna",
                       col_names = TRUE,
                       col_types = "text") %>%
  select(any_of(keep_cols))

df_hominin <- read_excel("C:file path here",   ##<---add downloaded PleIStO Arch-AID hominin file 
                         sheet     = "Hominin",
                         col_names = TRUE,
                         col_types = "text") %>%
  select(any_of(keep_cols))

data_map <- bind_rows(
  df_fauna   %>% filter(rowSums(!is.na(across(everything()))) > 0),
  df_hominin %>% filter(rowSums(!is.na(across(everything()))) > 0)
)

# Recode to match names in map exactly
data_map <- data_map %>%
  filter(!is.na(site_country) & site_country != "") %>%
  mutate(
    site_country = trimws(site_country),
    site_country = recode(site_country,
                          "DR Congo"             = "Dem. Rep. Congo",
                          "Republic of Djibouti" = "Djibouti",
                          "The Gambia"           = "Gambia",
                          "Côte d'Ivoire"        = "Côte d'Ivoire"   # keep as-is, just in case encoding varies
    )
  )


# Count entries per country
country_counts <- data_map %>%
  group_by(site_country) %>%
  summarise(DataPoints = n(), .groups = "drop")

# Get African countries shapefile

africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Join data to the map by country name

africa_data <- africa %>%
  left_join(country_counts, by = c("name" = "site_country"))

# Plot map

ggplot(data = africa_data) +
  geom_sf(aes(fill = DataPoints), color = "black", size = 0.3) +
  scale_fill_continuous_sequential(palette = "YlGnBu", 
                                   na.value = "grey95", 
                                   limits = c(1, 4000)) +
  labs(
    title = "Sample Density per Country",
    fill = "Total Sample Density"
  ) +
  theme_minimal()


# --------------------------------------------
#      Figure 2A: Archaeological Fauna
# --------------------------------------------

# Load the Excel data

data_arch <- read_excel("C:file path here")   ##<---add downloaded PleIStO Arch-AID fauna file 

# Clean and count per country

data_arch <- data_arch %>%
  filter(site_age != "Modern") %>%
  mutate(
    site_country = trimws(site_country),
    site_country = recode(site_country,
                          "Democratic Republic of the Congo" = "Dem. Rep. Congo",
                          "Dem. Rep. Congo" = "Dem. Rep. Congo",
                          "DR Congo" = "Dem. Rep. Congo",
                          "Congo" = "Congo",
                          "Republic of the Congo" = "Congo",
                          "Congo - Brazzaville" = "Congo",
                          "Congo - Kinshasa" = "Dem. Rep. Congo",
                          "Ivory Coast" = "Côte d'Ivoire",
                          "Cote d'Ivoire" = "Côte d'Ivoire"
    )
  )

country_counts_arch <- data_arch %>%
  group_by(site_country) %>%
  summarise(DataPoints = n(), .groups = "drop")

# Get African countries shapefile

africa_arch <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Join data to shapefile

africa_archdata <- africa_arch %>%
  left_join(country_counts_arch, by = c("name" = "site_country"))

# Filter valid coordinate points

point_archdata <- data_arch %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)  # WGS84

# Plot the map

ggplot() +
  geom_sf(data = africa_archdata, aes(fill = DataPoints), color = "black", size = 0.3) +
  scale_fill_continuous_sequential(
    palette = "YlGnBu", 
    na.value = "grey95", 
    limits = c(1, 3000)) +
  geom_sf(data = point_archdata, color = "black", shape = 21, fill = "deeppink3", size = 2.5, alpha = 0.7) +
  labs(
    fill = "Sample Count"
  ) +
  theme_minimal()

# --------------------------------------------
#      Figure 2B: Archaeological Hominins
# --------------------------------------------

# Load your Excel data

data_hominin <- read_excel("C:file path here")   ##<---add downloaded PleIStO Arch-AID hominin file 

# Clean and count per country

data_hominin <- data_hominin %>%
  mutate(
    site_country = trimws(site_country),
    site_country = recode(site_country,
                          "Democratic Republic of the Congo" = "Dem. Rep. Congo",
                          "Dem. Rep. Congo" = "Dem. Rep. Congo",
                          "DR Congo" = "Dem. Rep. Congo",
                          "Congo" = "Congo",
                          "Republic of the Congo" = "Congo",
                          "Congo - Brazzaville" = "Congo",
                          "Congo - Kinshasa" = "Dem. Rep. Congo",
                          "Ivory Coast" = "Côte d'Ivoire",
                          "Cote d'Ivoire" = "Côte d'Ivoire"
    )
  )

country_counts_hominin <- data_hominin %>%
  group_by(site_country) %>%
  summarise(DataPoints = n(), .groups = "drop")

# Get African countries shapefile

africa_hominin <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Join data to shapefile

africa_data_hominin <- africa_hominin %>%
  left_join(country_counts_hominin, by = c("name" = "site_country"))

# Filter valid coordinate points

point_data_hominin <- data_hominin %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)  # WGS84

# Plot the map

ggplot() +
  geom_sf(data = africa_data_hominin, aes(fill = DataPoints), color = "black", size = 0.3) +
  scale_fill_continuous_sequential(
    palette = "YlGnBu", 
    na.value = "grey95", 
    limits = c(1, 3000)) +
  geom_sf(data = point_data_hominin, color = "black", shape = 24, fill = "deeppink3", size = 2.5, alpha = 0.7) +
  labs(
    fill = "Sample Count"
  ) +
  theme_minimal()

# --------------------------------------------
#      Figure 2C: Modern Fauna
# --------------------------------------------

# Load your Excel data

data_mod <- read_excel("C:file path here")   ##<---add downloaded PleIStO Arch-AID fauna file 

# Clean and count per country

data_mod <- data_mod %>%
  filter(site_age == "Modern") %>%
  mutate(
    site_country = trimws(site_country),
    site_country = iconv(site_country, from = "UTF-8", to = "ASCII//TRANSLIT"),
    site_country = recode(site_country,
                          "Democratic Republic of the Congo" = "Dem. Rep. Congo",
                          "Dem. Rep. Congo" = "Dem. Rep. Congo",
                          "DR Congo" = "Dem. Rep. Congo",
                          "Congo" = "Congo",
                          "Republic of the Congo" = "Congo",
                          "Congo - Brazzaville" = "Congo",
                          "Congo - Kinshasa" = "Dem. Rep. Congo",
                          "Ivory Coast" = "Côte d'Ivoire",
                          "Cote d'Ivoire" = "Côte d'Ivoire",
                          "South Sudan" = "S. Sudan"
    )
  )

country_counts_mod <- data_mod %>%
  group_by(site_country) %>%
  summarise(DataPoints = n(), .groups = "drop")

# Get African countries shapefile

africa_mod <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")

# Join data to shapefile

africa_datamod <- africa_mod %>%
  left_join(country_counts_mod, by = c("name" = "site_country"))

# Filter valid coordinate points

point_datamod <- data_mod %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)  # WGS84

# Plot the map

ggplot() +
  geom_sf(data = africa_datamod, aes(fill = DataPoints), color = "black", size = 0.3) +
  scale_fill_continuous_sequential(
    palette = "YlGnBu", 
    na.value = "grey95", 
    limits = c(1, 3000)) +
  geom_sf(data = point_datamod, color = "black", shape = 22, fill = "deeppink3", size = 2.5, alpha = 0.7) +
  labs(
    fill = "Sample Count"
  ) +
  theme_minimal()


# --------------------------------------------
# Figure 4: Histogram of total data over time
# --------------------------------------------

# Load and combine excel files

keep_cols <- c("date_min_ma", "date_max_ma", "site_age")

df_fauna <- read_excel("C:file path here",   ##<---add downloaded PleIStO Arch-AID fauna file 
                       sheet     = "Fauna",
                       col_names = TRUE,
                       col_types = "text") %>%
  select(any_of(keep_cols)) %>%
  mutate(source = "Fauna")

df_hominin <- read_excel("C:file path here",   ##<---add downloaded PleIStO Arch-AID hominin file 
                         sheet     = "Hominin",
                         col_names = TRUE,
                         col_types = "text") %>%
  select(any_of(keep_cols)) %>%
  mutate(source = "Hominin")

df <- bind_rows(
  df_fauna   %>% filter(rowSums(!is.na(across(everything()))) > 0),
  df_hominin %>% filter(rowSums(!is.na(across(everything()))) > 0)
)

# Convert numeric columns

df <- df %>%
  mutate(across(any_of(c("date_min_ma", "date_max_ma")), as.numeric))

# Classify site type and get midpoint

bin_width <- 0.25

df_clean <- df %>%
  mutate(
    site_type = if_else(trimws(site_age) == "Modern", "Modern",
                        "Archaeological / Paleontological")
  ) %>%
  mutate(
    date_mid_ma = case_when(
      site_type == "Modern" ~ NA_real_,  # ← Modern handled separately
      !is.na(date_min_ma) & !is.na(date_max_ma) ~ (date_min_ma + date_max_ma) / 2,
      TRUE ~ NA_real_
    )
  )

# Bin arch/paleo data into 0.25 Ma intervals

arch_data <- df_clean %>%
  filter(site_type == "Archaeological / Paleontological",
         !is.na(date_mid_ma))

max_date <- ceiling(max(arch_data$date_mid_ma) / bin_width) * bin_width
breaks   <- seq(0, max_date, by = bin_width)

arch_data <- arch_data %>%
  mutate(
    bin     = cut(date_mid_ma, breaks = breaks,
                  include.lowest = TRUE, right = TRUE),
    bin_mid = -(breaks[as.integer(bin)] + bin_width / 2)
  )

df_arch_hist <- arch_data %>%
  group_by(bin_mid, source) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(site_type = "Archaeological / Paleontological")

# Modern data as separate bin

modern_pos <- bin_width * 2

df_mod_hist <- df_clean %>%
  filter(site_type == "Modern") %>%
  group_by(source) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    bin_mid   = modern_pos,
    site_type = "Modern"
  )

df_hist <- bind_rows(df_arch_hist, df_mod_hist) %>%
  mutate(source = factor(source, levels = c("Fauna", "Hominin")))

# Build x axis breaks and labels

arch_breaks <- seq(0, max_date, by = 1)
all_breaks  <- c(-rev(arch_breaks), modern_pos)
all_labels  <- c(as.character(rev(arch_breaks)), "")  # ← empty string removes bottom label

# Add color by data source

source_colours <- c(
  "Fauna"   = "#4575b4",
  "Hominin" = "#f46d43"
)

# Plot the graph

ggplot(df_hist, aes(x = bin_mid, y = count, fill = source)) +
  geom_col(
    width     = bin_width * 0.9,
    colour    = "white",
    linewidth = 0.3
  ) +
  geom_vline(
    xintercept = bin_width,
    linetype   = "dashed",
    colour     = "grey50",
    linewidth  = 0.5
  ) +
  scale_x_continuous(
    name   = "Age (Ma)",
    breaks = all_breaks,
    labels = all_labels,
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  scale_y_continuous(
    name   = "Number of samples",
    labels = scales::label_comma()
  ) +
  scale_fill_manual(
    name = "",
    values = source_colours
  ) +
  annotate(
    "text",
    x        = modern_pos,
    y        = max(df_hist$count) * 1.05,
    label    = "Modern",
    hjust    = 0.1,
    size     = 3.5,
    colour   = "grey40"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(size = 10, colour = "grey40",
                                      margin = margin(b = 10)),
    axis.text.x        = element_text(hjust = 1, size = 9),
    axis.text.y        = element_text(size = 11),
    legend.position    = "bottom",
    legend.title       = element_text(size = 11),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

# R maps tutorial

## this needs to be cleaned up A LOT... but committing for now

install.packages(c("ggplot2", "devtools", "dplyr", "stringr"))
install.packages(c("maps", "mapdata"))
install.packages(c("ggmap"))

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

usa <- map_data("usa")
dim(usa)
head(usa)
tail(usa)

w2hr <- map_data("world2Hires")
dim(w2hr)
head(w2hr)
tail(w2hr)
object.size(w2hr)

# usa map
ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group)) +
  coord_fixed(ratio = 1.3)
ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), 
               fill = NA, color = "red") +
  coord_fixed(ratio = 1.3)
ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "red") +
  coord_fixed(ratio = 1.3)
# add points to usa map
g1 <- ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), 
               fill = "lightgray", color = "red") +
  coord_fixed(ratio = 1.3)
labs <- data.frame(
  long = c(-122.064873, -122.306417),
  lat = c(36.951968, 47.644855),
  names = c("SWFSC-FED", "NWFSC"),
  stringsAsFactors = FALSE
)
g1 + 
  geom_point(data = labs, aes(x = long, y = lat), color = "blue", size = 3) +
  geom_point(data = labs, aes(x = long, y = lat), color = "white", size = 2)

# states map
states <- map_data("state")
dim(states)
head(states)
tail(states)
ggplot(data = states) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = region), 
               color = "white") +
  coord_fixed(ratio = 1.3) +
  guides(fill = FALSE)

# west coast map
west_coast <- subset(states, region %in% c("california", "oregon", "washington"))
ggplot(data = west_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "white") +
  coord_fixed(ratio = 1.3)

# midwest map
midwest <- subset(states, 
                  region %in% c("ohio", "michigan", "indiana", "illinois", 
                                "wisconsin", "minnesota", "iowa", "missouri"))
ggplot(data = midwest) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "lightblue", color = "white") +
  coord_fixed(ratio = 1.3)
 
# michigan
michigan <- subset(states, region %in% c("michigan"))
ggplot(data = michigan) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "lightblue", color = "white") +
  coord_fixed(ratio = 1.3)

# usa counties
counties <- map_data("county")
dim(counties)
head(counties)
tail(counties)

# michigan counties
mi_counties <- subset(counties, region == "michigan")
# michigan base
# mi_base <- ggplot(data = michigan) +
#   geom_polygon(aes(x = long, y = lat, group = group), fill = "lightblue", color = "white") +
#   coord_fixed(ratio = 1.3)
mi_base <- ggplot(data = michigan, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
mi_base + theme_nothing()
# michigan base + county outlines
mi_base + 
  # theme_nothing() +
  geom_polygon(data = mi_counties, fill = NA, color = "white") +
  geom_polygon(fill = NA, color = "black")

# michigan counties + data
mi_county_data <- read.csv("mi_counties_data.csv", stringsAsFactors = FALSE)
mi_county_data$population <- as.numeric(mi_county_data$population)
mi_county_data$area <- as.numeric(mi_county_data$area)
mi_county_data <- mi_county_data[, c(1, 3, 4)]
mi_county_data$density <- mi_county_data$population / mi_county_data$area
head(mi_county_data)
# inner_join dfs by 'subregion' (ie, county)
mi_county_data_big <- dplyr::inner_join(mi_counties, mi_county_data, by = "subregion")

nix_lat_long_grid <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
  )

mi_base +
  geom_polygon(data = mi_county_data_big, aes(fill = density), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  nix_lat_long_grid

mi_base +
  geom_polygon(data = mi_county_data_big, aes(fill = density), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  scale_fill_gradient(trans = "log10") +
  nix_lat_long_grid

mi_base +
  geom_polygon(data = mi_county_data_big, aes(fill = density), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  nix_lat_long_grid +
  scale_fill_gradientn(colors = rev(terrain.colors(7)),
                       breaks = c(1, 10, 100, 1000),
                       trans = "log10")

mi_base +
  geom_polygon(data = mi_county_data_big, aes(fill = density), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  nix_lat_long_grid +
  scale_fill_gradientn(colors = rev(rainbow(7)),
                       breaks = c(1, 10, 100, 1000),
                       trans = "log10")

### reshape county report data
library(magrittr)
UDS_county_report <- read.csv("UMMAPMindsetRegistry_DATA_LABELS_2018-03-01_0852.csv", 
                              stringsAsFactors = FALSE, na.strings = "")
UDS_county_report$Event.Name <- as.factor(UDS_county_report$Event.Name)
UDS_county_report <- UDS_county_report[, c(1, 2, 4, 3)]
UDS_county_report$Event.Name <- gsub(pattern = " ", replacement = "", x = UDS_county_report$Event.Name)
sort(unique(UDS_county_report$County))
UDS_county_report$County <- gsub(pattern = "^Genessee$", replacement = "Genesee", x = UDS_county_report$County)
UDS_county_report$County <- gsub(pattern = "^Oakand$", replacement = "Oakland", x = UDS_county_report$County)
UDS_county_report$County <- gsub(pattern = "^Eaton $", replacement = "Eaton", x = UDS_county_report$County)

UDS_county_report %>%
  tidyr::gather()
UDS_county_report %>% 
  tidyr::spread(Event.Name, Exam.Date)
UDS_county_report %>% 
  tidyr::unite(Exam.Date_County, Exam.Date, County, sep = "_") %>% 
  head(.)
UDS_county_report_wide <- UDS_county_report %>% 
  tidyr::unite(Exam.Date_County, Exam.Date, County, sep = "_") %>% 
  tidyr::spread(Event.Name, Exam.Date_County) %>% 
  tidyr::separate(Baseline, into = c("Baseline.Date", "Baseline.County"), sep = "_") %>% 
  tidyr::separate(Visit01, into = c("Visit01.Date", "Visit01.County"), sep = "_") %>% 
  tidyr::separate(Visit02, into = c("Visit02.Date", "Visit02.County"), sep = "_") %>% 
  tidyr::separate(Visit03, into = c("Visit03.Date", "Visit03.County"), sep = "_") %>% 
  tidyr::separate(Visit04, into = c("Visit04.Date", "Visit04.County"), sep = "_") %>% 
  tidyr::separate(Visit05, into = c("Visit05.Date", "Visit05.County"), sep = "_") %>% 
  tidyr::separate(Visit06, into = c("Visit06.Date", "Visit06.County"), sep = "_") %>% 
  tidyr::separate(Visit07, into = c("Visit07.Date", "Visit07.County"), sep = "_") %>% 
  tidyr::separate(Visit08, into = c("Visit08.Date", "Visit08.County"), sep = "_") %>% 
  tidyr::separate(Visit09, into = c("Visit09.Date", "Visit09.County"), sep = "_")
UDS_county_report_wide[UDS_county_report_wide == "NA"] <- NA
UDS_county_report_wide_baseline <- UDS_county_report_wide %>% 
  dplyr::select(Subject.ID, Baseline.Date, Baseline.County)
class(UDS_county_report_wide_baseline$Baseline.Date)
UDS_county_report_wide_baseline$Baseline.Date <- as.Date(UDS_county_report_wide_baseline$Baseline.Date)
UDS_county_report_wide_baseline <- UDS_county_report_wide_baseline %>% 
  dplyr::filter(Baseline.Date >= as.Date("2017-03-01"))
UDS_county_count <- UDS_county_report_wide_baseline %>% 
  dplyr::group_by(Baseline.County) %>% 
  dplyr::summarize(Count = n()) %>% 
  dplyr::rename(subregion = Baseline.County) %>% 
  na.omit(.)
UDS_county_count$subregion <- tolower(UDS_county_count$subregion)

# left join michigan_counties data and UDS county count
mi_county_data_UDS <- dplyr::left_join(mi_counties, UDS_county_count, by = "subregion")

mi_base_UDS <- ggplot(data = michigan, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray", size = 0.5)
mi_base_UDS + theme_nothing()
mi_base_UDS +
  geom_polygon(data = mi_county_data_UDS, aes(fill = Count), color = "white", size = 0.1) +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  nix_lat_long_grid +
  scale_fill_gradient(low = "white", high = "darkblue",
                      breaks = c(0, 10, 20, 30, 40, 50)) +
  ggtitle("UDS Participants Counts by County",
          subtitle = "At Baseline Visit since Mar 2017")
ggsave("Participant_counts_by_county.png", width = 6, height = 4)

### same map with better data ###########
### reshape county report data
library(magrittr)
UDS_county_report <- read.csv("UMMAPMindsetRegistry_DATA_LABELS_2018-03-01_1114.csv", 
                              stringsAsFactors = FALSE, na.strings = "")
sort(unique(UDS_county_report$County))
UDS_county_report$County <- gsub(pattern = "^Genessee$", replacement = "Genesee", x = UDS_county_report$County)
UDS_county_report$County <- gsub(pattern = "^Eaton $", replacement = "Eaton", x = UDS_county_report$County)
sort(unique(UDS_county_report$County))
UDS_county_report <- UDS_county_report %>%
  dplyr::select(Subject.ID, County)

# UDS_county_report$Event.Name <- as.factor(UDS_county_report$Event.Name)
# UDS_county_report <- UDS_county_report[, c(1, 2, 4, 3)]
# UDS_county_report$Event.Name <- gsub(pattern = " ", replacement = "", x = UDS_county_report$Event.Name)
# unique(UDS_county_report$County)
# UDS_county_report$County <- gsub(pattern = "^Genessee$", replacement = "Genesee", x = UDS_county_report$County)
# UDS_county_report$County <- gsub(pattern = "^Oakand$", replacement = "Oakland", x = UDS_county_report$County)
# UDS_county_report$County <- gsub(pattern = "^Eaton $", replacement = "Eaton", x = UDS_county_report$County)

# UDS_county_report %>%
#   tidyr::gather()
# UDS_county_report %>% 
#   tidyr::spread(Event.Name, Exam.Date)
# UDS_county_report %>% 
#   tidyr::unite(Exam.Date_County, Exam.Date, County, sep = "_") %>% 
#   head(.)
# UDS_county_report_wide <- UDS_county_report %>% 
#   tidyr::unite(Exam.Date_County, Exam.Date, County, sep = "_") %>% 
#   tidyr::spread(Event.Name, Exam.Date_County) %>% 
#   tidyr::separate(Baseline, into = c("Baseline.Date", "Baseline.County"), sep = "_") %>% 
#   tidyr::separate(Visit01, into = c("Visit01.Date", "Visit01.County"), sep = "_") %>% 
#   tidyr::separate(Visit02, into = c("Visit02.Date", "Visit02.County"), sep = "_") %>% 
#   tidyr::separate(Visit03, into = c("Visit03.Date", "Visit03.County"), sep = "_") %>% 
#   tidyr::separate(Visit04, into = c("Visit04.Date", "Visit04.County"), sep = "_") %>% 
#   tidyr::separate(Visit05, into = c("Visit05.Date", "Visit05.County"), sep = "_") %>% 
#   tidyr::separate(Visit06, into = c("Visit06.Date", "Visit06.County"), sep = "_") %>% 
#   tidyr::separate(Visit07, into = c("Visit07.Date", "Visit07.County"), sep = "_") %>% 
#   tidyr::separate(Visit08, into = c("Visit08.Date", "Visit08.County"), sep = "_") %>% 
#   tidyr::separate(Visit09, into = c("Visit09.Date", "Visit09.County"), sep = "_")
# UDS_county_report_wide[UDS_county_report_wide == "NA"] <- NA
# UDS_county_report_wide_baseline <- UDS_county_report_wide %>% 
#   dplyr::select(Subject.ID, Baseline.Date, Baseline.County)
# class(UDS_county_report_wide_baseline$Baseline.Date)
# UDS_county_report_wide_baseline$Baseline.Date <- as.Date(UDS_county_report_wide_baseline$Baseline.Date)
# UDS_county_report_wide_baseline <- UDS_county_report_wide_baseline %>% 
#   dplyr::filter(Baseline.Date >= as.Date("2017-03-01"))
UDS_county_count <- UDS_county_report %>% 
  dplyr::group_by(County) %>% 
  dplyr::summarize(Count = n()) %>% 
  dplyr::rename(subregion = County) %>% 
  na.omit(.)
UDS_county_count$subregion <- tolower(UDS_county_count$subregion)

# inner join michigan_counties data with UDS county count
mi_county_data_UDS <- dplyr::left_join(mi_counties, UDS_county_count, by = "subregion")

mi_base_UDS <- ggplot(data = michigan, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray", size = 0.5)
mi_base_UDS + theme_nothing()
mi_base_UDS +
  geom_polygon(data = mi_county_data_UDS, aes(fill = Count), color = "white", size = 0.1) +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  nix_lat_long_grid +
  scale_fill_gradient(low = "#EFEFEF", high = "darkblue",
                      breaks = c(1, 20, 40, 60, 80)) +
  ggtitle("UDS Participant Counts by County",
          subtitle = "Since March 2017")
ggsave("Participant_counts_by_county.png", width = 6, height = 4)






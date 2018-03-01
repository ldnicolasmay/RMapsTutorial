# R maps tutorial

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

mi_base +
  geom_polygon(data = mi_county_data_big, aes(fill = population), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  scale_fill_gradient(trans = "log10") +
  theme_bw()

mi_base +
  geom_polygon(data = mi_county_data_big, aes(fill = density), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  #scale_fill_gradient(trans = "log10") +
  theme_bw()






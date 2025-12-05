library(ggplot2)
library(sf)
library(readxl)
library(data.table)
library(spdep)

town_shp <- read_sf("map_data/townssurvey_shp/TOWNSSURVEY_POLY.shp")
dim(town_shp)
head(town_shp)
length(unique(town_shp$TOWN))

ggplot(town_shp) +
  geom_sf()

by_city_df <- read_xlsx("by_city.xlsx")
head(by_city_df)
all(by_city_df$region %in% town_shp$TOWN)
by_city_df

setDT(town_shp)
town_shp_j <- town_shp[by_city_df, on = list(TOWN = region)]
town_shp_j

# add geometry back?
town_shp <- st_as_sf(town_shp)
town_shp_j <- st_as_sf(town_shp_j)

ggplot(town_shp) +
  geom_sf(fill = 'grey') +
  geom_sf(data = town_shp_j,
          aes(fill = mean_annual_attr_ED_rate_est),
          linewidth = 0.25) +
  scale_fill_binned(low = 'white',
                      high = 'darkred',
                        name = 'Annual Heat Attributable\nED visit rate\n(# per 100,000)')

ggsave("img/city_rate.png", width = 8.1, height = 4.37)

ggplot(town_shp) +
  geom_sf(fill = 'grey') +
  geom_sf(data = town_shp_j,
          aes(fill = mean_annual_attr_ED_visit_est),
          linewidth = 0.25) +
  scale_fill_binned(low = 'white',
                      high = 'darkblue',
                      name = 'Annual Heat Attributable\nED visits\n(#)')

ggsave("img/city_visits.png", width = 8.1, height = 4.37)

# get adjacency
ma_mat <- nb2mat(poly2nb(town_shp), style = "B", zero.policy = T)

# get all the neighbors for a town
town1 <- 'DUXBURY'
rr1 <- which(town_shp$TOWN == town1)
oo1 <- apply(ma_mat[rr1, ], 1, function(x) which(x > 0))
rr2 <- unique(do.call(c, oo1))
towns2 <- town_shp[rr2, ]
unique(towns2$TOWN)

ggplot(town_shp) +
  geom_sf() +
  geom_sf(data = st_as_sf(town_shp[rr1, ]), fill = 'blue') +
  geom_sf(data = st_as_sf(towns2), fill = 'red') 


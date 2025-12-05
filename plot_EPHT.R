library(readxl)
library(tidyverse)
library(janitor)

MA_EPHT1 <- read_xlsx("MA by Community_Heat Stress ED Visits_2000-2021.xlsx", sheet = 1)
MA_EPHT2 <- read_xlsx("MA by Community_Heat Stress ED Visits_2000-2021.xlsx", sheet = 2)
MA_EPHT <- rbind(MA_EPHT1, MA_EPHT2)
head(MA_EPHT)
MA_EPHT <- janitor::clean_names(MA_EPHT)

# remove avg
MA_EPHT <- MA_EPHT[!grepl("- Average", MA_EPHT$geo_description),]

# year
MA_EPHT$year <- as.numeric(MA_EPHT$year)

# bounds
b <- strsplit(MA_EPHT$confidence_intervals, split = " - ", fixed = T)
bounds <- vector("list", length(b)) 
for(i in 1:length(b)) {
  if(length(b[[i]]) == 1) {
    bounds[[i]] = c(NA, NA)
  } else {
    bounds[[i]] = as.numeric(b[[i]])
  }
}
bounds <- do.call(rbind, bounds)
MA_EPHT$lb <- bounds[, 1]
MA_EPHT$ub <- bounds[, 2]

# filter
years <- 2010:2021

subregions <- c('Chelsea', 'Revere', 'Everett',
                'Malden', 'Winthrop')

sub_MA_EPHT <- MA_EPHT %>%
  filter(geo_description %in% subregions, 
         year %in% years)

head(sub_MA_EPHT)

rr <- sub_MA_EPHT$crude_rate == 'NS'

sub_MA_EPHT$crude_rate[rr] = NA

sub_MA_EPHT$crude_rate <- as.numeric(sub_MA_EPHT$crude_rate)

sub_MA_EPHT

ggplot(sub_MA_EPHT) +
  geom_point(aes(x = year, y = crude_rate)) + 
  geom_errorbar(aes(x = year, ymin = lb, ymax = ub)) + 
  facet_grid(~geo_description)
  
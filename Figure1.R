library(readr)
library(tidyr)
library(dplyr)
library(usmap)
library(ggplot2)
library(maptools)

seg <- read_csv("CitySegregationSt.csv")
loc <- read_csv("uscities.csv")

seg$City <- as.factor(seg$City)
seg$State <- as.factor(seg$State)
loc$city_ascii <- as.factor(loc$city_ascii)
loc$state_id <- as.factor(loc$state_id)

seg_loc <- seg %>%
  left_join(loc, by = c("City" = "city_ascii", "State" = "state_id")) %>%
  select(c(1:72,75:78))

replacements <- c("Athens","Augusta","Barnstable","Boise","New brunswick",
                  "Hartford","Hartford","Volo","Kenosha","Lexington",
                  "Louisville","Nashville","Garden city","Huntington",
                  "Vicksburg","New york","Brentwood","Oxnard","Middletown",
                  "Portsmouth","Rochester","Sacramento","Roseville","Saginaw",
                  "San Luis Chipo","Scranton","Trenton","Salem")

seg_loc <- seg_loc[!is.na(seg_loc$lat),]

map_points <- usmap_transform(data.frame(seg_loc$lng, seg_loc$lat))

seg_loc <- seg_loc %>%
  left_join(map_points, by = c("lng" = "seg_loc.lng", "lat" = "seg_loc.lat"))

plot_usmap(include = "KS") +
  geom_point(data = seg_loc%>%filter(State == "KS"), aes(x = seg_loc.lng.1, 
                                                         y = seg_loc.lat.1, 
                                                         color = `09WhiteBlackDiss`,
                                                         size = `09TotalCount`)) +
  scale_color_gradientn(colors = c("green","blue","red"), 
                        limits = c(20,100))

hcrimes <- read_csv("HateCrimes.csv")

hcrimes$State <- as.factor(hcrimes$State)
hcrimes$Agency <- as.factor(hcrimes$Agency)


sum_hc <- hcrimes %>%
  group_by(St, Agency, Year) %>%
  summarise(crimes = sum(RaceBias))

sum_hc <- sum_hc %>% 
  pivot_wider(names_from = Year, values_from = crimes)

sum_hc[is.na(sum_hc)] <- 0

seg_hc <- seg_loc %>%
  left_join(sum_hc, by = c("City" = "Agency", "State" = "St")) %>%
  mutate(TotalCrimes = (`1991`+`1992`+`1993`+`1994`+`1995`+`1996`+`1997`+`1998`+
                          `1999`+`2000`+`2001`+`2002`+`2003`+`2004`+`2005`+
                          `2006`+`2007`+`2008`+`2009`+`2010`+`2011`+`2012`+
                          `2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`))

plot_usmap() +
  geom_point(data = seg_hc, aes(x = seg_loc.lng.1, 
                                                         y = seg_loc.lat.1, 
                                                         color = `09WhiteBlackDiss`,
                                                         size = TotalCrimes)) +
  scale_color_gradientn(colors = c("green","blue","red"), 
                        limits = c(20,100))

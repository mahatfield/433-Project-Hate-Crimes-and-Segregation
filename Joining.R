seg <- read_csv("Segregation.csv")

seg_dashes <- seg$City[which(str_detect(seg$City,"-"))]

seg_stat_area <- seg[which(str_detect(seg$metroName, "Statistical Area")),c(1:4)]
seg_met_div <- seg[which(str_detect(seg$metroName, "Metropolitan Division")),c(1:4)]

seg[which(seg$metroID == 35620), 2] <- "New York-Northern New Jersey-Long Island, NY-NJ-PA Metropolitan Statistical Area"

seg <- seg %>% 
  mutate(City = strsplit(as.character(City), "-")) %>%
  unnest() %>%
  filter(City != "")

duplicates <- seg %>%
  select(City, State) %>%
  duplicated(nmax = 2)

duplicates_full <- seg[duplicates,]

removals <- seg %>% 
  inner_join(duplicates_full[,3:4], by = c("City" = "City","State" = "State"))

removals <- removals[which(str_detect(removals$metroName, "Metropolitan Division")),]

seg <- seg %>%
  anti_join(removals)

write_csv(seg, "CitySegregation.csv")

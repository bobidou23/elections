library(pacman)
p_load(rvest,tidyverse,stringr,plotly,magrittr,sf)

#str_extract(read, "var plc_odds[\\s\\S]+var oddsranks") THIS WORKED

##RESULTS 2015
read_csv("http://www.elections.ca/res/rep/off/ovr2015app/41/data_donnees/table_tableau12.csv") %>% View()


setwd("~/Desktop/Datasets")
url <- "http://ftp.maps.canada.ca/pub/elections_elections/Electoral-districts_Circonscription-electorale/Elections_Canada_2019/federal_electoral_districts_boundaries_2019.shp.zip"
temp <- tempfile()
download.file(url, temp)
unzip(temp)
ca_districts <- read_sf("FED_CA_2019_EN.shp")
setwd("~/Dropbox/Projects/elections")

ggplot(ca_districts) +
  geom_sf(size=0.01,fill=NA) +
  geom_point(aes(geometry=geometry),stat = "sf_coordinates",shape=18,size=0.002,color="red",alpha=0.7)
ggsave("reference_map.png",width=30,height=32.82)

ca_provinces <- ca_districts %>%
  # filter(FEDNUM<60000) %>%
  group_by(PROVCODE) %>%
  summarize(geometry = st_union(geometry)) %>%
  ggplot() +
  geom_sf()

ggplot(ca_districts) +
  geom_sf(data=ca_provinces) +
  geom_point(aes(geometry=geometry),stat = "sf_coordinates",shape=18,size=2)
ggsave("prov_lines.svg")

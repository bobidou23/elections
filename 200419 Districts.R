library(pacman)
p_load(rvest,tidyverse,stringr,plotly,magrittr,sf)

#str_extract(read, "var plc_odds[\\s\\S]+var oddsranks") THIS WORKED

##RESULTS 2015
read_csv("http://www.elections.ca/res/rep/off/ovr2015app/41/data_donnees/table_tableau12.csv") %>% View()



url <- "http://ftp.maps.canada.ca/pub/elections_elections/Electoral-districts_Circonscription-electorale/Elections_Canada_2019/federal_electoral_districts_boundaries_2019.shp.zip"
temp <- tempfile()
download.file(url, temp)
unzip(temp)
ca_districts <- read_sf("FED_CA_2019_EN.shp")
#i in sort(unique(ca_districts$FEDNUM))
# dist_odds <- data.frame()
# https://stackoverflow.com/questions/39056103/iterating-rvest-scrape-function-gives-error-in-open-connectionx-rb-time
dist_odds2 <- dist_odds[1:278,]
dist_odds <- data.frame()
for (i in sort(unique(ca_districts$FEDNUM))) { 
  ok <- TRUE
  while (ok == TRUE) {
    read <- tryCatch({ read_html(paste0("https://www.338canada.com/districts/",i,"e.htm")) %>%
        html_nodes(xpath = "/html/body/font/div/article/center/script[6]/text()") %>%
        html_text() },
        error = function(e) { Sys.sleep(2)
          e })
    if ("error" %in% class(read)) { cat("Bad", " ") } 
    else { cat(i, " ")
      ok <- FALSE }}
  single <- data.frame(ID = i,
                       Lib = str_extract(read, "var plc_odds(.)+") %>% gsub(".*=","",.) %>% str_sub(end=-2) %>% as.numeric(),
                       Con = str_extract(read, "var pcc_odds(.)+") %>% gsub(".*=","",.) %>% str_sub(end=-2) %>% as.numeric(),
                       NDP = str_extract(read, "var npd_odds(.)+") %>% gsub(".*=","",.) %>% str_sub(end=-2) %>% as.numeric(),
                       Green = str_extract(read, "var pvc_odds(.)+") %>% gsub(".*=","",.) %>% str_sub(end=-2) %>% as.numeric(),
                       Bloc = str_extract(read, "var bq_odds(.)+") %>% gsub(".*=","",.) %>% str_sub(end=-2) %>% as.numeric(),
                       PPC = str_extract(read, "var ppc_odds(.)+") %>% gsub(".*=","",.) %>% str_sub(end=-2) %>% as.numeric(),
                       Ind = str_extract(read, "var ind1_odds(.)+") %>% gsub(".*=","",.) %>% str_sub(end=-2) %>% as.numeric())
  dist_odds <- bind_rows(dist_odds, single)
}

dist_odds %>%
  pivot_longer(-ID, names_to = "party", values_to = "prob") %>%
  group_by(ID) %>%
  arrange(-prob)

left_join(ca_districts, dist_odds, c("FEDNUM"="ID")) %>%
  filter(PROVCODE == "QC") %>%
  ggplot() + 
  geom_sf(fill="turquoise",size=0,aes(alpha=Bloc))


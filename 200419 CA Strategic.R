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
#i in sort(unique(ca_districts$FEDNUM))
# dist_odds <- data.frame()
# https://stackoverflow.com/questions/39056103/iterating-rvest-scrape-function-gives-error-in-open-connectionx-rb-time
dist_odds2 <- dist_odds
dist_odds <- data.frame()
for (i in sort(unique(ca_districts$FEDNUM))) { 
  ok <- TRUE
  while (ok == TRUE) {
    read <- tryCatch({ read_html(paste0("https://www.338canada.com/",i,"e.htm")) %>%
        html_nodes(xpath = "/html/body/main/center/div[4]/script/text()") %>%
        html_text() },
        error = function(e) { Sys.sleep(2)
          e })
    if ("error" %in% class(read)) { cat("Bad", " ") }
    else { cat(i, " ")
      ok <- FALSE }}
  single <- c(i,str_extract(read,"(?<=partiesodds = \\[).+(?=,];)") %>% strsplit(",") %>% unlist()) %>% as.numeric() %>% t() %>% data.frame() %>%
    set_names(c("riding",str_extract(read,"(?<=partiescouleurs = \\[).+(?=,];)") %>% strsplit(",") %>% unlist()))
  dist_odds <- bind_rows(dist_odds, single)
}
dist_odds[is.na(dist_odds)] <- 0

# dist_odds <- data.frame()
# for (i in sort(unique(ca_districts$FEDNUM))) { 
#   ok <- TRUE
#   while (ok == TRUE) {
#     read <- tryCatch({ read_html(paste0("https://www.338canada.com/",i,"e.htm")) %>%
#         html_nodes(xpath = "/html/body/font/div/article/center/script[6]/text()") %>%
#         html_text() },
#         error = function(e) { Sys.sleep(2)
#           e })
#     if ("error" %in% class(read)) { cat("Bad", " ") }
#     else { cat(i, " ")
#       ok <- FALSE }}
#   single <- data.frame(ID = i,
#                        Lib = str_extract(read, "var plc_odds(.)+") %>% gsub(".*=","",.) %>% str_sub(end=-2) %>% as.numeric(),
#                        Con = str_extract(read, "var pcc_odds(.)+") %>% gsub(".*=","",.) %>% str_sub(end=-2) %>% as.numeric(),
#                        NDP = str_extract(read, "var npd_odds(.)+") %>% gsub(".*=","",.) %>% str_sub(end=-2) %>% as.numeric(),
#                        Green = str_extract(read, "var pvc_odds(.)+") %>% gsub(".*=","",.) %>% str_sub(end=-2) %>% as.numeric(),
#                        Bloc = str_extract(read, "var bq_odds(.)+") %>% gsub(".*=","",.) %>% str_sub(end=-2) %>% as.numeric(),
#                        PPC = str_extract(read, "var ppc_odds(.)+") %>% gsub(".*=","",.) %>% str_sub(end=-2) %>% as.numeric(),
#                        Ind = str_extract(read, "var ind1_odds(.)+") %>% gsub(".*=","",.) %>% str_sub(end=-2) %>% as.numeric())
#   dist_odds <- bind_rows(dist_odds, single)
# }

dist_odds %>%
  pivot_longer(-ID, names_to = "party", values_to = "prob") %>%
  group_by(ID) %>%
  arrange(-prob)

left_join(ca_districts, dist_odds, c("FEDNUM"="ID")) %>%
  filter(PROVCODE == "QC") %>%
  ggplot() + 
  geom_sf(fill="turquoise",size=0,aes(alpha=Bloc))


##APP CODE
library(shiny)
library(leaflet)
library(spData)
library(htmltools)
library(sf)

ca_districts <- read_sf("ca_districts.shp", stringsAsFactors = FALSE)

# Define UI for application that filters map points based on year and minimum population
ui <- fluidPage(
  titlePanel("A Strategic Voter Guide to the 2019 Election"),
  fillPage(leafletOutput("map"),
           absolutePanel(top = 60, left = 60, 
                         selectInput(inputId="select",
                                     label="Select:",
                                     choices=c("Anything But Conservative" = "abc",
                                               "For a Liberal minority" = "prog_only"),
                                     selected="abc"))))

server <- function(input, output) {
  output$map <- renderLeaflet({
    data <- switch(input$select, 
                   "abc" = ca_districts$abc,
                   "prog_only" = ca_districts$prog_only)
    pal1 <- colorFactor(c("lightblue", "lightyellow", "red", "orange", "purple", "grey"),
                        domain=NULL)
    pal2 <- colorFactor(c("lightyellow", "grey20", "purple", "grey80", "orange"),
                        domain=NULL)
    
    leaflet(ca_districts) %>%
      addTiles() %>%
      setView(lng = -90, lat = 53, zoom = 4) %>%
      addPolygons(fillColor = if(input$select=="abc") {~pal1(data)} else {~pal2(data)},
                  weight = 0.2,
                  fillOpacity = 0.5,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = paste("<strong>",ca_districts$ENNAME,"</strong><br/>",data) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"))
  })
}

shinyApp(ui = ui, server = server)

library(pacman)
p_load(rvest,tidyverse,stringr,plotly,magrittr,sf)

ca2019 <- read_csv("https://www.elections.ca/res/rep/off/ovr2019app/51/data_donnees/table_tableau12.csv")
colnames(ca2019) <- c("province", "district","district_no", "candidate","candidate_residence","candidate_occupation",
                      "votes_raw","votes_perc","majority","majority_perc")
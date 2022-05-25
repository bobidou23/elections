library(pacman)
p_load(tidyverse,magrittr)

p_load(usethis, credentials)
usethis::create_github_token()
set_github_pat()

#Source: https://www.govdata.de/en/suchen/-/details/bundestagswahl-2017
btw17 <- read.csv("https://www.bundeswahlleiter.de/dam/jcr/72f186bb-aa56-47d3-b24c-6a46f5de22d0/btw17_kerg.csv",skip=5,sep=";")

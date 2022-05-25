library(pacman)
p_load(rvest,tidyverse,stringr,plotly,magrittr,sf)
p_load(gganimate, gifski, png)

manifestos <- read_csv("~/Desktop/MPDataset_MPDS2020a.csv")
manifestos2 <- manifestos %>% select(countryname, partyname, date, rile, pervote) %>% filter(date > 201500, pervote>5)
mani_markets <- manifestos %>% 
  select(countryname, partyname, date, markeco, pervote) %>% filter(date > 201500, pervote>5)
mani_welfare <- manifestos %>% 
  select(countryname, partyname, date, welfare, pervote) %>% filter(date > 201500, pervote>5)

mani_dems <- manifestos %>% 
  filter(countryname == "United States", partyname == "Democratic Party") %>% 
  select(date, rile, markeco, welfare, pervote)

{ggplot(mani_dems, aes(markeco,welfare)) +
  geom_point(size=10, color="blue") +
  labs(title = "Democrats' position: top left = Left, bottom right = Right",
         subtitle = "Election: {closest_state}") +
  xlab("Support for free market") +
  ylab("Support for social welfare") +
  transition_states(date, wrap=FALSE)} %>% anim_save()


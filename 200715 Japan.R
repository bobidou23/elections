library(pacman)
p_load(rvest,tidyverse,stringr,plotly,lubridate)

japan_elections <- read_csv("japan_elections.csv") %>%
  select(-X2) %>%
  pivot_longer(-c(elxn,date,turnout), names_to = "party", values_to = "perc") %>%
  filter(!is.na(perc)) %>%
  mutate(date = ymd(date))

unique(japan_elections$party)

japan_elections <- japan_elections %>%
  select(-type,-position) %>%
  #if you're doing this again, remember to filter out the existing columns first
  left_join(data.frame(party = c("communist","sdp","cdp","dem","reiwa","pnp","npn","ozawa","kibo",
                     "jnp","sakigake","your","ishin","other",
                     "komeito","ldp","con","kokoro","n_koku"),
           type = c("left","left","left","left","left","centre","centre","centre","centre",
                    "liberal","liberal","liberal","liberal","other",
                    "right","right","right","right","right"),
           position = c(1,2,3,4,3.5,4.1,4.2,5,5,6,6.5,6,6,7,8,9,10,10,10)))

{japan_elections %>%
  group_by(elxn) %>%
  arrange(position) %>%
  mutate(right = cumsum(perc),
         left=lag(right),
         left=case_when(is.na(left)~0,TRUE~left)) %>%
  ungroup() %>%
  ggplot() +
  geom_rect(aes(xmin=left,xmax=right,ymin=date,ymax=date+200,fill=party)) +
  theme_minimal() + 
  theme(legend.position = "none")} %>% ggplotly()
ggsave("~/Desktop/japan_elections.svg")


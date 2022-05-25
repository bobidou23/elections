library(pacman)
p_load(tidyverse,magrittr,lubridate)
theme_set(theme_get() + theme(text = element_text(family = 'Fira Sans')))

pr_crises1 <- read.csv("pr_crises.csv") %>%
  # filter(country != "Israel") %>%
  mutate(collapse = ymd(collapse),
         election = ymd(election),
         formation = ymd(formation)) %>%
  select(country,collapse,election,formation)

#collapses: filter collapse is a number. end can be election or formation
#elections: filter election is a number. end is formation
pr_crises2 <- pr_crises1 %>% 
  filter(!is.na(collapse)) %>%
  mutate(xmin=collapse,
         xmax=case_when(is.na(election)~formation, TRUE~election),
         type="Crisis") %>%
  bind_rows(filter(pr_crises1,!is.na(election)) %>% mutate(xmin=election,xmax=formation,type="Formation")) %>%
  mutate(duration = xmax-xmin) %>%
  mutate(start_date = case_when(country %in% c("Malta","Poland","Estonia",
                                               "Slovakia","Latvia","Slovenia","Czechia")~ymd("2004-05-01"),
                                country %in% c("Romania","Bulgaria")~ymd("2007-05-01"),
                                country=="Croatia"~ymd("2013-05-01"),
                                TRUE~ymd("1997-07-23"))) %>%
  group_by(country) %>%
  mutate(total = as.numeric(sum(duration))/as.numeric(today()-start_date))

pr_crises3 <- pr_crises2 %>%
  arrange(-total) %$%
  unique(country) %>%
  data.frame(country = .) %>%
  mutate(ymin = row_number(),
         ymax = ymin+1) %>%
  left_join(pr_crises2, by="country")


ggplot(pr_crises3) +
  geom_rect(aes(ymin=-ymin-0.25,ymax=-ymax+0.25,xmin=xmin,xmax=formation,fill=type)) +
  geom_text(aes(label=country,y=-ymin-0.5),family="Fira Sans",x=ymd("1997-01-01"),hjust="right") +
  geom_text(aes(label=round(total*100,1),y=-ymin-0.5),family="Fira Sans",x=ymd("2023-01-01"),hjust="center") +
  xlim(ymd("1990-01-01"),ymd("2025-01-01")) +
  scale_fill_manual(values=c("Crisis"="darkgoldenrod1","Formation"="steelblue2")) +
  theme_void() +
  theme(panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        axis.text.x = element_text(),
        text = element_text(family="Fira Sans")) +
  guides(fill=FALSE)
ggsave("pr_crises.svg",width=10,height=4)







library(pacman)
p_load(tidyverse,plotly,magrittr)
ca_provincial <- read_csv("ca_provincial.csv") %>%
  gather(-prov, -year, -type, key = "party", value = "value")

#Creating an Atlantic aggregate
atl_votes <- filter(ca_provincial, prov %in% c("NB", "PE", "NS", "NL"), type=="Seats") %>%
  group_by(year, prov) %>%
  summarise(total=sum(value,na.rm = TRUE)) %>%
  group_by(year) %>%
  mutate(yeartotal=sum(total),
         share=total/yeartotal) %>%
  select(-total,-yeartotal) %>% #Created weights
  left_join(filter(ca_provincial, prov %in% c("NB","PE","NS","NL"), type=="Votes"),
            c("prov","year")) %>%
  group_by(year,party) %>%
  summarise(value=sum(value*share)) %>% #Weighted vote-shares by party by year
  mutate(prov="ATL",type="Votes")

atl_seats <- filter(ca_provincial, prov %in% c("NB", "PE", "NS", "NL"), type=="Seats") %>%
  group_by(year,party) %>%
  summarise(value=sum(value,na.rm=TRUE)) %>%
  mutate(prov="ATL",type="Seats")

ca_provincial <- bind_rows(ca_provincial, atl_votes, atl_seats)

for(prov0 in c("QC","ON","BC","ATL","AB","MB","SK"))
{prov0 <- "AB"
  avg1 <- filter(ca_provincial, prov==prov0, type=="Votes", party=="liberal", year>=1963, year<1984) %$% mean(value)
avg2 <- filter(ca_provincial, prov==prov0, type=="Votes", party=="liberal", year>=1993, year<2006) %$% mean(value)
avg3 <- filter(ca_provincial, prov==prov0, type=="Votes", party=="liberal", year>=2015, year<2020) %$% mean(value)
filter(ca_provincial, prov==prov0, type=="Votes") %>%
  ggplot(aes(x=year,y=value)) +
  geom_line(aes(color=party)) +
  scale_colour_manual(values=ca_cols)+
  annotate(geom="rect",xmin=1953,xmax=1957,ymin=-2,ymax=70,fill="black",alpha=0.2) +
  annotate(geom="rect",xmin=1963,xmax=1979,ymin=-2,ymax=70,fill="black",alpha=0.2) +
  annotate(geom="rect",xmin=1980,xmax=1984,ymin=-2,ymax=70,fill="black",alpha=0.2) +
  annotate(geom="rect",xmin=1993,xmax=2006,ymin=-2,ymax=70,fill="black",alpha=0.2) +
  annotate(geom="rect",xmin=2015,xmax=2020,ymin=-2,ymax=70,fill="black",alpha=0.2) +
  annotate(geom="segment",fill="black",alpha=0.7,x=1963,xend=1984,y=avg1,yend=avg1) +
  annotate(geom="segment",fill="black",alpha=0.7,x=1993,xend=2006,y=avg2,yend=avg2) +
  annotate(geom="segment",fill="black",alpha=0.7,x=2015,xend=2020,y=avg3,yend=avg3) +
  annotate(geom="text",x=1973.5,y=40,label=paste0("avg =\n",round(avg1,1)),family="Fira Sans") +
  annotate(geom="text",x=1999.5,y=40,label=paste0("avg =\n",round(avg2,1)),family="Fira Sans") +
  annotate(geom="text",x=2017.5,y=40,label=paste0("avg =\n",round(avg3,1)),family="Fira Sans") +
  labs(title=paste0("Liberal vote shares in ",prov0," over time"),
       caption = paste0("Shades indicate Liberal governments",case_when(prov0=="ATL" ~ "\nVote shares are weighted by province, \ni.e. PEI is over-weighted + bit of rounding error",
                                                                        TRUE~""))) +
  theme(text=element_text(family="Fira Sans"))
ggsave(paste0("~/Dropbox/Products/200424 ",prov0," Votes.png"),height=4.5,width=4)}

ca_cols <- c("bq"="turquoise","conservative"="blue","green"="green",
          "liberal"="red","ndp"="orange","other"="grey50",
          "pc"="steelblue1","reform"="darkgreen","sc"="darkgreen")
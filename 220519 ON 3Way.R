library(pacman)
p_load(tidyverse,plotly)

on18 <- read.csv("caon_14-18.csv")

on18 <- on18 %>% filter(IsGeneralElection==1) %>%
  select(dist_no = ElectoralDistrictNumber,
         dist = ElectoralDistrictNameEnglish,
         party = PoliticalInterestCode,
         perc = PercentOfTotalValidBallotsCast) %>%
  mutate(perc=perc*100) %>%
  filter(party %in% c("LIB","NDP","PCP"))

on18a <- on18 %>% pivot_wider(id_cols=dist,names_from=party,values_from=perc) %>%
  mutate(winner = case_when((NDP>PCP & NDP>LIB) ~ "NDP",
                            (PCP>NDP & PCP>LIB) ~ "PCP",
                            (LIB>NDP & LIB>PCP) ~ "LIB"),
         NL = case_when((winner=="NDP" & NDP-LIB>30) ~ 31,
                        (winner=="NDP" & NDP-LIB<30) ~ NDP-LIB,
                        TRUE ~ 0),
         NC = case_when((winner=="NDP" & NDP-PCP>30) ~ 31,
                        (winner=="NDP" & NDP-PCP<30) ~ NDP-PCP,
                        TRUE ~ 0),
         CL = case_when((winner=="PCP" & PCP-LIB>30) ~ 31,
                        (winner=="PCP" & PCP-LIB<30) ~ PCP-LIB,
                        TRUE ~ 0),
         CN = case_when((winner=="PCP" & PCP-NDP>30) ~ 31,
                        (winner=="PCP" & PCP-NDP<30) ~ PCP-NDP,
                        TRUE ~ 0),
         LC = case_when((winner=="LIB" & LIB-PCP>30) ~ 31,
                        (winner=="LIB" & LIB-PCP<30) ~ LIB-PCP,
                        TRUE ~ 0),
         LN = case_when((winner=="LIB" & LIB-NDP>30) ~ 31,
                        (winner=="LIB" & LIB-NDP<30) ~ LIB-NDP,
                        TRUE ~ 0),
         x = CN/2*sqrt(3)-NC/2*sqrt(3)-LC/2*sqrt(3)+LN/2*sqrt(3),
         y = -CN*.5+CL-NC*.5+NL-LC/2-LN/2)

# CL 22.109   CN 27.699

#PC wins: x CN/2*1.73 / y CN *-.5 + CL 
#NDP wins: x -NC/2*1.73 / y NC *-.5 + NL
#Lib wins: x -LC/2*1.73+LN/2*1.73 / y LC/2 + LN/2

#Re-run the maths
#Create Over Lib / Over NDP labels
#Launch app first
#Add regions
#Add projections

{ggplot(on18a) + 
    geom_point(aes(x=x,y=y,color=winner,group=dist,
                   text=sprintf("Over PC: %s<br>Over NDP: %s<br>Over Lib: %s",
                                round(LC+NC,1), round(LN+CN,1), round(NL+CL,1)))) +
    annotate("segment",x=0,xend=0,y=0,yend=32,arrow=arrow(),size=1)+
    annotate("segment",x=0,xend=16*sqrt(3),y=0,yend=-16,arrow=arrow(),size=1)+ #down-right
    annotate("segment",x=0,xend=-16*sqrt(3),y=0,yend=-16,arrow=arrow(),size=1)+ #down-left
    #vertical
    annotate("segment",x=15*sqrt(3),xend=15*sqrt(3),y=-15,yend=17,size=.5)+
    annotate("segment",x=10*sqrt(3),xend=10*sqrt(3),y=-10,yend=22,size=.3)+
    annotate("segment",x=5*sqrt(3),xend=5*sqrt(3),y=-5,yend=27,size=.3)+
    annotate("segment",x=-5*sqrt(3),xend=-5*sqrt(3),y=-5,yend=27,size=.3)+
    annotate("segment",x=-10*sqrt(3),xend=-10*sqrt(3),y=-10,yend=22,size=.3)+
    annotate("segment",x=-15*sqrt(3),xend=-15*sqrt(3),y=-15,yend=17,size=.5)+
    #down-right (parallel to LIB-CON)
    annotate("segment",x=0,xend=16*sqrt(3),y=30,yend=30-16,size=0.5)+
    annotate("segment",x=0,xend=16*sqrt(3),y=20,yend=20-16,size=0.3)+
    annotate("segment",x=0,xend=16*sqrt(3),y=10,yend=10-16,size=0.3)+
    annotate("segment",x=-5*sqrt(3),xend=11*sqrt(3),y=-5,yend=-5-16,size=0.3)+
    annotate("segment",x=-10*sqrt(3),xend=6*sqrt(3),y=-10,yend=-10-16,size=0.3)+
    annotate("segment",x=-15*sqrt(3),xend=sqrt(3),y=-15,yend=-15-16,size=0.5)+
    #down-left (parallel to NDP-LIB)
    annotate("segment",x=0,xend=-16*sqrt(3),y=30,yend=30-16,size=0.5)+
    annotate("segment",x=0,xend=-16*sqrt(3),y=20,yend=20-16,size=0.3)+
    annotate("segment",x=0,xend=-16*sqrt(3),y=10,yend=10-16,size=0.3)+
    annotate("segment",x=5*sqrt(3),xend=-11*sqrt(3),y=-5,yend=-5-16,size=0.3)+
    annotate("segment",x=10*sqrt(3),xend=-6*sqrt(3),y=-10,yend=-10-16,size=0.3)+
    annotate("segment",x=15*sqrt(3),xend=-sqrt(3),y=-15,yend=-15-16,size=0.5)+
    #visuals
    scale_color_manual(values=c("NDP"="orange","PCP"="blue","LIB"="red")) + 
    theme_void() +
    theme(legend.position = "none")} %>%
  ggplotly(tooltip= c("group","text"))
ggsave("on18.png",width=10,height=10)



library(pacman)
p_load(rsconnect)
rsconnect::setAccountInfo(name='koji',
                          token='54E3228F24A698088DD194344E0003F1',
                          secret='<SECRET>')


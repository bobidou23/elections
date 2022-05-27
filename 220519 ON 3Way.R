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

on18a <- on18 %>% pivot_wider(id_cols=c(dist_no,dist),names_from=party,values_from=perc) %>%
  mutate(winner = case_when((NDP>PCP & NDP>LIB) ~ "NDP",
                            (PCP>NDP & PCP>LIB) ~ "PCP",
                            (LIB>NDP & LIB>PCP) ~ "LIB"),
         NL = case_when((winner=="NDP" & NDP-LIB>30) ~ 31,
                        (winner=="NDP" & NDP-LIB<30) ~ NDP-LIB, TRUE ~ 0),
         NC = case_when((winner=="NDP" & NDP-PCP>30) ~ 31,
                        (winner=="NDP" & NDP-PCP<30) ~ NDP-PCP, TRUE ~ 0),
         CL = case_when((winner=="PCP" & PCP-LIB>30) ~ 31,
                        (winner=="PCP" & PCP-LIB<30) ~ PCP-LIB, TRUE ~ 0),
         CN = case_when((winner=="PCP" & PCP-NDP>30) ~ 31,
                        (winner=="PCP" & PCP-NDP<30) ~ PCP-NDP, TRUE ~ 0),
         LC = case_when((winner=="LIB" & LIB-PCP>30) ~ 31,
                        (winner=="LIB" & LIB-PCP<30) ~ LIB-PCP, TRUE ~ 0),
         LN = case_when((winner=="LIB" & LIB-NDP>30) ~ 31,
                        (winner=="LIB" & LIB-NDP<30) ~ LIB-NDP, TRUE ~ 0),
         x = CN/2*sqrt(3)-NC/2*sqrt(3)-LC/2*sqrt(3)+LN/2*sqrt(3),
         y = -CN*.5+CL-NC*.5+NL-LC/2-LN/2)

# CL 22.109   CN 27.699

#PC wins: x CN/2*1.73 / y CN *-.5 + CL 
#NDP wins: x -NC/2*1.73 / y NC *-.5 + NL
#Lib wins: x -LC/2*1.73+LN/2*1.73 / y LC/2 + LN/2

#Launch app first
#Add regions
#Add projections

gridlines <- list(
  #vertical
  annotate("segment",x=15*sqrt(3),xend=15*sqrt(3),y=-15,yend=17,size=.5),
  annotate("segment",x=10*sqrt(3),xend=10*sqrt(3),y=-10,yend=22,size=.3),
  annotate("segment",x=5*sqrt(3),xend=5*sqrt(3),y=-5,yend=27,size=.3),
  annotate("segment",x=0,xend=0,y=0,yend=32,arrow=arrow(),size=1),
  annotate("segment",x=-5*sqrt(3),xend=-5*sqrt(3),y=-5,yend=27,size=.3),
  annotate("segment",x=-10*sqrt(3),xend=-10*sqrt(3),y=-10,yend=22,size=.3),
  annotate("segment",x=-15*sqrt(3),xend=-15*sqrt(3),y=-15,yend=17,size=.5),
  #down-right (parallel to LIB-CON)
  annotate("segment",x=0,xend=16*sqrt(3),y=30,yend=30-16,size=0.5),
  annotate("segment",x=0,xend=16*sqrt(3),y=20,yend=20-16,size=0.3),
  annotate("segment",x=0,xend=16*sqrt(3),y=10,yend=10-16,size=0.3),
  annotate("segment",x=0,xend=16*sqrt(3),y=0,yend=-16,arrow=arrow(),size=1),
  annotate("segment",x=-5*sqrt(3),xend=11*sqrt(3),y=-5,yend=-5-16,size=0.3),
  annotate("segment",x=-10*sqrt(3),xend=6*sqrt(3),y=-10,yend=-10-16,size=0.3),
  annotate("segment",x=-15*sqrt(3),xend=sqrt(3),y=-15,yend=-15-16,size=0.5),
  #down-left (parallel to NDP-LIB)
  annotate("segment",x=0,xend=-16*sqrt(3),y=30,yend=30-16,size=0.5),
  annotate("segment",x=0,xend=-16*sqrt(3),y=20,yend=20-16,size=0.3),
  annotate("segment",x=0,xend=-16*sqrt(3),y=10,yend=10-16,size=0.3),
  annotate("segment",x=0,xend=-16*sqrt(3),y=0,yend=-16,arrow=arrow(),size=1),
  annotate("segment",x=5*sqrt(3),xend=-11*sqrt(3),y=-5,yend=-5-16,size=0.3),
  annotate("segment",x=10*sqrt(3),xend=-6*sqrt(3),y=-10,yend=-10-16,size=0.3),
  annotate("segment",x=15*sqrt(3),xend=-sqrt(3),y=-15,yend=-15-16,size=0.5),
  theme_void(),
  coord_fixed(),
  theme(text = element_text(family = "Fira Sans"),
        plot.margin = margin(10,10,10,10))
  )

ggplot(on18a) + 
  gridlines +
  annotate("text",x=2,y=c(10,20,30),label=c(10,20,30),family = "Fira Sans") +
  annotate("text",x=c(5*sqrt(3)-2, 10*sqrt(3)-2, 15*sqrt(3)-2),
           y=c(-5,-10,-15),label=c(10,20,30),family = "Fira Sans") +
  annotate("text",x=c(-5*sqrt(3)+2, -10*sqrt(3)+2, -15*sqrt(3)+2),
           y=c(-5,-10,-15),label=c(10,20,30),family = "Fira Sans") +
  #visuals
  annotate("label",x=1,y=7,label="Points behind\nfor Lib",family="Fira Sans",hjust=0,lineheight=0.7,label.size=0) +
  annotate("label",x=10.5,y=-8,label="Points behind\nfor NDP",family="Fira Sans",hjust=1,lineheight=0.7,label.size=0) +
  annotate("label",x=-15.5,y=-7,label="Points behind\nfor PC",family="Fira Sans",hjust=1,lineheight=0.7,label.size=0) +
  scale_color_manual(values=c("NDP"="orange","PCP"="blue","LIB"="red")) +
  geom_point(aes(x=x,y=y,color=winner,group=dist,
                 text=sprintf("Over PC: %s<br>Over NDP: %s<br>Over Lib: %s",
                              round(LC+NC,1), round(LN+CN,1), round(NL+CL,1)))) +
  theme(legend.position = "none") +
  labs(title = "Visualizing the 2018 Ontario election results")

  ggplotly(tooltip= c("group","text"))
ggsave("on18.png",width=10,height=10)

### THE TOPLINE
top <- read_html("https://www.338canada.com/ontario/") %>%
  html_nodes(xpath = "/html/body/main/center/div[3]/script/text()") %>%
  html_text()
lib_today <- (str_extract(top, "(?<=var	lib_moy_vote =	)[1-9.]+") %>% as.numeric()) - 19.57
con_today <- (str_extract(top, "(?<=var	con_moy_vote =	)[1-9.]+") %>% as.numeric()) - 40.5
ndp_today <- (str_extract(top, "(?<=var	npd_moy_vote =	)[1-9.]+") %>% as.numeric()) - 33.59
NL <- lib_today - ndp_today #presuming ndp_today is the smallest of the values
NC <- con_today - ndp_today

x = -NC/2*sqrt(3)
y = -NC*.5+NL

ggplot(on18b) + 
  gridlines +
  #today bars
  annotate("segment",x=x,y=y,xend=x,yend=32+x/sqrt(3),color="red",size=1) +
  annotate("segment",x=x,y=y,xend=16*sqrt(3),yend=y-(16*sqrt(3)-x)/sqrt(3),color="red",size=1) +
  annotate("segment",x=x,y=y,xend=-16*sqrt(3),yend=y-(x+16*sqrt(3))/sqrt(3),color="red",size=1) +
  annotate("label",x=-8*sqrt(3),y=0,color="red",label="NDP->Lib",family="Fira Sans",hjust=0.5,lineheight=0.7,label.size=0)+
  annotate("label",x=5*sqrt(3),y=3,color="red",label="PC->Lib",family="Fira Sans",hjust=0.5,lineheight=0.7,label.size=0)+
  annotate("text",x=-3,y=22,color="red",label="PC->\nNDP",family="Fira Sans",hjust=0.5,lineheight=0.7,label.size=0)+
  #visuals
  geom_point(aes(x=x,y=y,color=winner,group=dist,
                 text=sprintf("Over PC: %s<br>Over NDP: %s<br>Over Lib: %s",
                              round(LC+NC,1), round(LN+CN,1), round(NL+CL,1)))) +
  scale_color_manual(values=c("NDP"="orange","PCP"="blue","LIB"="red")) +
  theme(plot.subtitle = element_text(color = "red",lineheight=0.4),
        legend.position = "none") +
  labs(title = "Visualizing A 'Uniform Swing' Model in Ontario",
       subtitle = "What happens if you compare the provincial polling average\n
       to the 2018 results, and apply the same swing to \n
       every riding (a simple but very bad assumption!)")


### DISTRICT PROJECTIONS
on_odds <- data.frame()
for (i in 1:124) { 
  ok <- TRUE
  while (ok == TRUE) {
    prob <- tryCatch({
      page <- read_html(paste0("https://www.338canada.com/ontario/1",sprintf("%03d", i),"e.htm"))},
      error = function(e) { Sys.sleep(2)
        e })
    if ("error" %in% class(read)) { cat("Bad", " ") }
    else { cat(i, " ")
      ok <- FALSE }}
  name <- html_nodes(page, xpath = "/html/body/main/center/div[1]/h1") %>% html_text()
  prob <- html_nodes(page, xpath = "/html/body/main/center/div[4]/script/text()") %>% html_text()
  avgs <- html_nodes(page, xpath = "/html/body/main/center/div[2]/script/text()") %>% html_text()
  single <- c(c(i,
              str_extract(avgs,"(?<=moyennes = \\[).+(?=,])") %>% strsplit(",") %>% unlist(),
              str_extract(prob,"(?<=partiesodds = \\[).+(?=,];)") %>% strsplit(",") %>% unlist()),name) %>% 
    t() %>% data.frame() %>%
    set_names(c("dist_no",
                str_extract(avgs,"(?<=parties = \\[).+(?=,])") %>% strsplit(",") %>% unlist(),
                str_extract(prob,"(?<=partiescouleurs = \\[).+(?=,];)") %>% strsplit(",") %>% unlist(),
                "dist"))
  on_odds <- bind_rows(on_odds, single)
}
#Clean that data
on_odds[is.na(on_odds)] <- 0
on_odds[on_odds=="Leeds—Grenville—1000 Islands & Rideau Lakes"] <- "Leeds—Grenville—Thousand Islands and Rideau Lakes"
on_odds[on_odds=="Toronto—St. Paul`s"] <- "Toronto—St. Paul's"

#Get regional groupings
on_regs <- data.frame()
for(i in c("toronto","gta","ottawa","east","hamilton-niagara","southwest","central","north")) {
  ok <- TRUE
  while (ok == TRUE) {
    prob <- tryCatch({
      page <- read_html(paste0("https://www.338canada.com/ontario/",i,".htm"))},
      error = function(e) { Sys.sleep(2)
        e })
    if ("error" %in% class(read)) { cat("Bad", " ") }
    else { cat(i, " ")
      ok <- FALSE }}
  single2 <- html_nodes(page, xpath = "//*[@id=\"myTable\"]") %>% html_table() %>% data.frame() %>%
    mutate(dist_no = str_sub(Electoral.district,end=3),
           region=i)
  on_regs <- bind_rows(on_regs, single2)
}
#Clean that
on_regs <- on_regs %>%
  mutate(proj = gsub(" gain","",gsub(" hold","",Latest.projection)),
         proj = case_when(str_sub(proj,end=7)=="Toss up" ~ "Toss up",
                          TRUE ~ proj)) %>% 
  select(dist_no,region,proj)

#Clean + Combine three datasets
on_odds <- on_odds %>%
  mutate(pcp_proj=as.numeric(`'PCPO'`),
       lib_proj=as.numeric(`'OLP'`),
       ndp_proj=as.numeric(`'NDP'`),
       pcp_prob=as.numeric(`con_color`),
       lib_prob=as.numeric(`lib_color`),
       ndp_prob=as.numeric(`npd_color`),
       dist_no=sprintf("%03s",dist_no)) %>%
  select(dist,dist_no,pcp_proj,lib_proj,ndp_proj,
         pcp_prob,lib_prob,ndp_prob) %>%
  left_join(on_regs)
on18b <- left_join(on18a,on_odds,by="dist")
          
# Safe >99.9 / Likely 90-99.9 / Leaning 70-90 / Toss up <70

#Graphize that
on18c <- on18b %>% 
  mutate(winner2 = case_when((ndp_proj>pcp_proj & ndp_proj>lib_proj) ~ "NDP",
                             (pcp_proj>ndp_proj & pcp_proj>lib_proj) ~ "PCP",
                             (lib_proj>ndp_proj & lib_proj>pcp_proj) ~ "LIB"),
         NL2 = case_when((winner2=="NDP" & ndp_proj-lib_proj>30) ~ 31,
                        (winner2=="NDP" & ndp_proj-lib_proj<30) ~ ndp_proj-lib_proj, TRUE ~ 0),
         NC2 = case_when((winner2=="NDP" & ndp_proj-pcp_proj>30) ~ 31,
                        (winner2=="NDP" & ndp_proj-pcp_proj<30) ~ ndp_proj-pcp_proj, TRUE ~ 0),
         CL2 = case_when((winner2=="PCP" & pcp_proj-lib_proj>30) ~ 31,
                        (winner2=="PCP" & pcp_proj-lib_proj<30) ~ pcp_proj-lib_proj, TRUE ~ 0),
         CN2 = case_when((winner2=="PCP" & pcp_proj-ndp_proj>30) ~ 31,
                        (winner2=="PCP" & pcp_proj-ndp_proj<30) ~ pcp_proj-ndp_proj, TRUE ~ 0),
         LC2 = case_when((winner2=="LIB" & lib_proj-pcp_proj>30) ~ 31,
                        (winner2=="LIB" & lib_proj-pcp_proj<30) ~ lib_proj-pcp_proj, TRUE ~ 0),
         LN2 = case_when((winner2=="LIB" & lib_proj-ndp_proj>30) ~ 31,
                        (winner2=="LIB" & lib_proj-ndp_proj<30) ~ lib_proj-ndp_proj, TRUE ~ 0),
         x2 = CN2/2*sqrt(3)-NC2/2*sqrt(3)-LC2/2*sqrt(3)+LN2/2*sqrt(3),
         y2 = -CN2*.5+CL2-NC2*.5+NL2-LC2/2-LN2/2,
         winner3 = case_when(proj=="Toss up" ~ "Toss up",
                             str_sub(proj,start=-4)=="PCPO" ~ "PCP",
                             TRUE ~ str_sub(proj,start=-3)),
         likely = case_when(proj=="Toss up" ~ "Toss up",
                            TRUE ~ str_trim(str_sub(proj,end=-5))),
         deltax = x2-x,
         deltay = y2-y)


ggplot(on18c) + 
  gridlines +
  geom_point(aes(x=x2,y=y2,color=winner3,shape=likely,group=dist,
                 text=sprintf("Over PC: %s<br>Over NDP: %s<br>Over Lib: %s",
                              round(LC2+NC2,1), round(LN2+CN2,1), round(NL2+CL2,1)))) +
  scale_color_manual(values=c("NDP"="orange","PCP"="blue","OLP"="red","Toss up"="grey50")) +
  labs(title = "Phillippe J. Fournier's Model for Ontario 2022")

#Deltas
ggplot(on18c) +
    annotate("segment",x=0,xend=0,y=0,yend=-32,arrow=arrow(),size=1) +
    annotate("segment",x=0,xend=16*sqrt(3),y=0,yend=16,arrow=arrow(),size=1) +
    annotate("segment",x=0,xend=-16*sqrt(3),y=0,yend=16,arrow=arrow(),size=1) +
    annotate("text",x=-1,y=-20,label="Lib relative gain",family="Fira Sans",hjust=1,lineheight=0.7,label.size=0) +
    annotate("text",x=-10.5,y=5,label="NDP relative gain",family="Fira Sans",hjust=1,lineheight=0.7,label.size=0) +
    annotate("text",x=15.5,y=10,label="PC relative gain",family="Fira Sans",hjust=1,lineheight=0.7,label.size=0) +
    geom_point(aes(x=deltax,y=deltay,color=region,group=dist,
                   text=sprintf("PC gain: %s<br>Lib gain: %s<br>NDP gain: %s",
                                round(pcp_proj-PCP,1), round(lib_proj-LIB,1), round(ndp_proj-NDP,1)))) +
    theme_void() +
    coord_fixed() +
    theme(text = element_text(family = "Fira Sans"),
          plot.margin = margin(1,1,1,1)) +
    labs(title = "How Fournier's model swings ridings",
         subtitle = "Not a uniform swing, but different swings for each riding. This is the\n'secret sauce' of his model.")
  ggplotly(tooltip= c("group","color","text"))

#The App
library(shiny)
ui <- fluidPage(
  
)

server <- function(input, output){
  
}

shinyApp(ui = ui, server = server)

#Sharing the App
library(rsconnect)
rsconnect::setAccountInfo(name='koji',
                          token='54E3228F24A698088DD194344E0003F1',
                          secret='<SECRET>')


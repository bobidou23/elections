library(pacman)
p_load(tidyverse,plotly,rvest,sf)

### PREPARE THE GROUNDWORKS
setwd("~/Desktop/Datasets")
temp <- tempfile()
download.file(url="https://www.elections.on.ca/content/dam/NGW/sitecontent/2017/preo/shapefiles/Electoral%20District%20Shapefile%20-%202022%20General%20Election.zip",
              destfile = temp)
unzip(temp)
on_geo <- read_sf("ELECTORAL_DISTRICT.shp") %>%
  select(dist_no="ED_ID",
         geometry)
setwd("~/Dropbox/Projects/elections")

on18 <- read.csv("caon_14-18.csv") %>% 
  filter(IsGeneralElection==1) %>%
  select(dist_no = ElectoralDistrictNumber,
         dist = ElectoralDistrictNameEnglish,
         party = PoliticalInterestCode,
         perc = PercentOfTotalValidBallotsCast) %>%
  mutate(perc=perc*100) %>%
  filter(party %in% c("LIB","NDP","PCP")) %>% 
  pivot_wider(id_cols=c(dist_no,dist),names_from=party,values_from=perc)

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
  theme(text = element_text(family = "Fira Sans"))
)

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
  # mutate(proj = gsub(" gain","",gsub(" hold","",Latest.projection)),
  #        proj = case_when(str_sub(proj,end=7)=="Toss up" ~ "Toss up",
  #                         TRUE ~ proj)) %>% 
  select(dist_no,region)

### THE TOPLINE: Refresh
top <- read_html("https://www.338canada.com/ontario/") %>%
  html_nodes(xpath = "/html/body/main/center/div[3]/script/text()") %>%
  html_text()
lib18 <- 19.57 - (str_extract(top, "(?<=var	lib_moy_vote =	)[1-9.]+") %>% as.numeric())
pcp18 <- 40.5 - (str_extract(top, "(?<=var	con_moy_vote =	)[1-9.]+") %>% as.numeric())
ndp18 <- 33.59 - (str_extract(top, "(?<=var	npd_moy_vote =	)[1-9.]+") %>% as.numeric())
# LN <- ndp18-lib18
# LC <- con18-lib18
# x0 = -LC2/2*sqrt(3)+LN2/2*sqrt(3)
# y0 = -LC2/2-LN2/2

x0 = -(pcp18-lib18)/2*sqrt(3)+(ndp18-lib18)/2*sqrt(3)
y0 = -(pcp18-lib18)/2-(ndp18-lib18)/2

### DISTRICTS: Refresh
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
on_odds[is.na(on_odds)] <- 0
on_odds[on_odds=="Leeds—Grenville—1000 Islands & Rideau Lakes"] <- "Leeds—Grenville—Thousand Islands and Rideau Lakes"
on_odds[on_odds=="Toronto—St. Paul`s"] <- "Toronto—St. Paul's"

### MERGE AND CLEAN
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
  left_join(on_regs) %>%
  select(-dist_no)

# on_odds <- on_odds %>%
#   group_by(dist) %>%
#   summarize(dist = head(dist,1),
#             pcp_proj = head(pcp_proj,1),
#             lib_proj = head(lib_proj,1),
#             ndp_proj = head(ndp_proj,1),
#             pcp_prob = head(pcp_prob,1),
#             lib_prob = head(lib_prob,1),
#             ndp_prob = head(ndp_prob,1),
#             region = head(region,1),
#             proj = head(proj,1))

on22 <- left_join(on18,on_odds,by="dist")

on22a <- on22 %>% 
  filter(dist!="Guelph") %>%
  mutate(ndp_uniform = NDP-ndp18,
         lib_uniform = LIB-lib18,
         pcp_uniform = PCP-pcp18,
         win18 = case_when((ndp_uniform>pcp_uniform & ndp_uniform>lib_uniform) ~ "NDP",
                             (pcp_uniform>ndp_uniform & pcp_uniform>lib_uniform) ~ "PCP",
                             (lib_uniform>ndp_uniform & lib_uniform>pcp_uniform) ~ "LIB"),
         NL0 = case_when((win18=="NDP" & ndp_uniform-lib_uniform>30) ~ 31,
                         (win18=="NDP" & ndp_uniform-lib_uniform<30) ~ ndp_uniform-lib_uniform, TRUE ~ 0),
         NC0 = case_when((win18=="NDP" & ndp_uniform-pcp_uniform>30) ~ 31,
                         (win18=="NDP" & ndp_uniform-pcp_uniform<30) ~ ndp_uniform-pcp_uniform, TRUE ~ 0),
         CL0 = case_when((win18=="PCP" & pcp_uniform-lib_uniform>30) ~ 31,
                         (win18=="PCP" & pcp_uniform-lib_uniform<30) ~ pcp_uniform-lib_uniform, TRUE ~ 0),
         CN0 = case_when((win18=="PCP" & pcp_uniform-ndp_uniform>30) ~ 31,
                         (win18=="PCP" & pcp_uniform-ndp_uniform<30) ~ pcp_uniform-ndp_uniform, TRUE ~ 0),
         LC0 = case_when((win18=="LIB" & lib_uniform-pcp_uniform>30) ~ 31,
                         (win18=="LIB" & lib_uniform-pcp_uniform<30) ~ lib_uniform-pcp_uniform, TRUE ~ 0),
         LN0 = case_when((win18=="LIB" & lib_uniform-ndp_uniform>30) ~ 31,
                         (win18=="LIB" & lib_uniform-ndp_uniform<30) ~ lib_uniform-ndp_uniform, TRUE ~ 0),
         x0 = CN0/2*sqrt(3)-NC0/2*sqrt(3)-LC0/2*sqrt(3)+LN0/2*sqrt(3),
         y0 = -CN0*.5+CL0-NC0*.5+NL0-LC0/2-LN0/2,
         win22 = case_when((ndp_proj>pcp_proj & ndp_proj>lib_proj) ~ "NDP",
                             (pcp_proj>ndp_proj & pcp_proj>lib_proj) ~ "PCP",
                             (lib_proj>ndp_proj & lib_proj>pcp_proj) ~ "LIB",
                             pcp_prob>lib_prob ~ "PCP",
                             lib_prob>pcp_prob ~ "LIB"),
         NL2 = case_when((win22=="NDP" & ndp_proj-lib_proj>30) ~ 31,
                         (win22=="NDP" & ndp_proj-lib_proj<30) ~ ndp_proj-lib_proj, TRUE ~ 0),
         NC2 = case_when((win22=="NDP" & ndp_proj-pcp_proj>30) ~ 31,
                         (win22=="NDP" & ndp_proj-pcp_proj<30) ~ ndp_proj-pcp_proj, TRUE ~ 0),
         CL2 = case_when((win22=="PCP" & pcp_proj-lib_proj>30) ~ 31,
                         (win22=="PCP" & pcp_proj-lib_proj<30) ~ pcp_proj-lib_proj, TRUE ~ 0),
         CN2 = case_when((win22=="PCP" & pcp_proj-ndp_proj>30) ~ 31,
                         (win22=="PCP" & pcp_proj-ndp_proj<30) ~ pcp_proj-ndp_proj, TRUE ~ 0),
         LC2 = case_when((win22=="LIB" & lib_proj-pcp_proj>30) ~ 31,
                         (win22=="LIB" & lib_proj-pcp_proj<30) ~ lib_proj-pcp_proj, TRUE ~ 0),
         LN2 = case_when((win22=="LIB" & lib_proj-ndp_proj>30) ~ 31,
                         (win22=="LIB" & lib_proj-ndp_proj<30) ~ lib_proj-ndp_proj, TRUE ~ 0),
         x2 = CN2/2*sqrt(3)-NC2/2*sqrt(3)-LC2/2*sqrt(3)+LN2/2*sqrt(3),
         y2 = -CN2*.5+CL2-NC2*.5+NL2-LC2/2-LN2/2,
         deltax = x2-x0,
         deltay = y2-y0,
         maxprob = pmax(pcp_prob, lib_prob, ndp_prob),
         likely = case_when(maxprob > 99.9 ~ "Safe",
                            maxprob > 90 ~ "Likely",
                            maxprob > 70 ~ "Leaning",
                            TRUE ~ "Toss up"),
         win22b = case_when(likely=="Toss up" ~ "Toss up",
                             TRUE ~ win22),
         strat = case_when(dist_no==84 ~ "Green",
                           ndp_proj-lib_proj > 5 ~ "NDP",
                           lib_proj-ndp_proj > 5 ~ "LIB",
                           TRUE ~ "Lib or NDP"),
         strat_max = case_when((likely!="Safe")&pcp_prob>0.1 ~ strat,
                            (likely!="Safe") ~ "Lib or NDP",
                            TRUE ~ "Doesn't matter"),
         strat = case_when((likely=="Leaning"|likely=="Toss up")&pcp_prob>10 ~ strat,
                           (likely=="Leaning"|likely=="Toss up") ~ "Lib or NDP",
                           TRUE ~ "Doesn't matter")) %>%
  select(dist_no,dist,pcp18=PCP,ndp18=NDP,lib18=LIB,
         pcp_uniform,ndp_uniform,lib_uniform,
         pcp_proj,ndp_proj,lib_proj,
         win18,win_proj=win22b,likely,strat,strat_max,region,
         x0,y0,x2,y2)
on22b <- left_join(on22a, on_geo, by="dist_no")
write.csv(on22a, "~/Desktop/three-way-elections/ontario2022.csv", row.names = FALSE)

#1 Fournier
ggplot(on22a) + 
  gridlines +
  geom_point(aes(x=x2,y=y2,color=winner2,shape=likely,group=dist,
                 text=sprintf("Over PC: %s<br>Over NDP: %s<br>Over Lib: %s",
                              round(LC2+NC2,1), round(LN2+CN2,1), round(NL2+CL2,1)))) +
  scale_color_manual(values=c("NDP"="orange","PCP"="blue","LIB"="red","Toss up"="grey50")) +
  labs(title = "Phillippe J. Fournier's Model for Ontario 2022")

table(on22a$winner2,on22a$likely)

#2 Uniform swing
ggplot(on22a) + 
  gridlines +
  annotate("segment",x=x0,y=y0,xend=x0,yend=32-x0/sqrt(3),color="grey50",size=1) +
  annotate("segment",x=x0,y=y0,xend=16*sqrt(3)-(pcp18-lib18)/2*sqrt(3),yend=-16-(pcp18-lib18)/2,color="grey50",size=1) +
  annotate("segment",x=x0,y=y0,xend=-16*sqrt(3)+(ndp18-lib18)/2*sqrt(3),yend=-16-(ndp18-lib18)/2,color="grey50",size=1) +
  annotate("label",x=-6*sqrt(3),y=-12,color="grey50",label="NDP->Lib",family="Fira Sans",hjust=0.5,lineheight=0.7,label.size=0)+
  annotate("label",x=9*sqrt(3),y=-13,color="grey50",label="PC->Lib",family="Fira Sans",hjust=0.5,lineheight=0.7,label.size=0)+
  annotate("text",x=3,y=12,color="grey50",label="PC->\nNDP",family="Fira Sans",hjust=0.5,lineheight=0.7,label.size=0)+
  geom_point(aes(x=x0,y=y0,color=winner0,group=dist,
                 text=sprintf("Over PC: %s<br>Over NDP: %s<br>Over Lib: %s",
                              round(LC0+NC0,1), round(LN0+CN0,1), round(NL0+CL0,1)))) +
  scale_color_manual(values=c("NDP"="orange","PCP"="blue","LIB"="red")) +
  theme(plot.subtitle = element_text(color = "grey50",lineheight=0.4),
        legend.position = "none") +
  labs(title = "Visualizing A 'Uniform Swing' Model in Ontario",
       subtitle = "What happens if you compare the provincial polling average\n
       to the 2018 results, and apply the same swing to \n
       every riding (a simple but very bad assumption!)")
ggplotly()

#3 Comparing the two
{ggplot(on22a) + 
  gridlines +
  geom_point(aes(x=x2,y=y2,color=region,group=dist,
                 text=sprintf("Over PC: %s<br>Over NDP: %s<br>Over Lib: %s",
                              round(LC0+NC0,1), round(LN0+CN0,1), round(NL0+CL0,1)))) +
  geom_segment(aes(x=x0,y=y0,xend=x2,yend=y2,color=region),alpha=0.3) +
  # scale_color_manual(values=c("NDP"="orange","PCP"="blue","LIB"="red")) +
  theme(plot.subtitle = element_text(color = "grey50",lineheight=0.4),
        legend.position = "none") +
  labs(title = "Comparing Fournier's Model to Uniform Swing",
       subtitle = "The characteristics of individual ridings that mean\n
       it swings in particular directions, compared to the provincial average")} %>%
ggplotly()
#3b Regions map
ggplot(on22a) +
  geom_sf(aes(fill=region,geometry=geometry),size=0)

#4 Strategic
ggplot(on22a) +
  gridlines +
  geom_point(aes(x=x2,y=y2,color=strat,group=dist,
                 text=sprintf("Over PC: %s<br>Over NDP: %s<br>Over Lib: %s",
                              round(LC2+NC2,1), round(LN2+CN2,1), round(NL2+CL2,1)))) +
  scale_color_manual(values=c("NDP"="orange","LIB"="red","Green"="green","Lib or NDP"="purple","Doesn't matter"="grey50"))

#4b Strategic map
ggplot(on22a) +
  geom_sf(aes(fill=strat,geometry=geometry),size=0) +
  scale_fill_manual(values=c("NDP"="orange","LIB"="red","Green"="green","Lib or NDP"="purple","Doesn't matter"="grey50"))

ggplot(on22a) +
  geom_sf(aes(fill=ndp_proj,geometry=geometry),size=0)

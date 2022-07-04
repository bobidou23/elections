library(pacman)
p_load(tidyverse,rvest,sf,lubridate,magrittr)

ilpoll <- function(url,location,doubleheader=FALSE,year=NA) {
  scrape <- read_html(paste0("https://en.wikipedia.org/wiki/Opinion_polling_for_the_",url,"_Israeli_legislative_election")) %>%
    html_node(xpath=paste0("//*[@id=\"mw-content-text\"]/div[1]/table[",location,"]")) %>%
    html_table() %>%
    .[,nchar(names(.))>0] #some tables have unnecessary blank columns at the right
  if(doubleheader) { #some tables have merges in the top row
    colnames(scrape) <- scrape[1,]
    scrape <- scrape[2:nrow(scrape),]
  }
  if (!("Publisher" %in% colnames(scrape))) { #some tables combine the columns in one
    scrape <- separate(scrape, `Polling firm`, c("Polling firm", "Publisher"), sep="/", extra="merge")
  }
  filter(scrape, !str_detect(Shas,"[A-Z]"), #filter out rows with just event updates
         !str_detect(`Polling firm`,"[0-9]"), #filter out rows with just a new year
         row_number()!=1) %>%
    lapply(function(y) gsub("\\[[a-z0-9]+\\]","",y)) %>% #take out footnotes
    as.data.frame() %>%
    {if(is.na(year)) mutate(., Date = gsub("[0-9]+\\–","",Date)) #add years where necessary
      else mutate(., Date = paste(gsub("[0-9]+\\–","",Date),year))} %>%
    mutate(Date = dmy(Date)) %>%
    filter(!is.na(Date))
}

#il_polls dataset scraped below

il_polls2 <- il_polls %>%
  pivot_longer(cols=!c(Date,Polling.firm,Publisher), names_to="party", values_to="seats", values_drop_na=TRUE) %>%
  mutate(seats = case_when(str_detect(seats,"[0-9]\\([0-9.%]+\\)") ~ str_sub(seats,start=2), #some entries like "4(3.4%)"
                           TRUE ~ seats),
         seats = case_when(party %in% c("Ra.am","Ta.al") & seats%in%c(9,10,11) ~ NA_character_,
                           TRUE ~ seats),
         seats = case_when(seats=="–" ~ 0, #for entries like "-"
                           str_detect(seats,"\\(") ~ as.numeric(str_sub(seats,start=2,end=-3))*1.2, #for entries like "2.5%"
                           TRUE~as.numeric(seats))) %>% #character -> numeric
  # unique(party) %>% as.data.frame() %>% anti_join(il_parties, c("."="party")) #make sure you got all the parties
  group_by(Date,Polling.firm,Publisher,party) %>% #some polls in the early Gantz era give two scenarios each. Pick the latter = one with Gantz
  summarise(party = tail(party,1),
            seats = tail(seats,1)) %>% 
  ungroup() %>%
  inner_join(il_parties) %>% #standardize party names
  select(-party) %>%
  pivot_wider(id_cols=c(Date,Polling.firm,Publisher),names_from=party2, values_from=seats) %>%
  arrange(desc(Date)) #need it for the lapply

# il_polls2 %>%
#   pivot_longer(cols=!c(Date,Polling.firm,Publisher), names_to="party", values_to="seats", values_drop_na=TRUE) %>%
#   arrange(Date) %>%
#   group_by(party) %>%
#   summarise(start = head(Date,1),
#             end = tail(Date,1)) %>%
#   View()
# il_polls2 %>%
#   mutate(BlueWhite = case_when(Date>"2020-04-02") ~ NaN, TRUE ~ BlueWhite,
#          YeshAtid = case_when(Date>"2019-02-21" & Date<"2020-04-02" ~ NaN, TRUE ~ YeshAtid),
#          Gantz = case_when(Date > "2020-04-02" ~ BlueWhite, TRUE ~ Gantz),
#          LaborMeretz = case_when(Date>"2020-01-13" & Date<"2020-04-04" ~ LaborMeretz, TRUE ~ NaN),
#          Labor = case_when(Date>"2020-01-13" & Date<"2020-04-04" ~ Labor, TRUE ~ NaN),
#          Meretz = case_when(Date>"2020-01-13" & Date<"2020-04-04" ~ Meretz, TRUE ~ NaN),
#          Yamina = case_when(Date > "2019-07-29" & Date <"2020-05-14" ~ Yamina, TRUE ~ NaN),
#          ReligiousZionist = case_when(Date > "2019-07-29" & Date <"2020-05-14" ~ NaN, TRUE ~ JewishHome + ReligiousZionist),
#          Bennett = case_when(Date < "2019-07-29" ~ NewRight,
#                              Date > "2020-05-14" ~ Yamina, TRUE ~ NaN),
#          JointList = case_when(Date > "2019-01-16" & Date < "2019-08-07" ~ NaN, TRUE ~ JointList),
#          HadashTaal = case_when(Date > "2019-01-16" & Date < "2019-08-07" ~ HadashTaal, TRUE ~ NaN),
#          RaamBalad = case_when((Date > "2019-01-16" & Date < "2019-08-07") | Date > "" ~ HadashTaal, TRUE ~ NaN),
#   )

#How to get approximte percentages for parties below the threshold
il_below <- data.frame(matrix(NA,nrow=length(seq(as.Date("2018-12-25"), today(), by=1))*(length(unique(il_parties$party2))), #moving averages
                              ncol=4))
for (i in seq(as.Date("2018-12-25"), today(), by=1)) {
  i <- as.Date(i, origin = lubridate::origin)
  minrow <- (i-as.Date("2018-12-25"))*length(unique(il_parties$party2))+1
  maxrow <- (i-as.Date("2018-12-25")+1)*length(unique(il_parties$party2))
  row <- lapply(filter(il_polls2, Date>i-days(15), Date<=i+days(15))[,4:ncol(il_polls2)], function(x) c(mean(x<4,na.rm=TRUE),mean(x[x<4&x>0],na.rm=TRUE))) %>%
    as.data.frame()
  il_below[minrow:maxrow,3:4] <- row %>% transpose()
  il_below[minrow:maxrow,2] <- row %>% names()
  # il_below[minrow:maxrow,1] <- i
}
colnames(il_below) <- c("Date","party","prop","below_avg")
il_below[,1] <- rep(seq(as.Date("2018-12-25"), today(), by=1), each=length(unique(il_parties$party2)))
il_below <- filter(il_below, prop>0, !is.na(prop)) %>%
  mutate(prop=as.numeric(prop),
         below_avg=as.numeric(below_avg))
below_lm <- lm(il_below$below_avg ~ il_below$prop)
il_below2 <- il_below %>%
  mutate(fitted = below_lm$coefficients[1]+below_lm$coefficients[2]*prop,
         below_avg = case_when(is.na(below_avg) ~ fitted,
                               TRUE ~ below_avg)) %>%
  select(Date,party,below_avg)

il_polls3 <- il_polls2 %>% #bringing the fitted values into the main polls sheet
  pivot_longer(cols=!c(Date,Polling.firm,Publisher), names_to="party", values_to="seats", values_drop_na=TRUE) %>%
  left_join(il_below2) %>%
  mutate(seats = case_when(seats==0 ~ round(below_avg,2),
                           TRUE ~ seats)) %>%
  select(-below_avg) %>%
  pivot_wider(id_cols=c(Date,Polling.firm,Publisher),names_from=party, values_from=seats)

il_changes <- il_polls3 %>%
  pivot_longer(cols=!c(Date,Polling.firm,Publisher), names_to="party", values_to="seats") %>%
  arrange(Date) %>%
  group_by(party) %>%
  mutate(boundary = "",
         boundary = case_when(!is.na(seats)&is.na(lag(seats,1))&is.na(lag(seats,2))&is.na(lag(seats,3))&is.na(lag(seats,4))&is.na(lag(seats,5)) ~ paste0("start",boundary),TRUE~boundary),
         boundary = case_when(!is.na(seats)&is.na(lead(seats,1))&is.na(lead(seats,2))&is.na(lead(seats,3))&is.na(lead(seats,4))&is.na(lead(seats,5)) ~ paste0(boundary,"end"),TRUE~boundary)) %>%
  filter(boundary=="start"|boundary=="end") %>%
  mutate(time = ceiling(row_number()/2)) %>%
  arrange(party) %>%
  select(party,time,Date,boundary) %>%
  pivot_wider(id_cols=c(party,time),names_from=boundary,values_from=Date)

impdates <- c(unique(il_elections$Date),"2020-12-06")
#Calculate rolling averages (30 days)
il_avgs <- data.frame(matrix(NA,nrow=length(seq(as.Date("2018-12-25"), today(), by=1)), #moving averages
                             ncol=length(unique(il_parties$party2))+2))
for (i in seq(as.Date("2018-12-25"), today(), by=1)) {
  i <- as.Date(i, origin = lubridate::origin)
  nrow <- i-as.Date("2018-12-25")+1
  il_avgs[nrow,1] <- nrow(filter(il_polls3, Date>i-days(10), Date<=i))
  # screen <- if(nrow(filter(il_polls3, Date>i-days(15), Date<=i)) > 5) {15} else
  #   if(nrow(filter(il_polls3, Date>i-days(30), Date<=i)) > 5) {30} else {60}
  # screen <- 
  # il_avgs[nrow,2] <- screen
  mindate <- max(impdates[impdates <= i],i-days(30))
  il_avgs[nrow,3:(ncol(il_polls3)-1)] <- lapply(filter(il_polls3, Date>mindate, Date<=i)[,4:ncol(il_polls3)],
                                                function(x) if (i<=max(il_polls3$Date[!is.na(x)])) {mean(x, na.rm=TRUE)} else {NA}) %>%
  unlist()
}

lapply(il_polls3[,4:ncol(il_polls3)],
       function(x) {
         for (i in seq(as.Date("2018-12-25"), today(), by=1)) {
           i <- as.Date(i, origin = lubridate::origin)
           nrow <- i-as.Date("2018-12-25")+1
           mindate <- max(impdates[impdates <= i],i-days(30))
           
           
       }})
colnames(il_avgs) <- c("Date","screen",colnames(il_polls3)[-1:-3]) #add column names
il_avgs[,1] <- seq(as.Date("2018-12-25"), today(), by=1) #add date column
il_avgs <- il_avgs %>% #AVOID DOUBLE COUNTING
  rowwise() %>%
  mutate(bibi_bloc = sum(JewishHome,Kahlon,Likud,Shas,UTJ,Feiglin,OtzmaYehudit,ReligiousZionist,na.rm=TRUE),
         bibi_bloc = case_when(Date<"2019-05-01" ~ sum(bibi_bloc+Yamina+Lieberman,na.rm=TRUE),
                               Date<"2021-06-02" ~ sum(bibi_bloc+Yamina,na.rm=TRUE),
                               TRUE~bibi_bloc),
         bibi_bloc = case_when(bibi_bloc>0 ~ bibi_bloc, TRUE~NA),
         left_arab = sum(JointList,Meretz,YeshAtid,Labor,Livni,HadashTaal,BlueWhite,Raam,Barak,Shaffir,Huldai,Shelah,na.rm=TRUE)-3,
         left_arab = case_when(left_arab>0 ~ left_arab, TRUE~NA),
         left_right = sum(Levy,YeshAtid,Lieberman,Labor,Livni,BlueWhite,Barak,Shaffir,HendelHauser,Saar,Huldai,Shelah,Yaalon,na.rm=TRUE),
         left_right = case_when(Date>"2021-06-02"~sum(left_right,Yamina,Meretz,Raam,na.rm=TRUE),
                                Date>"2020-03-11"~sum(left_right,Yamina,na.rm=TRUE),
                                Date<"2019-05-01"~sum(left_right,Lieberman,na.rm=TRUE),
                                TRUE ~ left_right),
         left_right = case_when(left_right>0 ~ left_right, TRUE~NA))
il_avgs[(il_avgs$Date %in% (c(rep(unique(il_elections$Date),each=30)+seq(1,30,by=1)))),2:ncol(il_avgs)] <- NA #put breaks just after elections, for the graph

il_avgs %>% #GRAPH-MAKING
  # select(-bibi_bloc,-left_arab,-left_right, -screen) %>%
  pivot_longer(cols=-Date,names_to="party",values_to="seats") %>% #keep NA so line graph doesn't connect
  left_join(il_parties2,"party") %>%
  ggplot(aes(x=Date,y=seats,group=party,color=type)) +
  geom_line(size=0.3) +
  geom_jitter(data=il_elections,width=10,alpha=0.6,height=0,shape=17,size=4) +
  scale_color_manual(values=il_colors) +
  facet_wrap(~party) +
  geom_hline(yintercept=4) +
  theme(legend.position = "none")

il_colors <- c("forestgreen","red","darkorange","blue","navyblue","turquoise","black")
names(il_colors) <- c("Arab","Left","Centre","Right","Likud","Haredi","Far-right")

ggsave("~/Desktop/il_avg.svg")

#graph of polls
pivot_longer(il_polls2, cols=!c(Date,Polling.firm,Publisher), names_to="party", values_to="seats", values_drop_na=TRUE) %>%
  ggplot(aes(x=Date,y=seats,color=party)) +
  geom_point(alpha=0.1) +
  geom_jitter(data=il_elections,width=10,alpha=0.6,height=0,shape=17,size=4)
  
filter(il_polls,!is.na(Ra.am)) %>%
  ggplot(aes(x=Date,y=Ra.am)) +
  geom_point()

#Sandbox
read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_April_2019_Israeli_legislative_election") %>%
  html_node(xpath="//*[@id=\"mw-content-text\"]/div[1]/table[1]") %>%
  html_table() %>%
  .[,nchar(names(.))>0] %>%
  filter(!str_detect(Shas,"[A-Z]"),
         !str_detect(`Polling firm`,"[0-9]"),
         row_number()!=1) %>%
  lapply(function(y) gsub("\\[[a-z0-9]+\\]","",y)) %>%
  as.data.frame() %>%
  {if(noyear) mutate(., Date = paste(gsub("[0-9]+\\–","",Date),str_sub(url, start=-2)))
    else mutate(., Date = gsub("[0-9]+\\–","",Date))} %>%
  mutate(Date = dmy(Date)) %>%
  filter(!is.na(Date))

illead_22 <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Israeli_legislative_election") %>%
  html_node(xpath="//*[@id=\"mw-content-text\"]/div[1]/table[8]")


ilbennett_22 <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Israeli_legislative_election") %>%
  html_node(xpath="//*[@id=\"mw-content-text\"]/div[1]/table[9]")

illapid_22 <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Israeli_legislative_election") %>%
  html_node(xpath="//*[@id=\"mw-content-text\"]/div[1]/table[10]")

ilgantz_22 <- read_html("https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Israeli_legislative_election") %>%
  html_node(xpath="//*[@id=\"mw-content-text\"]/div[1]/table[12]")

#Manual entry
il_polls <- bind_rows(ilpoll("next",1),
                      ilpoll("2021",1),
                      ilpoll("2021",2),
                      ilpoll("2021",3),
                      ilpoll("2021",4),
                      ilpoll("2021",5),
                      ilpoll("2021",6),
                      ilpoll("2021",7),
                      ilpoll("2021",8),
                      ilpoll("2020",1),
                      ilpoll("2020",2)[1:2,],
                      ilpoll("2020",3,TRUE)[c(1,5,9,13,15,17,18,20:22),],
                      ilpoll("2020",4),
                      ilpoll("2020",5),
                      ilpoll("September_2019",1,year=2019),
                      ilpoll("September_2019",2,year=2019),
                      ilpoll("September_2019",3,year=2019),
                      ilpoll("September_2019",4,year=2019),
                      ilpoll("September_2019",5,year=2019),
                      ilpoll("September_2019",6,year=2019),
                      ilpoll("April_2019",1,year=2019),
                      ilpoll("April_2019",3,year=2019),
                      ilpoll("April_2019",4,year=2019),
                      ilpoll("April_2019",5,year=2018),
                      ilpoll("April_2019",7))

il_parties <- data.frame(party = c("Hadash.Ta.al","Ta.al",
                                   "Ra.am","Ra.am.Balad",
                                   "Emet","Labor.Meretz",
                                   "Dem.Union","Meretz",
                                   "Labor","Labor.Gesher","ZionistUnion",
                                   "Gesher","OrlyLevy",
                                   "Yesh.Atid.Telem","YeshAtid",
                                   "B.W","Blue..White","Hosen","Hosen.Telem",
                                   "NEP","NewEconomic",
                                   "YB","YisraelBeiteinu",
                                   "Likud","Likud.Kulanu",
                                   "NewRight","Yamina", #All Bennett/Shaked parties
                                   "JewishHome","JH","JH.NU","UJH","URWP", #Rafi Peretz (incl. alliance with Otzma)
                                   "NU","ReligiousZionist","ReligiousZionist.a.","RZ", #Bezalel Smotrich (incl. alliance with Otzma)
                                   "Otzma","OtzmaYehudit","OtzmaYehudit.Noam","OY",
                                   "DerekhEretz","Green","Hatnuah","IDP","Israelis","JointList","Kulanu",
                                   "NewHope","Shas","Telem","Tnufa","UTJ","Zehut"),
                         party2 = c("HadashTaal","HadashTaal",
                                    "Raam","Raam",
                                    "LaborMeretz","LaborMeretz",
                                    "Meretz","Meretz",
                                    "Labor","Labor","Labor",
                                    "Levy","Levy",
                                    "YeshAtid","YeshAtid",
                                    "BlueWhite","BlueWhite","Gantz","Gantz",
                                    "Zelekha","Zelekha",
                                    "Lieberman","Lieberman",
                                    "Likud","Likud",
                                    "Bennett","Yamina",
                                    "JewishHome","JewishHome","JewishHome","JewishHome","JewishHome",
                                    "ReligiousZionist","ReligiousZionist","ReligiousZionist","ReligiousZionist",
                                    "OtzmaYehudit","OtzmaYehudit","OtzmaYehudit","OtzmaYehudit",
                                    "HendelHauser","Shaffir","Livni","Barak","Huldai","JointList","Kahlon",
                                    "Saar","Shas",
                                    "Yaalon","Shelah","UTJ","Feiglin"))
il_parties2 <- data.frame(party = c("HadashTaal","Raam",
                                    "LaborMeretz","Meretz","Labor","Levy",
                                    "BlueWhite","YeshAtid","Gantz","Zelekha",
                                    "Lieberman","Likud","Yamina","Bennett",
                                    "JewishHome","ReligiousZionist","OtzmaYehudit",
                                    "HendelHauser","Shaffir","Livni","Barak","Huldai","JointList","Kahlon",
                                    "Saar","Shas","Yaalon","Shelah","UTJ","Feiglin"),
                          type= c("Arab","Arab",
                                  "Left","Left","Left","Centre",
                                  "Centre","Centre","Centre","Centre",
                                  "Right","Likud","Right","Right",
                                  "Far-right","Far-right","Far-right",
                                  "Right","Left","Centre","Left","Left","Arab","Right",
                                  "Right","Haredi","Right","Centre","Haredi","Right"))

il_elections <- data.frame(Date = as.Date(c("2019-04-09","2019-09-17","2020-03-02","2021-03-23")),
                           JointList = c(NA,13,15,6),
                           HadashTaal = c(6,NA,NA,NA),
                           Raam = c(4,NA,NA,4),
                           LaborMeretz = c(NA,NA,7,NA),
                           Labor = c(6,6,NA,7),
                           Meretz = c(4,5,NA,6),
                           Levy = c(2.1,NA,NA,NA),
                           BlueWhite = c(35,33,33,NA),
                           Gantz = c(NA,NA,NA,8),
                           YeshAtid = c(NA,NA,NA,17),
                           Zelekha = c(NA,NA,NA,0.9),
                           Shas = c(8,8,9,9),
                           UTJ = c(8,7,7,7),
                           Lieberman = c(5,8,7,7),
                           Saar = c(NA,NA,NA,6),
                           Kahlon = c(4,NA,NA,NA),
                           Likud = c(35,32,36,30),
                           Feiglin = c(3.3,NA,NA,NA),
                           Yamina = c(NA,7,6,NA),
                           Bennett = c(3.9,NA,NA,7),
                           JewishHome = c(5,NA,NA,NA),
                           ReligiousZionist = c(NA,NA,NA,6),
                           OtzmaYehudit = c(NA,2.3,0.5,NA)) %>%
  pivot_longer(cols=-Date,names_to="party",values_to="seats") %>%
  left_join(il_parties2,"party")


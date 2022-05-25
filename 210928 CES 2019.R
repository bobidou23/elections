library(pacman)
p_load(rvest,tidyverse,stringr,plotly,magrittr,sf,foreign)
p_load(data.table)
p_load(rms)

summary(model3_c0)
model3_c0 <- mini19 %>% #Con~Eth
  with(glm(as.factor(convote) ~ protestant*eth_brit + religious + cannabis + west + atlantic + quartile2 + quartile3 + quartile4, family = "binomial"))
model3_c1 <- mini19 %>% #Con~Eth
  with(glm(as.factor(convote) ~ protestant + religious + cannabis + west + atlantic + quartile2 + quartile3 + quartile4 + 
             eth_row+eth_north+eth_south+eth_ctl+eth_east+eth_fr, family = "binomial"))
model3_c2 <- mini19 %>% #Con~Eth ON
  filter(ontario==1) %>%
  with(glm(as.factor(convote) ~ protestant + religious + quartile2 + quartile3 + quartile4 + 
             eth_row+eth_north+eth_south+eth_ctl+eth_east+eth_fr, family = "binomial"))
mini19 %>% #Con~Eth ON
  filter(ontario==1) %>%
  with(glm(as.factor(convote) ~ protestant + religious + quartile2 + quartile3 + quartile4 + 
             eth_row+eth_north+eth_south+eth_ctl+eth_east+eth_fr, family = "binomial"))
model3_c3 <- mini19 %>% #Con~EthID
  with(glm(as.factor(convote) ~ protestant + religious + west + atlantic + quartile2 + quartile3 + quartile4 + 
             eth_id + imm, family = "binomial"))
model3_c4 <- mini19 %>% #Con~EthID ON
  filter(ontario==1) %>%
  with(glm(as.factor(convote) ~ protestant + religious + quartile2 + quartile3 + quartile4 + 
             eth_id + imm, family = "binomial"))
model3_c5 <- mini19 %>% #Con~ROW
  with(glm(as.factor(convote) ~ protestant + religious + cannabis + west + atlantic + quartile2 + quartile3 + quartile4 + 
             imm*eth_row, family = "binomial"))
model3_c6 <- mini19 %>% #Con~ROW ID
  with(glm(as.factor(convote) ~ protestant + cannabis + west + atlantic + quartile2 + quartile3 + quartile4 + 
             eth_id*eth_row, family = "binomial"))
model3_c7 <- mini19 %>% #Con~EE
  with(glm(as.factor(convote) ~ protestant + religious + west + atlantic + quartile2 + quartile3 + quartile4 + 
             imm*eth_east, family = "binomial"))
model3_c8 <- mini19 %>% #Con~EE ID
  with(glm(as.factor(convote) ~ protestant + religious + west + atlantic + quartile2 + quartile3 + quartile4 + 
             eth_id*eth_east, family = "binomial"))

model3_l3 <- mini19 %>% #Lib~EthID
  with(glm(as.factor(libvote) ~ catholic + west + atlantic + quartile2 + quartile3 + quartile4 + 
             imm + eth_id, family = "binomial"))
model3_l4 <- mini19 %>% #Lib~EthID
  filter(ontario==1) %>%
  with(glm(as.factor(libvote) ~ catholic + quartile2 + quartile3 + quartile4 + 
             imm + eth_id, family = "binomial"))
model3_l5 <- mini19 %>% #Lib~ROW
  with(glm(as.factor(libvote) ~ catholic + west + atlantic + quartile2 + quartile3 + quartile4 + 
             imm*eth_row, family = "binomial"))
model3_l6 <- mini19 %>% #LIb~ROW 
  with(glm(as.factor(libvote) ~ catholic + west + atlantic + quartile2 + quartile3 + quartile4 + 
             eth_id*eth_row, family = "binomial"))

require(rms)
with(mini19, lrm(as.factor(convote) ~ catholic + west + atlantic + quartile2 + quartile3 + quartile4 + #add policies/spending
           catholic_pct + catholic_pct^2))


model4_1 <- glm(as.factor(convote) ~ protestant + west + atlantic + quartile2 + quartile3 + quartile4 + #add policies/spending
                  protestant_pct+I(protestant_pct^2), data=mini19, family = "binomial")
summary(model4_1)
model4_1 %>% 
  predict(newdata = with(mini19,data.frame(protestant=c(1,1,1),atlantic=mean(atlantic),west=mean(west),
                                           quartile2=mean(quartile2),quartile3=mean(quartile3),quartile4=mean(quartile4),
                                           protestant_pct=c(14.72,20.82,26.91))),
          type="response")

model4_2 <- glm(as.factor(convote) ~ catholic + west + atlantic + quartile2 + quartile3 + quartile4 + #add policies/spending
                  catholic_pct + I(catholic_pct^2), data=mini19, family = "binomial")
summary(model4_2)
model4_2 %>% 
  predict(newdata = with(mini19,data.frame(catholic=c(1,1,1),atlantic=mean(atlantic),west=mean(west),
                                           quartile2=mean(quartile2),quartile3=mean(quartile3),quartile4=mean(quartile4),
                                           catholic_pct = c(20.91,26.81,32.91))),
          type="response")
model4_3 <- glm(as.factor(convote) ~ catholic + west + atlantic + quartile2 + quartile3 + quartile4 + #add policies/spending
                  orthodox_pct + I(orthodox_pct^2), data=mini19, family = "binomial")
summary(model4_3)
model4_3 %>% 
  predict(newdata = with(mini19,data.frame(catholic=c(1,1,1),atlantic=mean(atlantic),west=mean(west),
                                           quartile2=mean(quartile2),quartile3=mean(quartile3),quartile4=mean(quartile4),
                                           orthodox_pct = c(0.46,1.26,2.39))),
          type="response")

##Creating the dataset
mini19qc <- ces2019 %>%
  filter(!is.na(cps19_votechoice),cps19_votechoice!=9,
         !is.na(cps19_religion),
         !is.na(constituencynumber),
         !is.na(cps19_province),!(cps19_province %in% c(19,21,26))) %>%
  left_join(dist_quart, c(constituencynumber="FEDNUM")) %>%
  left_join(riding_religion, c(constituencynumber="Geo_Code")) %>%
  mutate(libvote = case_when(cps19_votechoice==1~1, TRUE~0), #possibly also pes19_votechoice
         convote = case_when(cps19_votechoice==2~1, TRUE~0),
         ndpvote = case_when(cps19_votechoice==3~1, TRUE~0),
         religion = as.factor(case_when(cps19_religion %in% c(1,2,22,23) ~ 0,
                                        cps19_religion %in% c(8,9,13,15,16,17,18,19,20) ~ 1,
                                        TRUE ~ as.double(cps19_religion))),
         protestant = case_when(cps19_religion %in% c(8,9,13,15,16,17,18,19,20) ~ 1, TRUE ~ 0),
         catholic = case_when(cps19_religion==10 ~ 1, TRUE~0),
         minirel = case_when(cps19_religion %in% c(3,4,5,6,7,11,12,14,21) ~ 1, TRUE~0),
         prothard = case_when(cps19_religion %in% c(9,15) ~ 1, TRUE ~ 0),
         anglican = case_when(cps19_religion==8 ~ 1, TRUE~0),
         baptist = case_when(cps19_religion==9 ~ 1, TRUE~0),
         lutheran = case_when(cps19_religion==13 ~ 1, TRUE~0),
         pentecostal = case_when(cps19_religion==15 ~ 1, TRUE~0),
         presbyterian = case_when(cps19_religion==16 ~ 1, TRUE~0),
         united = case_when(cps19_religion==18 ~ 1, TRUE~0),
         reformed = case_when(cps19_religion==19 ~ 1, TRUE~0),
         salvation = case_when(cps19_religion==20 ~ 1, TRUE~0),
         orthodox = case_when(cps19_religion==11 ~ 1, TRUE~0),
         buddhist = case_when(cps19_religion==3 ~ 1, TRUE~0),
         hindu = case_when(cps19_religion==4 ~ 1, TRUE~0),
         jewish = case_when(cps19_religion==5 ~ 1, TRUE~0),
         muslim = case_when(cps19_religion==6 ~ 1, TRUE~0),
         sikh = case_when(cps19_religion==7 ~ 1, TRUE~0),
         west = case_when(cps19_province %in% c(14,15,16,25)~1, TRUE~0),
         atlantic = case_when(cps19_province %in% c(17,18,20,23)~1, TRUE~0),
         quebec = case_when(cps19_province==24~1, TRUE~0),
         ontario = case_when(cps19_province==22~1, TRUE~0),
         enviro = case_when(cps19_pos_jobs%in%c(4,5)~1,
                            cps19_pos_jobs%in%c(1,2,3,6)~0,
                            TRUE ~ NA_real_),
         religious = case_when(cps19_rel_imp%in%c(1,2)~1,
                               cps19_rel_imp%in%c(3,4,5)~0,
                               TRUE ~ NA_real_),
         rel1 = case_when(cps19_rel_imp==1~1,
                               cps19_rel_imp%in%c(3,4,5)~0,
                               TRUE ~ NA_real_),
         rel2 = case_when(cps19_rel_imp==2~1,
                          cps19_rel_imp%in%c(3,4,5)~0,
                          TRUE ~ NA_real_),
         abortion = case_when((pes19_abort1==2 | pes19_abort2==3 | pes19_abort3==1 | pes19_abort4==21 | pes19_abort5==1 | pes19_abort6==5) ~ 0,
                              (pes19_abort1%in%c(1,3) | pes19_abort2%in%c(1,2,4) | pes19_abort3%in%c(2,3,4,5,6) |
                                 pes19_abort4%in%c(22,23,24) | pes19_abort5%in%c(2,3,4,5) | pes19_abort6%in%c(1,2,3,4,6)) ~ 1,
                              TRUE ~ NA_real_),
         newerlife = case_when(pes19_newerlife%in%c(4,5)~1, #Newer lifestyles are contributing to the breakdown of our society.
                               pes19_newerlife%in%c(1,2,3,6)~0,
                               TRUE ~ NA_real_),
         life = case_when(cps19_pos_life%in%c(4,5)~1,
                          cps19_pos_life%in%c(1,2,3,6)~0,
                          TRUE ~ NA_real_),
         cannabis = case_when(cps19_pos_cannabis%in%c(4,5)~1,
                              cps19_pos_cannabis%in%c(1,2,3,6)~0,
                              TRUE~NA_real_),
         spend_educ = case_when(cps19_spend_educ==3~1,
                                cps19_spend_educ%in%c(1,2,4)~0,
                                TRUE~NA_real_),
         spend_defence = case_when(cps19_spend_defence==3~1,
                                   cps19_spend_defence%in%c(1,2,4)~0,
                                   TRUE~NA_real_),
         spend_just = case_when(cps19_spend_just_law==3~1,
                                cps19_spend_just_law%in%c(1,2,4)~0,
                                TRUE~NA_real_),
         spend_imm = case_when(cps19_spend_imm_min==3~1,
                               cps19_spend_imm_min%in%c(1,2,4)~0,
                               TRUE~NA_real_),
         abor = case_when(cps19_ethnicity_23==1~1,TRUE~0),
         brit = case_when(cps19_ethnicity_24==1~1,TRUE~0),
         chin = case_when(cps19_ethnicity_25==1~1,TRUE~0),
         dutch = case_when(cps19_ethnicity_26==1~1,TRUE~0),
         eng = case_when(cps19_ethnicity_27==1~1,TRUE~0),
         fr = case_when(cps19_ethnicity_28==1~1,TRUE~0),
         frcan = case_when(cps19_ethnicity_29==1~1,TRUE~0),
         ger = case_when(cps19_ethnicity_30==1~1,TRUE~0),
         hisp = case_when(cps19_ethnicity_31==1~1,TRUE~0),
         ind = case_when(cps19_ethnicity_32==1~1,TRUE~0),
         inuit = case_when(cps19_ethnicity_33==1~1,TRUE~0),
         irish = case_when(cps19_ethnicity_34==1~1,TRUE~0),
         ital = case_when(cps19_ethnicity_35==1~1,TRUE~0),
         metis = case_when(cps19_ethnicity_36==1~1,TRUE~0),
         pol = case_when(cps19_ethnicity_37==1~1,TRUE~0),
         que = case_when(cps19_ethnicity_38==1~1,TRUE~0),
         scot = case_when(cps19_ethnicity_39==1~1,TRUE~0),
         ukr = case_when(cps19_ethnicity_40==1~1,TRUE~0),
         text1 = cps19_ethnicity_41_TEXT, text2 = cps19_ethnicity_42_TEXT,
         eth_brit = case_when((brit==1 | eng==1 | irish==1 | scot==1 |
                                 text1%in%(filter(ethoth,Cat3=="Brit")%$%Category) |
                                 text2%in%(filter(ethoth,Cat3=="Brit")%$%Category)) ~ 1, TRUE~0),
         eth_row = case_when((chin==1 | hisp==1 | ind==1 |
                                text1%in%(filter(ethoth,Cat3=="Rest of world")%$%Category) |
                                text2%in%(filter(ethoth,Cat3=="Rest of world")%$%Category)) ~ 1, TRUE~0),
         eth_north = case_when((text1%in%(filter(ethoth,Cat3=="North")%$%Category) |
                                  text2%in%(filter(ethoth,Cat3=="North")%$%Category)) ~ 1, TRUE~0),
         eth_south = case_when((ital==1 | text1%in%(filter(ethoth,Cat3=="South")%$%Category) |
                                  text2%in%(filter(ethoth,Cat3=="South")%$%Category)) ~ 1, TRUE~0),
         eth_ctl = case_when((dutch==1 | ger==1 | text1%in%(filter(ethoth,Cat3=="Centre")%$%Category) |
                                text2%in%(filter(ethoth,Cat3=="Centre")%$%Category)) ~ 1, TRUE~0),
         eth_east = case_when((ukr==1 | pol==1 | text1%in%(filter(ethoth,Cat3=="East")%$%Category) |
                                 text2%in%(filter(ethoth,Cat3=="East")%$%Category)) ~ 1, TRUE~0),
         eth_fr = case_when((fr==1 | frcan==1 | que==1)~1, TRUE~0),
         eth_id = case_when(pes19_ethid%in%c(4,5)~1, #“My ethnicity and language are important parts of my identity”
                            pes19_ethid%in%c(1,2,3)~0,
                            TRUE ~ NA_real_),
         imm = case_when(cps19_bornin_canada==2~1, #Were you born in Canada?
                         cps19_bornin_canada==1~0,
                         TRUE ~ NA_real_)) %>%
  select(libvote, convote, ndpvote, religion, protestant, catholic, minirel, 
         prothard, anglican, baptist, lutheran, pentecostal, presbyterian, united, reformed, salvation,
         orthodox, buddhist, hindu, jewish, muslim, sikh,
         west, atlantic, quebec, ontario, quartile1, quartile2, quartile3, quartile4,catholic_pct, protestant_pct, orthodox_pct,
         protestant4,protestant3,protestant2,protestant1,catholic4,catholic3,catholic2,catholic1,orthodox2,orthodox1,
         enviro, religious, rel1, rel2, abortion, newerlife, life, cannabis, spend_educ, spend_defence, spend_just, spend_imm,
         abor, brit, chin, dutch, eng, fr, frcan, ger, hisp, ind, inuit, irish, ital, metis, pol, que, scot, ukr, text1, text2,
         eth_brit, eth_row, eth_north, eth_south, eth_ctl, eth_east, eth_fr, eth_id, imm)
mini19 <- filter(mini19qc, quebec==0)

sapply(mini04,mean,na.rm=T)
sapply(mini06, function(x) sum(is.na(x)))

ces2019 %>%
  count(cps19_votechoice, floor((cps19_imm_year+1614)/10)) %>%
  spread(key=cps19_votechoice, value=n)

mini06 <- cesprev %>%
  filter(ces06_CPS_S21 %in% c("Other [not coded elsewhere]","Liberal (Grits)","Conservative (Tory, PCs)",
                              "NDP (New Democrats)","Bloc Quebecois (BQ, PQ, Bloc)","Green Party (Greens)"),
         !is.na(ces06_CPS_S9),
         !is.na(ces06_PROVINCE)) %>%
  mutate(libvote = case_when(ces06_CPS_S21=="Liberal (Grits)"~1, TRUE~0), #does it matter if other values match?
         convote = case_when(ces06_CPS_S21=="Conservative (Tory, PCs)"~1, TRUE~0),
         ndpvote = case_when(ces06_CPS_S21=="NDP (New Democrats)"~1, TRUE~0),
         # religion = case_when(ces08_CPS_S9 ==0 ~ 0, #No agnostic option
         #                      ces08_CPS_S9 %in% c(1,2,9,12,13,14,16,18,19) ~ 1,
         #                      TRUE ~ as.double(ces08_CPS_S9)),
         protestant = case_when(ces06_CPS_S9 %in% c("Anglican",
                                                    "Baptist",
                                                    "Lutheran",
                                                    "Pentecostal / Fundamentalist / ...",
                                                    "Presbyterian",
                                                    "Protestant (only after probe)",
                                                    "United Church of Canada",
                                                    "Christian Reform",
                                                    "Salvation Army") ~ 1,
                                TRUE ~ 0),
         catholic = case_when(ces06_CPS_S9=="Catholic/Roman Catholic/RC" ~ 1, TRUE~0), #10
         orthodox = case_when(ces06_CPS_S9=="Greek / Ukrainian / Russian / Eastern Orthodox" ~ 1, TRUE~0), #11
         buddhist = case_when(ces06_CPS_S9=="Buddhist/Buddhism" ~ 1, TRUE~0), #3
         hindu = case_when(ces06_CPS_S9=="Hindu" ~ 1, TRUE~0), #4
         jewish = case_when(ces06_CPS_S9=="Jewish / Judaism / Jewish Orthodox" ~ 1, TRUE~0), #5
         muslim = case_when(ces06_CPS_S9=="Muslim / Islam" ~ 1, TRUE~0), #6
         sikh = case_when(ces06_CPS_S9=="Sikh / Sikhism" ~ 1, TRUE~0), #7
         west = case_when(ces06_PROVINCE %in% c("Manitoba","Sask","Alberta","bc")~1, TRUE~0), 
         atlantic = case_when(ces06_PROVINCE %in% c("Nfld","pei","ns","nb")~1, TRUE~0),
         quebec = case_when(ces06_PROVINCE=="Quebec"~1, TRUE~0),
         ontario = case_when(ces06_PROVINCE=="Ontario"~1, TRUE~0),
         religious = case_when(ces06_CPS_S11 == "Very important" ~1,
                              ces06_CPS_S11 %in% c("Somewhat important","Not very important","Not important at all") ~0,
                              TRUE ~ NA_real_),
         eth_brit = case_when(ces06_CPS_S14%in%c("Canadian","British","English","Irish","Scottish",
                                                 "Anglo Saxon / WASP / Caucasian / White, etc.")~1, TRUE~0), 
         eth_row = case_when(ces06_CPS_S14%in%c("Black/African","Chinese, et. al.","Indian","Japanese","Pakistani",
                                                "Filipino","Welsh")~1, TRUE~0), 
         eth_north = case_when(ces06_CPS_S14%in%c("Danish","Finnish","Norwegian","Swedish")~1, TRUE~0), 
         eth_south = case_when(ces06_CPS_S14%in%c("Italian","Portuguese","Spanish")~1, TRUE~0), 
         eth_ctl = case_when(ces06_CPS_S14%in%c("Dutch","German","Holland")~1, TRUE~0), 
         eth_east = case_when(ces06_CPS_S14%in%c("Greek","Hungarian","Jewish/Hebrew","Polish","Russian",
                                                 "Ukrainian")~1, TRUE~0), 
         eth_fr = case_when(ces06_CPS_S14%in%c("French","Quebecois, Fr. Cdn, Francophone")~1, TRUE~0), 
         imm = case_when(ces06_CPS_S12=="Canada"~0,
                         ces06_CPS_S12=="<NA>"~NA_real_,
                         TRUE~1)) %>%
  filter(quebec==0) %>%
  select(libvote, convote, ndpvote, protestant, catholic, orthodox, buddhist, hindu, jewish, muslim, sikh,
         west, atlantic, quebec, ontario, religious, eth_brit, eth_row, eth_north, eth_south, eth_ctl, eth_east, eth_fr, imm)

mini04 <- cesprev %>%
  filter(ces04_CPS_B4_3 %in% c("Other [specify]","Liberal (Grits)","Conservative (Conservative Party, new ...)",
                               "ndp","Bloc Quebecois","Green Party"),
         ces04_CPS_S9!="<NA>",
         !is.na(ces04_PROVINCE)) %>%
  mutate(libvote = case_when(ces04_CPS_B4_3=="Liberal (Grits)"~1, TRUE~0), #does it matter if other values match?
         convote = case_when(ces04_CPS_B4_3=="Conservative (Conservative Party, new ...)"~1, TRUE~0),
         ndpvote = case_when(ces04_CPS_B4_3=="ndp"~1, TRUE~0),
         # religion = case_when(ces04_CPS_S9 ==0 ~ 0, #No agnostic option
         #                      ces04_CPS_S9 %in% c(1,2,9,12,13,14,16,18,19) ~ 1,
         #                      TRUE ~ as.double(ces04_CPS_S9)),
         protestant = case_when(ces04_CPS_S9 %in% c("Anglican",
                                                    "Baptist",
                                                    "Lutheran",
                                                    "Pentecostal / Fundamentalist / ...",
                                                    "Presbyterian",
                                                    "Protestant (only after probe)",
                                                    "United Church of Canada",
                                                    "Christian Reform",
                                                    "Salvation Army") ~ 1,
                                TRUE ~ 0),
         catholic = case_when(ces04_CPS_S9=="Catholic/Roman Catholic/RC" ~ 1, TRUE~0), #10
         orthodox = case_when(ces04_CPS_S9=="Greek / Ukrainian / Russian / Eastern Orthodox" ~ 1, TRUE~0), #11
         buddhist = case_when(ces04_CPS_S9=="Buddhist/Buddhism" ~ 1, TRUE~0), #3
         hindu = case_when(ces04_CPS_S9=="Hindu" ~ 1, TRUE~0), #4
         jewish = case_when(ces04_CPS_S9=="Jewish / Judaism / Jewish Orthodox" ~ 1, TRUE~0), #5
         muslim = case_when(ces04_CPS_S9=="Muslim / Islam" ~ 1, TRUE~0), #6
         sikh = case_when(ces04_CPS_S9=="Sikh / Sikhism" ~ 1, TRUE~0), #7
         west = case_when(ces04_PROVINCE %in% c("Manitoba","Sask","Alberta","bc")~1, TRUE~0), 
         atlantic = case_when(ces04_PROVINCE %in% c("Nfld","pei","ns","nb")~1, TRUE~0),
         quebec = case_when(ces04_PROVINCE=="Quebec"~1, TRUE~0),
         ontario = case_when(ces04_PROVINCE=="Ontario"~1, TRUE~0),
         religious = case_when(ces04_CPS_S11 == "Very important" ~1,
                               ces04_CPS_S11 %in% c("Somewhat important","Not very important","Not important at all") ~0,
                               TRUE ~ NA_real_),
         eth_brit = case_when(ces04_CPS_S14%in%c("Canadian","British","English","Irish","Scottish",
                                                 "Anglo Saxon / WASP / Caucasian / White, etc.")~1, TRUE~0), 
         eth_row = case_when(ces04_CPS_S14%in%c("Black/African","Chinese, et. al.","Indian","Japanese","Pakistani",
                                                "Filipino","Welsh")~1, TRUE~0), 
         eth_north = case_when(ces04_CPS_S14%in%c("Danish","Finnish","Norwegian","Swedish")~1, TRUE~0), 
         eth_south = case_when(ces04_CPS_S14%in%c("Italian","Portuguese","Spanish")~1, TRUE~0), 
         eth_ctl = case_when(ces04_CPS_S14%in%c("Dutch","German","Holland")~1, TRUE~0), 
         eth_east = case_when(ces04_CPS_S14%in%c("Greek","Hungarian","Jewish/Hebrew","Polish","Russian",
                                                 "Ukrainian")~1, TRUE~0), 
         eth_fr = case_when(ces04_CPS_S14%in%c("French","Quebecois, Fr. Cdn, Francophone")~1, TRUE~0), 
         imm = case_when(ces04_CPS_S12=="Canada"~0,
                         ces04_CPS_S12=="<NA>"~NA_real_,
                         TRUE~1)) %>%
  filter(quebec==0) %>%
  select(libvote, convote, ndpvote, protestant, catholic, orthodox, buddhist, hindu, jewish, muslim, sikh,
         west, atlantic, quebec, ontario, religious, eth_brit, eth_row, eth_north, eth_south, eth_ctl, eth_east, eth_fr, imm)

miniprev <- bind_rows(mini04,mini06)

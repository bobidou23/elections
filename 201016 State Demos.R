##Data from https://data.census.gov/cedsci/table?q=Educational%20Attainment%20in%20the%20United%20States&t=Educational%20Attainment&g=0100000US.04000.001&tid=ACSST1Y2018.S1501&moe=false&tp=false&hidePreview=true

library(pacman)
p_load(tidyverse,magrittr,plotly)

colnames(edu_data) <- edu_data[1,]
edu_data <- edu_data[-1,]

str_sub(colnames(edu_data)[str_sub(colnames(edu_data),0,79)=="Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!"],80)

edu_mini <- select(edu_data, id, "Geographic Area Name",
       "Estimate!!Total!!Population 25 years and over",
       "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!Black alone",
       "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!White alone, not Hispanic or Latino",
       "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!White alone, not Hispanic or Latino!!Bachelor's degree or higher") %>%
  rename(state = "Geographic Area Name",
         totalpop = "Estimate!!Total!!Population 25 years and over",
         black = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!Black alone",
         white_all = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!White alone, not Hispanic or Latino",
         white_college = "Estimate!!Total!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!White alone, not Hispanic or Latino!!Bachelor's degree or higher") %>%
  mutate(totalpop = as.numeric(totalpop),
         white_all = as.numeric(white_all),
         white_college = as.numeric(white_college),
         black = as.numeric(black),
         white_prop = white_all / totalpop,
         white_noncoll = white_all - white_college,
         white_nc_prop = white_noncoll / totalpop,
         white_col_prop = white_college / totalpop,
         black_prop = black / totalpop)

edu_mini %>%
  select(state, white_nc_prop) %>%
  View()

edu_mini %>%
  filter(state != "Puerto Rico") %>%
  summarise(a=sum(totalpop),
            b=sum(white_noncoll),
            prop = b/a)


########

state_demos <- read.csv("state_demos.csv")
# glimpse(state_demos)
state_demos <- state_demos %>%
  inner_join(select(edu_mini,state,white_nc_prop,white_col_prop,black_prop),"state") %>%
  mutate(demo_change = white2010-white1990,
         dem_change = -2.9+biden-gore,
         white_nc_prop = white_nc_prop*100,
         white_col_prop = white_col_prop*100,
         black_prop = black_prop*100) %>%
  glimpse()

#Model-Making and -Visualizing
lm(state_demos$biden ~ state_demos$white_nc_prop + state_demos$college) %>%
  summary()

state_demos2 <- state_demos %>%
  mutate(fitted = 26.474-0.314*white_nc_prop + 1.202*college,
         residual = biden-fitted) %>%
  arrange(residual)
state_demos2 %>%
  mutate(state = factor(state, levels=state_demos2$state)) %>%
  ggplot(aes(x=residual,y=state, fill=biden)) +
  geom_col() +
  scale_fill_distiller(palette="RdBu", direction=1, limits=c(24,76))

lm(state_demos$dem_change ~ state_demos$college + state_demos$demo_change + state_demos$white2010) %>%
  summary()

{ggplot(state_demos2, aes(x=black_prop, y=residual, group=state)) +
  geom_point()} %>%
  ggplotly()

#Test
state_demos %>%
    filter(state!="United States",
           state!="District of Columbia") %>%
    mutate(biden = biden/(biden + trump)*100) %>%
    ggplot(aes(x=wwc2018,y=biden,size=ev,label=abbr),alpha=0.5) +
    geom_point(aes(color=biden)) +
    geom_text() +
    scale_color_distiller(palette="RdBu", direction=1, limits=c(24,76)) +
    theme_bw()

{state_demos %>%
    filter(state!="United States",
           state!="District of Columbia") %>%
    mutate(clinton = clinton/(clinton + trump)*100) %>%
    ggplot(aes(x=college,y=demo_change,label=abbr),alpha=0.5) +
    geom_point(aes(color=clinton,size=ev*2)) +
    geom_text(aes(size=ev)) +
    scale_color_distiller(palette="RdBu", direction=1, limits=c(24,76)) +
    # scale_y_log10() +
    scale_size(range=c(3, 10)) +
    theme_bw()} %>%
  ggplotly()


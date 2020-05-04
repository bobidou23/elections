library(pacman)
p_load(tidyverse,stringr,plotly,magrittr,gganimate)

# monthly_stats %>%
#   ggplot(aes(x=c(0),y=perclate,colour=departure_station)) + 
#   geom_point(aes(alpha=I(0.2),size=total_num_trips)) +
#   geom_hline(aes(yintercept = monthlylate,colour=departure_station,size=3)) + 
#   facet_grid(departure_station~.) +
#   coord_flip() + 
#   transition_states(yearmonth) +
#   theme(axis.text.y = element_blank(), 
#         axis.ticks = element_blank(),
#         legend.position = "none", 
#         strip.text.y = element_text(angle = 0)) +
#   labs(title = "Lateness % in the 15 busiest stations in France (month: {closest_state})") +
#   ylab("Percentage of departing trains that are late") +
#   xlab("")

dem_demos <- read_csv("dem_demos_2020.csv")
names(dem_demos) <- c("month","candidate","general","men","women","gender",
                      "nonwhite","white","race","less45","older45","age",
                      "noncollege","college","education","liberal","moderate","ideology")
dem_demos <- dem_demos %>%
  mutate(gender = case_when(men>women ~ -men/women+1, TRUE ~ women/men-1),
         race = case_when(nonwhite>white ~ -nonwhite/white+1, TRUE ~ white/nonwhite-1),
         age = case_when(less45>older45 ~ -less45/older45+1, TRUE ~ older45/less45-1),
         education = case_when(noncollege>college ~ -noncollege/college+1, TRUE ~ college/noncollege-1),
         ideology = case_when(liberal>moderate ~ -liberal/moderate+1, TRUE ~ moderate/liberal-1))

dem_demos2 <- dem_demos %>%
  mutate(gender = case_when(women>men ~ men/women-1, TRUE ~ 1-women/men),
         race = case_when(white>nonwhite ~ nonwhite/white-1, TRUE ~ 1-white/nonwhite),
         age = case_when(less45>older45 ~ older45/less45-1, TRUE ~ 1-less45/older45),
         education = case_when(college>noncollege ~ noncollege/college-1, TRUE ~ 1-college/noncollege),
         ideology = case_when(liberal>moderate ~ moderate/liberal-1, TRUE ~ 1-liberal/moderate))

p_load(gifski, png)
dem_demos %>%
    ggplot(aes(x=ideology, y=education)) +
    geom_point(aes(size=general, color=candidate),alpha=0.3) +
    # geom_text(aes(label=candidate), family="Fira Sans") +
    scale_size(range = c(0, 60)) +
    scale_colour_manual(values=dem_cols) +
    theme(text = element_text(family = "Fira Sans"),
          legend.position = "none") +
    labs(title = "Relative support bases for each candidate (month: {closest_state})")
    # transition_states(month, state_length = 10, transition_length = 0)} %>% animate(end_pause = 20)

filter(dem_demos2,month<202002) %>% 
  select(month,candidate,general,gender,race,age,education,ideology) %>%
  gather(-month,-candidate,-general,key=demo,value=value) %>%
  ggplot(aes(x=c(0), y=value)) +
  geom_hline(yintercept=0) +
  geom_point(aes(size=general, color=candidate, alpha=factor(month),group=candidate),position=position_dodge(width=1)) +
  coord_flip() +
  scale_size_area(max_size=10) +
  scale_colour_manual(values=dem_cols) +
  facet_wrap(vars(demo), labeller = as_labeller(dem_facets)) +
  labs(title = "If Twitter Isn't Real Life, What Is?",
       subtitle = "Relative support bases for each candidate, according to CNN poll crosstabs") +
  theme(text = element_text(family = "Fira Sans"),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        strip.text.y = element_text(angle = 0))
  # transition_states(month, transition_length = 0)} %>% animate(end_pause = 20)

for(cand in c("Biden","Buttigieg","Harris","Warren","Sanders")) {
  for(demo0 in c("gender","race","age","education","ideology")) {
    print(paste(cand,demo0))
    select(dem_demos2,month,candidate,general,gender,race,age,education,ideology) %>%
      gather(-month,-candidate,-general,key=demo,value=value) %>%
      filter(candidate == cand, demo == demo0, month < 202002) %>%
      select(month, value) %>%
      mutate(month = rank(month)) %$%
      lm(value ~ month) %$%
      summary(.)$coef[2,] %>%
      print()
  }
  }

dem_facets <- c(age = "Young ~ Old",
                education = "College ~ Non-College",
                gender = "Women ~ Men",
                ideology = "Liberal ~ Moderate",
                race = "White ~ Nonwhite")
dem_cols <- c("Sanders"="#2C9FD8", "Biden"="#243E95",
              "Harris"="#4F4196", "O'Rourke"="black",
              "Warren"="#6CC69C", "Buttigieg"="#F1B940",
              "Bloomberg"="#1C355E")

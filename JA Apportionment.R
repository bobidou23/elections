library(pacman)
p_load(tidyverse,readxl)

temp <- tempfile()
download.file(destfile=temp,url="https://www.stat.go.jp/data/jinsui/2021np/zuhyou/05k2021-2.xlsx")
excel_sheets(temp)
ja_pop <- read_excel(temp) %>%
  select(pref=`...3`,pop=`...9`) %>%
  filter(row_number()>11 & row_number()<59) %>%
  mutate(pop = as.numeric(pop)*1000)

p_load(rvest)

read_html("https://uub.jp/pjn/pb.html") %>%
  html_nodes(xpath="/html/body/div[2]/div[6]/div/div/table") %>% html_table()

ja_pops <- read_html("https://uub.jp/pjn/pb.html") %>%
  html_elements("table") %>% html_table() %>%
  data.frame()
colnames(ja_pops) <- ja_pops[1,]
ja_pops <- ja_pops[2:48,]
colnames(ja_pops) <- c("都道府県","とどうふけん","都道府県庁所在地","とどうふけんちょうしょざいち","人口","面積","人口密度")
ja_pops <- ja_pops %>%
  mutate(pop=as.numeric(gsub(",","",pop)),
         area=as.numeric(gsub(",","",area)),
         de=pop/area)
colnames(ja_pops) <- c("pref","pref_kana","capital","capital_kana","pop","area","density")


ja_apport <- ja_pops %>%
  select(pref,pop) %>%
  mutate(current = case_when(pref=="東京都" ~ 6,
                             pref%in%c("埼玉県","神奈川県","愛知県","大阪府") ~ 4,
                             pref%in%c("北海道","千葉県","兵庫県","福岡県") ~ 3,
                             pref%in%c("茨城県","静岡県","京都府","広島県") ~ 2,
                             pref%in%c("鳥取県","島根県","徳島県","高知県") ~ 0.5,
                             TRUE ~ 1))

teisu <- 75
ja_apport %>%
  mutate(quota = teisu*pop/sum(pop),
         remainder = quota-floor(quota),
         largrem = case_when(remainder>=sort(remainder, decreasing=T)[teisu-sum(floor(quota))] ~ ceiling(quota),
                             TRUE ~ floor(quota)),
         sqpop = sqrt(pop),
         sqquota = teisu*sqpop/sum(sqpop),
         sqremainder = sqquota - floor(sqquota),
         sqlargrem = case_when(sqremainder>=sort(sqremainder, decreasing=T)[teisu-sum(floor(sqquota))] ~ ceiling(sqquota),
                               TRUE ~ floor(sqquota))) %>%
  View()

ja_apport <- ja_apport %>%
  mutate(quota = teisu*pop/sum(pop),
         first = ceiling(quota))

while()
         

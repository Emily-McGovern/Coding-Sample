##----------------------------------------------------------------------------------------##
##Author:   Emily McGovern
##Date:     June 2022
##Task:     Data manipulation for recruitment process
##Data Source: https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv
##--------------------------------------------------------------------------------------------##

#Data setup
require(pacman)
pacman::p_load(tidyverse,
               countrycode,
               janitor,
               lubridate,
               zoo,
               here)

#source required functions
list.files(here::here("R"), full.names = T) %>% 
  map(~source(.))


#make output dir
out.dir<-format(Sys.Date(), "%d-%B-%y")

#check if directory exists - if not create daily directory 
if(!dir.exists(paste0(here("out/Monkey-Pox/"), out.dir))) {dir.create(paste0(here("out/Monkey-Pox/"), out.dir))}

#retrive latest Monkey Pox data
mp.data <- read_csv("https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv")

#save daily data as record

write.csv(mp.data, file=gzfile(paste0(here("out/Monkey-Pox/"), out.dir, "/mpdata-", out.dir, ".csv.gz")))


#quick look at the data structure and fields
dplyr::glimpse(mp.data)

#look at MP data in more detail
summary(mp.data)

#create new required variables and tidy data
mp.cases<-mp.data %>%
  tidyr::replace_na(list(Gender="unknown")) %>%
  dplyr::mutate(region= countrycode(sourcevar = Country_ISO3, origin = "iso3c",destination = "region"),
                Gender = factor(Gender, levels=c("female", "male", "unknown"))) %>%  #add variable for region which corresponds with country
  dplyr::filter(Status =="confirmed") %>% #only interested in confirmed case
  dplyr::group_by(region, Country, Date_confirmation, Gender ) %>%
  dplyr::count(name = "cases") %>%
  dplyr::arrange(Gender) %>%
  tidyr::pivot_wider(names_from = Gender, values_from= cases, values_fill = 0) %>%
  janitor::adorn_totals(c("col", "row")) %>%
  dplyr::arrange(desc(Date_confirmation), desc(Total), Country)


write_csv(mp.cases, file=(paste0(here("out/Monkey-Pox/"), out.dir, "/mpcases_summary-", out.dir, ".csv")))

top.countries.per.reg<-mp.cases %>% 
  ungroup() %>% 
  slice(1:n()-1) %>% 
  group_by(region,Country) %>% 
  summarise("total_cases"=sum(Total)) %>% 
  arrange(desc(total_cases)) %>% 
  slice(1:10)

write_csv(top.countries.per.reg, file=(paste0(here("out/Monkey-Pox/"), out.dir, "/mpcases-top-ten-cases-by-country-region", out.dir, ".csv")))

# initial plot
p <-
  mp.cases %>% 
  slice(1:n()-1) %>%
  dplyr::mutate(Date_confirmation = ymd(Date_confirmation),
                top_10 = ifelse(Country %in% top.countries.per.reg$Country, Country, "Other")) %>% 
  ggplot(aes(x = Date_confirmation, y = Total, fill = top_10)) +
  geom_bar(position='stack', 
           stat='identity') +
  theme(legend.position="none")+
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day",
               date_labels = "%d %b")+
  scale_y_continuous(limits=c(0,round(max(mp.cases$Total), digits = -1)), 
                     breaks=seq(0,round(max(mp.cases$Total), digits = -1), by = 25))+ 
  coord_flip()+
  facet_wrap(vars(region))


#quick peak 
p <-
  p +
  labs(title = 'MONKEY POX OUTBREAK', 
       subtitle = paste0(format(min(mp.cases$Date_confirmation), "%b %d %Y"), " to ", format(get_last_weekdate(), "%b %d %Y")), 
       caption = paste0("DATA SOURCE: Global.health Monkeypox (accessed on ",  format(Sys.Date(), "%Y-%m-%d"), ")"), 
       x = '', 
       y = 'Daily Case Count', 
       fill = '')

# Quick Peek
p

# Theme adjustments
p2<-plot_theme(p)


# Save plot
ggsave(filename = (paste0(here("out/Monkey-Pox/"), out.dir, "/mpplot-", out.dir, ".png")), 
       width = 40, height = 20, dpi = 320, units = "cm")


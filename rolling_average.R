##----------------------------------------------------------------------------------------##
##Author:   Emily McGovern
##Date:     June 2022
##Task:     Statisical task for recruitment process
##Data Source: https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv
##--------------------------------------------------------------------------------------------##
#Data setup
require(pacman)
pacman::p_load(tidyverse,
               lubridate,
               zoo,
               here)

#source required functions
list.files(here::here("R"), full.names = T) %>% 
  purrr::map(~source(.))


#make output dir
out.dir<-base::format(Sys.Date(), "%d-%B-%y")

#check if directory exists - if not create daily directory 
if(!dir.exists(paste0(here("out/Monkey-Pox/"), out.dir))) {dir.create(paste0(here("out/Monkey-Pox/"), out.dir))}

#retrive latest Monkey Pox data
mp.data <- readr::read_csv("https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv")

# Dervive rolling average 
roll.mp.case<-mp.data %>% 
  dplyr::filter(Status =="confirmed") %>%  #use only confirmed cases
  dplyr::group_by(Date_confirmation) %>% #group by day and count cases
  dplyr::count(Status) %>% 
  dplyr::ungroup() %>% 
  tidyr::complete(Date_confirmation = seq(min(Date_confirmation, na.rm = T),max(Date_confirmation, na.rm = T),1), 
           fill = list(n = 0)) %>% #add missing dates 
  dplyr::mutate(roll = zoo::rollmean(n, k = 7, fill = NA, align = "right"),
                day = lubridate::wday(Date_confirmation),
                weekday= base::ifelse(day %in% c(1,7), "Weekend", "Weekday")) #calculate rolling average - average for day is calaculate based on cases for previous 7 days

# initial plot of rolling average
 p<-roll.mp.case %>% 
  ggplot2::ggplot(aes(x = Date_confirmation, 
                      y = n)) +
  ggplot2::geom_bar(position='stack', 
                    stat='identity',
                    aes(x=Date_confirmation, y=n, fill=weekday), 
                    width=0.7)+
  ggplot2::geom_line(data= roll.mp.case,
            aes(x=Date_confirmation, 
                y=roll),
            stat='identity',
            color="red",
            size=1)+
  ggplot2::scale_x_date(date_breaks = "1 week", 
                        date_minor_breaks = "1 day",
                        date_labels = "%d %b")

# add title and caption text etc
p<-p +
  ggplot2::labs(title = "Daily Confirmed Monkey Pox Cases with moving 7-day average", 
                subtitle = paste0(format(min(roll.mp.case$Date_confirmation), "%b %d %Y"), " to ", format(get_last_weekdate(), "%b %d %Y")), 
                caption = paste0("DATA SOURCE: Global.health Monkeypox (accessed on ",  format(Sys.Date(), "%Y-%m-%d"), ")"), 
                y = "Case Count", 
                x = "Date of Confirmation",
                fill = "") 

#add theme
p2<-plot_theme(p)


# Save plot
ggplot2::ggsave(filename = (paste0(here("out/Monkey-Pox/"), out.dir, "/mpplot-rolling_mean", out.dir, ".png")), 
                width = 40, height = 20, dpi = 320, units = "cm")



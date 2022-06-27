##--------------------------------------------------------------------------------------------##
## Author:   Emily McGovern
## Date:     June 2022
## Task:     Data manipulation for recruitment process
## Data Source: https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv
##--------------------------------------------------------------------------------------------##

# Data setup
require(pacman)
pacman::p_load(tidyverse,
               countrycode,
               janitor,
               lubridate,
               zoo,
               here)

# source required functions
base::list.files(here::here("R"), full.names = T) %>%
  purrr::map( ~ source(.))


# make output dir
dir.name <- base::format(Sys.Date(), "%d-%B-%y")
out.dir <- paste0(here("out/Monkey-Pox/"), dir.name)

# check if directory exists - if not create daily directory
if (!base::dir.exists(out.dir)) {
  base::dir.create(out.dir)
}

#retrive latest Monkey Pox data
mp.data <-
  readr::read_csv("https://raw.githubusercontent.com/globaldothealth/monkeypox/main/latest.csv")

# save daily data as record
utils::write.csv(mp.data, file = gzfile(paste0(out.dir, "/mpdata-", dir.name, ".csv.gz")))


# quick look at the data structure and fields
dplyr::glimpse(mp.data)

# look at MP data in more detail
base::summary(mp.data)

# create new required variables and tidy data
mp.cases <-
  mp.data %>%
  tidyr::replace_na(list(Gender = "unknown")) %>%
  dplyr::mutate(
    region = countrycode::countrycode(
      sourcevar = Country_ISO3,
      origin = "iso3c",
      destination = "region"
    ),
    Gender = base::factor(Gender, levels = c("female", "male", "unknown"))
  ) %>%  #add variable for region which corresponds with country
  dplyr::filter(Status == "confirmed") %>% #only interested in confirmed case
  dplyr::group_by(region, Country, Date_confirmation, Gender) %>%
  dplyr::count(name = "cases") %>%
  dplyr::arrange(Gender) %>%
  tidyr::pivot_wider(names_from = Gender,
                     values_from = cases,
                     values_fill = 0) %>%
  janitor::adorn_totals(c("col", "row")) %>%
  dplyr::arrange(desc(Date_confirmation), desc(Total), Country)

# save summary of cases by country and gender
readr::write_csv(mp.cases, file = (paste0(
  out.dir, "/mpcases_summary-", dir.name, ".csv"
)))

# get top ten highest cases by country and region for plot
top.countries.per.reg <-
  mp.cases %>%
  dplyr::ungroup() %>%
  dplyr::slice(1:n() - 1) %>%
  dplyr::group_by(region, Country) %>%
  dplyr::summarise("total_cases" = sum(Total)) %>%
  dplyr::arrange(desc(total_cases)) %>%
  dplyr::slice(1:10)

# write top ten highest cases by country and region for plot
readr::write_csv(top.countries.per.reg, file = (
  paste0(
    out.dir,
    "/mpcases-top-ten-cases-by-country-region",
    dir.name,
    ".csv"
  )
))

# manipulate data for plot
mp.cases.plot <-
  mp.cases %>%
  dplyr::slice(1:n() - 1) %>%
  dplyr::mutate(
    Date_confirmation = ymd(Date_confirmation),
    top_10 = ifelse(Country %in% top.countries.per.reg$Country, Country, "Other")
  )

# initial plot
p <-
  mp.cases.plot %>%
  ggplot2::ggplot(aes(x = Date_confirmation, y = Total, fill = top_10)) +
  ggplot2::geom_bar(position = 'stack',
                    stat = 'identity') +
  ggplot2::theme(legend.position = "none") +
  ggplot2::scale_x_date(
    date_breaks = "1 week",
    date_minor_breaks = "1 day",
    date_labels = "%d %b"
  ) +
  ggplot2::scale_y_continuous(limits = c(0, round(max(
    mp.cases.plot$Total
  ), digits = -1)),
  breaks = seq(0, round(max(
    mp.cases.plot$Total
  ), digits = -1), by = 25)) +
  ggplot2::coord_flip() +
  ggplot2::facet_wrap(vars(region))


# add title and caption text etc
p <-
  p +
  labs(
    title = 'MONKEY POX OUTBREAK',
    subtitle = paste0(
      format(min(mp.cases$Date_confirmation), "%b %d %Y"),
      " to ",
      format(get_last_weekdate(), "%b %d %Y")
    ),
    caption = paste0(
      "DATA SOURCE: Global.health Monkeypox (accessed on ",
      format(Sys.Date(), "%Y-%m-%d"),
      ")"
    ),
    x = '',
    y = 'Daily Case Count',
    fill = ''
  )

# quick Peek
p

# theme adjustments
p <- plot_theme(p)


# save plot
ggsave(
  filename = (paste0(out.dir, "/mpplot-", dir.name, ".png")),
  width = 40,
  height = 20,
  dpi = 320,
  units = "cm"
)

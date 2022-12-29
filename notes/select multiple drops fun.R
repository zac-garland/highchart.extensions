devtools::load_all()
options("highcharter.theme" = hc_theme_hcrt())
library(fpp3)

employment_example <- us_employment %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  filter(str_detect(title,"Total")) %>%
  # filter(!str_detect(title,":")) %>%
  rename(value = employed) %>%
  mutate(month = lubridate::as_date(month)) %>%
  group_by(title) %>%
  mutate(yoy = value/lag(value,12)-1,
         `Three year comp` = value/lag(value,36)-1) %>%
  gather(key,value,value:`Three year comp`) %>%
  mutate(month = datetime_to_timestamp(month)) %>%
  highcharter::hchart("line", highcharter::hcaes(month, value, name = title)) %>%
  hc_xAxis(type = "datetime") %>%
  add_multi_drop(
    selectors = c("title", "key"),
    selected = c("Total Private", "Three year comp")
  )


employment_example

gapminder::gapminder %>%
  gather(key,value,lifeExp:pop) %>%
  hchart("scatter",hcaes(gdpPercap,value,group = continent,name = country)) %>%
  # hc_add_theme(hc_theme_hcrt()) %>%
  add_multi_drop(
    c("year","key")
  )



gapminder::gapminder %>%
  mutate(iso_2 = countrycode::countrycode(country, "country.name", "iso2c")) %>%
  filter(country != "Kuwait") %>%
  gather(key,value,lifeExp:gdpPercap) %>%
  hcmap(
    map = "custom/world",
    download_map_data = TRUE,
    joinBy = c("iso-a2", "iso_2"),
    name = "",
    value = "value"
  ) %>%
  add_multi_drop(
    c("year","key"),
    c("2002","lifeExp")
  )

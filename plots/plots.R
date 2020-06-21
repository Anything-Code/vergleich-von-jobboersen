library(pacman)

pacman::p_load(
  tidyverse,
  ggthemes,
  ggrepel
)

blue <- '#1f78b4'

jobsites_old_DE <- read_delim("data/jobsites_old_DE.csv", ",", trim_ws = TRUE)
jobsites_2004_DE <- read_delim("data/jobsites_2004_DE.csv", ",", trim_ws = TRUE)
jobsites_2019_DE <- read_delim("data/jobsites_2019_DE.csv", ",", trim_ws = TRUE)
jobsites_old_GL <- read_delim("data/jobsites_old_GL.csv", ",", trim_ws = TRUE)
jobsites_2004_GL <- read_delim("data/jobsites_2004_GL.csv", ",", trim_ws = TRUE)
jobsites_2019_GL <- read_delim("data/jobsites_2019_GL.csv", ",", trim_ws = TRUE)

myTheme <- function () {
  theme_fivethirtyeight() +
    theme(axis.title = element_text())
}

jobsites_old_DE %>% 
  mutate(monat = parse_date(monat, format = '%Y-%m')) %>% 
  mutate(`Bundesagentur für Arbeit` = bfa) %>% 
  select(-bfa) %>% 
  gather('key', 'value', indeed, xing, `Bundesagentur für Arbeit`, stepStone, linkedIn) %>% 
  ggplot(aes(monat, value, color = key, group = key)) +
  geom_line(size = 1) +
  scale_color_discrete(
    name = 'Jobportal',
    limits = c(
      'xing',
      'stepStone',
      'linkedIn',
      'indeed',
      'Bundesagentur für Arbeit'
    )
  ) + 
  myTheme() +
  labs(
    title = 'Vergleich der zu betrachtenden Recruiting-Plattformen in Deutschland',
    subtitle = 'Werte sind relativ zum maximalen aufgezeichneten Wert angegeben',
    caption = 'Quelle: Google Trends',
    x = 'Jahr'
  ) + 
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

jobsites_old_GL %>% 
  mutate(monat = parse_date(monat, format = '%Y-%m')) %>% 
  mutate(`Bundesagentur für Arbeit` = bfa) %>% 
  select(-bfa) %>% 
  gather('key', 'value', indeed, xing, `Bundesagentur für Arbeit`, stepStone, linkedIn) %>% 
  ggplot(aes(monat, value, color = key, group = key)) +
  geom_line(size = 1) +
  scale_color_discrete(
    name = 'Jobportal',
    limits = c(
      'xing',
      'stepStone',
      'linkedIn',
      'indeed',
      'Bundesagentur für Arbeit'
    )
  ) + 
  myTheme() +
  labs(
    title = 'Vergleich der zu betrachtenden Recruiting-Plattformen weltweit',
    subtitle = 'Angaben sind relativ zum maximalen aufgezeichneten Wert angegeben',
    caption = 'Quelle: Google Trends',
    x = 'Jahr'
  ) + 
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

jobsites_2004_DE %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(year = '2004') %>% 
  rbind(jobsites_2019_DE %>% mutate(year = '2019')) %>% 
  mutate(`Bundesagentur für Arbeit` = bfa) %>% 
  select(-bfa) %>% 
  gather('key', 'value', indeed, xing, `Bundesagentur für Arbeit`, stepStone, linkedIn) %>% 
  ggplot(aes(region, value, fill = key)) +
  geom_col() +
  scale_fill_discrete(
    name = 'Jobportal',
    limits = c(
      'xing',
      'stepStone',
      'linkedIn',
      'indeed',
      'Bundesagentur für Arbeit'
    )
  ) + 
  myTheme() +
  facet_wrap(~year) +
  coord_flip() +
  labs(
    title = 'Vergleich zu betrachtenden Recruiting-Plattformen im Jahre 2004 und 2019',
    subtitle = ' bezogen auf Google Suchen in Deutschland',
    caption = 'Quelle: Google Trends',
    x = 'Bundesland'
  ) + 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


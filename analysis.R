library(ggplot2)
library(dplyr)
library(ggthemes)
library(lubridate)

data_file <- 'bereinigt.csv'
questionary <- read.csv(data_file)

questionary %>%
  group_by(Geschlecht) %>%
  tally() %>%
  mutate(percent = round(100 * (n / sum(n)), 1))

questionary %>%
  ggplot(aes(x = Alter, fill = Geschlecht)) +
  geom_histogram(bins = 10)

# linux was released in 1991, questionary was 2019
# adding a year for really early birds
max_usage_years <- 2019 - 1991 + 1
questionary %>%
  filter(Dauer.der.Nutzung.von.Linux < max_usage_years) %>%
  ggplot(aes(x = Dauer.der.Nutzung.von.Linux, fill = Altersgruppe)) +
  geom_histogram(bins = 10)


# Private Nutzung
questionary %>%
  separate_rows(col = Private.Nutzung, sep = ";") %>%
  mutate(Private.Nutzung = ifelse(Private.Nutzung == "", "keine Angabe", Private.Nutzung)) %>%
  group_by(Private.Nutzung) %>%
  tally() %>%
  ggplot(aes(x = reorder(Private.Nutzung, n), y = n)) +
  geom_col(fill = "orange") +
  xlab("") +
  coord_flip() +
  theme_tufte() +
  ggtitle("Private Nutzung")

# Berufliche Nutzung
questionary %>%
  separate_rows(col = Berufliche.Nutzung, sep = ";") %>%
  mutate(Berufliche.Nutzung = ifelse(Berufliche.Nutzung == "", "keine Angabe", Berufliche.Nutzung)) %>%
  group_by(Berufliche.Nutzung) %>%
  tally() %>%
  ggplot(aes(x = reorder(Berufliche.Nutzung, n), y = n)) +
  geom_col(fill = "orange") +
  xlab("") +
  coord_flip() +
  theme_tufte() +
  ggtitle("Berufliche Nutzung")
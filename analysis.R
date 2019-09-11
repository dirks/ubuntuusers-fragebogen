library(ggplot2)
library(dplyr)
library(ggthemes)
library(lubridate)
library(tidyr)

data_file <- 'bereinigt.csv'
questionary <- read.csv(data_file)

plot_multicolumn <- function(variable, title) {
  variable <- enquo(variable)

  questionary %>%
    separate_rows(col = !! variable, sep = ";") %>%
    mutate(variable = ifelse(!! variable == "", "keine Angabe", !! variable)) %>%
    group_by(variable) %>%
    tally() %>%
    ggplot(aes(x = reorder(variable, n), y = n)) +
    geom_col(fill = "orange") +
    xlab("") +
    coord_flip() +
    theme_tufte() +
    ggtitle(title)
}

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

plot_multicolumn(Private.Nutzung, "Private Nutzung")
plot_multicolumn(Berufliche.Nutzung, "Berufliche Nutzung")
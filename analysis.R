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
    mutate(!! variable := ifelse(!! variable == "", "keine Angabe", !! variable)) %>%
    group_by(!!variable) %>%
    tally() %>%
    ggplot(aes(x = reorder(!! variable, n), y = n)) +
    geom_col(fill = "orange") +
    xlab("") +
    coord_flip() +
    theme_tufte() +
    ggtitle(title)
}

frequency_table <- function(variable) {
  variable <- enquo(variable)

    questionary %>%
    group_by(!! variable) %>%
    tally() %>%
    mutate(percent = round(100 * (n / sum(n)), 1))
}

# Alter
questionary %>%
  ggplot(aes(x = Alter, fill = Geschlecht)) +
  geom_histogram(bins = 10) +
  theme_tufte()

# Nutzungsdauer
# linux was released in 1991, questionary was 2019
# adding a year for really early birds
max_usage_years <- 2019 - 1991 + 1
questionary %>%
  filter(Dauer.der.Nutzung.von.Linux < max_usage_years) %>%
  ggplot(aes(x = Dauer.der.Nutzung.von.Linux, fill = Altersgruppe)) +
  geom_histogram(bins = 10) +
  theme_tufte()

# one selection per question
frequency_table(Geschlecht)
frequency_table(Grund.für.Anmeldung)
frequency_table(Worüber.erfolgte.der.Einstieg.bei.ubuntuusers.de.)
frequency_table(Wie.bewertest.du.den.optischen.Auftritt.von.ubuntuusers.de.)
frequency_table(Wie.bewertest.du.den.Funktionsumfang.und.die.Bedienung.der.Plattform.)
frequency_table(Wie.bewertest.du.Entscheidungen.des.Teams..z..B..Moderationsentscheidungen.oder.Anforderungen.des.Wikiteams..)
frequency_table(Wie.finanziert.sich.ubuntuusers.de.deiner.Meinung.nach.)
frequency_table(Welche.Verbindung.hat.ubuntuusers.de.zu.Canonical.)
frequency_table(Wer.ist.im.Team.von.ubuntuusers.de.)

# multiple selections per question
plot_multicolumn(Private.Nutzung, "Private Nutzung")
plot_multicolumn(Berufliche.Nutzung, "Berufliche Nutzung")
plot_multicolumn(Welche.Bereiche.von.ubuntuusers.de.nutzt.du.sehr.oft., "Welche Bereiche von ubuntuusers.de nutzt du sehr oft?")
plot_multicolumn(Welche.Bereiche.von.ubuntuusers.de.nutzt.du.gelegentlich., "Welche Bereiche von ubuntuusers.de nutzt du gelegentlich?")
plot_multicolumn(Welche.Bereiche.von.ubuntuusers.de.nutzt.du.nie., "Welche Bereiche von ubuntuusers.de nutzt du nie?")
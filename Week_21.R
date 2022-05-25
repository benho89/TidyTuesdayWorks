library(tidyverse)
library(tidytuesdayR)
library(ggoxford)
library(ggtext)

tues_dat <- tt_load(2022, week = 21)

sevens <- tues_dat$sevens

win_pct_sevens <- sevens %>%
  group_by(team_1) %>%
  summarise(n_wins = sum(winner == team_1),
            n_losses = sum(loser == team_1),
            n_draws = sum(winner == "Draw"),
            n_games = n_wins + n_losses + n_draws,
            win_pct = sum(n_wins) / n_games) %>%
  arrange(-n_games) %>%
  filter(n_games > 100) %>%
  mutate_at("win_pct", round, 2) %>%
  mutate(iso3 = countrycode::countrycode(team_1, origin = "country.name.en",
                                         destination = "iso3c"),
         iso3 = reorder(iso3, -win_pct)) %>%
  filter(team_1 != "England") %>% # No England flag.
  arrange(-win_pct)

colours <- c("#012169", "#00AEC7", "#012169", "#6CACE4", "#EF3340", "#EF3340",
             "#C8102E", "#EF3340", "#0055A4", "#C8102E", "#0072CE", "#62B5E5",
             "#BF0D3E", "#009639", "#169B62", "#EF3340", "#EF3340", "#DE2408")

win_pct_sevens$colour <- colours

win_pct_sevens <- win_pct_sevens %>%
  mutate(colour = case_when(iso3 == "CAN" ~ "#EF3340",
                            iso3 == "FJI" ~ "#62B5E5",
                            iso3 =="FRA" ~ "#0055A4",
                            iso3 == "RUS" ~ "#0072CE",
                            iso3 == "USA" ~ "#BF0D3E",
                            TRUE ~ as.character(colour)))

ggplot(win_pct_sevens, aes(x = iso3, y = win_pct, label = win_pct)) +
  ggthemes::theme_fivethirtyeight() +
  geom_segment(aes(x = iso3, xend = iso3, y = 0, yend = win_pct,
                   colour = colour), size = 2.5) +
  geom_point(aes(colour = colour), size = 9) +
  geom_text(colour = "white", size = 3) +
  scale_colour_identity() +
  geom_axis_flags(breaks = win_pct_sevens$iso3,
                  labels = win_pct_sevens$iso3,
                  country_icons = win_pct_sevens$iso3,
                  icons_only = TRUE,
                  width = 30,
                  lineheight = 2) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                     labels = scales::number_format(accuracy = 0.01)) +
  labs(title = "*<span style = 'color: red;'>Win Rate</span> For Women's Rugby Sevens Nations: 1997-2022*",
       subtitle = "*Minimum of 100 Matches | All Match Types*",
       y = "Win Rate",
       caption = "*Tidy Tuesday* | **Data:** ScrumQueens | **Plot:** @BenHorsley89") +
  theme(panel.grid.major.x = element_blank(),
        plot.title = element_markdown(face = "bold", size = 16),
        plot.subtitle = element_markdown(size = 12),
        plot.caption = element_markdown(size = 8),
        axis.title.y = element_text(face = "bold")) +
  annotate("curve", x = 7.5, xend = 6.5, y = 0.95, yend = 0.88,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate("text", x = 8, y = 0.95, label = "ENG: 0.88",
           size = 2.5)

ggsave("Week_21_Rugby_Sevens.png", width = 11, height = 10 / 1.7, dpi = 500)

library(xml2)
library(rvest)
library(stringr)
library(ggplot2)
library(dplyr)
library(gganimate)
library(cranlogs)

cran_downloaded_url <- "https://www.r-pkg.org/downloaded"

packages_downloaded <- xml2::read_html(cran_downloaded_url) %>%
  rvest::html_nodes("h3 a strong") %>%
  rvest::html_text() %>%
  stringr::str_remove_all("\n|\t") %>%
  stringr::str_trim() %>%
  cran_downloads(from = "2016-01-01", to = "2021-03-31")

#################################################

anim_plot <- packages_downloaded %>%
  group_by(package) %>%
  mutate(acum = cumsum(downloads)) %>%
  group_by(end) %>%
  mutate(
    rank = rank(-acum),
    max_p = acum / max(acum)
  ) %>%
  group_by(package) %>%
  filter(rank <= 10) %>%
  ggplot(aes(x = rank, y = acum, fill = package)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(y = "Downloads per million") +
  coord_flip(clip = "off", expand = FALSE) +
  geom_text(aes(y = 0, label = paste(package, " ")), vjust = 0.2, hjust = 1, size = 7) +
  geom_text(aes(
    y = acum,
    label = paste0(" ", as.character(round(acum / 1000000, 1))),
    hjust = 0
  ),
  size = 5
  ) +
  scale_x_reverse() +
  theme(
    plot.margin = unit(c(2, 5, 2, 5), "cm"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  transition_states(end, transition_length = 3, state_length = 1) +
  ease_aes("sine-in-out") +
  ggtitle("Accumulated downloads from CRAN packages since 2016",
    subtitle = "{closest_state}"
  ) +
  theme(
    plot.title = element_text(hjust = 0.6, size = 16, colour = "black", vjust = 5),
    plot.subtitle = element_text(hjust = 0.4)
  )

anim_a <- animate(anim_plot, fps = 60, duration = 20)
anim_save("second_saved_animation.gif")



anim_a <- animate(anim_plot, fps = 60, duration = 25, renderer = av_renderer())
anim_save("second_saved_animation.mp4")

##########################################

anim_plot <- packages_downloaded %>%
  group_by(package) %>%
  mutate(acum = cumsum(count)) %>%
  group_by(date) %>%
  mutate(
    rank = rank(-acum),
    max_p = acum / max(acum)
  ) %>%
  group_by(package) %>%
  filter(rank <= 10) %>%
  ggplot(aes(x = rank, y = acum, fill = package)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(y = "Downloads per million") +
  coord_flip(clip = "off", expand = FALSE) +
  geom_text(aes(y = 0, label = paste(package, " ")), vjust = 0.2, hjust = 1, size = 7) +
  geom_text(aes(
    y = acum,
    label = paste0(" ", as.character(round(acum / 1000000, 1))),
    hjust = 0
  ),
  size = 5
  ) +
  scale_x_reverse() +
  theme(
    plot.margin = unit(c(2, 5, 2, 5), "cm"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  transition_states(date, transition_length = 3, state_length = 1) +
  ease_aes("sine-in-out") +
  ggtitle("Accumulated downloads from CRAN packages since 2016",
    subtitle = "{closest_state}"
  ) +
  theme(
    plot.title = element_text(hjust = 0.6, size = 16, colour = "black", vjust = 5),
    plot.subtitle = element_text(hjust = 0.4)
  )


anim_a <- animate(anim_plot, fps = 60, duration = 52, , nframes = 1917, renderer = av_renderer())
anim_save("second_saved_animation.mp4")

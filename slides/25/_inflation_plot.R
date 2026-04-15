library(tidyverse)
library(gganimate)
library(gifski)
library(scales)

flag_colors <- c(
  "USA" = "#aecff7",
  "Turkiye" = "#e57886",
  "Australia" = "#9cffc3",
  "Europe" = "#ffeb9d",
  "Japan" = "#FFFFFF"
)

inflation <- read_csv("slides/25/data/inflation.csv")

inflation_long <- inflation |>
  pivot_longer(
    cols = -year,
    names_to = "country",
    values_to = "cumulative_pct"
  )

inflation_long_with_offsets <- inflation_long |>
  group_by(year) |>
  mutate(
    max_cumulative_pct = max(cumulative_pct),
    rank = rank(cumulative_pct, ties.method = "first"),
    offset = case_when(
      country == "Turkiye" ~ 0,
      rank == 1 ~ 0,
      rank == 2 ~ max_cumulative_pct * 0.04,
      rank == 3 ~ max_cumulative_pct * 0.08,
      rank == 4 ~ max_cumulative_pct * 0.12,
      .default = 0
    ),
    label_y = cumulative_pct + offset
  )

inflation_plot <- inflation_long_with_offsets |>
  ggplot(aes(x = year, y = cumulative_pct, color = country, group = country)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = flag_colors) +
  scale_y_continuous(labels = label_comma(suffix = "%")) +
  scale_x_continuous(breaks = seq(1996, 2024, by = 2)) +
  labs(
    title = "Cumulative inflation since 1996",
    subtitle = "Year: {round(frame_along)}",
    x = "Year",
    y = "Cumulative inflation (%)"
  ) +
  geom_text(
    aes(
      y = label_y,
      label = paste0(country, " ", round(cumulative_pct, 1), "%"),
    ),
    hjust = 0,
    nudge_x = 0.5,
    size = 6
  ) +
  coord_cartesian(
    clip = "off",
    xlim = c(
      min(inflation_long_with_offsets$year),
      max(inflation_long_with_offsets$year) + 5
    )
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.background = element_rect(fill = "black", color = "black"),
    panel.grid.major = element_line(color = "gray30"),
    panel.grid.minor = element_line(color = "gray20"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white"),
    plot.title = element_text(color = "white", face = "bold"),
    plot.subtitle = element_text(color = "gray80"),
    legend.position = "none",
    plot.margin = margin(5, 150, 5, 5)
  ) +
  transition_reveal(year) +
  view_follow(fixed_x = FALSE, fixed_y = FALSE)

animate(
  inflation_plot,
  width = 900,
  height = 560,
  renderer = gifski_renderer()
)

anim_save("slides/25/gifs/inflation_plot.gif")

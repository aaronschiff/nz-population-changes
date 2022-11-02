# Analyse components of changes in NZ regional populations

# Setup
library(tidyverse)
library(here)
library(janitor)
library(as.charts)
library(as.utils)
library(scales)

# Data
dat <- read_csv(
  file = here("data/TABLECODE7510_Data_85a1cf8e-0104-4751-9605-98d8d4504cae.csv"),
  col_types = "cciic"
) |>
  clean_names()

# Regions
dat_reg <- dat |>
  filter(
    !str_detect(string = area, pattern = "Total New Zealand"),
    !str_detect(string = area, pattern = "local board"),
    !str_detect(string = area, pattern = "district"),
    !str_detect(string = area, pattern = "city"),
    !str_detect(string = area, pattern = "Area outside"),
    area != "Chatham Islands territory",
    area != "Auckland"
  ) |>
  filter(
    measure != "Population",
    measure != "Net migration"
  ) |>
  mutate(area = order_regc(area)) |>
  mutate(measure = custom_order(
    x = measure,
    order_info = tibble(
      levels = c(
        "Net international migration",
        "Net internal migration",
        "Natural increase"
      )
    )
  )) |>
  arrange(area, measure, year_at_30_june)

# Region totals (absolute value)
dat_reg_totals <- dat_reg |>
  group_by(area, year_at_30_june) |>
  summarise(total_change = sum(abs(value))) |>
  ungroup()

# Region chart
chart_reg <- dat_reg |>
  left_join(y = dat_reg_totals, by = c("area", "year_at_30_june")) |>
  mutate(pct = value / total_change) |>
  ggplot(mapping = aes(
    x = year_at_30_june,
    y = pct,
    fill = measure
  )) +
  geom_col(
    colour = "white",
    size = 0.25
  ) +
  geom_hline(
    yintercept = 0,
    size = 0.25
  ) +
  facet_wrap(
    facets = vars(area),
    ncol = 4
  ) +
  scale_colour_qual(
    values = c(
      "Net internal migration",
      "Net international migration",
      "Natural increase"
    ),
    value_labels = c(
      "Net internal migration",
      "Net international migration",
      "Natural increase"
    ),
    value_grey = "Natural increase",
    guide = guide_legend(),
    name = NULL
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.5),
    expand = expansion(0, 0)
  ) +
  scale_x_continuous(
    limits = c(2018.5, 2022.5),
    breaks = 2019:2022,
    expand = expansion(0, 0)
  )

output_chart(
  chart = chart_reg,
  path = here("outputs"),
  orientation = "wide",
  legend_position = "top",
  xlab = "",
  ylab = "",
  ggtitle = "Proportion of total gross population changes by Regional Council",
  flipped = TRUE,
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
  axis.text.y = element_text(size = rel(0.85)),
  panel.spacing.x = unit(30, "pt"),
  panel.spacing.y = unit(8, "pt"),
  strip.text = element_text(margin = margin(0, 0, 0, 0, "pt")),
  plot.margin = margin(4, 12, 4, 4, "pt")
)


# TAs
dat_ta <- dat |>
  filter(
    !str_detect(string = area, pattern = "Total New Zealand"),
    !str_detect(string = area, pattern = "local board"),
    !str_detect(string = area, pattern = "region"),
    area != "Area outside territorial authority",
    area != "Chatham Islands territory"
  ) |>
  filter(
    measure != "Population",
    measure != "Net migration"
  ) |>
  mutate(area = order_ta(area)) |>
  mutate(measure = custom_order(
    x = measure,
    order_info = tibble(
      levels = c(
        "Net international migration",
        "Net internal migration",
        "Natural increase"
      )
    )
  )) |>
  arrange(area, measure, year_at_30_june)

# TA totals (absolute value)
dat_ta_totals <- dat_ta |>
  group_by(area, year_at_30_june) |>
  summarise(total_change = sum(abs(value))) |>
  ungroup()

# TA chart
chart_ta <- dat_ta |>
  left_join(y = dat_ta_totals, by = c("area", "year_at_30_june")) |>
  mutate(pct = value / total_change) |>
  ggplot(mapping = aes(
    x = year_at_30_june,
    y = pct,
    fill = measure
  )) +
  geom_col(
    colour = "white",
    size = 0.25
  ) +
  geom_hline(
    yintercept = 0,
    size = 0.25
  ) +
  facet_wrap(
    facets = vars(area),
    ncol = 6,
    labeller = label_wrap_gen(width = 12)
  ) +
  scale_colour_qual(
    values = c(
      "Net internal migration",
      "Net international migration",
      "Natural increase"
    ),
    value_labels = c(
      "Net internal migration",
      "Net international migration",
      "Natural increase"
    ),
    value_grey = "Natural increase",
    guide = guide_legend(),
    name = NULL
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.5),
    expand = expansion(0, 0)
  ) +
  scale_x_continuous(
    limits = c(2018.5, 2022.5),
    breaks = 2019:2022,
    expand = expansion(0, 0)
  )

output_chart(
  chart = chart_ta,
  path = here("outputs"),
  orientation = "taller",
  legend_position = "top",
  xlab = "",
  ylab = "",
  ggtitle = "Proportion of total gross population changes by Territorial Authority",
  flipped = TRUE,
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
  axis.text.y = element_text(size = rel(0.85)),
  panel.spacing.x = unit(20, "pt"),
  panel.spacing.y = unit(8, "pt"),
  strip.text = element_text(margin = margin(0, 0, 0, 0, "pt")),
  plot.margin = margin(4, 12, 4, 4, "pt")
)

# Auckland local boards
dat_akllb <- dat |>
  filter(
    str_detect(string = area, pattern = "local board")
  ) |>
  filter(
    measure != "Population",
    measure != "Net migration"
  ) |>
  mutate(area = order_akl_lb(area)) |>
  mutate(measure = custom_order(
    x = measure,
    order_info = tibble(
      levels = c(
        "Net international migration",
        "Net internal migration",
        "Natural increase"
      )
    )
  )) |>
  arrange(area, measure, year_at_30_june)

# Local board totals (absolute value)
dat_akllb_totals <- dat_akllb |>
  group_by(area, year_at_30_june) |>
  summarise(total_change = sum(abs(value))) |>
  ungroup()

# Auckland local board chart
chart_akllb <- dat_akllb |>
  left_join(y = dat_akllb_totals, by = c("area", "year_at_30_june")) |>
  mutate(pct = value / total_change) |>
  ggplot(mapping = aes(
    x = year_at_30_june,
    y = pct,
    fill = measure
  )) +
  geom_col(
    colour = "white",
    size = 0.25
  ) +
  geom_hline(
    yintercept = 0,
    size = 0.25
  ) +
  facet_wrap(
    facets = vars(area),
    ncol = 3
  ) +
  scale_colour_qual(
    values = c(
      "Net internal migration",
      "Net international migration",
      "Natural increase"
    ),
    value_labels = c(
      "Net internal migration",
      "Net international migration",
      "Natural increase"
    ),
    value_grey = "Natural increase",
    guide = guide_legend(),
    name = NULL
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.5),
    expand = expansion(0, 0)
  ) +
  scale_x_continuous(
    limits = c(2018.5, 2022.5),
    breaks = 2019:2022,
    expand = expansion(0, 0)
  )

output_chart(
  chart = chart_akllb,
  path = here("outputs"),
  orientation = "square",
  legend_position = "top",
  xlab = "",
  ylab = "",
  ggtitle = "Proportion of total gross population changes by Auckland local board area",
  flipped = TRUE,
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
  axis.text.y = element_text(size = rel(0.85)),
  panel.spacing.x = unit(30, "pt"),
  panel.spacing.y = unit(8, "pt"),
  strip.text = element_text(margin = margin(0, 0, 0, 0, "pt")),
  plot.margin = margin(4, 12, 4, 4, "pt")
)

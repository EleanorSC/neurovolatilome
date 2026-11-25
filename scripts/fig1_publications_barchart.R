# Script to create fig.1 temporal trends in VOC research across brain health phenotypes.

# Load relevant packages

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# 1. Prepare annual publication counts

publication_trends <- priority %>%
  mutate(
    Year = as.integer(Year),
    Group = case_when(
      is.na(Candidate_phenotype_group) |
        str_trim(Candidate_phenotype_group) == "" ~
        "Unclassified",
      str_detect(Candidate_phenotype_group, ";") ~
        "Multiple phenotype groups",
      TRUE ~ Candidate_phenotype_group
    )
  ) %>%
  filter(!is.na(Year), Year < 2026) %>%
  distinct(PMID, Year, Group) %>%
  count(Year, Group, name = "N") %>%
  complete(
    Year = seq(min(Year), max(Year)),
    Group,
    fill = list(N = 0)
  )

# 2. Define colours

trend_colours <- c(
  "Neurodegenerative disorders" = "#6684A6",
  "Other neurological disorders" = "#5D998F",
  "Neuropsychiatric disorders" = "#BF8658",
  "Neurodevelopmental disorders" = "#9584AF",
  "Prodromal neurocognitive phenotypes" = "#BF8297",
  "Neurometabolic disorders" = "#A3AC79",
  "Multiple phenotype groups" = "#78838F",
  "Unclassified" = "#D9D9D9"
)

publication_trends <- publication_trends %>%
  mutate(
    Group = factor(Group, levels = names(trend_colours))
  )

# 3. Create stacked publication trends plot

p_trends <- ggplot(
  publication_trends,
  aes(x = Year, y = N, fill = Group)
) +
  geom_col(
    width = 0.75,
    position = position_stack(reverse = TRUE)
  ) +
  scale_fill_manual(
    values = trend_colours,
    name = NULL
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 10),
    expand = expansion(add = 0.6)
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 5),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = "Year of publication",
    y = "Number of publications"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey88",
      linewidth = 0.6
    ),
    axis.text = element_text(
      colour = "black",
      size = 11
    ),
    axis.title.x = element_text(
      margin = margin(t = 20)
    ),
    axis.title.y = element_text(
      margin = margin(r = 15)
    ),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.margin = margin(20, 20, 15, 20)
  ) +
  guides(
    fill = guide_legend(ncol = 2, byrow = TRUE)
  )

# 4. Display figure

print(p_trends)

# 5. Save figure

ggsave(
  "VOC_publication_trends.pdf",
  plot = p_trends,
  width = 12,
  height = 6,
  bg = "white"
)

ggsave(
  "VOC_publication_trends.png",
  plot = p_trends,
  width = 12,
  height = 6,
  dpi = 600,
  bg = "white"
)

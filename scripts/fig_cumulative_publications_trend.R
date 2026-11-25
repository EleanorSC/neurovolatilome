install.packages("zoo")

tech_colours <- c(
  "GC-MS"           = "#376795",
  "Electronic nose" = "#C45A48",
  "PTR-MS"          = "#398A75",
  "GC-FID"          = "#8965A8",
  "GC-IMS"          = "#D49B42",
  "SIFT-MS"         = "#B96D94",
  "IMS"             = "#729BB5",
  "FAIMS"           = "#84744D",
  "DMS"             = "#777777"
)

technology_smooth <- technology_trends %>%
  group_by(Platform) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    N_smooth = zoo::rollmean(
      N,
      k = 3,
      fill = NA,
      align = "center"
    )
  ) %>%
  ungroup()

p_technology <- ggplot(
  technology_smooth,
  aes(Year, N_smooth, colour = Platform)
) +
  geom_line(
    linewidth = 1.2,
    lineend = "round",
    linejoin = "round",
    na.rm = TRUE
  ) +
  scale_colour_manual(values = tech_colours) +
  ) +
  labs(
    x = "Publication year",
    y = "Annual publications (3-year rolling mean)",
    colour = "Analytical platform"
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    span = 0.35,
    se = FALSE,
    linewidth = 1.2,
    na.rm = TRUE
  ) +
  theme_minimal(base_size = 13) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    axis.text = element_text(colour = "grey20"),
    plot.margin = margin(15, 20, 15, 15)
  )

print(p_technology)


technology_cumulative <- technology_trends %>%
  group_by(Platform) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(Cumulative_N = cumsum(N)) %>%
  ungroup()

p_cumulative_tech <- ggplot(
  technology_cumulative,
  aes(Year, Cumulative_N, colour = Platform)
) +
  geom_line(
    linewidth = 1.3,
    lineend = "round"
  ) +
  scale_colour_manual(
    values = tech_colours
  ) +
  labs(
    x = "Publication year",
    y = "Cumulative number of publications",
    colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  ) +
  guides(
    colour = guide_legend(nrow = 2)
  )

print(p_cumulative_tech)

##
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

technology_families <- priority %>%
  select(
    PMID,
    Year,
    Platform = Candidate_platform
  ) %>%
  separate_rows(Platform, sep = ";\\s*") %>%
  mutate(
    Platform = str_trim(Platform),
    Year = as.integer(Year),
    
    Technology_family = case_when(
      
      Platform %in% c(
        "GC-MS", "GC-FID", "GC-IMS"
      ) ~ "Gas chromatography-based",
      
      Platform %in% c(
        "PTR-MS", "SIFT-MS"
      ) ~ "Direct-injection mass spectrometry",
      
      Platform %in% c(
        "IMS", "FAIMS", "DMS"
      ) ~ "Ion mobility-based",
      
      Platform == "Electronic nose" ~
        "Electronic noses",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(Technology_family),
    !is.na(Year),
    Year < 2026
  ) %>%
  
  # Avoid duplicate publications within families
  distinct(PMID, Year, Technology_family) %>%
  
  count(
    Year,
    Technology_family,
    name = "N"
  ) %>%
  
  complete(
    Year = seq(min(Year), max(Year)),
    Technology_family,
    fill = list(N = 0)
  ) %>%
  
  group_by(Technology_family) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(Cumulative_N = cumsum(N)) %>%
  ungroup()

family_colours <- c(
  "Gas chromatography-based" = "#376795",
  "Electronic noses" = "#C45A48",
  "Direct-injection mass spectrometry" = "#398A75",
  "Ion mobility-based" = "#8965A8"
)

p_family <- ggplot(
  technology_families,
  aes(
    Year,
    Cumulative_N,
    colour = Technology_family
  )
) +
  geom_line(
    linewidth = 1.3,
    lineend = "round",
    linejoin = "round"
  ) +
  scale_colour_manual(
    values = family_colours
  ) +
  scale_x_continuous(
    breaks = seq(1980, 2025, by = 5)
  ) +
  labs(
    x = "Publication year",
    y = "Cumulative number of publications",
    colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.key.width = grid::unit(1.5, "cm")
  ) +
  guides(
    colour = guide_legend(nrow = 2)
  )

print(p_family)

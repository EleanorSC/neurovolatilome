
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)
library(scales)
library(patchwork)
library(tibble)
library(wesanderson)

root <- path.expand("~/Documents/VOCs_project/data")

figure_dir <- file.path(root, "figures", "cleaned_dataset")

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

cleaned <- read.csv(
  file.path(root, "PubMed_priority_cleaned.csv"),
  stringsAsFactors = FALSE,
  colClasses = c(PMID = "character")
) %>%
  as_tibble() %>%
  distinct(PMID, .keep_all = TRUE) %>%
  mutate(Year = as.integer(Year))

n_publications <- nrow(cleaned)

# Colour palettes

zissou <- wes_palette(
  "Zissou1", 5, type = "discrete"
)

zissou_continuous <- wes_palette(
  "Zissou1", 100, type = "continuous"
)

phenotype_colours <- c(
  "Neurodegenerative disorders" = "#3B9AB2",
  "Other neurological disorders" = "#78B7C5",
  "Neuropsychiatric disorders" = "#E1AF00",
  "Neurodevelopmental disorders" = "#EBCC2A",
  "Prodromal neurocognitive phenotypes" = "#E67E22",
  "Neurometabolic disorders" = "#F21A00"
)

specimen_colours <- c(
  "Urine" = "#EBCB35",
  "Breath" = "#3B9AB2",
  "Blood" = "#B52B32",
  "Sebum" = "#E88B32",
  "Stool" = "#79513A",
  "Sweat" = "#A4D5DE",
  "Saliva" = "#7EAD91",
  "Skin" = "#D8AD94",
  "Other" = "#999999"
)

# Common plotting theme

theme_voc <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold", size = 15
    ),
    plot.subtitle = element_text(
      colour = "grey40", size = 11
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(colour = "grey20"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    plot.caption = element_text(
      colour = "grey45", hjust = 0
    ),
    plot.margin = margin(12, 15, 12, 12)
  )

# Helper function to split multi-label classifications

make_long <- function(data, column, new_name) {

  data %>%
    select(PMID, Year, all_of(column)) %>%
    rename(!!new_name := all_of(column)) %>%
    separate_rows(all_of(new_name), sep = ";\\s*") %>%
    mutate(
      across(all_of(new_name), str_trim)
    ) %>%
    filter(
      !is.na(.data[[new_name]]),
      .data[[new_name]] != ""
    ) %>%
    distinct(PMID, Year, .data[[new_name]])
}

phenotype_long <- make_long(
  cleaned,
  "Candidate_phenotype_group",
  "Phenotype_group"
)

disorder_long <- make_long(
  cleaned,
  "Candidate_disorder",
  "Disorder"
)

specimen_long <- make_long(
  cleaned,
  "Candidate_biospecimen",
  "Biospecimen"
)

platform_long <- make_long(
  cleaned,
  "Candidate_platform",
  "Platform"
)

# Common figure export function

save_figure <- function(plot, filename,
                        width = 10, height = 7) {

  ggsave(
    file.path(figure_dir, paste0(filename, ".pdf")),
    plot,
    width = width,
    height = height,
    bg = "white"
  )

  ggsave(
    file.path(figure_dir, paste0(filename, ".png")),
    plot,
    width = width,
    height = height,
    dpi = 600,
    bg = "white"
  )
}



annual_data <- cleaned %>%
  filter(!is.na(Year)) %>%
  count(Year, name = "N") %>%
  complete(
    Year = seq(min(Year), max(Year)),
    fill = list(N = 0)
  ) %>%
  arrange(Year) %>%
  mutate(
    Cumulative = cumsum(N),
    Search_year = if_else(
      Year == 2026,
      "Incomplete year",
      "Complete year"
    )
  )

p_annual <- ggplot(
  annual_data,
  aes(Year, N, fill = Search_year)
) +
  geom_col(width = 0.85) +
  scale_fill_manual(
    values = c(
      "Complete year" = "#3B9AB2",
      "Incomplete year" = "#EBCC2A"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1975, 2030, 5)
  ) +
  labs(
    title = "Growth of neurovolatilomics research",
    subtitle = paste(n_publications, "candidate publications"),
    x = NULL,
    y = "Annual publications",
    fill = NULL
  ) +
  theme_voc

p_cumulative <- ggplot(
  annual_data,
  aes(Year, Cumulative)
) +
  geom_area(
    fill = "#78B7C5",
    alpha = 0.35
  ) +
  geom_line(
    colour = "#3B9AB2",
    linewidth = 1.2
  ) +
  scale_x_continuous(
    breaks = seq(1975, 2030, 5)
  ) +
  labs(
    x = "Publication year",
    y = "Cumulative publications"
  ) +
  theme_voc

p_growth <- p_annual / p_cumulative +
  plot_annotation(tag_levels = "A")

p_growth

save_figure(
  p_growth,
  "01_publication_growth",
  width = 11,
  height = 8
)

phenotype_frequency <- phenotype_long %>%
  count(Phenotype_group, name = "N") %>%
  mutate(
    Percentage = 100 * N / n_publications
  )

p_phenotype <- ggplot(
  phenotype_frequency,
  aes(
    x = N,
    y = fct_reorder(Phenotype_group, N),
    fill = Phenotype_group
  )
) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = sprintf("%d (%.1f%%)", N, Percentage)),
    hjust = -0.12,
    size = 3.6
  ) +
  scale_fill_manual(values = phenotype_colours) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.30))
  ) +
  labs(
    title = "Phenotypic distribution of neurovolatilomics research",
    x = "Publications",
    y = NULL
  ) +
  theme_voc +
  theme(legend.position = "none")

p_phenotype

save_figure(
  p_phenotype,
  "02_phenotype_groups",
  width = 10,
  height = 5
)

disorder_frequency <- disorder_long %>%
  count(Disorder, name = "N", sort = TRUE)

p_disorders <- disorder_frequency %>%
  slice_max(N, n = 25, with_ties = FALSE) %>%
  ggplot(
    aes(N, fct_reorder(Disorder, N))
  ) +
  geom_segment(
    aes(
      x = 0,
      xend = N,
      yend = Disorder
    ),
    colour = "grey80",
    linewidth = 0.8
  ) +
  geom_point(
    colour = "#3B9AB2",
    size = 4
  ) +
  geom_text(
    aes(label = N),
    hjust = -0.7,
    size = 3.5
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Most frequently investigated brain health phenotypes",
    subtitle = "Top 25 candidate disorders",
    x = "Publications",
    y = NULL
  ) +
  theme_voc

p_disorders

save_figure(
  p_disorders,
  "03_disorder_frequency",
  width = 10,
  height = 9
)
top_disorders <- disorder_frequency %>%
  slice_max(N, n = 20, with_ties = FALSE) %>%
  pull(Disorder)

disorder_temporal <- disorder_long %>%
  filter(
    Disorder %in% top_disorders,
    !is.na(Year)
  ) %>%
  mutate(
    Period = paste0(
      floor(Year / 5) * 5,
      "–",
      floor(Year / 5) * 5 + 4
    )
  ) %>%
  count(Disorder, Period, name = "N") %>%
  complete(
    Disorder,
    Period,
    fill = list(N = 0)
  ) %>%
  mutate(
    Disorder = factor(
      Disorder,
      levels = rev(top_disorders)
    )
  )

p_temporal_phenotype <- ggplot(
  disorder_temporal,
  aes(Period, Disorder, fill = N)
) +
  geom_tile(
    colour = "white",
    linewidth = 0.6
  ) +
  geom_text(
    aes(label = if_else(N > 0, as.character(N), "")),
    size = 3
  ) +
  scale_fill_gradientn(
    colours = c("grey97", zissou_continuous),
    name = "Publications"
  ) +
  labs(
    title = "Temporal evolution of phenotype-specific research",
    subtitle = "Publication frequency within five-year periods",
    x = "Publication period",
    y = NULL
  ) +
  theme_voc +
  theme(
    axis.text.x = element_text(
      angle = 45, hjust = 1
    ),
    panel.grid = element_blank()
  )

p_temporal_phenotype

save_figure(
  p_temporal_phenotype,
  "04_temporal_phenotype_heatmap",
  width = 12,
  height = 9
)
specimen_frequency <- specimen_long %>%
  count(Biospecimen, name = "N") %>%
  mutate(
    Percentage = 100 * N / n_publications
  )

p_specimens <- ggplot(
  specimen_frequency,
  aes(
    N,
    fct_reorder(Biospecimen, N),
    fill = Biospecimen
  )
) +
  geom_col(width = 0.65) +
  geom_text(
    aes(label = sprintf("%d (%.1f%%)", N, Percentage)),
    hjust = -0.1,
    size = 3.8
  ) +
  scale_fill_manual(values = specimen_colours) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title = "Biospecimen landscape of neurovolatilomics",
    x = "Publications",
    y = NULL
  ) +
  theme_voc +
  theme(legend.position = "none")

p_specimens

save_figure(
  p_specimens,
  "05_biospecimen_distribution",
  width = 9,
  height = 5
)

disorder_specimen <- disorder_long %>%
  select(PMID, Disorder) %>%
  inner_join(
    specimen_long %>%
      select(PMID, Biospecimen),
    by = "PMID",
    relationship = "many-to-many"
  ) %>%
  distinct(PMID, Disorder, Biospecimen) %>%
  count(
    Disorder,
    Biospecimen,
    name = "N"
  ) %>%
  filter(Disorder %in% top_disorders) %>%
  complete(
    Disorder,
    Biospecimen,
    fill = list(N = 0)
  ) %>%
  mutate(
    Disorder = factor(
      Disorder,
      levels = rev(top_disorders)
    )
  )

p_disorder_specimen <- ggplot(
  disorder_specimen,
  aes(Biospecimen, Disorder)
) +
  geom_tile(
    fill = "grey97",
    colour = "white"
  ) +
  geom_point(
    data = filter(disorder_specimen, N > 0),
    aes(size = N, fill = Biospecimen),
    shape = 21,
    colour = "grey25",
    stroke = 0.3,
    alpha = 0.9
  ) +
  scale_size_area(
    max_size = 13,
    name = "Publications"
  ) +
  scale_fill_manual(
    values = specimen_colours,
    guide = "none"
  ) +
  labs(
    title = "Biospecimen architecture of brain health research",
    subtitle = "Publication-level phenotype and biospecimen co-classifications",
    x = NULL,
    y = NULL
  ) +
  theme_voc +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 45, hjust = 1
    )
  )

p_disorder_specimen

save_figure(
  p_disorder_specimen,
  "06_phenotype_biospecimen_matrix",
  width = 11,
  height = 10
)

disorder_specimen_normalised <- disorder_specimen %>%
  mutate(Disorder = as.character(Disorder)) %>%
  left_join(
    disorder_frequency %>%
      rename(Total_publications = N),
    by = "Disorder"
  ) %>%
  mutate(
    Proportion = N / Total_publications,
    Disorder = factor(
      Disorder,
      levels = rev(top_disorders)
    )
  )

p_specimen_normalised <- ggplot(
  disorder_specimen_normalised,
  aes(Biospecimen, Disorder, fill = Proportion)
) +
  geom_tile(colour = "white") +
  geom_text(
    aes(
      label = if_else(
        N > 0,
        percent(Proportion, accuracy = 1),
        ""
      )
    ),
    size = 3
  ) +
  scale_fill_gradientn(
    colours = c("grey97", zissou_continuous),
    labels = label_percent(),
    limits = c(0, 1),
    name = "Proportion"
  ) +
  labs(
    title = "Relative biospecimen representation across disorders",
    x = NULL,
    y = NULL
  ) +
  theme_voc +
  theme(panel.grid = element_blank())

p_specimen_normalised

save_figure(
  p_specimen_normalised,
  "06b_normalised_biospecimen_matrix",
  width = 11,
  height = 10
)
platform_frequency <- platform_long %>%
  count(Platform, name = "N", sort = TRUE)

p_platform <- ggplot(
  platform_frequency,
  aes(N, fct_reorder(Platform, N))
) +
  geom_col(
    fill = "#3B9AB2",
    width = 0.65
  ) +
  geom_text(
    aes(label = N),
    hjust = -0.3,
    size = 4
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Analytical platforms used in neurovolatilomics",
    x = "Publications",
    y = NULL
  ) +
  theme_voc

p_platform

save_figure(
  p_platform,
  "07_analytical_platforms",
  width = 9,
  height = 5
)
platform_temporal <- platform_long %>%
  filter(!is.na(Year)) %>%
  count(Year, Platform, name = "N") %>%
  complete(
    Year = seq(min(Year), max(Year)),
    Platform,
    fill = list(N = 0)
  ) %>%
  group_by(Platform) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(Cumulative = cumsum(N)) %>%
  ungroup()

platform_colours <- setNames(
  wes_palette(
    "Zissou1",
    length(unique(platform_temporal$Platform)),
    type = "continuous"
  ),
  sort(unique(platform_temporal$Platform))
)

p_platform_temporal <- ggplot(
  platform_temporal,
  aes(
    Year,
    Cumulative,
    colour = Platform
  )
) +
  geom_step(linewidth = 1.2) +
  scale_colour_manual(values = platform_colours) +
  labs(
    title = "Technological evolution of neurovolatilomics",
    subtitle = "Cumulative publications by analytical platform",
    x = "Publication year",
    y = "Cumulative publications",
    colour = "Platform"
  ) +
  theme_voc

p_platform_temporal

save_figure(
  p_platform_temporal,
  "08_temporal_platform_landscape",
  width = 11,
  height = 6
)

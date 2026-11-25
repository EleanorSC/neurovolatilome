# Script to create fig.2, visualisation of trends across brain health phenotype categories.

library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

records <- read.xlsx(
  "~/Documents/VOCs_project/data/PubMed_prioritised_screening.xlsx",
  sheet = "Screening"
)

priority <- records %>%
  filter(Screening_tier %in% c(1, 2))

nrow(priority)
# Expected: 341

groups <- c(
  "Neurodegenerative disorders",
  "Other neurological disorders",
  "Neuropsychiatric disorders",
  "Neurodevelopmental disorders",
  "Prodromal neurocognitive phenotypes",
  "Neurometabolic disorders"
)

specimens <- c(
  "Breath",
  "Sweat",
  "Sebum",
  "Skin emissions",
  "Urine",
  "Stool",
  "Saliva",
  "Blood",
  "Earwax",
  "Semen",
  "Other"
)



unique(priority$Candidate_biospecimen)

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

plot_groups <- c(
  "Neurodegenerative disorders",
  "Other neurological disorders",
  "Neuropsychiatric disorders",
  "Neurodevelopmental disorders",
  "Prodromal neurocognitive phenotypes",
  "Neurometabolic disorders"
)

plot_specimens <- c(
  "Breath",
  "Sweat",
  "Sebum",
  "Skin emissions",
  "Urine",
  "Stool",
  "Saliva",
  "Blood",
  "Earwax",
  "Semen",
  "Other"
)

# Separate multiple phenotype and biospecimen classifications

observed_counts <- priority %>%
  select(
    PMID,
    Group = Candidate_phenotype_group,
    Specimen = Candidate_biospecimen
  ) %>%
  separate_rows(Group, sep = ";\\s*") %>%
  separate_rows(Specimen, sep = ";\\s*") %>%
  mutate(
    Group = str_trim(Group),
    Specimen = str_trim(Specimen)
  ) %>%
  filter(
    Group %in% plot_groups,
    Specimen %in% plot_specimens
  ) %>%
  distinct(PMID, Group, Specimen) %>%
  count(Group, Specimen, name = "N")

# Construct complete evidence matrix

z <- expand_grid(
  Group = plot_groups,
  Specimen = plot_specimens
) %>%
  left_join(
    observed_counts,
    by = c("Group", "Specimen")
  ) %>%
  mutate(
    N = replace_na(N, 0L),
    Group = factor(Group, levels = rev(plot_groups)),
    Specimen = factor(Specimen, levels = plot_specimens)
  )

# Integrity checks

stopifnot(nrow(z) == 66)
stopifnot(sum(z$N) > 0)

print(z, n = Inf)

# display the evidence matrix as a conventional table:
z %>%
  pivot_wider(
    names_from = Specimen,
    values_from = N
  ) %>%
  print(width = Inf)



#Ordering 
specimen_order <- priority %>%
  select(PMID, Specimen = Candidate_biospecimen) %>%
  separate_rows(Specimen, sep = ";\\s*") %>%
  mutate(Specimen = str_trim(Specimen)) %>%
  filter(Specimen %in% plot_specimens) %>%
  distinct(PMID, Specimen) %>%
  count(Specimen, name = "N_publications") %>%
  arrange(desc(N_publications))

# Append biospecimens with zero publications
ordered_specimens <- c(
  specimen_order$Specimen,
  setdiff(plot_specimens, specimen_order$Specimen)
)

z <- z %>%
  mutate(
    Specimen = factor(
      Specimen,
      levels = ordered_specimens
    )
  )

print(specimen_order)

#Plot

p <- ggplot(z, aes(Specimen, Group)) +
  
  # Zero-count combinations
  geom_point(
    data = filter(z, N == 0),
    shape = 4,
    colour = "grey85",
    size = 1.5,
    stroke = 0.5
  ) +
  
  # Publication counts
  geom_point(
    data = filter(z, N > 0),
    aes(size = N, colour = N),
    alpha = 0.9
  ) +
  
  scale_size_area(
    max_size = 11,
    breaks = c(5, 10, 20, 40),
    name = "Publications"
  ) +
  
  scale_colour_viridis_c(
    option = "mako",
    direction = -1,
    guide = "none"
  ) +
  
  scale_x_discrete(
    labels = function(x) {
      x[x == "Skin emissions"] <- "Skin"
      x
    },
    expand = expansion(add = 0.65)
  ) +
  
  scale_y_discrete(
    labels = c(
      "Neurodegenerative disorders" =
        "Neurodegenerative",
      "Other neurological disorders" =
        "Other neurological",
      "Neuropsychiatric disorders" =
        "Neuropsychiatric",
      "Neurodevelopmental disorders" =
        "Neurodevelopmental",
      "Prodromal neurocognitive phenotypes" =
        "Prodromal neurocognitive",
      "Neurometabolic disorders" =
        "Neurometabolic"
    ),
    expand = expansion(add = 0.6)
  ) +
  
  labs(
    x = "Biospecimen",
    y = "Phenotype group"
  ) +
  
  guides(
    size = guide_legend(
      override.aes = list(
        colour = "#356A9A",
        alpha = 1
      )
    )
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      colour = "grey20",
      margin = margin(t = 10)
    ),
    
    axis.text.y = element_text(
      colour = "grey20",
      margin = margin(r = 10)
    ),
    
    axis.title = element_text(
      face = "bold",
      size = 12
    ),
    
    axis.title.x = element_text(
      margin = margin(t = 15)
    ),
    
    axis.title.y = element_text(
      margin = margin(r = 15)
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major = element_line(
      colour = "grey94",
      linewidth = 0.3
    ),
    
    legend.position = "right",
    legend.key.height = grid::unit(1.1, "cm"),
    legend.spacing.y = grid::unit(0.3, "cm"),
    
    plot.margin = margin(15, 20, 15, 15)
  )

print(p)

#### Now focus on neurodegenerative subcategories:

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Identify neurodegenerative disorders from the dictionary

neurodegenerative_disorders <- phenotype_lookup %>%
  filter(Phenotype_group == "Neurodegenerative disorders") %>%
  pull(Disorder) %>%
  unique()

# Extract disorder–biospecimen combinations

nd_records <- priority %>%
  select(
    PMID,
    Disorder = Candidate_disorder,
    Specimen = Candidate_biospecimen
  ) %>%
  separate_rows(Disorder, sep = ";\\s*") %>%
  separate_rows(Specimen, sep = ";\\s*") %>%
  mutate(
    Disorder = str_trim(Disorder),
    Specimen = str_trim(Specimen)
  ) %>%
  filter(
    Disorder %in% neurodegenerative_disorders,
    Specimen %in% plot_specimens
  ) %>%
  distinct(PMID, Disorder, Specimen)

# Count unique publications per combination

nd_counts <- nd_records %>%
  count(Disorder, Specimen, name = "N")

print(nd_counts, n = Inf)

#Order disorders and biospecimens by publication frequency
# Order disorders by number of unique publications

nd_disorder_order <- nd_records %>%
  distinct(PMID, Disorder) %>%
  count(Disorder, sort = TRUE) %>%
  pull(Disorder)

# Order biospecimens by number of unique publications

nd_specimen_order <- nd_records %>%
  distinct(PMID, Specimen) %>%
  count(Specimen, sort = TRUE) %>%
  pull(Specimen)

# Append biospecimens without publications

nd_specimen_order <- c(
  nd_specimen_order,
  setdiff(plot_specimens, nd_specimen_order)
)

# Construct complete evidence matrix

z_nd <- expand_grid(
  Disorder = nd_disorder_order,
  Specimen = nd_specimen_order
) %>%
  left_join(
    nd_counts,
    by = c("Disorder", "Specimen")
  ) %>%
  mutate(
    N = replace_na(N, 0L),
    
    Disorder = factor(
      Disorder,
      levels = rev(nd_disorder_order)
    ),
    
    Specimen = factor(
      Specimen,
      levels = nd_specimen_order
    )
  )

stopifnot(sum(z_nd$N) > 0)


#3. Generate the neurodegenerative disorder bubble plot
#This preserves the visual style of your previous figure, including the reversed mako colour palette, smaller bubbles, subtle zero-count crosses and shortened Skin label.

p_nd <- ggplot(z_nd, aes(Specimen, Disorder)) +
  
  geom_point(
    data = filter(z_nd, N == 0),
    shape = 4,
    colour = "grey85",
    size = 1.5,
    stroke = 0.5
  ) +
  
  geom_point(
    data = filter(z_nd, N > 0),
    aes(size = N, colour = N),
    alpha = 0.9
  ) +
  
  scale_size_area(
    max_size = 11,
    name = "Publications"
  ) +
  
  scale_colour_viridis_c(
    option = "mako",
    direction = -1,
    guide = "none"
  ) +
  
  scale_x_discrete(
    labels = function(x) {
      x[x == "Skin emissions"] <- "Skin"
      x
    },
    expand = expansion(add = 0.65)
  ) +
  
  scale_y_discrete(
    labels = function(x) str_wrap(x, width = 35),
    expand = expansion(add = 0.6)
  ) +
  
  labs(
    x = "Biospecimen",
    y = "Neurodegenerative disorder"
  ) +
  
  guides(
    size = guide_legend(
      override.aes = list(
        colour = "#356A9A",
        alpha = 1
      )
    )
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      colour = "grey20",
      margin = margin(t = 10)
    ),
    
    axis.text.y = element_text(
      colour = "grey20",
      margin = margin(r = 10)
    ),
    
    axis.title = element_text(
      face = "bold",
      size = 12
    ),
    
    axis.title.x = element_text(
      margin = margin(t = 15)
    ),
    
    axis.title.y = element_text(
      margin = margin(r = 15)
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major = element_line(
      colour = "grey94",
      linewidth = 0.3
    ),
    
    legend.position = "right",
    legend.key.height = grid::unit(1.1, "cm"),
    legend.spacing.y = grid::unit(0.3, "cm"),
    
    plot.margin = margin(15, 20, 15, 15)
  )

print(p_nd)


#4. Include all 21 neurodegenerative disorders
#The code above displays only disorders with at least one identified publication. 
#However, for a systematic evidence-mapping review, it may be more informative to 
#include all 21 neurodegenerative disorders, including those for which no publications were identified.

#To do this, add the following immediately before constructing z_nd:

  nd_disorder_order <- c(
    nd_disorder_order,
    setdiff(
      neurodegenerative_disorders,
      nd_disorder_order
    )
  )
  
  # Construct complete evidence matrix
  
  z_nd <- expand_grid(
    Disorder = nd_disorder_order,
    Specimen = nd_specimen_order
  ) %>%
    left_join(
      nd_counts,
      by = c("Disorder", "Specimen")
    ) %>%
    mutate(
      N = replace_na(N, 0L),
      
      Disorder = factor(
        Disorder,
        levels = rev(nd_disorder_order)
      ),
      
      Specimen = factor(
        Specimen,
        levels = nd_specimen_order
      )
    )
  
  stopifnot(sum(z_nd$N) > 0)
  
  p_nd <- ggplot(z_nd, aes(Specimen, Disorder)) +
    
    geom_point(
      data = filter(z_nd, N == 0),
      shape = 4,
      colour = "grey85",
      size = 1.5,
      stroke = 0.5
    ) +
    
    geom_point(
      data = filter(z_nd, N > 0),
      aes(size = N, colour = N),
      alpha = 0.9
    ) +
    
    scale_size_area(
      max_size = 11,
      name = "Publications"
    ) +
    
    scale_colour_viridis_c(
      option = "mako",
      direction = -1,
      guide = "none"
    ) +
    
    scale_x_discrete(
      labels = function(x) {
        x[x == "Skin emissions"] <- "Skin"
        x
      },
      expand = expansion(add = 0.65)
    ) +
    
    scale_y_discrete(
      labels = function(x) str_wrap(x, width = 35),
      expand = expansion(add = 0.6)
    ) +
    
    labs(
      x = "Biospecimen",
      y = "Neurodegenerative disorder"
    ) +
    
    guides(
      size = guide_legend(
        override.aes = list(
          colour = "#356A9A",
          alpha = 1
        )
      )
    ) +
    
    theme_minimal(base_size = 12) +
    
    theme(
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        colour = "grey20",
        margin = margin(t = 10)
      ),
      
      axis.text.y = element_text(
        colour = "grey20",
        margin = margin(r = 10)
      ),
      
      axis.title = element_text(
        face = "bold",
        size = 12
      ),
      
      axis.title.x = element_text(
        margin = margin(t = 15)
      ),
      
      axis.title.y = element_text(
        margin = margin(r = 15)
      ),
      
      panel.grid.minor = element_blank(),
      
      panel.grid.major = element_line(
        colour = "grey94",
        linewidth = 0.3
      ),
      
      legend.position = "right",
      legend.key.height = grid::unit(1.1, "cm"),
      legend.spacing.y = grid::unit(0.3, "cm"),
      
      plot.margin = margin(15, 20, 15, 15)
    )
  
  print(p_nd)

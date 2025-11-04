# List of CAZs with implementation dates
caz_list <- list(
  list(name = "Bradford", year = 2022),
  list(name = "Portsmouth", year = 2021),
  list(name = "Newcastle upon Tyne", year = 2023),
  list(name = "Sheffield", year = 2023)
)

template <- '---
title: "CAZ Prescription Trends: {{CAZ_NAME}}"
---

## Overview

This page presents prescription trend analysis for the **{{CAZ_NAME}}** Clean Air Zone. The analysis examines changes in prescription patterns before and after CAZ implementation ({{YEAR}}), comparing practices within and around the CAZ boundaries to a control group of similar practices outside the zone.

The following medications and metrics are analyzed:

- **SABA inhalers**: Short-Acting Beta Agonist inhalers for asthma rescue treatment
- **Montelukast**: Leukotriene receptor antagonist for asthma/allergy management
- **Main BNF codes**: Comprehensive medication categories for broader pattern analysis

All prescription metrics are normalized by patient population to account for varying practice sizes.

## Setup

### Package Installation

Loading required packages for data analysis and visualisation.

```{r}
#| label: packages
#| message: false

options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("remotes")) {
  install.packages("remotes")
}
pkgs = c(
  "sf",
  "tidyverse",
  "here",
  "tmap",
  "data.table",
  "cols4all"
)

remotes::install_cran(pkgs)
sapply(pkgs, require, character.only = TRUE)
```

## Loading data

Loading pre-processed trend data for {{CAZ_NAME}}.

```{r}
#| label: load-trends-data

load(file = "data_raw/trends_data.rda")

# Filter for this specific CAZ
current_caz <- "{{CAZ_NAME}}"
```

## SABA Analysis

### SABA Ratio

Comparing the ratio of SABA inhalers to all respiratory inhalers. Lower ratios may indicate improved asthma control with better use of preventative medications.

```{r}
#| label: plot-saba-ratio
#| message: false
#| warning: false

saba_ratio_plot <- CAZ_SABA |>
  left_join(
    CAZ_practices |>
      st_drop_geometry() |>
      select(code, location_class, CAZ_name),
    by = c("org_id" = "code")
  ) |>
  filter(CAZ_name == current_caz) |>
  summarise(
    across(c(NUMBER_OF_PATIENTS, numerator, denominator), \(x) {
      sum(x, na.rm = T)
    }),
    .by = c(location_class, month, year, date)
  ) |>
  mutate(calc_value = numerator / denominator) |>
  ggplot(aes(x = date, y = calc_value, colour = location_class)) +
  geom_line(aes(group = location_class), alpha = 0.2, linewidth = 0.2) +
  geom_smooth(
    method = "lm",
    formula = '\'y ~ x\'',
    se = FALSE,
    aes(linetype = location_class)
  ) +
  theme_minimal() +
  scale_colour_manual(
    values = c(cols4all::c4a(palette = "set1")[1:4], "gray30")
  ) +
  scale_y_continuous(limits = c(0, 0.7)) +
  scale_linetype_manual(values = c(rep("solid", 4), "dashed")) +
  guides(linetype = "none") +
  labs(
    title = paste("SABA Ratio -", current_caz),
    y = "SABA ratio",
    col = "Location Class",
    x = "Date"
  )

print(saba_ratio_plot)
```

### SABA Numerator (per patient)

Prescription rates of SABA inhalers per registered patient.

```{r}
#| label: plot-saba-numerator
#| message: false
#| warning: false

saba_numerator_plot <- CAZ_SABA |>
  left_join(
    CAZ_practices |>
      st_drop_geometry() |>
      select(code, location_class, CAZ_name),
    by = c("org_id" = "code")
  ) |>
  filter(CAZ_name == current_caz) |>
  summarise(
    across(c(NUMBER_OF_PATIENTS, numerator, denominator), \(x) {
      sum(x, na.rm = T)
    }),
    .by = c(location_class, month, year, date)
  ) |>
  ggplot(aes(
    x = date,
    y = numerator / NUMBER_OF_PATIENTS,
    colour = location_class
  )) +
  geom_line(aes(group = location_class), alpha = 0.2, linewidth = 0.2) +
  geom_smooth(
    method = "lm",
    formula = '\'y ~ x\'',
    se = FALSE,
    aes(linetype = location_class)
  ) +
  theme_minimal() +
  scale_colour_manual(
    values = c(cols4all::c4a(palette = "set1")[1:4], "gray30")
  ) +
  scale_linetype_manual(values = c(rep("solid", 4), "dashed")) +
  guides(linetype = "none") +
  labs(
    title = paste("SABA Prescriptions per Patient -", current_caz),
    y = "Prescriptions of SABA inhalers per patient",
    col = "Location Class",
    x = "Date"
  )

print(saba_numerator_plot)
```

### SABA Denominator (all inhalers per patient)

Overall inhaler prescription rates as a baseline metric.

```{r}
#| label: plot-saba-denominator
#| message: false
#| warning: false

saba_denominator_plot <- CAZ_SABA |>
  left_join(
    CAZ_practices |>
      st_drop_geometry() |>
      select(code, location_class, CAZ_name),
    by = c("org_id" = "code")
  ) |>
  filter(CAZ_name == current_caz) |>
  summarise(
    across(c(NUMBER_OF_PATIENTS, numerator, denominator), \(x) {
      sum(x, na.rm = T)
    }),
    .by = c(location_class, month, year, date)
  ) |>
  ggplot(aes(
    x = date,
    y = denominator / NUMBER_OF_PATIENTS,
    colour = location_class
  )) +
  geom_line(aes(group = location_class), alpha = 0.2, linewidth = 0.2) +
  geom_smooth(
    method = "lm",
    formula = '\'y ~ x\'',
    se = FALSE,
    aes(linetype = location_class)
  ) +
  theme_minimal() +
  scale_colour_manual(
    values = c(cols4all::c4a(palette = "set1")[1:4], "gray30")
  ) +
  scale_linetype_manual(values = c(rep("solid", 4), "dashed")) +
  guides(linetype = "none") +
  labs(
    title = paste("All Inhalers per Patient -", current_caz),
    y = "Prescriptions of inhalers per patient",
    col = "Location Class",
    x = "Date"
  )

print(saba_denominator_plot)
```

## Montelukast Analysis

Prescription trends for Montelukast (BNF code 0303020G0), a leukotriene receptor antagonist used in asthma management.

```{r}
#| label: plot-montelukast
#| message: false
#| warning: false

mtlkst_plot <- CAZ_mtlkst |>
  left_join(
    CAZ_practices |>
      st_drop_geometry() |>
      select(code, location_class, CAZ_name),
    by = c("row_id" = "code")
  ) |>
  filter(CAZ_name == current_caz) |>
  summarise(
    across(c(NUMBER_OF_PATIENTS, items, quantity), \(x) sum(x, na.rm = T)),
    .by = c(location_class, month, year, date, bnf)
  ) |>
  ggplot(aes(
    x = date,
    y = items / NUMBER_OF_PATIENTS,
    colour = location_class
  )) +
  geom_line(aes(group = location_class), alpha = 0.2, linewidth = 0.2) +
  geom_smooth(
    method = "lm",
    formula = '\'y ~ x\'',
    se = FALSE,
    aes(linetype = location_class)
  ) +
  theme_minimal() +
  scale_colour_manual(
    values = c(cols4all::c4a(palette = "set1")[1:4], "gray30")
  ) +
  scale_linetype_manual(values = c(rep("solid", 4), "dashed")) +
  guides(linetype = "none") +
  labs(
    title = paste("Montelukast Items per Patient -", current_caz),
    y = "Items per patient",
    col = "Location Class",
    x = "Date"
  )

print(mtlkst_plot)
```

## Main BNF Codes Analysis

Prescription trends across all main BNF medication categories.

```{r}
#| label: plot-bnf
#| message: false
#| warning: false

bnf_plots <- CAZ_bnf |>
  left_join(
    CAZ_practices |>
      st_drop_geometry() |>
      select(code, location_class, CAZ_name),
    by = c("row_id" = "code")
  ) |>
  filter(CAZ_name == current_caz) |>
  summarise(
    across(c(NUMBER_OF_PATIENTS, items, quantity), \(x) sum(x, na.rm = T)),
    .by = c(location_class, month, year, date, bnf)
  ) |>
  nest(.by = c(bnf)) |>
  mutate(
    plots = map(data, \(.x) {
      .x |>
        ggplot(aes(
          x = date,
          y = items / NUMBER_OF_PATIENTS,
          colour = location_class
        )) +
        geom_line(aes(group = location_class), alpha = 0.2, linewidth = 0.2) +
        geom_smooth(
          method = "lm",
          formula = '\'y ~ x\'',
          se = FALSE,
          aes(linetype = location_class)
        ) +
        theme_minimal() +
        scale_colour_manual(
          values = c(cols4all::c4a(palette = "set1")[1:4], "gray30")
        ) +
        scale_linetype_manual(values = c(rep("solid", 4), "dashed")) +
        guides(linetype = "none") +
        labs(
          title = paste("Items per Patient - BNF Code:", unique(.x$bnf)),
          y = "Items per patient",
          col = "Location Class",
          x = "Date"
        )
    })
  )

for (plot in bnf_plots$plots) {
  print(plot)
}
```

## Summary

This analysis for {{CAZ_NAME}} compares prescription trends across three medication categories, examining whether air quality improvements in the CAZ are associated with changes in respiratory medication use or broader prescribing patterns. Results are stratified by proximity to CAZ boundaries, allowing assessment of dose-response relationships with CAZ exposure.
'

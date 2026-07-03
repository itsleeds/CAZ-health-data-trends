# Create individual CAZ DiD analysis files from template by changing only the parameter

library(readr)
library(stringr)
library(stringr)
library(sf)

# Read the template
template_path <- "qmds_ghpages/03_results/template_DiD.qmd"
template_content <- readLines(template_path)

caz_boundaries <- st_read(
  file.path("data_raw", "CAZ_boundaries.gpkg"),
  quiet = TRUE
)

# Define CAZ names
caz_names <- caz_boundaries$name

# Create output directory if it doesn't exist
output_dir <- "qmds_ghpages/03_results"
dir.create(output_dir, showWarnings = FALSE)

# For each CAZ, create a new file
for (caz in caz_names) {
  # Create filename from CAZ name (replace spaces and "upon Tyne" with underscore)
  filename <- caz |>
    str_replace_all(" upon ", "_upon_") |>
    str_replace_all(" ", "_")

  output_path <- file.path(output_dir, paste0(filename, "_DiD_analysis.qmd"))

  modified_content <- template_content

  # Find and replace the current_caz parameter (typically on line 4)
  modified_content <- str_replace(
    modified_content,
    'current_caz: "[^"]*"',
    paste0('current_caz: "', caz, '"')
  )

  # Write to file
  writeLines(modified_content, output_path)

  cat("✓ Created: ", output_path, "\n", sep = "")
}

cat("\n✓ All CAZ DiD analysis files created successfully!\n")

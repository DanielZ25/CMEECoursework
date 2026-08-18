library(tidyverse)

cov <- read_csv("Results/biotraits_coverage.csv", show_col_types = FALSE)


classify_mirevtd <- function(col) {
  # Trait Description
  if (str_detect(col, regex("TraitName|TraitDef|TraitValue|TraitUnit", ignore_case = TRUE)))
    return("Trait Description")

  if (str_detect(col, regex("Location|Latitude|Longitude|Coordinate", ignore_case = TRUE)))
    return("Trait Description (Study location)")
  # Organism — taxonomy
  if (str_detect(col, regex("Kingdom|Phylum|Class|Order|Family|Genus|Species|Con$|Res$|Interactor", ignore_case = TRUE)))
    return("Organism (taxonomy)")
  # Organism — life stage / sex
  if (str_detect(col, regex("Stage|Sex|Age", ignore_case = TRUE)))
    return("Organism (life stage/sex)")

  if (str_detect(col, regex("Temp|Humidity|Photoperiod|Light|pH|Salinity|Stressor|Ambient", ignore_case = TRUE)))
    return("Axes of Variation")
  # Metadata
  if (str_detect(col, regex("Citation|SubmittedBy|DOI|Email|ID$|^ID|Embargo|Contributor", ignore_case = TRUE)))
    return("Metadata")

  return("Not in MIReVTD")
}

cov <- cov %>%
  mutate(MIReVTD_category = map_chr(column, classify_mirevtd))

write_csv(cov, "Results/biotraits_coverage_mirevtd.csv")


cov %>% count(MIReVTD_category, sort = TRUE) %>% print()

cat("\nSaved to Results/biotraits_coverage_mirevtd.csv\n")

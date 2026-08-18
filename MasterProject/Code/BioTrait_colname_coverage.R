library(tidyverse)


biotraits <- read_csv(
  "/Users/danielzhu/Documents/Master Project/Data/GlobalDataset.csv",
  locale = locale(encoding = "ISO-8859-1"),
  show_col_types = FALSE
)

cat(sprintf("Total rows: %d\n", nrow(biotraits)))
cat(sprintf("Total columns: %d\n", ncol(biotraits)))

# unique (name + def) pairs
n_unique <- biotraits %>%
  distinct(OriginalTraitName, OriginalTraitDef) %>%
  nrow()
cat(sprintf("Unique (name+def) pairs: %d\n\n", n_unique))


coverage_table <- biotraits %>%
  summarise(across(everything(), ~mean(!is.na(.) & str_trim(as.character(.)) != ""))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "coverage") %>%
  arrange(desc(coverage)) %>%
  mutate(
    coverage_pct = paste0(round(coverage * 100, 1), "%"),
    clustering_use = case_when(
      column == "OriginalTraitName" ~ "V1-V6 (core trait semantics)",
      column == "OriginalTraitDef"  ~ "V2-V6 (core trait semantics)",
      column == "OriginalTraitUnit" ~ "V3-V6 (core trait semantics)",
      column == "ConOrder"          ~ "V4-V6 (taxonomic context)",
      column == "ConFamily"         ~ "V5-V6 (taxonomic context)",
      column == "ConStage"          ~ "V6 (life stage) - coverage 56.6%, optional",
      TRUE ~ ""
    )
  ) %>%
  select(column, coverage_pct, clustering_use)


write_csv(coverage_table, "Results/biotraits_coverage.csv")


coverage_table %>%
  filter(as.numeric(str_remove(coverage_pct, "%")) >= 30) %>%
  print(n = Inf)

cat("\nSaved to Results/biotraits_coverage.csv\n")

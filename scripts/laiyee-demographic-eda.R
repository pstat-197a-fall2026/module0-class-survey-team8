summary_table <- tibble(
  Comparison = c(
    "Class vs. School",
    "Class vs. DS",
    "Class vs. School",
    "Class vs. DS"
  ),
  Category = c(
    "Ethnicity", "Ethnicity",
    "Gender", "Gender"
  ),
  `Chi-square` = map_dbl(demographic_tests, ~ unname(.x$test$statistic)),
  `p-value` = map_dbl(demographic_tests, ~ .x$test$p.value)
) %>%
  mutate(
    Significant = if_else(`p-value` < 0.05, "Yes", "No")
  )

print(summary_table)

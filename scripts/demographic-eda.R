library(tidyverse)

school_eth <- read_csv('data/demographics/Sheet 3-school-wide ethnicity.csv')
school_gen <- read_csv('data/demographics/Sheet 4-school-wide gender.csv')
ds_eth <- read_csv('data/demographics/Sheet 1-ds ethnicity.csv')
ds_gen <- read_csv('data/demographics/Sheet 2-ds gender.csv')
class_eth <- read_csv('data/demographics/Sheet 5-class ethnicity.csv')
class_gen <- read_csv('data/demographics/Sheet 6-class gender.csv')

# class to pop
compare_class <- function(class_data, population_data, category) {
  counts <- class_data %>%
    select(all_of(category), Class = Count) %>%
    full_join(
      population_data %>% select(all_of(category), Population = Count),
      by = category
    )

  stopifnot(
    !anyDuplicated(counts[[category]]),
    !anyNA(counts),
    all(counts$Class >= 0),
    all(counts$Population >= counts$Class)
  )

  contingency_table <- rbind(
    Class = counts$Class,
    Rest = counts$Population - counts$Class
  )
  colnames(contingency_table) <- counts[[category]]
  stopifnot(all(rowSums(contingency_table) > 0),
            all(colSums(contingency_table) > 0))

# null hypothesis: category proportions are the same in the class and the rest of the population. 
# use Pearson chi-square p-value; small counts 
  test <- suppressWarnings(chisq.test(contingency_table))
  expected_counts <- test$expected
  if (any(expected_counts < 5)) {
    test <- chisq.test(contingency_table, simulate.p.value = TRUE, B = 100000)
  }

  list(counts = contingency_table, expected = expected_counts, test = test)
}

set.seed(197)
demographic_tests <- list(
  class_vs_school_ethnicity = compare_class(class_eth, school_eth, 'Ethnicity'),
  class_vs_ds_ethnicity = compare_class(class_eth, ds_eth, 'Ethnicity'),
  class_vs_school_gender = compare_class(class_gen, school_gen, 'Gender'),
  class_vs_ds_gender = compare_class(class_gen, ds_gen, 'Gender')
)

for (comparison in names(demographic_tests)) {
  cat('\n', comparison, '\n')
  print(demographic_tests[[comparison]]$counts)
  print(demographic_tests[[comparison]]$test)
}

summary_table %>%
  mutate(
    `Chi-square` = round(`Chi-square`, 2),
    `p-value` = format.pval(`p-value`, digits = 3, eps = 0.001)
  ) %>%
  knitr::kable(
    caption = "Demographic comparison of class enrollment with school and DS populations",
    align = c('l','l', 'r', 'r', 'c')
  )


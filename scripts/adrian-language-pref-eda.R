# Findings (language preference vs domains / comfort ratings, n = 49)
# - Python is the top answer in most domains, but it is 45% of the class, so no clear domain pattern
# - Comfort ratings (prog, math, stat) did not differ by language (all p > 0.2)
# - No preference people checked more domains (Kruskal-Wallis p = 0.012)
#   No pref vs R: adj p = 0.017, No pref vs Python: adj p = 0.049 (borderline), Python vs R: p = 0.41
# - Small sample, so treat these as trends

library(tidyverse)

processed <- read_csv("data/background_interest_processed.csv")

# columns 31-45 are the domains, 46-56 are the types of work
# (check these still match the CSV if the processing script changes)
domains <- names(processed)[31:45]
methods <- names(processed)[46:56]

# 1. how many people picked each domain (drop the ones only 1 person picked)
counts <- sort(colSums(processed[domains]), decreasing = TRUE)
counts
keep <- names(counts[counts > 1])

# percent of each domain's pickers by language
domain_pct <- processed %>%
  pivot_longer(all_of(keep), names_to = "domain", values_to = "picked") %>%
  filter(picked == 1) %>%
  mutate(pref = ifelse(language == "R", "R", "Not R"))

# plot 1: Python vs R vs No preference
domain_pct %>%
  count(domain, language) %>%
  group_by(domain) %>%
  mutate(total = sum(n), pct = 100 * n / total) %>%
  ggplot(aes(reorder(domain, total), pct, fill = language)) +
  geom_col() + coord_flip() +
  labs(x = NULL, y = "% of people who picked the domain")
# ggsave("results/domain_pct_by_language.png", width = 7, height = 4)

# plot 2: R vs Not R
domain_pct %>%
  count(domain, pref) %>%
  group_by(domain) %>%
  mutate(total = sum(n), pct = 100 * n / total) %>%
  ggplot(aes(reorder(domain, total), pct, fill = pref)) +
  geom_col() + coord_flip() +
  labs(x = NULL, y = "% of people who picked the domain", fill = NULL)
# ggsave("results/domain_pct_R_vs_notR.png", width = 7, height = 4)

# 2. number of boxes each person checked
processed$n_domains <- rowSums(processed[keep])
processed$n_methods <- rowSums(processed[methods])
processed$n_picks <- processed$n_domains + processed$n_methods

mean(processed$n_picks)   # overall average

processed %>%
  group_by(language) %>%
  summarize(people = n(),
            avg_domains = mean(n_domains),
            avg_methods = mean(n_methods),
            avg_picks = mean(n_picks))

# plot 3: domains checked by language
ggplot(processed, aes(language, n_domains)) +
  geom_boxplot() +
  labs(x = NULL, y = "number of domains checked")
# ggsave("results/domains_checked_by_language.png", width = 6, height = 4)

# plot 4: total boxes checked (domains + types of work) by language
ggplot(processed, aes(language, n_picks)) +
  geom_boxplot() +
  labs(x = NULL, y = "# of boxes checked from both domains + type of analysis")

# 3. average comfort ratings (1 to 5) by language
processed %>%
  group_by(language) %>%
  summarize(prog = mean(prog.comf), math = mean(math.comf), stat = mean(stat.comf))

# 4. tests: do the 3 language groups differ?
# comfort ratings (none significant)
kruskal.test(prog.comf ~ language, data = processed)
kruskal.test(math.comf ~ language, data = processed)
kruskal.test(stat.comf ~ language, data = processed)

# boxes checked (both significant, domains is the cleaner result)
kruskal.test(n_domains ~ language, data = processed)
kruskal.test(n_picks ~ language, data = processed)

# which groups differ on domains checked?
# No pref vs R: 0.017, No pref vs Python: 0.049 (weaker), Python vs R: 0.405
pairwise.wilcox.test(processed$n_domains, processed$language, exact = FALSE)
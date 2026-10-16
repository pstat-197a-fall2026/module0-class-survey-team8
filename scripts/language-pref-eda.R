# Findings (language preference vs domains / comfort ratings, n = 49)
# Sample: 22 Python, 16 no preference, 11 R. Groups are unbalanced, R is the smallest.
# Cleaning: dropped 7 domains that only 1 person picked (8 domains kept). All 11 types of work kept.
#
# 1. Domain picks vs language: Python is the top answer in most domains, but it is 45% of the class,
#    so that alone is not a pattern. No domain clearly separated the groups.
#
# 2. Comfort ratings (1-5) vs language: Python people rated themselves slightly higher.
#    Averages (Python / R / No pref): prog 3.86 / 3.55 / 3.56, math 4.18 / 4.00 / 3.69, stat 4.27 / 4.27 / 3.88
#    Not significant (Kruskal-Wallis: prog p = 0.42, math p = 0.19, stat p = 0.22), so these are
#    possible trends that need a bigger sample.
#
# 3. Number of boxes checked vs language: no preference people checked more.
#    Domains: 5.06 (No pref) vs 3.68 (Python) vs 3.27 (R), Kruskal-Wallis p = 0.012
#      No pref vs R: adj p = 0.017, No pref vs Python: adj p = 0.049 (borderline), Python vs R: p = 0.41
#    Total (domains + types of work): 12.4 vs 10.1 vs 9.73, p = 0.047 (borderline, side note)
#    Possible reason: broader interests or less experience, but the data cannot say why.
#
# Limits: small sample, small R group, self-reported answers, one class, several tests run.

# Here's code for making the table of language vs comfortabilties (for report):
###
# processed %>%
#   group_by(language) %>%
#   summarize(People = n(),
#             Programming = round(mean(prog.comf), 2),
#             Math = round(mean(math.comf), 2),
#             Stats = round(mean(stat.comf), 2)) %>%
#   knitr::kable(caption = "Average comfort rating (1 to 5) by language preference")
###

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


# proficiency vs comfort in coding
# transform proficiency levels (beg/int/adv) into numbers 1, 2, 3
to_num <- function(x) ifelse(x == "beg", 1, ifelse(x == "int", 2, 3))
processed$prog.p <- to_num(processed$prog.prof)

# average gap between proficiency and comfort
# gap > 0: more comfortable than their stated proficiency (overestimate)
# gap < 0: less comfortable than their stated proficiency (underestimate)
processed$prog.gap <- (processed$prog.comf - 1) / 4 - (processed$prog.p - 1) / 2
mean(processed$prog.gap)

# count students who over/under-estimate by language
processed$label <- cut(processed$prog.gap, c(-Inf, -0.25, 0.25, Inf), labels = c("Under", "Aligned", "Over"))
counts <- table(processed$language, processed$label)
counts
round(prop.table(counts, margin = 2), 2)



### Courses taken vs. Project Preference

head(processed)
dim(processed)
glimpse(processed)

# Take columns
proj_interest <- processed[, 31:56]
glimpse(proj_interest)

classes_taken <- processed[, 13:30]
glimpse(classes_taken)

# Correlation heat map
cross <- cor(proj_interest, classes_taken, use = "pairwise.complete.obs")
dim(cross)

# Plot it
cross_f <- cross
cross_f[abs(cross_f) < 0.1] <- 0     # adjust the threshold
corrplot(cross_f, method = "color", is.corr = TRUE, number.cex = 0.6, tl.cex = 0.8)

# Barplots of number of each project interested in or class taken
barplot(sort(colSums(proj_interest), decreasing = TRUE), las = 2, cex.names = 0.7)
barplot(sort(colSums(classes_taken), decreasing = TRUE), las = 2, cex.names = 0.7)

table(rowSums(proj_interest))
table(rowSums(classes_taken))

# stats test
cross <- cor(proj_interest, classes_taken, use = "pairwise.complete.obs")
obs <- sum(cross^2, na.rm = TRUE)

set.seed(1)
B <- 5000
perm <- replicate(B, {
  shuf <- classes_taken[sample(nrow(classes_taken)), ]
  sum(cor(proj_interest, shuf, use = "pairwise.complete.obs")^2, na.rm = TRUE)
})

(sum(perm >= obs) + 1) / (B + 1)   # p-value
hist(perm); abline(v = obs, col = "red")

# p-value: 0.3537293

res <- expand.grid(x = names(proj_interest), y = names(classes_taken),
                   stringsAsFactors = FALSE)

res$p <- mapply(function(x, y) {
  tab <- table(factor(proj_interest[[x]], levels = c(0, 1)),
               factor(classes_taken[[y]], levels = c(0, 1)))
  fisher.test(tab)$p.value
}, res$x, res$y)

res$p_adj <- p.adjust(res$p, method = "BH")
res <- res[order(res$p), ]

top <- head(res, 10)
top[, c("p", "p_adj")] <- round(top[, c("p", "p_adj")], 3)
rownames(top) <- NULL
top

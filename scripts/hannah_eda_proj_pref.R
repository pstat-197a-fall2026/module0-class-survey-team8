# Download libraries
library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)

# Read and view dataset
df <- read_csv("data/background_interest_processed.csv")

head(df)
dim(df)
glimpse(df)

# Take columns
proj_interest <- df[, 31:56]
glimpse(proj_interest)

classes_taken <- df[, 13:30]
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



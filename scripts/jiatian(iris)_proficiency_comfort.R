# load data
data <- read.csv("background_interest_processed.csv")

# transform proficiency levels (beg/int/adv) into numbers 1, 2, 3
to_num <- function(x) ifelse(x == "beg", 1, ifelse(x == "int", 2, 3))
data$math.p <- to_num(data$math.prof)
data$stat.p <- to_num(data$stat.prof)
data$prog.p <- to_num(data$prog.prof)

# correlation (closer to 1 = they match)
cor(data$math.p, data$math.comf, method = "spearman")
cor(data$stat.p, data$stat.comf, method = "spearman")
cor(data$prog.p, data$prog.comf, method = "spearman")

# average gap per area (between proficiency and comfort)
# gap > 0: more comfortable than their stated proficiency (overestimate)
# gap < 0: less comfortable than their stated proficiency (underestimate)
data$math.gap <- (data$math.comf - 1) / 4 - (data$math.p - 1) / 2
data$stat.gap <- (data$stat.comf - 1) / 4 - (data$stat.p - 1) / 2
data$prog.gap <- (data$prog.comf - 1) / 4 - (data$prog.p - 1) / 2

mean(data$math.gap)
mean(data$stat.gap)
mean(data$prog.gap)

# count students who over/under-estimate
label <- function(gap) ifelse(gap > 0.25, "Over", ifelse(gap < -0.25, "Under", "Aligned"))
table(label(data$math.gap))
table(label(data$stat.gap))
table(label(data$prog.gap))

# plot for each area
par(mfrow = c(1, 3))
boxplot(math.comf ~ math.p, data = data, main = "Math",   xlab = "Proficiency (1-3)", ylab = "Comfort")
boxplot(stat.comf ~ stat.p, data = data, main = "Stats",  xlab = "Proficiency (1-3)", ylab = "Comfort")
boxplot(prog.comf ~ prog.p, data = data, main = "Coding", xlab = "Proficiency (1-3)", ylab = "Comfort")
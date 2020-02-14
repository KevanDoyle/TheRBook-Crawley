library(tidyverse)

comp <- read.table('./data/competition.txt', header = TRUE)
names(comp)
head(comp)
comp$clipping <- fct_relevel(comp$clipping,
                             c('control', 'n25', 'n50', 'r5', 'r10'))
# Make clipping an ordered factor
#comp$clipping <- factor(comp$clipping, ordered = TRUE)

# Break clipping up into 'n' and 'r' levels
comp$cliptype <- substr(as.character(comp$clipping),1,1)
comp$cliptype <- factor(comp$cliptype, c('c','n','r'))
comp$cliplevel <- substr(as.character(comp$clipping),2,3)
comp$cliplevel[comp$cliplevel == 'on'] <- '0'
comp$cliplevel <- factor(comp$cliplevel, c('0','5','10','25','50'),
                         ordered = FALSE)

# Calculate ranks in order to plot a Q-Q plot of all data as a
# single group but breaking into groups by color.
# The ggplot2 geom_qq() doesn't seem to fortify the plot with
# the quantile values and it also groups the quantiles once they
# are grouped by color and so plots five separate curves.
# I was interested in plotting all the data as a single group
# on a Q-Q plot but grouping the color by clipping to see
# if it showed anything. It does, but a boxplot is more
# informative and it shows the results more clearly.
#
# Oddly, percent_rank() and cume_dist() don't give the same ranks.
comp$pct_rank <- percent_rank(comp$biomass)
comp$cum_dist <- cume_dist(comp$biomass)
ggplot(comp, aes(x = cum_dist, y = pct_rank)) +
    geom_point() + geom_abline()

# Plots
# Line plot of clipping
comp %>% ggplot(aes(x = factor(c(1:nrow(comp))), y = clipping)) +
    geom_point()
# Line plot of biomass
comp %>% ggplot(aes(x = factor(c(1:nrow(comp))), y = biomass)) +
    geom_point()
# Q-Q plots
comp %>% ggplot(aes(sample = biomass)) +
    geom_qq() + geom_qq_line()
comp %>% ggplot(aes(sample = biomass, color = clipping)) +
    geom_qq() + geom_qq_line()
comp %>% ggplot(aes(sample = biomass)) +
    geom_qq(aes(color = clipping, group = 123)) + geom_qq_line()
# Q-Q Plot grouped by color but ignoring the group for the
# overall plot
comp %>% ggplot(aes(x = qnorm(cum_dist), y = biomass)) +
    geom_point(aes(color = clipping, shape = clipping)) +
    geom_smooth(method = 'lm', se = FALSE, group = 1)
# Boxplot
comp %>% ggplot(aes(y = biomass, x = cliptype, fill = cliplevel)) +
    geom_boxplot() + geom_point(position = position_jitterdodge())
# Histogram (actually, a density plot - the histogram doesn't
# look very informative)
comp %>% ggplot(aes(x = biomass, fill = clipping)) +
    geom_density(alpha = 0.5)

# Model biomass by clipping
model1 <- lm(biomass ~ clipping, data = comp)
summary(model1)
summary.lm(model1)
summary.aox(model1)
plot(model1)

# Model biomass by cliplevel and cliptype
model2 <- lm(biomass ~ cliplevel + cliptype, data = comp)
summary(model2)
summary.lm(model2)
summary.aov(model2)
plot(model2)


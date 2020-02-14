library(tidyverse)
yields <- read_tsv('./DataFiles/farms.txt')
yields %>% ggplot(aes(x = N, y = size)) +
    geom_point() + stat_smooth(method = 'lm', se = FALSE) +
    facet_wrap(vars(farm))
yields %>% ggplot(aes(x = N, y = size, color = factor(farm))) +
    geom_point() + stat_smooth(method = 'lm', se = FALSE)
yields %>% ggplot(aes(x = N, y = size, color = factor(farm))) +
    stat_smooth(method = 'lm', se = FALSE)
slopes <- lm(size~N+farm, yields)

# The distribution of slopes seems multi-modal with just a few modes.
library(nlme)
lmods <- lmList(size~N|farm, yields)
coefs <- coef(lmods)
coefs_norm <- coefs %>% ggplot(aes(sample = N)) + geom_qq()
coefs_norm + geom_qq_line(line.p = c(.50, .65)) +
    geom_qq_line(line.p = c(.35, .50))



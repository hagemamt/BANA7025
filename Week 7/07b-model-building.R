# prerequisites
library(tidyverse)
library(modelr)
library(hexbin)
options(na.action = na.warn)

##############
# the set-up #
##############
# why are low quality diamonds more expensive?
p1 <- ggplot(diamonds, aes(cut, price)) + geom_boxplot() + scale_y_continuous(labels = scales::dollar) + ggtitle("Price vs Cut")
p2 <- ggplot(diamonds, aes(color, price)) + geom_boxplot() + scale_y_continuous(labels = scales::dollar) + ggtitle("Price vs Color")
p3 <- ggplot(diamonds, aes(clarity, price)) + geom_boxplot() + scale_y_continuous(labels = scales::dollar) + ggtitle("Price vs Clarity")

gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

# spend a few minutes discussing the logic behind this with your neighbor


################################
# a major confounding variable #
################################
# carat has a big impact on price
ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50)

# how can we visualize this trend better?
ggplot(diamonds, aes(carat, price)) + 
  geom_hex(bins = 50) +
  scale_x_log10() +
  scale_y_log10()


######################
# Your Turn - Part 1 #
######################
# What is the strength of this linear trend? 
cor.test(diamonds$carat, diamonds$price)

# Does it differ depending on the different levels of cut, color, and clarity
diamonds %>% group_by(cut) %>% 
  summarise(corr = cor(carat, price))

diamonds %>% group_by(color) %>% 
  summarise(corr = cor(carat, price))

diamonds %>% group_by(clarity) %>% 
  summarise(corr = cor(carat, price))

diamonds %>% ggplot(aes(carat, price, color = cut)) + 
  geom_point() +
  facet_wrap(~ cut, nrow = 2)

diamonds %>% ggplot(aes(carat, price, color = color)) + 
  geom_point() +
  facet_wrap(~ color, nrow = 2)

diamonds %>% ggplot(aes(carat, price, color = clarity)) + 
  geom_point() +
  facet_wrap(~ clarity, nrow = 2)

# let's remove the price-carat relationship so we can see if there is still an
# unusual relationship between price and cut, color, and clarity

######################
# Your Turn - Part 2 #
######################
# 1. Fit a linear model between the price and carat variables
lm_carat_price <- lm(price ~ carat, data = diamonds)
# 2. Assess model numerically
summary(lm_carat_price)
# 3. Get prediction and residual data and add it to the diamonds data set
diamonds %>%
  add_predictions(lm_carat_price) %>%
  add_residuals(lm_carat_price)
# 4. Visually assess model predictions
diamonds %>%
  add_predictions(lm_carat_price) %>% 
  ggplot(aes(carat, price)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red", size = 1)
# 5. Visually assess model residuals
diamonds %>%
  add_residuals(lm_carat_price) %>% 
  ggplot(aes(carat, resid)) +
  geom_ref_line(h = 0) +
  geom_point()
# 6. Visually assess relationship between residuals and cut, color, clarity. What does this tell you?
diamonds %>%
  add_residuals(lm_carat_price) %>% 
  ggplot(aes(cut, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

diamonds %>%
  add_residuals(lm_carat_price) %>% 
  ggplot(aes(color, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

diamonds %>%
  add_residuals(lm_carat_price) %>% 
  ggplot(aes(clarity, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

############################
# building onto this model #
############################
# create a more complex model that incorporates cut, color, and clarity
diamonds3 <- diamonds %>% 
  select(price, carat, color, cut, clarity)

mod_diamond <- lm(log10(price) ~ log10(carat) + color + cut + clarity, 
                  data = diamonds3)

# how does this model appear to fit numerically
summary(mod_diamond)

# assess predictions
# example with cut
diamonds3 %>% 
  data_grid(cut, .model = mod_diamond) %>% 
  add_predictions(mod_diamond) %>%
  mutate(trans_pred = 10 ^ pred) %>%
  ggplot(aes(cut, trans_pred)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar)

# example with color
diamonds3 %>% 
  data_grid(color, .model = mod_diamond) %>% 
  add_predictions(mod_diamond) %>%
  mutate(trans_pred = 10 ^ pred) %>%
  ggplot(aes(color, trans_pred)) +
  geom_point()

# example with clarity
diamonds3 %>% 
  data_grid(clarity, .model = mod_diamond) %>% 
  add_predictions(mod_diamond) %>%
  mutate(trans_pred = 10 ^ pred) %>%
  ggplot(aes(clarity, trans_pred)) +
  geom_point()


#############
# Your Turn #
#############
# Lastly, how do the residuals look for this mod_diamond model?
diamonds %>%
  add_residuals(mod_diamond) %>% 
  ggplot(aes(price, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

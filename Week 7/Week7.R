library(tidyverse)
library(modelr) # demo data sets
options(na.action = na.warn)

##############
# Your Turn! #
##############
# assess the linear relationship between mpg and wt in the mtcars data set
cor.test(mtcars$mpg, mtcars$wt)

# how could you do this using dplyr (think summarise())?
mtcars %>% summarise(correlation = cor(mpg, wt))
cor.test(mtcars$mpg, mtcars$wt)

# how could you visually compare a linear vs non-linear relationship 
# (hint: geom_smooth)
mtcars %>% ggplot( aes(mpg, wt)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  geom_smooth(method = "gam", se = FALSE, color = "red") 


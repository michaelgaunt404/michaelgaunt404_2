

#bl
library(tidyverse)

df = mtcars

model = lm(mpg ~as.factor(vs) + as.factor(am) + wt , mtcars)

model %>% summary()


mtcars %>%
  ggplot() +
  geom_boxplot(aes(as.factor(vs), mpg))

mtcars %>%
  group_by(vs) %>%
  summarise(mean(mpg))

model = lm(mpg ~as.factor(vs), mtcars)
model %>% summary()

model = lm(mpg ~as.factor(vs)*wt, mtcars)
model %>% summary()

install.packages("interactions")
library(interactions)
fiti <- lm(mpg ~ hp * wt, data = mtcars)
sim_slopes(fiti, pred = mpg, modx = hp, jnplot = TRUE)

l
fitiris <- lm(Petal.Length ~ Species, data = iris)
summary(fitiris)
interact_plot(fitiris, pred = Petal.Width, modx = Species, plot.points = TRUE
              ,centered = "none")
summary(fitiris)

states <- as.data.frame(state.x77)
fiti <- lm(Income ~ Illiteracy * Murder + `HS Grad`, data = states)


iris %>%
  ggplot() +
  geom_boxplot(aes(Species, Petal.Length))

fiti <- lm(mpg ~ hp * wt, data = mtcars)


iris_df = iris %>%
  mutate(sp = ifelse(Sepal.Width>3.3, T, F))

iris_df %>%
  ggplot() +
  geom_boxplot(aes(Species, Petal.Length, fill = sp))

iris_df %>%
  group_by(Species, sp) %>%
  summarise(Petal.Length = mean(Petal.Length))
lm(Petal.Length ~ Species , data = iris_df) %>% summary()
lm(Petal.Length ~ Species*sp, data = iris_df) %>% summary()
fitiris = lm(Petal.Length ~ Species*sp, data = iris_df)
interact_plot(fitiris, pred = Petal.Length, modx = Species, plot.points = TRUE
              ,centered = "none")


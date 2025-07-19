# install.packages("dplyr")
library(dplyr)

iris # Flower (plant)

head(iris)

iris_subset <- iris %>%
  select(Sepal.Length, Species)

iris_setosa <- iris_subset %>%
  filter(Species == "setosa")

iris %>%
  group_by(Species) %>%
  summarize(
    mean_sl = mean(Sepal.Length)
  )
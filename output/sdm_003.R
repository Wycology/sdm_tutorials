
# Change quantification ---------------------------------------------------

library(sdm)

m <- read.sdm("output/m.sdm")

cur <- rast("output/current.tif")
fut <- rast("output/future.tif")

par(mfrow = c(1, 2))
plot(cur); plot(fut)

cur_pa <- pa(x = cur, y = m, id = "ensemble", opt = 2)
fut_pa <- pa(x = fut, y = m, id = "ensemble", opt = 2)

cur_pa <- as.factor(cur_pa)
levels(cur_pa) <- data.frame(value = c(0, 1),
                             label = c("Absence", 
                                       "Presence"))
fut_pa <- as.factor(fut_pa)
levels(fut_pa) <- data.frame(value = c(0, 1),
                             label = c("Absence", 
                                       "Presence"))

par(mfrow = c(1, 2))
plot(cur_pa, col = c("gray", "darkgreen"), main = "Current")
plot(fut_pa, col = c("gray", "darkgreen"), main = "Future")

# Change map --------------------------------------------------------------

ch <- rast(cur_pa)

ch[] <- ifelse(cur_pa[] == 0 & fut_pa[] == 0, 0, # Unsuitable
               ifelse(cur_pa[] == 0 & fut_pa[] == 1, 1, # Gain
                      ifelse(cur_pa[] == 1 & fut_pa[] == 1, 2, 3))) # Suitable, Loss

plot(ch)

# Alternatively -----------------------------------------------------------

code_raster <- 2 * cur_pa + fut_pa

plot(cur_pa); plot(fut_pa)

change <- subst(code_raster, from = c(0, 1, 2, 3),
                to = c(0, 1, 3, 2))

change <- as.factor(change)
levels(change) <- data.frame(value = c(0, 2, 3),
                             label = c("Unsuitable",
                                       "Suitable",
                                       "Loss"))
plot(change, col = c("gray", "darkgreen", "red"))

# Calculate area change ---------------------------------------------------

df_area <- expanse(change, unit = 'km', byValue = TRUE)

library(ggplot2)

df_area |> 
  ggplot(aes(x = reorder(value, -area), y = log(area), fill = value)) +
  geom_col() +
  geom_text(aes(label = round(log(area), 1)),
            vjust = -0.3,
            colour = 'black',
            size = 5,
            fontface = 'bold') +
  theme_minimal() +
  theme(
    axis.text = element_text(
      color = 'black',
      face = 'bold',
      size = 12
    ),
    axis.title = element_text(
      color = 'black',
      face = 'bold',
      size = 14
    ),
    plot.title = element_text(
      color = 'black',
      face = 'bold',
      size = 16,
      hjust = 0.5
    )
  ) +
  labs(x = "Change",
       y = "Area (log km²)",
       title = "Habitat Suitability Area Change") +
  scale_fill_manual(values = c(
    "Unsuitable" = "gray",
    "Suitable" = "darkgreen",
    "Loss" = "red"
  ))

library(sdm) # version 1.2.56

po <- vect(system.file("external/po_spatial_points.shp", 
                  package = "sdm"))
preds <- rast(system.file("external/predictors.tif", 
                          package = 'sdm'))

# Visualize ---------------------------------------------------------------

plot(preds[[1]]); plot(po, add = TRUE)

# Background --------------------------------------------------------------

b1 <- background(preds, n = nrow(po), method = "gRandom")
b2 <- background(preds, n = nrow(po), method = "eRandom")
b3 <- background(preds, n = nrow(po), method = "gDist", sp = po)
b4 <- background(preds, n = nrow(po), method = "eDist", sp = po)

# Visualization -----------------------------------------------------------

b1$bg_method <- "gRandom"
b2$bg_method <- "eRandom"
b3$bg_method <- "gDist"
b4$bg_method <- "eDist"

bg_all <- rbind(b1, b2, b3, b4)

class(po)

po_vals <- extract(preds, po, xy = TRUE)

po_vals$bg_method <- "Presence"

head(po_vals)

po_vals <- po_vals[, 2:6]

po_bg <- rbind(po_vals, bg_all)

library(ggplot2)

po_bg |> 
  ggplot(aes(x = bg_method, y = NDVI, 
             fill = ifelse(bg_method == "Presence", "Presence", "Other"))) +
  geom_boxplot(
    outlier.color = "blue",
    lwd = 0.8,
    aes(colour = after_scale("magenta"))
  ) +
  scale_fill_manual(
    values = c("Presence" = "green", 
               "Other" = "white"),
    guide = "none"
  ) +
  theme(
    axis.text = element_text(
      color = "black",
      face = "bold",
      size = 12
    ),
    axis.title = element_text(
      color = "black",
      face = "bold",
      size = 14
    ),
    text = element_text(color = "black")
  ) +
  labs(
    x = "Background Method",
    y = "NDVI by Method",
    title = "NDVI by Background Method"
  ) +
  theme(plot.title = element_text(
    color = "black",
    face = "bold",
    size = 16,
    hjust = 0.5
  ))

# Compare the models -----------------------------------------------------

d_eDist <- sdmData(sp4 ~., train = po, predictors = preds, bg = b4)
d_gRandom <- sdmData(sp4 ~., train = po, predictors = preds, bg = b1)

m_eDist <- sdm(sp4 ~., 
               data = d_eDist, 
               methods = 'maxent', 
               replication = "boot", 
               test.p = 30, 
               n = 3)

m_gRandom <- sdm(sp4 ~., 
               data = d_gRandom, 
               methods = 'maxent', 
               replication = "boot", 
               test.p = 30, 
               n = 3)

m_eDist
m_gRandom
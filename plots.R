#### set up ####
rm(list=ls()) # clear workspace
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/HerbVar/Null Model") # set working directory

# load packages
library(EnvStats)
library(DescTools)
library(ggplot2)
library(ggpubr)
library(patchwork)

# function to load an RData file with a new name
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# set up plot aesthetics
gg_options <- function() {
  theme_bw() + theme(
    panel.grid = element_blank(), # transparent grid lines
    strip.background = element_blank(), # transparent facet label bg
    panel.background = element_blank(), # transparent panel bg
    legend.key = element_blank(), # transparent legend bg
    plot.background = element_rect(fill='transparent', color = NA), #transparent plot bg
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'), # transparent legend area bg
    text = element_text(size = 15), # text size
    panel.border = element_blank(), # remove panel border
    axis.line = element_line(color = "black", linewidth = 0.4) # add axis lines
  )
}

#### plot means together ####

# summarize plant data
novar.small <- loadRData("data/leaf.level.variance/smallleaves.noleafvar.plants.RData")
smvar.small <- loadRData("data/leaf.level.variance/smallleaves.smleafvar.plants.RData")
bigvar.small <- loadRData("data/leaf.level.variance/smallleaves.bigleafvar.plants.RData")
novar.large <- loadRData("data/leaf.level.variance/largeleaves.noleafvar.plants.RData")
smvar.large <- loadRData("data/leaf.level.variance/largeleaves.smleafvar.plants.RData")
bigvar.large <- loadRData("data/leaf.level.variance/largeleaves.bigleafvar.plants.RData")

novar.large.means <- aggregate(novar.large$pct.dam.gini, by = list(novar.large$mean.damage), mean)
names(novar.large.means) <- c("mean.damage", "mean.gini")

novar.large.means$gini.quant25 <- aggregate(novar.large$pct.dam.gini, by = list(novar.large$mean.damage), FUN = quantile, probs = 0.25)$x
novar.large.means$gini.quant75 <- aggregate(novar.large$pct.dam.gini, by = list(novar.large$mean.damage), FUN = quantile, probs = 0.75)$x

novar.large.means$mean.pct.damage <- aggregate(novar.large$pct.damage, by = list(novar.large$mean.damage), FUN = mean)$x
novar.large.means$damage.quant05 <- aggregate(novar.large$pct.damage, by = list(novar.large$mean.damage), FUN = quantile, probs = 0.05)$x
novar.large.means$damage.quant95 <- aggregate(novar.large$pct.damage, by = list(novar.large$mean.damage), FUN = quantile, probs = 0.95)$x

novar.large.means$mean <- rep("Large", nrow(novar.large.means))
novar.large.means$sd <- rep("None", nrow(novar.large.means))

smvar.large.means <- aggregate(smvar.large$pct.dam.gini, by = list(smvar.large$mean.damage), mean)
names(smvar.large.means) <- c("mean.damage", "mean.gini")

smvar.large.means$gini.quant25 <- aggregate(smvar.large$pct.dam.gini, by = list(smvar.large$mean.damage), FUN = quantile, probs = 0.25)$x
smvar.large.means$gini.quant75 <- aggregate(smvar.large$pct.dam.gini, by = list(smvar.large$mean.damage), FUN = quantile, probs = 0.75)$x

smvar.large.means$mean.pct.damage <- aggregate(smvar.large$pct.damage, by = list(smvar.large$mean.damage), FUN = mean)$x
smvar.large.means$damage.quant05 <- aggregate(smvar.large$pct.damage, by = list(smvar.large$mean.damage), FUN = quantile, probs = 0.05)$x
smvar.large.means$damage.quant95 <- aggregate(smvar.large$pct.damage, by = list(smvar.large$mean.damage), FUN = quantile, probs = 0.95)$x

smvar.large.means$mean <- rep("Large", nrow(smvar.large.means))
smvar.large.means$sd <- rep("Small", nrow(smvar.large.means))

bigvar.large.means <- aggregate(bigvar.large$pct.dam.gini, by = list(bigvar.large$mean.damage), mean, na.rm = T)
names(bigvar.large.means) <- c("mean.damage", "mean.gini")

bigvar.large.means$gini.quant25 <- aggregate(bigvar.large$pct.dam.gini, by = list(bigvar.large$mean.damage), FUN = quantile, probs = 0.25, na.rm = T)$x
bigvar.large.means$gini.quant75 <- aggregate(bigvar.large$pct.dam.gini, by = list(bigvar.large$mean.damage), FUN = quantile, probs = 0.75, na.rm = T)$x

bigvar.large.means$mean.pct.damage <- aggregate(bigvar.large$pct.damage, by = list(bigvar.large$mean.damage), FUN = mean)$x
bigvar.large.means$damage.quant05 <- aggregate(bigvar.large$pct.damage, by = list(bigvar.large$mean.damage), FUN = quantile, probs = 0.05)$x
bigvar.large.means$damage.quant95 <- aggregate(bigvar.large$pct.damage, by = list(bigvar.large$mean.damage), FUN = quantile, probs = 0.95)$x

bigvar.large.means$mean <- rep("Large", nrow(bigvar.large.means))
bigvar.large.means$sd <- rep("Large", nrow(bigvar.large.means))

novar.small.means <- aggregate(novar.small$pct.dam.gini, by = list(novar.small$mean.damage), mean, na.rm = T)
names(novar.small.means) <- c("mean.damage", "mean.gini")

novar.small.means$gini.quant25 <- aggregate(novar.small$pct.dam.gini, by = list(novar.small$mean.damage), FUN = quantile, probs = 0.25, na.rm = T)$x
novar.small.means$gini.quant75 <- aggregate(novar.small$pct.dam.gini, by = list(novar.small$mean.damage), FUN = quantile, probs = 0.75, na.rm = T)$x

novar.small.means$mean.pct.damage <- aggregate(novar.small$pct.damage, by = list(novar.small$mean.damage), FUN = mean, na.rm = T)$x
novar.small.means$damage.quant05 <- aggregate(novar.small$pct.damage, by = list(novar.small$mean.damage), FUN = quantile, probs = 0.05, na.rm = T)$x
novar.small.means$damage.quant95 <- aggregate(novar.small$pct.damage, by = list(novar.small$mean.damage), FUN = quantile, probs = 0.95, na.rm = T)$x

novar.small.means$mean <- rep("Small", nrow(novar.small.means))
novar.small.means$sd <- rep("None", nrow(novar.small.means))

smvar.small.means <- aggregate(smvar.small$pct.dam.gini, by = list(smvar.small$mean.damage), mean, na.rm = T)
names(smvar.small.means) <- c("mean.damage", "mean.gini")

smvar.small.means$gini.quant25 <- aggregate(smvar.small$pct.dam.gini, by = list(smvar.small$mean.damage), FUN = quantile, probs = 0.25, na.rm = T)$x
smvar.small.means$gini.quant75 <- aggregate(smvar.small$pct.dam.gini, by = list(smvar.small$mean.damage), FUN = quantile, probs = 0.75, na.rm = T)$x

smvar.small.means$mean.pct.damage <- aggregate(smvar.small$pct.damage, by = list(smvar.small$mean.damage), FUN = mean, na.rm = T)$x
smvar.small.means$damage.quant05 <- aggregate(smvar.small$pct.damage, by = list(smvar.small$mean.damage), FUN = quantile, probs = 0.05, na.rm = T)$x
smvar.small.means$damage.quant95 <- aggregate(smvar.small$pct.damage, by = list(smvar.small$mean.damage), FUN = quantile, probs = 0.95, na.rm = T)$x

smvar.small.means$mean <- rep("Small", nrow(smvar.small.means))
smvar.small.means$sd <- rep("Small", nrow(smvar.small.means))

bigvar.small.means <- aggregate(bigvar.small$pct.dam.gini, by = list(bigvar.small$mean.damage), mean, na.rm = T)
names(bigvar.small.means) <- c("mean.damage", "mean.gini")

bigvar.small.means$gini.quant25 <- aggregate(bigvar.small$pct.dam.gini, by = list(bigvar.small$mean.damage), FUN = quantile, probs = 0.25, na.rm = T)$x
bigvar.small.means$gini.quant75 <- aggregate(bigvar.small$pct.dam.gini, by = list(bigvar.small$mean.damage), FUN = quantile, probs = 0.75, na.rm = T)$x

bigvar.small.means$mean.pct.damage <- aggregate(bigvar.small$pct.damage, by = list(bigvar.small$mean.damage), FUN = mean)$x
bigvar.small.means$damage.quant05 <- aggregate(bigvar.small$pct.damage, by = list(bigvar.small$mean.damage), FUN = quantile, probs = 0.05)$x
bigvar.small.means$damage.quant95 <- aggregate(bigvar.small$pct.damage, by = list(bigvar.small$mean.damage), FUN = quantile, probs = 0.95)$x

bigvar.small.means$mean <- rep("Small", nrow(bigvar.small.means))
bigvar.small.means$sd <- rep("Large", nrow(bigvar.small.means))


mean.sizes <- rbind(novar.small.means, smvar.small.means, bigvar.small.means,novar.large.means, smvar.large.means, bigvar.large.means)

gini.plots <- ggplot(data = mean.sizes, aes(color = sd, fill = sd)) +
  geom_line(aes(x = mean.damage, y = mean.gini), key_glyph = "path") +
  geom_ribbon(aes(x = mean.damage, ymin = gini.quant25, ymax = gini.quant75), alpha = 0.25, linetype = "dashed", key_glyph = "path") +
  facet_grid(~factor(mean, levels = c("Small", "Large")), labeller = as_labeller(c("Small" = "Small Leaves", "Large" = "Large Leaves"))) +
  scale_color_manual(values = c("lightskyblue", "darkseagreen","lightsalmon2"), breaks = c("None", "Small", "Large")) +
  scale_fill_manual(values = c("lightskyblue", "darkseagreen","lightsalmon2"), breaks = c("None", "Small", "Large")) +
  xlab("Population Percent Damage") +
  ylab("Mean Gini Coefficient") +
  labs(color = "Leaf Size Variation", fill = "Leaf Size Variation") +
  xlim(c(0.005,0.15)) +
  ylim(c(0,1)) +
  gg_options() +
  theme(strip.background = element_rect(fill = "grey"), aspect.ratio = 1, panel.spacing = unit(2, "lines")) +
  rremove("xlab") + rremove("x.text")

dam.plots <- ggplot(data = mean.sizes, aes(color = sd, fill = sd)) +
  geom_line(aes(x = mean.damage, y = mean.pct.damage), key_glyph = "path") +
  geom_ribbon(aes(x = mean.damage, ymin = damage.quant05, ymax = damage.quant95), alpha = 0.25, linetype = "dashed", key_glyph = "path") +
  facet_grid(~factor(mean, levels = c("Small", "Large"))) +
  scale_color_manual(values = c("lightskyblue", "darkseagreen","lightsalmon2"), breaks = c("None", "Small", "Large")) +
  scale_fill_manual(values = c("lightskyblue", "darkseagreen","lightsalmon2"), breaks = c("None", "Small", "Large")) +
  xlab("Population Percent Damage") +
  ylab("Mean Plant Damage") +
  labs(color = "Leaf Size Variation", fill = "Leaf Size Variation") +
  xlim(c(0.005,0.15)) +
  gg_options() +
  theme(strip.text = element_blank(), aspect.ratio = 1, panel.spacing = unit(2, "lines"))

gini.plots + dam.plots + theme(legend.position = "right") + plot_layout(nrow = 2, guides = "collect")

#### panels of pop level box plots (ginis between plants) ####
novar.small <- loadRData("data/var.bites/smallleaves.noleafvar.pops.RData")
smvar.small <- loadRData("data/var.bites/smallleaves.smleafvar.pops.RData")
bigvar.small <- loadRData("data/var.bites/smallleaves.bigleafvar.pops.RData")
novar.large <- loadRData("data/var.bites/largeleaves.noleafvar.pops.RData")
smvar.large <- loadRData("data/var.bites/largeleaves.smleafvar.pops.RData")
bigvar.large <- loadRData("data/var.bites/largeleaves.bigleafvar.pops.RData")

novar.small$mean <- rep("Small Leaves", nrow(novar.small))
novar.small$sd <- rep("No Variation", nrow(novar.small))

smvar.small$mean <- rep("Small Leaves", nrow(smvar.small))
smvar.small$sd <- rep("Small Variation", nrow(smvar.small))

bigvar.small$mean <- rep("Small Leaves", nrow(bigvar.small))
bigvar.small$sd <- rep("Large Variation", nrow(bigvar.small))

novar.large$mean <- rep("Large Leaves", nrow(novar.small))
novar.large$sd <- rep("No Variation", nrow(novar.small))

smvar.large$mean <- rep("Large Leaves", nrow(smvar.small))
smvar.large$sd <- rep("Small Variation", nrow(smvar.small))

bigvar.large$mean <- rep("Large Leaves", nrow(bigvar.small))
bigvar.large$sd <- rep("Large Variation", nrow(bigvar.small))

all.data = rbind(novar.small, smvar.small, bigvar.small, novar.large, smvar.large, bigvar.large)

biome.data <- data.frame(biome.id = c("Boreal", "Desert", "Mediterranean", "Montane", "Temp.Broadleaf", "Temp.Conifer", "Temp.Grassland", "Trop.Dry", "Trop.Grassland", "Trop.Moist", "Tundra"),
                         mean.herb = c(0.02506431, 0.05627979, 0.04533952, 0.03794149, 0.03881102, 0.04394191, 0.03219492, 0.05298976, 0.04980792, 0.06472561, 0.03068827),
                         gini = c(0.7077831, 0.4934214, 0.5867222, 0.6749893, 0.6267859, 0.6059515, 0.6594645, 0.4792675, 0.4543182, 0.5725685, 0.7176359))

ggplot(data = all.data) +
  geom_boxplot(aes(x = mean.damage, y = pct.dam.gini, group = mean.damage)) +
  geom_point(data = biome.data, aes(x = mean.herb, y = gini, color = biome.id), size = 2) +
  # scale_color_hue(labels = c("Boreal Forests/Taiga", "Deserts & Xeric Shrublands", "Mediterranean Forests, Woodlands, & Scrub", "Montane Grasslands & Shrublands",
  #                            "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Temerate Grasslands, Savannas, & Shrublands",
  #                            "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Grasslands, Savannas, & Shrublands",
  #                            "Tropical & Subtropical Moist Broadleaf Forests", "Tundra")) +
  # labs(color = "Biome") +
  facet_grid(factor(mean, levels = c("Small Leaves", "Large Leaves"))~factor(sd, levels = c("No Variation", "Small Variation", "Large Variation"))) +
  theme_bw() +
  theme(panel.spacing = unit(2, "lines"), text = element_text(size = 15)) +
  xlab("Population Percent Damage") +
  ylab("Gini Coefficient")

ggplot(data = all.data) +
  geom_boxplot(aes(x = mean.damage, y = avg.dam.gini, group = mean.damage)) +
  # geom_point(data = biome.data, aes(x = mean.herb, y = gini, color = biome.id), size = 2) +
  facet_grid(factor(mean, levels = c("Small Leaves", "Large Leaves"))~factor(sd, levels = c("No Variation", "Small Variation", "Large Variation"))) +
  theme_bw() +
  theme(panel.spacing = unit(2, "lines"), text = element_text(size = 15)) +
  xlab("Population Percent Damage") +
  ylab("Gini Coefficient")

ggplot(data = all.data) +
  geom_boxplot(aes(x = mean.damage, y = no.dam.leaves, group = mean.damage)) +
  facet_grid(factor(mean, levels = c("Small Leaves", "Large Leaves"))~factor(sd, levels = c("No Variation", "Small Variation", "Large Variation"))) +
  theme_bw() + 
  ylab("Proportion of Leaves Without Damage") +
  xlab("Proportion of Population Damaged")

ggplot(data = all.data) +
  geom_boxplot(aes(x = mean.damage, y = low.dam.leaves, group = mean.damage)) +
  facet_grid(factor(mean, levels = c("Small Leaves", "Large Leaves"))~factor(sd, levels = c("No Variation", "Small Variation", "Large Variation"))) +
  theme_bw() +  
  ylab("Proportion of Leaves With Less Than 5% Damage") +
  xlab("Proportion of Population Damaged")

ggplot(data = all.data) +
  geom_boxplot(aes(x = mean.damage, y = no.dam.plants, group = mean.damage)) +
  facet_grid(factor(mean, levels = c("Small Leaves", "Large Leaves"))~factor(sd, levels = c("No Variation", "Small Variation", "Large Variation"))) +
  theme_bw() +  
  ylab("Proportion of Plants Without Damage") +
  xlab("Proportion of Population Damaged")

ggplot(data = all.data) +
  geom_boxplot(aes(x = mean.damage, y = low.dam.plants, group = mean.damage)) +
  facet_grid(factor(mean, levels = c("Small Leaves", "Large Leaves"))~factor(sd, levels = c("No Variation", "Small Variation", "Large Variation"))) +
  theme_bw() +  
  ylab("Proportion of Plants With Less Than 5% Average Damage") +
  xlab("Proportion of Population Damaged")



#### panels of plant level box plots (avg damage between plants) ####
novar.small <- loadRData("data/sm.plant.level.variance/smallleaves.noplantvar.plants.RData")
smvar.small <- loadRData("data/sm.plant.level.variance/smallleaves.smplantvar.plants.RData")
bigvar.small <- loadRData("data/sm.plant.level.variance/smallleaves.bigplantvar.plants.RData")
novar.large <- loadRData("data/sm.plant.level.variance/largeleaves.noplantvar.plants.RData")
smvar.large <- loadRData("data/sm.plant.level.variance/largeleaves.smplantvar.plants.RData")
bigvar.large <- loadRData("data/sm.plant.level.variance/largeleaves.bigplantvar.plants.RData")

novar.small$mean <- rep("Small Leaves", nrow(novar.small))
novar.small$sd <- rep("No Variation", nrow(novar.small))

smvar.small$mean <- rep("Small Leaves", nrow(smvar.small))
smvar.small$sd <- rep("Small Variation", nrow(smvar.small))

bigvar.small$mean <- rep("Small Leaves", nrow(bigvar.small))
bigvar.small$sd <- rep("Large Variation", nrow(bigvar.small))

novar.large$mean <- rep("Large Leaves", nrow(novar.small))
novar.large$sd <- rep("No Variation", nrow(novar.small))

smvar.large$mean <- rep("Large Leaves", nrow(smvar.small))
smvar.large$sd <- rep("Small Variation", nrow(smvar.small))

bigvar.large$mean <- rep("Large Leaves", nrow(bigvar.small))
bigvar.large$sd <- rep("Large Variation", nrow(bigvar.small))

all.data = rbind(novar.small, smvar.small, bigvar.small, novar.large, smvar.large, bigvar.large)
ggplot(data = all.data) +
  geom_boxplot(aes(x = mean.damage, y = avg.dam, group = mean.damage)) +
  facet_grid(factor(mean, levels = c("Small Leaves", "Large Leaves"))~factor(sd, levels = c("No Variation", "Small Variation", "Large Variation")), scales = "free_y")





#### comparing 3 hypotheses ####
null0.small <- loadRData("data/leaf.level.variance/smallleaves.noleafvar.pops.RData")
nullb.small <- loadRData("data/leaf.level.variance/smallleaves.bigleafvar.pops.RData")
nulln.small <- loadRData("data/var.bites/smallleaves.noleafvar.pops.RData")
null0.large <- loadRData("data/leaf.level.variance/largeleaves.noleafvar.pops.RData")
nullb.large <- loadRData("data/leaf.level.variance/largeleaves.bigleafvar.pops.RData")
nulln.large <- loadRData("data/var.bites/largeleaves.noleafvar.pops.RData")

null0.small$mean <- rep("Small Leaves", nrow(null0.small))
null0.small$sd <- rep("null_0", nrow(null0.small))

nullb.small$mean <- rep("Small Leaves", nrow(nullb.small))
nullb.small$sd <- rep("null_B", nrow(nullb.small))

nulln.small$mean <- rep("Small Leaves", nrow(nulln.small))
nulln.small$sd <- rep("null_N", nrow(nulln.small))

null0.large$mean <- rep("Large Leaves", nrow(null0.large))
null0.large$sd <- rep("null_0", nrow(null0.large))

nullb.large$mean <- rep("Large Leaves", nrow(nullb.large))
nullb.large$sd <- rep("null_B", nrow(nullb.large))

nulln.large$mean <- rep("Large Leaves", nrow(nulln.large))
nulln.large$sd <- rep("null_N", nrow(nulln.large))

all.data = rbind(null0.small, nullb.small, nulln.small, null0.large, nullb.large, nulln.large)

ggplot(data = all.data) +
  geom_boxplot(aes(x = mean.damage, y = pct.dam.gini, group = mean.damage)) +
  facet_grid(factor(mean, levels = c("Small Leaves", "Large Leaves"))~factor(sd, levels = c("null_0", "null_B", "null_N"))) +
  theme_bw() + 
  ylab("Gini Coefficient") +
  xlab("Proportion of Population Damaged")



#### all the fun plotting options ####
ggplot(data = plant.data) +
  # geom_violin(aes(x = mean.damage, y = pct.dam.gini, group = mean.damage), fill = "grey") +
  # ggforce::geom_sina(data = plant.data, aes(x = mean.damage, y = pct.dam.gini, group = mean.damage),alpha = 0.7,fill = "gray95",color = "gray55",shape=21, size = 0.4) +
  geom_boxplot(aes(x = mean.damage, y = pct.dam.gini, group = mean.damage), outliers = T) +
  # geom_jitter(aes(x = mean.damage, y = pct.dam.gini, group = mean.damage), alpha = 0.1, size = 0.4) +
  # gghalves::geom_half_boxplot(data = plant.data, aes(x = mean.damage, y = pct.dam.gini, group = mean.damage), errorbar.draw = F) +
  # gghalves::geom_half_violin(data = plant.data, aes(x = mean.damage, y = pct.dam.gini, group = mean.damage), side = "r") +
  geom_point(data = biome.data, aes(x = mean.herb, y = gini, color = biome.id), size = 4) +
  scale_color_hue(labels = c("Boreal Forests/Taiga", "Deserts & Xeric Shrublands", "Mediterranean Forests, Woodlands, & Scrub", "Montane Grasslands & Shrublands",
                             "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Temerate Grasslands, Savannas, & Shrublands",
                             "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Grasslands, Savannas, & Shrublands",
                             "Tropical & Subtropical Moist Broadleaf Forests", "Tundra")) +
  labs(color = "Biome") +
  xlab("Mean Percent Damage") +
  ylab("Gini Coefficient") +
  xlim(c(0.005,0.155)) +
  ylim(c(0,1)) +
  gg_options() +
  theme(legend.position = "inside", legend.position.inside = c(0.7,0.75))
# theme(legend.position = "none")


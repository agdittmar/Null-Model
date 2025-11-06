#### set up ####
rm(list=ls()) # clear workspace
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/HerbVar/Null Model") # set working directory

# function to load an RData file with a new name
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

#### create analytical data ####
b <- 100
l <- 10
p <- 60
n <- seq(0.01, 0.15, by = 0.01) * b * l * p
h = n / (l * p)

null0.leaf.var <- (h * (b - h) * (l * p - 1)) / (b^2 * (b * l * p - 1))
null0.plant.var <- (h * (b - h) * (l * p - 1)) / (b^2 * l * (b * l * p - 1))
null0.pop.var <- (h * (b - h) * (l * p - 1)) / (b^2 * l * p * (b * l * p - 1))

nullb.leaf.var <- (h * (b - h) * (l * p - 1 + l * p / b)) / (b^2 * (b * l * p - 1))
nullb.plant.var <- (h * (b - h) * (l * p - 1 + l * p / b)) / (b^2 * l * (b * l * p - 1))
nullb.pop.var <- (h * (b - h) * (l * p - 1 + l * p / b)) / (b^2 * l * p * (b * l * p - 1))

nulln.leaf.var <- (h * ((b - h) * (l * p - 1) + b - h - 1)) / (b^2 * (b * l * p - 1))
nulln.plant.var <- (h * ((b - h) * (l * p - 1) + b - h - 1)) / (b^2 * l * (b * l * p - 1))
nulln.pop.var <- (h * ((b - h) * (l * p - 1) + b - h - 1)) / (b^2 * l * p * (b * l * p - 1))

ana.mean <- h / b


#### clean simulation data ####
# null_0 data
novar.leaf.data <- loadRData("data/leaf.level.variance/largeleaves.noleafvar.RData")

novar.plant.data <- aggregate(novar.leaf.data$pct.damage, by = list(novar.leaf.data$dam.mean, novar.leaf.data$plant.id, novar.leaf.data$pop.id, novar.leaf.data$run.number), mean)
names(novar.plant.data) <- c("dam.mean", "plant.id", "pop.id", "run.id", "pct.damage")

novar.pop.data <- aggregate(novar.plant.data$pct.damage, by = list(novar.plant.data$dam.mean, novar.plant.data$pop.id, novar.plant.data$run.id), mean)
names(novar.pop.data) <- c("dam.mean", "pop.id","run.id", "pct.damage")

null0.leaf.data <- aggregate(novar.leaf.data$pct.damage, by = list(novar.leaf.data$dam.mean), var)
names(null0.leaf.data) <- c("pop.damage", "variance")
null0.leaf.data$mean <- aggregate(novar.leaf.data$pct.damage, by = list(novar.leaf.data$dam.mean), mean)$x

null0.plant.data <- aggregate(novar.plant.data$pct.damage, by = list(novar.plant.data$dam.mean), var)
names(null0.plant.data) <- c("pop.damage", "variance")
null0.plant.data$mean <- aggregate(novar.plant.data$pct.damage, by = list(novar.plant.data$dam.mean), mean)$x

null0.pop.data <- aggregate(novar.pop.data$pct.damage, by = list(novar.pop.data$dam.mean), var)
names(null0.pop.data) <- c("pop.damage", "variance")
null0.pop.data$mean <- aggregate(novar.pop.data$pct.damage, by = list(novar.pop.data$dam.mean), mean)$x

# null_b data
smvar.leaf.data <- loadRData("data/leaf.level.variance/largeleaves.smleafvar.RData")

smvar.plant.data <- aggregate(smvar.leaf.data$pct.damage, by = list(smvar.leaf.data$dam.mean, smvar.leaf.data$plant.id, smvar.leaf.data$pop.id, smvar.leaf.data$run.number), mean)
names(smvar.plant.data) <- c("dam.mean", "plant.id", "pop.id", "run.id", "pct.damage")

smvar.pop.data <- aggregate(smvar.plant.data$pct.damage, by = list(smvar.plant.data$dam.mean, smvar.plant.data$pop.id, smvar.plant.data$run.id), mean)
names(smvar.pop.data) <- c("dam.mean", "pop.id","run.id", "pct.damage")

nullb.leaf.data <- aggregate(smvar.leaf.data$pct.damage, by = list(smvar.leaf.data$dam.mean), var)
names(nullb.leaf.data) <- c("pop.damage", "variance")
nullb.leaf.data$mean <- aggregate(smvar.leaf.data$pct.damage, by = list(smvar.leaf.data$dam.mean), mean)$x

nullb.plant.data <- aggregate(smvar.plant.data$pct.damage, by = list(smvar.plant.data$dam.mean), var)
names(nullb.plant.data) <- c("pop.damage", "variance")
nullb.plant.data$mean <- aggregate(smvar.plant.data$pct.damage, by = list(smvar.plant.data$dam.mean), mean)$x

nullb.pop.data <- aggregate(smvar.pop.data$pct.damage, by = list(smvar.pop.data$dam.mean), var)
names(nullb.pop.data) <- c("pop.damage", "variance")
nullb.pop.data$mean <- aggregate(smvar.pop.data$pct.damage, by = list(smvar.pop.data$dam.mean), mean)$x


# null_n data
bitevar.leaf.data <- loadRData("data/var.bites/largeleaves.noleafvar.RData")

bitevar.plant.data <- aggregate(bitevar.leaf.data$pct.damage, by = list(bitevar.leaf.data$dam.mean, bitevar.leaf.data$plant.id, bitevar.leaf.data$pop.id, bitevar.leaf.data$run.number), mean)
names(bitevar.plant.data) <- c("dam.mean", "plant.id", "pop.id", "run.id", "pct.damage")

bitevar.pop.data <- aggregate(bitevar.plant.data$pct.damage, by = list(bitevar.plant.data$dam.mean, bitevar.plant.data$pop.id, bitevar.plant.data$run.id), mean)
names(bitevar.pop.data) <- c("dam.mean", "pop.id","run.id", "pct.damage")

nulln.leaf.data <- aggregate(bitevar.leaf.data$pct.damage, by = list(bitevar.leaf.data$dam.mean), var)
names(nulln.leaf.data) <- c("pop.damage", "variance")
nulln.leaf.data$mean <- aggregate(bitevar.leaf.data$pct.damage, by = list(bitevar.leaf.data$dam.mean), mean)$x

nulln.plant.data <- aggregate(bitevar.plant.data$pct.damage, by = list(bitevar.plant.data$dam.mean), var)
names(nulln.plant.data) <- c("pop.damage", "variance")
nulln.plant.data$mean <- aggregate(bitevar.plant.data$pct.damage, by = list(bitevar.plant.data$dam.mean), mean)$x

nulln.pop.data <- aggregate(bitevar.pop.data$pct.damage, by = list(bitevar.pop.data$dam.mean), var)
names(nulln.pop.data) <- c("pop.damage", "variance")
nulln.pop.data$mean <- aggregate(bitevar.pop.data$pct.damage, by = list(bitevar.pop.data$dam.mean), mean)$x



#### plot ####
plot(null0.leaf.data$variance, null0.leaf.var, xlim = c(0,0.0015), ylim = c(0,0.0015),
     xlab = "Simulated Variance Between Leaves", ylab = "Expected Variance Between Leaves")
abline(0,1)
plot(null0.plant.data$variance, null0.plant.var, xlim = c(0,0.00015), ylim = c(0,0.00015),
     xlab = "Simulated Variance Between Plants", ylab = "Expected Variance Between Plants")
abline(0,1)

# plot(nullb.leaf.data$variance, nullb.leaf.var, xlim = c(0, 0.0026), ylim = c(0, 0.0026),
#      xlab = "Simulated Variance Between Leaves", ylab = "Expected Variance Between Leaves")
# abline(0,1)
# plot(nullb.plant.data$variance, nullb.plant.var, xlim = c(0, 0.00026), ylim = c(0, 0.00026),
#      xlab = "Simulated Variance Between Plants", ylab = "Expected Variance Between Plants")
# abline(0,1)

plot(nulln.leaf.data$variance, nulln.leaf.var, xlim = c(0, 0.0015), ylim = c(0, 0.0015),
     xlab = "Simulated Variance Between Leaves", ylab = "Expected Variance Between Leaves")
abline(0,1)
plot(nulln.plant.data$variance, nulln.plant.var, xlim = c(0, 0.00015), ylim = c(0, 0.00015),
     xlab = "Simulated Variance Between Plants", ylab = "Expected Variance Between Plants")
abline(0,1)


par(mfrow = c(2,2))

plot(null0.leaf.data$pop.damage, ana.mean - null0.leaf.data$mean, xlab = "Proportion of Population Damaged", ylab = "Difference Between Simulated and Expected Means", main = "Leaf-level")
abline(0,0, lty = 2)
plot(null0.plant.data$pop.damage, ana.mean - null0.plant.data$mean, xlab = "Proportion of Population Damaged", ylab = "Difference Between Simulated and Expected Means", main = "Plant-level")
abline(0,0, lty = 2)

plot(null0.leaf.data$pop.damage, null0.leaf.var - null0.leaf.data$variance, xlab = "Proportion of Population Damaged", ylab = "Difference Between Simulated and Expected Variance")
abline(0,0, lty = 2)
plot(null0.plant.data$pop.damage, null0.plant.var - null0.plant.data$variance, xlab = "Proportion of Population Damaged", ylab = "Difference Between Simulated and Expected Variance")
abline(0,0, lty = 2)


plot(nulln.leaf.data$pop.damage, ana.mean - nulln.leaf.data$mean, xlab = "Proportion of Population Damaged", ylab = "Difference Between Simulated and Expected Means", main = "Leaf-level")
abline(0,0, lty = 2)
plot(nulln.plant.data$pop.damage, ana.mean - nulln.plant.data$mean, xlab = "Proportion of Population Damaged", ylab = "Difference Between Simulated and Expected Means", main = "Plant-level")
abline(0,0, lty = 2)

plot(nulln.leaf.data$pop.damage, nulln.leaf.var - nulln.leaf.data$variance, xlab = "Proportion of Population Damaged", ylab = "Difference Between Simulated and Expected Variance")
abline(0,0, lty = 2)
plot(nulln.plant.data$pop.damage, nulln.plant.var - nulln.plant.data$variance, xlab = "Proportion of Population Damaged", ylab = "Difference Between Simulated and Expected Variance")
abline(0,0, lty = 2)



#### histograms ####
load("data/leaf.level.variance/largeleaves.noleafvar.RData")
load("data/leaf.level.variance/largeleaves.noleafvar.plants.RData")

b <- 100
l <- 10
p <- 60
n <- 0.15 * b * l * p
h = n / (l * p)

hyper = dhyper(x = seq(0,36,by=1), m = b, n = b * l * p - b, k = n)

histogram <- hist(subset(leaf.data, dam.mean == 0.15)$pct.damage, main = "", xlab = "Percent Damage (leaf)", breaks = seq(0,0.36,by=0.01), freq = F, ylim = c(0,12))
lines(x = seq(-0.005,0.355,by=0.01), y = hyper / diff(histogram$mids[1:2]))

norm <- dnorm(x = seq(0.1,0.22, by = 0.001), mean = h / b, sd = sqrt(h * (b - h) * (l * p - 1) / (b ^ 2 * l * (b * l * p - 1))))

histogram <- hist(subset(plant.data, mean.damage == 0.15)$pct.damage, main = "", xlab = "Average Percent Damage (plant)", breaks = seq(0.1,0.22,by=0.005), freq = F, ylim = c(0,40))
lines(x = seq(0.1,0.22,by=0.001), y = norm)

load("data/var.bites/largeleaves.noleafvar.RData")
load("data/var.bites/largeleaves.noleafvar.plants.RData")

hyper = dhyper(x = seq(0,35,by=1), m = b, n = b * l * p - b, k = n)
histogram <- hist(subset(leaf.data, dam.mean == 0.15)$pct.damage, main = "", xlab = "Percent Damage (leaf)", breaks = seq(0,0.35,by=0.01), freq = F, ylim = c(0,12))
lines(x = seq(-0.005,0.345,by=0.01), y = hyper / diff(histogram$mids[1:2]) )

norm <- dnorm(x = seq(0.1, 0.2, by = 0.001), mean = h / b, sd = sqrt((h * ((b - h) * (l * p - 1) + b + h - 1)) / (b ^ 2 * l * (b * l * p - 1))))
histogram <- hist(subset(plant.data, mean.damage == 0.15)$pct.damage, main = "", xlab = "Average Percent Damage (plant)", breaks = seq(0.1,0.2, by = 0.005), freq = F, ylim = c(0,40))
lines(x = seq(0.1,0.2,by = 0.001), y = norm)


# histograms together
null.leaves <- loadRData("data/leaf.level.variance/largeleaves.noleafvar.RData")
bites.leaves <- loadRData("data/var.bites/largeleaves.noleafvar.RData")

hyper <- dhyper(x = seq(0,34,by=1), m = b, n = b * l * p - b, k = n)

p1_leaf <- hist(subset(null.leaves, dam.mean == 0.15)$pct.damage, breaks = seq(0,0.34,by=0.01), freq = F)
p2_leaf <- hist(subset(bites.leaves, dam.mean == 0.15)$pct.damage, breaks = seq(0,0.34,by=0.01), freq = F)

null.plants <- loadRData("data/leaf.level.variance/largeleaves.noleafvar.plants.RData")
bites.plants <- loadRData("data/var.bites/largeleaves.noleafvar.plants.RData")

norm <- dnorm(x = seq(0.1,0.22, by = 0.001), mean = h / b, sd = sqrt(h * (b - h) * (l * p - 1) / (b ^ 2 * l * (b * l * p - 1))))

p1_plant <- hist(subset(null.plants, mean.damage == 0.15)$pct.damage, breaks = seq(0.1,0.22,by=0.005), freq = F)
p2_plant <- hist(subset(bites.plants, mean.damage == 0.15)$pct.damage, breaks = seq(0.1,0.22,by=0.005), freq = F)

layout(matrix(c(1,2,3,3), ncol = 2, byrow = T), heights = c(5,1))

plot(p1_leaf, col = rgb(0,0,1,0.25), freq = F, ylim = c(0,12), main = "Leaf Damage", xlab = "Percent Damage (leaf)")
plot(p2_leaf, col = rgb(1,0,0,0.25),add=T, freq = F, ylim = c(0,12), main = "Leaf Damage", xlab = "Percent Damage (leaf)")
lines(x = seq(-0.005,0.335,by=0.01), y = hyper / diff(p1_leaf$mids[1:2]), lwd = 2)

plot(p1_plant, col = rgb(0,0,1,0.25), freq = F, ylim = c(0,40), main = "Plant Damage", xlab = "Average Percent Damage (plant)")
plot(p2_plant, col = rgb(1,0,0,0.25),add=T, freq = F, ylim = c(0,40), main = "Plant Damage", xlab = "Average Percent Damage (plant)")
lines(x = seq(0.1,0.22,by=0.001), y = norm, lwd = 2)

par(mai = c(0,0,0,0))
plot.new()
legend("center", ncol = 3, legend = c(expression(paste("null"["0"], " simulation")), expression(paste("null"["N"], " simulation")), expression(paste("analytical expectation"))),
       fill = c(rgb(0,0,1,0.25), rgb(1,0,0,0.25), "black"), bty = "n", inset = )




#### undamaged leaves ####
null0.nobites.data <- loadRData("data/leaf.level.variance/largeleaves.noleafvar.pops.RData")
nulln.nobites.data <- loadRData("data/var.bites/largeleaves.noleafvar.pops.RData")

b <- 100
l <- 10
p <- 60
n <- seq(0.01, 0.15, by = 0.01) * b * l * p
h = n / (l * p)

null0.leaf.zero <- rep(NA, length(n))

for(j in 1:length(null0.leaf.zero)) {
  all.b <- rep(NA, b)
  for(i in 0:(b - 1)) {
    all.b[i+1] <- (b * l * p - n[j] - i) / (b * l * p - i)
  }
  null0.leaf.zero[j] <- prod(all.b)
}

nulln.leaf.zero <- exp(-h)

par(mfrow = c(1,2))

plot(seq(0.01,0.15, by = 0.01), null0.leaf.zero, type = "l", xlab = "Proportion of Population Damaged", ylab = "Proportion of Leaves Without Damage", col = "red")
lines(seq(0.01,0.15, by = 0.01), aggregate(null0.nobites.data$no.dam.leaves, by = list(null0.nobites.data$mean.damage), FUN = mean)$x, col = "blue")

plot(seq(0.01,0.15, by = 0.01), nulln.leaf.zero, type = "l", xlab = "Proportion of Population Damaged", ylab = "Proportion of Leaves Without Damage", col = "red")
lines(seq(0.01,0.15, by = 0.01), aggregate(nulln.nobites.data$no.dam.leaves, by = list(nulln.nobites.data$mean.damage), FUN = mean)$x, col = "blue")

plot(seq(0.01, 0.15, by = 0.01), null0.leaf.zero - aggregate(null0.nobites.data$no.dam.leaves, by = list(null0.nobites.data$mean.damage), FUN = mean)$x,
     main = expression(paste("null"["0"])), xlab = "Proportion of population damaged", ylab = "Difference Between Simulated and Expected Zeroes")
lines(seq(0.01, 0.15, by = 0.01), null0.leaf.zero - aggregate(null0.nobites.data$no.dam.leaves, by = list(null0.nobites.data$mean.damage), FUN = mean)$x)
abline(0,0,lty=2)


plot(seq(0.01, 0.15, by = 0.01), null0.leaf.zero - aggregate(nulln.nobites.data$no.dam.leaves, by = list(nulln.nobites.data$mean.damage), FUN = mean)$x,
     main = expression(paste("null"["N"])), xlab = "Proportion of population damaged", ylab = "Difference Between Simulated and Expected Zeroes")
lines(seq(0.01, 0.15, by = 0.01), null0.leaf.zero - aggregate(nulln.nobites.data$no.dam.leaves, by = list(nulln.nobites.data$mean.damage), FUN = mean)$x)
abline(0,0,lty = 2)



#### testing sampling error ####
b <- 100
l <- c(4, 10, 100)
p <- 60
n <- 0.15 * b * l * p
h = n / (l * p)

null0.leaf.var <- (h * (b - h) * (l * p - 1)) / (b^2 * (b * l * p - 1))
null0.plant.var <- (h * (b - h) * (l * p - 1)) / (b^2 * l * (b * l * p - 1))
null0.pop.var <- (h * (b - h) * (l * p - 1)) / (b^2 * l * p * (b * l * p - 1))

nullb.leaf.var <- (h * (b - h) * (l * p - 1 + l * p / b)) / (b^2 * (b * l * p - 1))
nullb.plant.var <- (h * (b - h) * (l * p - 1 + l * p / b)) / (b^2 * l * (b * l * p - 1))
nullb.pop.var <- (h * (b - h) * (l * p - 1 + l * p / b)) / (b^2 * l * p * (b * l * p - 1))

nulln.leaf.var <- (h * ((b - h) * (l * p - 1) + b - h - 1)) / (b^2 * (b * l * p - 1))
nulln.plant.var <- (h * ((b - h) * (l * p - 1) + b - h - 1)) / (b^2 * l * (b * l * p - 1))
nulln.pop.var <- (h * ((b - h) * (l * p - 1) + b - h - 1)) / (b^2 * l * p * (b * l * p - 1))

ana.mean <- h / b

null0.4leaves.leaf.data <- loadRData("data/sampling.error/null0.4leaves.15pct.RData")
null0.10leaves.leaf.data <- loadRData("data/sampling.error/null0.10leaves.15pct.RData")
null0.100leaves.leaf.data <- loadRData("data/sampling.error/null0.100leaves.15pct.RData")

plot(as.factor(l), ana.mean - c(mean(null0.4leaves.leaf.data$pct.damage), mean(null0.10leaves.leaf.data$pct.damage), mean(null0.100leaves.leaf.data$pct.damage)))
plot.default(as.factor(l), null0.leaf.var - c(var(null0.4leaves.leaf.data$pct.damage), var(null0.10leaves.leaf.data$pct.damage), var(null0.100leaves.leaf.data$pct.damage)),
             type = "p", axes = FALSE, xlab = "Number of Leaves", ylab = "Difference Between Expected and Simulated Variance", ylim = c(-0.00025,0), pch = 19)
axis(side = 1, at = as.factor(l), labels = as.factor(l))
axis(side = 2, at = seq(-0.00025,0, by = 0.00005))
grid()
abline(0,0, lty=2, cex = 2)
points(as.factor(l), null0.leaf.var - c(var(null0.4leaves.leaf.data$pct.damage), var(null0.10leaves.leaf.data$pct.damage), var(null0.100leaves.leaf.data$pct.damage)),
       type = "p", pch = 19, cex = 1.5)

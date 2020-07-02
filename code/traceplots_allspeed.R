library(bamlss)
mod <- readRDS("../output/bamlss_allspeed_knee.RDS")
pdf("../output/traceplots_knee.pdf")
plot(mod, which = "samples")
dev.off()

mod <- readRDS("../output/bamlss_allspeed_hip.RDS")
pdf("../output/traceplots_hip.pdf")
plot(mod, which = "samples")
dev.off()

mod <- readRDS("../output/bamlss_allspeed_ankle.RDS")
pdf("../output/traceplots_ankle.pdf")
plot(mod, which = "samples")
dev.off()


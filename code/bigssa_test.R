library (bigsplines)
library (plot3D)
library (plot3Drgl)

df_ankle$sex = factor (df_ankle$sex)

rknots <- binsamp(as.matrix (df_ankle[,names (df_ankle) %in% c("speed", "age", "cycle")]), nmbin = c(10, 10, 20))

num_prm <- 0.01
ord_prm <- 1

m <- bigssa (val ~ sex+ cycle*speed*age,
             type=list(sex = "nom", cycle = "per", age = "cub", speed ="cub"),
             rparm=list(cycle = num_prm, speed = num_prm, age = num_prm, sex = ord_prm),
             nknots=100,
             data = df_ankle_downsamp,
             skip.iter=T)

s <- summary (m, fitresid = TRUE)

plot (s$fitted.values, s$residuals)



m1 <- bigssp (val ~ sex+ cycle*speed*age,
             type=list(sex = "nom", cycle = "per", age = "cub", speed ="prm"),
             rparm=list(cycle = num_prm, speed = num_prm, age = num_prm, sex = ord_prm),
             nknots = 100,
             data = df_ankle_downsamp,
             skip.iter=T)

s1 <- summary (m1, fitresid = TRUE)
plot (s1$fitted.values, s1$residuals)

m2 <- bigssp (log (val) ~ sex+ cycle*speed*age,
              type=list(sex = "nom", cycle = "per", age = "cub", speed ="prm"),
              rparm=list(cycle = num_prm, speed = num_prm, age = num_prm, sex = ord_prm),
              nknots = 100,
              data = df_ankle,
              skip.iter=T)

summ <- summary (m1, fitresid = TRUE)
windows()
plot (summ$fitted.values, summ$residuals)



newdata <- expand.grid(cycle = c(1:101),
                       age = seq (5, 85, 1),
                       speed = 0.7)
yhat = predict(m,
               newdata = newdata,
               se.fit=TRUE,
               include= c("age", "cycle", "speed"),
               includeint=T)

yhat_df <- bind_rows(yhat) %>%
  bind_cols(newdata) %>%
  mutate (lwr = fit - 2*se.fit,
          upr = fit + 2*se.fit)

z <- matrix (yhat_df[, "fit"] %>% pull(), nrow = length (c(1:101)), ncol = length (seq (5, 85, 1)))
z_lwr <- matrix (yhat_df[, "lwr"] %>% pull(), nrow = length (c(1:101)), ncol = length (seq (5, 85, 1)))
z_upr <- matrix (yhat_df[, "upr"] %>% pull(), nrow = length (c(1:101)), ncol = length (seq (5, 85, 1)))

par (mfrow = c(1,1))

clim <- c (min (range (z_lwr)), max (range (z_upr)))

xl <-  "cycle (0-100%)"
yl <- "age (years)"
zl <-  "power (W/kg)"
cl <- "power (W/kg)"


persp3D(x = c(1:101),
       y = seq (5, 85, 1),
       z = z,
       add = TRUE,
       clim = clim,
       theta = 30, colkey = FALSE, plot = FALSE)

persp3D(x = c(1:101),
        y = seq (5, 85, 1),
        z = z_lwr,
        facets = NA,
        col = jet.col(alpha =0.4),
        add = TRUE,
        clim = clim,
        theta = 30, colkey = FALSE, plot = FALSE)

persp3D(x = c(1:101),
        y = seq (5, 85, 1),
        z = z_upr,
        col = jet.col(alpha =0.4),
        facets = NA,
        add = TRUE,
        clim = clim,
        xlab = xl,
        ylab = yl,
        zlab = zl,
        clab = cl,
        theta = 30)


plotrgl()

fig <- plot_ly(showscale = FALSE)
fig <- fig %>% add_surface(z = ~z)
fig <- fig %>% add_surface(z = ~z_lwr, opacity = 0.98)
fig <- fig %>% add_surface(z = ~z_upr, opacity = 0.98)
fig

writeWebGL(filename = file.path("output", "ankle_pwr.html"))

f <- list (val ~ sex + s(cycle, k = 15, bs = "cc") + s(age, k = 10, bs = "cr") + + s(speed, k = 10, bs = "cr") + 
             ti(cycle, age, k = 15, bs = "cr") + ti(cycle, speed, k = 15, bs = "cr") + ti(age, speed, k = 10, bs = "cr"),
           sigma ~ sex + s(cycle, k = 15, bs = "cc") + s(age, k = 10, bs = "cr") + + s(speed, k = 10, bs = "cr") + 
             ti(cycle, age, k = 15, bs = "cr") + ti(cycle, speed, k = 15, bs = "cr") + ti(age, speed, k = 10, bs = "cr"))

f2 <- list (val ~ sex + s(cycle, k = 15, bs = "cc") + s(age, k = 10, bs = "cr") + + s(speed, k = 10, bs = "cr") + 
             ti(cycle, age, k = 15, bs = "cr") + ti(cycle, speed, k = 15, bs = "cr") + ti(age, speed, k = 10, bs = "cr"),
           sigma ~ s(cycle, k = 15, bs = "cc") + s(age, k = 10, bs = "cr") + + s(speed, k = 10, bs = "cr"),
           nu ~ s(cycle, k = 15, bs = "cc") + s(age, k = 10, bs = "cr") + + s(speed, k = 10, bs = "cr"),
           tau ~ s(cycle, k = 15, bs = "cc") + s(age, k = 10, bs = "cr") + + s(speed, k = 10, bs = "cr"))

b <- bamlss (f,
             data = df_ankle_downsamp,
             family = SHASH,
             binning = TRUE)

b1 <- bamlss (f2,
             data = df_ankle_downsamp,
             family = SHASH,
             binning = TRUE)

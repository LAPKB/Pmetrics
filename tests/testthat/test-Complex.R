library(tidyverse)
library(Pmetrics)
setwd("Complex")



### 1/13/23 Beginning of complex model/data test
### Not for automated testing yet.

mod <- PM_model$new(
  list(
    pri = list(
      KA1 = ab(0, 5),
      KA2 = ab(0, 5),
      F1 = ab(0, 1),
      F2 = ab(0, 1),
      TL1 = ab(0, 2),
      TL2 = ab(0, 2),
      V1s = ab(0, 10),
      V2s = ab(0, 1),
      KE1 = ab(0, 0.1),
      KE2 = ab(0.2, 0.5)
    ),
    cov = c("wt", "ic1", "ic2"),
    sec = c("V1 = V1s*WT",
            "V2 = V2s*WT"),
    bol = "NBCOMP(2) = 3",
    ini = c("X(2) = IC1*V1",
            "X(4) = IC2*V2"),
    lag = c("TLAG(1) = TL1",
            "TLAG(2) = TL2"),
    fa = c("FA(1) = F1",
           "FA(2) = F2"),
    dif = c("XP(1) = -KA1*X(1)",
            "XP(2) = RATEIV(1) + KA1*X(1) - KE1*X(2)",
            "XP(3) = -KA2*X(3)",
            "XP(4) = RATEIV(2) + KA2*X(3) - KE2*X(4)"),
    out = list(
      Y1 = list(
        value = "X(2)/V1",
        err = list(
          model = proportional(1, constant = T),
          assay = c(0.05, 0.01, 0, 0)
        )
      ),
      Y2 = list(
        value = "X(4)/V2",
        err = list(
          model = proportional(1, constant = T),
          assay = c(0.05, 0.01, 0, 0)
        )
      )
    )
  )
)


#simulation template
dat <- PM_data$new(as_tibble(
  matrix(
    c(
      # id, evid, time, dose, dur, input, out, outeq, wt, ic1, ic2
      1,  1,    0,    1000, 1,   1,     NA,  NA,    70,  2,  0.5,
      1,  1,    0,     500, 0.5, 2,     NA,  NA,    70,  2,  0.5,
      1,	1,  	12,	  1000,	0,   2,	    NA,  NA,  	70,	 2,	 0.5,
      1,	1,  	24,	  2000,	0,	 1,	    NA,  NA,    70,	 2,	 0.5,
      1,  0,  	36,	  NA,   NA,  NA,    -1,  1,   	70,	 2,	 0.5,
      1,  0,  	36,	  NA,   NA,  NA,    -1,  2,   	70,	 2,	 0.5), 
    nrow = 6, byrow = T, 
    dimnames = list(NULL, c("id", "evid", "time", "dose", "dur", "input", 
                            "out", "outeq", "wt", "ic1", "ic2"))
  )
)
)

#simulated parameter value distributions
means <- sapply(mod$model_list$pri,function(x) x$mean)
covMat <- diag(sapply(mod$model_list$pri,function(x) x$sd^2))
poppar <- list(1, means, covMat)

#simulate data
sim1 <- PM_sim$run(poppar = poppar, data = dat, model = mod, predInt = 1, nsim = 2, 
                   makecsv = "simtest.csv", obsNoise = NA, limits = NA, clean = T)     

dat2 <- PM_data$new("simtest.csv")
dat2$plot(outeq = 2)

fit1 <- PM_fit$new(dat2, mod)
fit1$run(cycles = 10)
run1 <- PM_load(1)
run1$data$plot(outeq = 1, log = F, color = "id", line = list(join = F, pred = run1$post))
run1$data$plot(outeq = 2, log = F, color = "id", line = list(join = F, pred = run1$post))
#fitted values
run1$final$popPoints
#simulated "true" values
sim1$parValues

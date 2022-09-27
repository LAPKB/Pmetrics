## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=F, message=F-------------------------------------------------
library(Pmetrics)
library(tidyverse)
library(plotly)

## ----echo = T, eval = F-------------------------------------------------------
#  mod1 <- PM_model$new("model.txt")
#  #assumes model.txt is in working directory
#  #check with list.files() and/or getwd()

## ----echo=T, eval=F-----------------------------------------------------------
#  mod <- PM_model$new(list(
#    pri = list(
#      Ke = ab(0,5),
#      V = msd(100,20),
#      eff = ab(-2,2,gtz=F)
#    )
#  ))

## ----echo=T, eval=F-----------------------------------------------------------
#  mod <- PM_model$new(list(
#    pri = list(...),
#    cov = list("wt","age")
#  ))

## ----echo=T, eval=F-----------------------------------------------------------
#  mod <- PM_model$new(list(
#    pri = list(
#      CL0 = ab(0,5),
#      V0 = msd(10,3),
#      eff = ab(-2,2,gtz=F)
#    ),
#    cov = list("wt","age"),
#    sec = list(
#      V = "V0*wt",
#      "&IF(age >18) V = V0 * 75",
#      CL = "CL0 * wt"
#    )
#  ))

## ----echo=T, eval=F-----------------------------------------------------------
#  mod <- PM_model$new(list(
#    bol = list("NBCOMP(1) = 2")
#  ))

## ----echo=T, eval=F-----------------------------------------------------------
#  mod <- PM_model$new(list(
#    pri = pri = list(
#      Ke = ab(0,5),
#      V = msd(100,30),
#      IC3 = ab(0,1000)
#    ),
#    cov = list("wt","age","IC2"),
#    ini = list(
#      X2 = "IC2*V",
#      X3 = "IC3"
#    )
#  ))

## ----echo=T, eval=F-----------------------------------------------------------
#  mod <- PM_model$new(list(
#    pri = pri = list(
#      Ke = ab(0,5),
#      V = msd(100,30),
#      FA1 = ab(0,1)
#    ),
#    fa = list(
#      fa1 = "FA1"
#    )
#  )

## ----echo=T, eval=F-----------------------------------------------------------
#  mod <- PM_model$new(list(
#    pri = pri = list(
#      Ke = ab(0,5),
#      V = msd(100,30),
#      lag1 = ab(0,4)
#    ),
#    lag = list(tlag1 = "lag1")
#    )
#  )

## ----echo=T, eval=F-----------------------------------------------------------
#  mod <- PM_model$new(list(
#    pri = pri = list(
#      Ka = ab(0,5),
#      Ke = ab(0,5),
#      V = msd(100,30),
#      Kcp = ab(0,5),
#      Kpc = ab(0,5)
#    ),
#    diff = list(
#      xp1 = "-Ka * X(1)",
#      xp2 = "RATEIV(1) + Ka * X(1) - (Ke + Kcp) * X(2) + Kpc * X(3)",
#      xp3 = "Kcp * X(2) - Kpc * X(3)"
#    )
#  ))

## ----echo=T, eval=F-----------------------------------------------------------
#  out = list(
#    Y1 = list(...)
#  )

## ----echo=T, eval=F-----------------------------------------------------------
#  out = list(
#    Y1 = list(
#      "X(1)/V",
#      err = list(...)
#    )
#  )

## ----echo=T, eval=F-----------------------------------------------------------
#  out = list(
#    Y1 = list(
#      "X(1)/V",
#      err = list(
#        model = proportional(1),
#        assay = c(0.15, 0.1, 0, 0)
#      )
#    )
#  )

## ----echo=T, eval=F-----------------------------------------------------------
#  out = list(
#    Y1 = list(
#      "X(1)/V",
#      err = list(
#        model = additive(1, fixed = TRUE)
#        assay = c(0.05, 0.1, 0, 0)
#      )
#  )

## ----echo=T, eval=F-----------------------------------------------------------
#  mod <- PM_model$new(list(
#    pri = pri = list(
#      Ke = ab(0,5),
#      V = msd(100,30),
#    ),
#    out = list(
#      y1 = list(
#        "X(1)/V",
#        err = list(
#          model = proportional(5),
#          assay = c(0.05, 0.1, 0, 0)
#        )
#      )
#    )
#  ))
#  
#  mod2 <- PM_model$new(list(
#    pri = pri = list(
#      kin = ab(0,5),
#      kout = ab(0,5),
#      tpd = ab(0,5),
#      V = msd(100,30),
#    ),
#    sec = list("RES = B(1) * KIN/(KIN-KOUT) * (EXP(-KOUT*TPD)-EXP(-KIN*TPD))"),
#    out = list(
#      y1 = list(
#        "RES/V",
#        err = list(
#          model = combination(0.4,3) #additive, proportional
#          assay = c(0.3, 0.15, 0, 0)
#        )
#      )
#    )
#  ))

## ----echo=T, eval = F---------------------------------------------------------
#  mod2 <- mod1
#  mod2$update(...)

## ----echo=T, eval = F---------------------------------------------------------
#  mod2 <- mod1$clone()
#  mod2$update(...)

## ----echo=T, eval = F---------------------------------------------------------
#  
#  mod2$update(list(
#    pri = list(Ke = ab(1,3),
#               V = NULL,
#               V0 = ab(0, 20)),
#    sec = list(V = "V0 * wt")
#  ))

## ----echo=F-------------------------------------------------------------------
res <- read.csv("Data/reserved.csv")
knitr::kable(res, col.names = gsub("[.]", " ", names(res)))

## ----echo=F-------------------------------------------------------------------
for1 <- read.csv("Data/fortran1.csv")
knitr::kable(for1, col.names = gsub("[.]", " ", names(for1)))



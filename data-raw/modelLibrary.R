## Run this code to prepare `modelLibrary` 
## whenever models added

#initialize row object 
modelRow <- list()

#function to add rows to row object in .GlobalEnv
add_model_row <- function(ncomp, par, bolus, elim, mod, name){
  modelRow[[length(modelRow)+1]] <<- 
    tibble::as_tibble_row(list(ncomp = ncomp,
                               par = par,
                               bolus = bolus,
                               elim = list(elim),
                               mod = list(mod),
                               name = name))
}


# Models ------------------------------------------------------------------

#Ke, V
add_model_row(ncomp = 1, 
              par = "K",
              bolus = FALSE,
              elim = 1,
              mod = PM_model$new(
                list(
                  pri = list(
                    Ke = ab(0, 5),
                    V = ab(0.01, 100)
                  ),
                  eqn = list(
                    "dX[1] = RATEIV[1] - Ke * X[1]"
                  ),
                  out = list(
                    y1 = list(
                      val = "X[1]/V",
                      err = list(
                        model = additive(0.1),
                        assay = errorPoly(c(0.1, 0.1, 0, 0))
                      )
                    )
                  ) 
                ) #end model list
              ),
              name = "One comp: Ke, V"
)

#Ka, Ke, V
add_model_row(ncomp = 2, 
              par = "K",
              bolus = TRUE,
              elim = 2,
              mod = PM_model$new(
                list(
                  pri = list(
                    Ke = ab(0, 5),
                    V = ab(0.01, 100),
                    Ka = ab(0, 5)
                  ),
                  eqn = list(
                    "dX[1] = BOLUS[1] - Ka*X[1]",
                    "dX[2] = RATEIV[1] + Ka*X[1] - Ke*X[2]"
                  ),
                  out = list(
                    y1 = list(
                      val = "X[2]/V",
                      err = list(
                        model = additive(0.1),
                        assay = errorPoly(c(0.1, 0.1, 0, 0))
                      )
                    )
                  ) 
                ) #end model list
              ),
              name = "One comp, bolus: Ka, Ke, V"
)


#Ke, V, KCP, KPC
add_model_row(ncomp = 2, 
              par = "K",
              bolus = FALSE,
              elim = 1,
              mod = PM_model$new(
                list(
                  pri = list(
                    Ke = ab(0, 5),
                    V = ab(0.01, 100),
                    KCP = ab(0, 5),
                    KPC = ab(0, 5)
                  ),
                  eqn = list(
                   "dX[1] = RATEIV[1] - (Ke + KCP)*X[1] + KPC*X[2]",
                   "dX[2] = KCP*X[1] - KPC*X[2]"
                  ),
                  out = list(
                    y1 = list(
                      val = "X[1]/V",
                      err = list(
                        model = additive(0.1),
                        assay = errorPoly(c(0.1, 0.1, 0, 0))
                      )
                    )
                  ) 
                ) #end model list
              ),
              name = "Two comp: Ke, V, KCP, KPC"
)

#Ka, Ke, V, KCP, KPC
add_model_row(ncomp = 3, 
              par = "K",
              bolus = TRUE,
              elim = 2,
              mod = PM_model$new(
                list(
                  pri = list(
                    Ke = ab(0, 5),
                    V = ab(0.01, 100),
                    Ka = ab(0, 5),
                    KCP = ab(0, 5),
                    KPC = ab(0, 5)
                  ),
                  eqn = list(
                    "dX[1] = BOLUS[1] - Ka*X[1]",
                    "dX[2] = RATEIV[1] + Ka*X[1] - (Ke + KCP)*X[2] + KPC*X[3]",
                    "dX[3] = KCP*X[2] - KPC*X[3]"
                  ),
                  out = list(
                    y1 = list(
                      val = "X[2]/V",
                      err = list(
                        model = additive(0.1),
                        assay = errorPoly(c(0.1, 0.1, 0, 0))
                      )
                    )
                  ) 
                ) #end model list
              ),
              name = "One comp, bolus: Ka, Ke, V"
)

# Assemble and use --------------------------------------------------------

#assemble library
modelLibrary <- purrr::list_rbind(modelRow) 

# #example entry - remove when building pacage
# modelLibrary
# model <- modelLibrary$mod[4][[1]]


usethis::use_data(modelLibrary, overwrite = T)



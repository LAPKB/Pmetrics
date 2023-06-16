# Run this code to prepare `modelLibrary` 
# whenever models added
# 
# format for algebraic string: parameters in alpha order, elimination compartment
# separated by commas

#initialize row object 
modelRow <- list()

#function to add rows to row object in .GlobalEnv
add_model_row <- function(ncomp, par, route, elim, mod, name, algebraic){
  modelRow[[length(modelRow)+1]] <<- 
    tibble::as_tibble_row(list(ncomp = ncomp,
                               par = par,
                               route = list(route),
                               elim = list(elim),
                               mod = list(mod),
                               name = name,
                               algebraic = algebraic))
}


# Models ------------------------------------------------------------------

#Ke, V
add_model_row(ncomp = 1, 
              par = "K",
              route = "Intravenous",
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
              name = "One comp: Ke, V",
              algebraic = "P[Ke,V], B[0], R[1], O[1]"
)

#CL, V
add_model_row(ncomp = 1, 
              par = "CL",
              route = "Intravenous",
              elim = 1,
              mod = PM_model$new(
                list(
                  pri = list(
                    CL = ab(0, 5),
                    V = ab(0.01, 100)
                  ),
                  sec = "Ke = CL/V",
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
              name = "One comp: CL, V",
              algebraic = "P[CL,V], B[0], R[1], O[1]"
)

#Ka, Ke, V
add_model_row(ncomp = 2, 
              par = "K",
              route = c("Oral", "Intravenous"),
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
              name = "Two comp, oral: Ka, Ke, V",
              algebraic = "P[Ka,Ke,V], B[1], R[2], O[2]"
)

#Ka, CL, V
add_model_row(ncomp = 2, 
              par = "CL",
              route = c("Oral", "Intravenous"),
              elim = 2,
              mod = PM_model$new(
                list(
                  pri = list(
                    CL = ab(0, 5),
                    V = ab(0.01, 100),
                    Ka = ab(0, 5)
                  ),
                  sec = "Ke = CL/V",
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
              name = "Two comp, oral: Ka, CL, V",
              algebraic = "P[Ka,CL,V], B[1], R[2], O[2]"
)


#Ke, V, KCP, KPC
add_model_row(ncomp = 2, 
              par = "K",
              route = "Intravenous",
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
              name = "Two comp: Ke, V, KCP, KPC",
              algebraic = "P[Ke,KCP,KPC,V], B[0], R[1], O[1]"
)

#CL, V, Q, Vp
add_model_row(ncomp = 2, 
              par = "CL",
              route = "Intravenous",
              elim = 1,
              mod = PM_model$new(
                list(
                  pri = list(
                    CL = ab(0, 5),
                    V = ab(0.01, 100),
                    Q = ab(0, 5),
                    Vp = ab(0.01, 100)
                  ),
                  sec = c(
                    "Ke = CL/V",
                    "KCP = Q/V",
                    "KPC = Q/Vp"
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
              name = "Two comp: CL, V, Q, Vp",
              algebraic = "P[CL,Q,V,Vp], B[0], R[1], O[1]"
)


#Ka, Ke, V, KCP, KPC
add_model_row(ncomp = 3, 
              par = "K",
              route = c("Intravenous", "Oral"),
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
              name = "Three comp, oral: Ka, Ke, V, KCP, KPC",
              algebraic = "P[Ka,Ke,KCP,KPC,V], B[1], R[2], O[2]"
)

#Ka, CL, V, Q, Vp
add_model_row(ncomp = 3, 
              par = "CL",
              route = c("Intravenous", "Oral"),
              elim = 2,
              mod = PM_model$new(
                list(
                  pri = list(
                    CL = ab(0, 5),
                    V = ab(0.01, 100),
                    Ka = ab(0, 5),
                    Q = ab(0, 5),
                    Vp = ab(0.01, 100)
                  ),
                  sec = c(
                    "Ke = CL/V",
                    "KCP = Q/V",
                    "KPC = Q/Vp"
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
              name = "Three comp, oral: Ka, CL, V, Q, Vp",
              algebraic = "P[CL,Ka,Q,V,Vp], B[1], R[2], O[2]"
)

# Assemble and use --------------------------------------------------------

#assemble library
modelLibrary <- purrr::list_rbind(modelRow) 

# #example entry - remove when building package
# modelLibrary
# model <- modelLibrary$mod[4][[1]]


usethis::use_data(modelLibrary, overwrite = T)



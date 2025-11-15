library(Pmetrics)
#initialize row object
modelRow <- list()

#function to add rows to row object in .GlobalEnv
add_model_row <- function(ncomp, comp, par, route, elim, model_list, name, alt){
  modelRow[[length(modelRow)+1]] <<-
    tibble::as_tibble_row(list(ncomp = ncomp,
                               comp = comp,
                               par = par,
                               route = route,
                               elim = list(elim),
                               model_list = list(model_list),
                               name = name,
                               alt = alt))
}


# Models ------------------------------------------------------------------



#Ke, V
add_model_row(ncomp = 1,
              comp = "1 = Central",
              par = "Ke, V",
              route = "Intravenous",
              elim = 1,
              model_list = 
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
                ),#end model list
              name = "one_comp_iv", 
              alt = "advan1\nadvan1-trans1")

#CL, V
add_model_row(ncomp = 1,
              comp = "1 = Central",
              par =  "CL, V",
              route = "Intravenous",
              elim = 1,
              model_list = 
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
                  ) #end model list
                ),
              name = "one_comp_iv_cl", 
              alt = "advan1\nadvan1-trans2"
)

#Ka, Ke, V
add_model_row(ncomp = 2,
              comp = "1 = Bolus\n2 = Central", 
              par = "Ka, Ke, V",
              route = "Oral\nIntravenous",
              elim = 2,
              model_list = 
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
                  ) #end model list
                ),
              name = "two_comp_bolus", 
              alt = "advan2\nadvan2-trans1"
)

#Ka, CL, V
add_model_row(ncomp = 2,
              comp = "1 = Bolus\n2 = Central", 
              par = "Ka, CL, V",
              route = "Oral\nIntravenous",
              elim = 2,
              model_list = 
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
                  ) #end model list
                ),
              name = "two_comp_bolus_cl", 
              alt = "advan2\nadvan2-trans2"
)


#Ke, V, KCP, KPC
add_model_row(ncomp = 2,
              comp = "1 = Central\n2 = Peripheral", 
              par = "Ke, V, KCP, KPC",
              route = "Intravenous",
              elim = 1,
              model_list = 
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
                  ) #end model list
                ),
              name = "two_comp_iv", 
              alt = "advan3\nadvan3-trans1"
)

#CL, V, Q, Vp
add_model_row(ncomp = 2,
              comp = "1 = Central\n2 = Peripheral", 
              par = "CL, V1, Q, V2",
              route = "Intravenous",
              elim = 1,
              model_list = 
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
                  ) #end model list
                ),
              name = "two_comp_iv_cl", 
              alt = "advan3\nadvan3-trans4"
)


#Ka, Ke, V, KCP, KPC
add_model_row(ncomp = 3,
              comp = "1 = Bolus\n2 = Central\n3 = Peripheral",
              par =  "Ka, Ke, V, KCP, KPC",
              route = "Oral\nIntravenous",
              elim = 2,
              model_list = 
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
                  ) #end model list
                ),
              name = "three_comp_bolus", 
              alt = "advan4\nadvan4-trans1"
)

#Ka, CL, V, Q, Vp
add_model_row(ncomp = 3,
              comp = "1 = Bolus\n2 = Central\n3 = Peripheral", 
              par = "Ka, CL, V2, Q, V3",
              route = "Oral\nIntravenous",
              elim = 2,
              model_list = 
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
                  ) #end model list
                ),
              name = "three_comp_bolus_cl", 
              alt = "advan4\nadvan4-trans4"
)

# Assemble and use --------------------------------------------------------

#assemble library
modelLibrary <- purrr::list_rbind(modelRow)
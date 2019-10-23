#' Calculates creatinine clearance or outputs Fortran code for in-model calculations. Patient covariates are supplied as vectors of equal length. 
#' If no patient values are supplied, the function outputs the Fortran code to be used in a \emph{Pmetrics} model file.
#' Please note that to estimate GFR using the Jelliffe equation using two creatinine levels, a standalone function \code{\link{PMgetCRCL}} is available.
#'
#' \code{PMestGFR} takes vectors with patient parameters and calculates estimated creatinine clearance using specified formula.
#'
#' @title Calculate creatinine clearance
#' @param formula Required. Identifies the formula to be used for calculation. Available options are \code{Cockroft-Gault}, \code{MDRD}, \code{MDRD-BUN}, \code{CKD-EPI}, \code{Schwartz}, \code{Mayo}, \code{Jelliffe73}.
#' @param scr Required. A vector containing serum creatinine levels either in \emph{μmol/l} or \emph{mg/dL} as specified by the \code{SI} parameter.
#' @param age Required. A vector containing age values in years.
#' @param male A vector specifying patients' sex, 1 for male, 0 for female.
#' @param wt A vector containing body weight values in \emph{kilograms} (note: regardless of the \code{SI} switch).
#' @param black A vector of 1's and 0's specifying if patient is black, used for the MDRD and CKD-EPI formulas.
#' @param BUN A vector of BUN values for the MDRD-BUN formula. If \code{SI=T}, units are mmol/l, otherwise mg/dL.
#' @param albumin A vector of albumin levels for the MDRD-BUN formula. If \code{SI=T}, units are g/l, otherwise g/dL.
#' @param preterm A vector of 1's and 0's specifying if a child was born preterm. Used for the Schwartz formula.
#' @param height A vector of heights in \emph{cm}. Used for the Schwartz formula.
#' @param SI A logical operator. If \code{FALSE}, creatinine and BUN is in mg/dL, albumin in g/dL. Default is \code{TRUE}, i.e. μmol/l, mmol/l, and g/l, respectively.
#' @return The output is a vector of creatinine clearance values of equal lentgh to input vectors. Please be aware that unlike other formulas, Cockroft-Gault returns mL/min!
#' @examples 
#' # this will return a vector of estimated creatinine clearance values:
#' eGFR <- PMestGFR(formula="Cockroft-Gault", scr=c(78,40,150), wt=c(70,50,30), age=c(50,78,30), male=c(1,0,0))
#' 
#' # this will add a new column with estimated GFR to an object called mdata.1:
#' mdata.1 <- cbind(mdata.1, eGFR=PMestGFR(formula="Cockroft-Gault", scr=mdata.1$scr, wt=mdata.1$wt, age=mdata.1$age, male=mdata.1$male))
#' 
#' # this will print the Fortran code to be used in a model file, using US units:
#' PMestGFR("Cockroft-Gault", SI=F)
#' 
#' @author Jan Strojil & Michael Neely
#' @seealso \code{\link{PMgetCRCL}}
#' @export
getCRCL <- function(formula, scr, wt, age, male, black, BUN, albumin, preterm, height, SI=T){
  
  samelength <- function(...){
    
    # stops if length of parameters is not the same
    if (length(unique(lengths(list(...))))!=1)
      stop("Vectors are not of equal length.", call. = F)
    
    # checks if all values are numeric
    if (!is.numeric(c(...)))
      stop("Vectors contain non-numeric values.", call. = F)
    
    #spits out a warning if any values are NAs
    if (anyNA(c(...)))
      warning("Vectors contain missing values.", call. = F)
  }
  
  # define subfunctions for individual calculations, all in US units
  CG <- function(scr, age, wt, male){
    samelength(scr, age, wt, male)
    
    scr[is.na(male)] <- NA
    coef <- rep(1, length(scr))
    coef[male == 0] <- 0.85
    CrCl_CG <-  ((140-age)*wt*coef)/(72*scr)
  }
  
  CKD_EPI <- function(scr,age,male,black){
    samelength(scr,age,male,black)
    
    scr[is.na(male)|is.na(black)] <- NA
    nsub <- length(scr)
    alpha <- rep(-0.329, nsub)
    kappa <- rep(0.7, nsub)
    coefF <- rep(1.018, nsub)
    coefB <- rep(1.159, nsub)
    alpha[male == 1]  <- -0.411
    kappa[male == 1]  <-  0.9
    coefF[male == 1]  <-  1
    coefB[black == 0]  <-  1
    maxi <-  scr/kappa
    mini <-  scr/kappa
    maxi[maxi < 1] <- 1
    mini[mini > 1] <- 1
    CrCl_CKD <-  141 * mini**alpha * maxi**(-1.209) * 0.993**age * coefF * coefB
  }
  MDRD <- function(scr, age, male, black){
    samelength(scr, age, male, black)    
    scr[is.na(male)|is.na(black)] <- NA
    nsub <- length(scr)
    coefF <- rep(1, nsub)
    coefB <- rep(1, nsub)
    coefF[male == 0] <- 0.742
    coefB[black == 1] <- 1.210
    CrCl_MDRD <- 186 * scr**(-1.154) * age**(-0.203) * coefF * coefB
  }
  MDRD_BUN <- function(scr, age, male, black, BUN, albumin){
    samelength(scr, age, male, black, BUN, albumin)
    scr[is.na(male)|is.na(black)] <- NA
    nsub <- length(scr)
    coefF <- rep(1, nsub)
    coefB <- rep(1, nsub)
    coefF[male == 0] <-  0.742
    coefB[black == 1] <-  1.210
    CrCl_MDRD_BUN <-  170 * scr**(-1.154) * age**(-0.203) * coefF * coefB * BUN**(-0.17) * albumin**(0.318)
  }
  Mayo <- function(scr, age, male){
    samelength(scr, age, male)    
    scr[is.na(male)] <- NA
    coefF <- rep(1, length(scr))
    scr[scr < 0.8]  <- 0.8
    coefF[male == 0]  <-  0.205
    CrCl_MCQE <- 2.718**(1.911 + 5.249/scr - 2.114/scr**2 - 0.00686 * age - coefF)
  }
  Schwartz <- function(scr, age, height, preterm){
    samelength(scr, age, height, preterm)
    scr[is.na(preterm)] <- NA
    k <- rep(0.55, length(scr))
    k[age < 1 & preterm==1] <- 0.33
    k[age < 1 & preterm==0] <- 0.45
    CrCl_Schwartz = k * height / scr
  }
  Jelliffe73 <- function(scr, age, male){
    samelength(scr, age, male)
    scr[is.na(male)] <- NA
    coefF <- rep(1, length(scr))
    coefF[male==0] <- 0.9
    CrCl_Jelliffe = coefF * (98-16*((age-20)/20))/scr
  }
  
  # Jelliffe72 <- function(creat1, creat2, time1, time2, wt, age, male){
  #   samelength(creat1, creat2, age, male)
  #   creat1[is.na(male)] <- NA
  #   coef1 <- rep(29.3, length(creat1))
  #   coef2 <- rep(0.203, length(creat1))
  #   coef1[male==0] <- 25.1
  #   coef2[male==0] <- 0.175
  #   ESS <- wt * (coef1 - (coef2 * age))
  #   avg_creat <- (creat1 + creat2)/2
  #   ESS_cor <- ESS * (1.035 - (0.0337 * avg_creat))
  #   E <- ESS_cor - 4 * wt * (creat2-creat1)/(time2-time1)
  #   CrCl_Jelliffe72 <- E / (14.4 * avg_creat)
  # }
  # 
  
  if (missing(formula)){
    cat("No formula specified. Available options are:\n1) Cockroft-Gault\n2) MDRD\n3) MDRD-BUN\n4) CKD-EPI\n5) Schwartz\n6) Mayo\n7) Jelliffe73")
    stop("No formula specified, don't know what to do.", call. = F)
  }
  
  if (formula %in% c("CG", "C-G")) formula <- "Cockroft-Gault"
  if (!formula %in% c("Cockroft-Gault", "MDRD","MDRD-BUN","CKD-EPI","Schwartz","Mayo", "Jelliffe73")){
    cat("Unknown formula name. Available options are:\n1) Cockroft-Gault\n2) MDRD\n3) MDRD-BUN\n4) CKD-EPI\n5) Schwartz\n6) Mayo\n7) Jelliffe73")
    stop("Unknown formula.", call. = F)
  }
  
  if (missing(scr) & !missing(formula)){ # print out Fortran code
    tCKD <- paste(
      "\nalpha=-0.329",
      "\nkappa=0.7",
      "\ncoefF=1.018",
      "\ncoefB=1.159",
      "\n&IF(MALE == 1) alpha = -0.411",
      "\n&IF(MALE == 1) kappa = 0.9", 
      "\n&IF(MALE == 1) coefF = 1",
      "\n&IF(BLACK == 0) coefB = 1",
      "\nmaxi = SCR/kappa",
      "\nmini = SCR/kappa",
      "\n&IF(maxi .LT. 1) maxi=1",
      "\n&IF(mini .GT. 1) mini=1",
      "\nCrCl_CKD =141 * mini**alpha * maxi**(-1.209) * 0.993**AGE * coefF * coefB", sep = "")
    
    formulas <- list("Cockroft-Gault"=c(paste("\nC ### Cocroft-Gault - US units (mg/dL, kg) ### returns: ml/min",
                                              "\nC requires: SCR, AGE, WT, MALE",
                                              "\ncoef = 1",
                                              "\n&IF(MALE == 0) coef = 0.85",
                                              "\nCrCl_CG = ((140-AGE) * WT * coef)/(72*SCR)", sep = ""),
                                        paste("\nC ### Cocroft-Gault - SI units (μmol/l, kg) ### returns: ml/min",
                                              "\nC requires: SCR, AGE, WT, MALE",
                                              "\ncoef = 1.23",
                                              "\n&IF(MALE == 0) coef = 1.04",
                                              "\nCrCl_CG = ((140-AGE) * WT * coef)/SCR", sep = "")),
                     "CKD-EPI"=c(paste("\nC ### CKD-EPI - US units (mg/dL) ### returns: mL/min/1.73 m²",
                                       "\nC requires: SCR, AGE, MALE, BLACK",
                                       tCKD, sep = ""),
                                 paste("\nC ### CKD-EPI - SI units (μmol/l) ### returns: mL/min/1.73 m²",
                                       "\nC requires: SCR, AGE, MALE, BLACK",
                                       "\nCREAT = SCR / 88.4", tCKD, sep = "")),
                     "MDRD"=c(paste("\nC ### MDRD - US units (mg/dL) ### returns: mL/min/1.73 m²",
                                    "\nC requires: SCR, AGE, MALE, BLACK",
                                    "\ncoefF=1",
                                    "\ncoefB=1",
                                    "\n&IF(MALE == 0) coefF = 0.742",
                                    "\n&IF(BLACK == 1) coefB = 1.210",
                                    "\nCrCl_MDRD = 186 * SCR**(-1.154) * AGE**(-0.203) * coefF * coefB", sep = ""),
                              paste("\nC ### MDRD - SI units (μmol/L) ### returns: mL/min/1.73 m²",
                                    "\nC requires: SCR, AGE, MALE, BLACK",
                                    "\ncoefF=1",
                                    "\ncoefB=1",
                                    "\n&IF(MALE == 0) coefF = 0.742",
                                    "\n&IF(BLACK == 1) coefB = 1.210",
                                    "\nCrCl_MDRD = 32788 * SCR**(-1.154) * AGE**(-0.203) * coefF * coefB", sep = "")),
                     "MDRD-BUN"=c(paste("\nC ### MDRD-BUN - US units (SCR and BUN in mg/dL, ALB in g/dL) ### returns: mL/min/1.73 m²",
                                        "\nC requires: SCR, AGE, MALE, BLACK, BUN, ALBUMIN",
                                        "\ncoefF=1",
                                        "\ncoefB=1",
                                        "\n&IF(MALE == 0) coefF = 0.762",
                                        "\n&IF(BLACK == 1) coefB = 1.180", 
                                        "\nCrCl_MDRD-BUN = 170 * SCR**(-1.154) * AGE**(-0.203) * coefF * coefB * BUN**(-0.17) * ALB**(0.318)", sep = ""),
                                  paste("\nC ### MDRD-BUN - SI units (SCR in μmol/L, BUN in mmol/L, ALB in g/L) ### returns: mL/min/1.73 m²",
                                        "\nC requires: SCR, AGE, MALE, BLACK, BUN, ALBUMIN",
                                        "\ncoefF=1",
                                        "\ncoefB=1",
                                        "\n&IF(MALE == 0) coefF = 0.742",
                                        "\n&IF(BLACK == 1) coefB = 1.210",
                                        "\nCrCl_MDRD-BUN = 170 * SCR**(-1.154) * AGE**(-0.203) * coefF * coefB * (BUN/0.3571)**(-0.17) * (ALB/10)**(0.318)", sep = "")),
                     "Mayo"=c(paste("\nC ### Mayo Quadratic - US units (SCR in mg/dL) ### returns: mL/min/1.73 m²",
                                    "\nC requires: SCR, AGE, MALE",
                                    "\ncoefF=1",
                                    "\n&IF(SCR < 0.8) SCR=0.8",
                                    "\n&IF(MALE == 0) coefF = 0.205",
                                    "\nCrCl_MCQ = 2.718**(1.911 + 5.249/SCR - 2.114/SCR**2 - 0.00686 * AGE - coefF)", sep = ""),
                              paste("\nC ### Mayo Quadratic - SI units (SCR in μmol/L) ### returns: mL/min/1.73 m²",
                                    "\nC requires: SCR, AGE, MALE",
                                    "\ncoefF=1",
                                    "\nSCR = SCR / 88.4",
                                    "\n&IF(SCR < 0.8) SCR=0.8",
                                    "\n&IF(MALE == 0) coefF = 0.205",
                                    "\nCrCl_MCQ = 2.718**(1.911 + 5.249/SCR - 2.114/SCR**2 - 0.00686 * AGE - coefF)", sep = "")),
                     "Schwartz"=c(paste("\nC ### Schwartz - US units (SCR in mg/dL, height in cm) ### returns: mL/min/1.73 m²",
                                        "\nC requires: SCR, AGE, HEIGHT, PRETERM",
                                        "\nk=0.55",
                                        "\n&IF(AGE.LT.1 .AND. PRETERM==1) k=0.33",
                                        "\n&IF(AGE.LT.1 .AND. PRETERM==0) k=0.45",
                                        "\nCrCl_Schwartz = k * HEIGHT / SCR", sep = ""),
                                  paste("\nC ### Schwartz - SI units (SCR in μmol/L, height in cm) ### returns: mL/min/1.73 m²",
                                        "\nC requires: SCR, AGE, HEIGHT, PRETERM",
                                        "\nSCR = SCR/88.4",
                                        "\nk=0.55",
                                        "\n&IF(AGE.LT.1 .AND. PRETERM==1) k=0.33",
                                        "\n&IF(AGE.LT.1 .AND. PRETERM==0) k=0.45",
                                        "\nCrCl_Schwartz = k * HEIGHT / SCR", sep = "")),
                     "Jelliffe73"=c(paste("\nC #### Jellife 1973 - US units (scr in mg/dL) ### returns: mL/min/1.73 m²",
                                          "\nC requires: SCR, AGE, MALE",
                                          "\nCrCl_Jelliffe = (98-16*((AGE-20)/20))/SCR",
                                          "\n&IF(MALE == 0) CrCl_Jelliffe = 0.9 * CrCl_Jelliffe", sep = ""),
                                    paste("\nC #### Jellife 1973 - SI units (scr in μmol/l) ### returns: mL/min/1.73 m²",
                                          "\nC requires: SCR, AGE, MALE",
                                          "\nCrCl_Jelliffe = (98-16*((AGE-20)/20))/(SCR/88.4)",
                                          "\n&IF(MALE == 0) CrCl_Jelliffe = 0.9 * CrCl_Jelliffe", sep = ""))
                     # "Jelliffe73"=c(paste("\nC #### Jellife 1972 - US units (scr in mg/dL) ### returns: mL/min/1.73 m²",
                     #                      "\nC requires: SCR1, SCR2, TIME1, TIME2, WT, AGE, MALE",
                     #                      "\n&IF(MALE==1) ESS = WT * (29.3 - (0.203 * AGE))",
                     #                      "\n&IF(MALE==0) ESS = WT * (25.1 - (0.175 * AGE))",
                     #                      "\navg_creat = (CREAT1 + CREAT2) / 2",
                     #                      "\nESS_cor = ESS * (1.035 - (0.0337 * avg_creat))",
                     #                      "\nE = ESS_cor - 4 * WT * (CREAT2 - CREAT1) / (TIME2 - TIME1)",
                     #                      "\nCRCL = E / (14.4 * avg_creat)"),
                     #                paste("\nC #### Jellife 1972 - SI units (scr in μmol/l) ### returns: mL/min/1.73 m²",
                     #                      "\nC requires: SCR1, SCR2, TIME1, TIME2, WT, AGE, MALE",
                     #                      "\nSCR1 = SCR1/88.4
                     #                      "\nSCR2 = SCR2/88.4
                     #                      "\n&IF(MALE==1) ESS = WT * (29.3 - (0.203 * AGE))",
                     #                      "\n&IF(MALE==0) ESS = WT * (25.1 - (0.175 * AGE))",
                     #                      "\navg_creat = (CREAT1 + CREAT2) / 2",
                     #                      "\nESS_cor = ESS * (1.035 - (0.0337 * avg_creat))",
                     #                      "\nE = ESS_cor - 4 * WT * (CREAT2 - CREAT1) / (TIME2 - TIME1)",
                     #                      "\nCRCL = E / (14.4 * avg_creat)"),
    )
    
    cat("\nCopy the code below into the #sec block of your model file, renaming the covariates as needed:\n")
    cat(unlist(formulas[formula])[as.integer(SI)+1])
    
  } # end of PRINT Fortran code
  else {
    
    # checks
    if (missing(scr)){
      stop("Missing serum creatinine values.", call. = F)
    }
    if (missing(age)){
      stop("Missing age values.", call. = F)
    }
    if (missing(male) & formula!="Schwartz"){
      stop("Missing vector with gender information.", call. = F)
    }
    
    # rudimentary unit check - in SI units, scr < 10 is very unlikely, so is albumin < 10
    if (SI){
      if (mean(scr)<10) warning("Creatinine levels are very low, are you sure they are in μmol/l?", call. = F)
      if (!missing(albumin)) if (mean(albumin)<10) warning("Albumin levels are very low, are you sure they are in g/l?", call. = F)
    } else {
      if (mean(scr)>10) warning("Creatinine levels are very high, are you sure they are in mg/dL?", call. = F)
      if (!missing(albumin)) if(mean(albumin)>10) warning("Albumin levels are very high, are you sure they are in g/dL?", call. = F)
    }
    
    # if SI, convert to US (it's shameful)
    if (SI){
      scr <- scr / 88.4
      if (!missing(albumin)) albumin <- albumin/10
      if (!missing(BUN)) BUN <- BUN/0.3571
    }
    
    
    # call function with corresponding formula
    switch(formula,
           "Cockroft-Gault"={rval <- CG(scr, age, wt, male)},
           "CKD-EPI"= {rval <- CKD_EPI(scr,age,male,black)},
           "MDRD"={rval <- MDRD(scr, age, male, black)},
           "MDRD-BUN"={rval <- MDRD_BUN(scr, age, male, black, BUN, albumin)},
           "Mayo"={rval <- Mayo(scr, age, male)},
           "Schwartz"={rval <- Schwartz(scr, age, height, preterm)},
           #           "Jelliffe72"={rval <- Jelliffe72(scr, scr2, time1, time2, wt, age, male)},
           "Jelliffe73"={rval <- Jelliffe73(scr, age, male)}
    )
    return(rval)
  }
}

# R6 ----------------------------------------------------------------------

#' @title Optimal Sample Times
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Contains optimal sampling times for a given model and dosage regimen.
#' @details
#' This object contains
#' the methods to create and results from optimal sampling algorithms. 
#' Currently the only option multiple-model optimization. 
#' This algorithm was published as 
#' Bayard, David S., and Michael Neely. "Experiment Design for Nonparametric
#' Models Based on Minimizing Bayes Risk: Application to Voriconazole."
#' Journal of Pharmacokinetics and Pharmacodynamics 44, no. 2 (April 2017):
#' 95–111. https://doi.org/10.1007/s10928-016-9498-5. It calculates the 
#' requested number of sample times where the concentration time profiles 
#' are the most separated, thereby minimizing the risk of choosing the incorrect 
#' Bayesian posterior for an individual. Future updates will add D-optimal
#' sampling times.
#' @author Michael Neely
#' @export
PM_opt <- R6::R6Class(
  "PM_opt",
  public <- list(
    #' @field sampleTime The optimal sample times, based on requested `type`
    #' argument to `$new` creation method.
    sampleTime = NULL,
    #' @field bayesRisk Only present for MM-optimal sampling.
    #' The Bayesian risk of mis-classifying a subject based on 
    #' the sample times.  This is more useful for comparisons between sampling 
    #' strategies, with minimization the goal.
    bayesRisk = NULL,
    #' @field simdata A [PM_sim] object with the simulated profiles
    simdata = NULL,
    #' @field mmInt A list with the `mmInt` values, `NULL` if `mmInt` argument
    #' to `$new` is missing.
    mmInt = NULL,
    #' @description
    #' `r lifecycle::badge("stable")`
    #'
    #' Determine optimal sample times which are the most informative about the 
    #' model parameters.
    #' 
    #' @details
    #' Currently, the only option is the multiple-model optimization algorithm.
    #'
    #' @param poppar There are several choices for the population parameters.
    #' * A [PM_result] loaded with [PM_load], in which case the `$final` field
    #' will be used, e.g. `run1 <- PM_load(1)` and `poppar = run1`.
    #' * A [PM_final] object, typically as a field in a [PM_result], e.g.,
    #' `poppar = run1$final`.
    #' * A list containing three items in this order,
    #' but of any name: vector of weights, vector of mean parameter values, and a
    #' covariance matrix. If only one distribution is to be specified the
    #' `weights` vector should be of length 1 and contain a 1. If multiple
    #' distributions are to be sampled, the `weights` vector should be of
    #' length equal to the number of distributions and its values should sum to 1,
    #' e.g. `c(0.25,0.05,0.7)`.  The means matrix may be a vector for a
    #' single distribution, or a matrix with `length(weights)` rows and
    #' number of columns equal to the number of parameters.
    #' @param model One of three choices:
    #' * [PM_model] object 
    #' * Character vector with the filename of a Pmetrics model in the working directory.
    #' * If `model` is missing, there are two possibilities:
    #' **  When `poppar` is a [PM_result] with a valid `$model` field,  
    #' the model in `poppar` will be used. 
    #' ** In the absence of a `poppar` with a valid model field, 
    #' look for a file called "model.txt" in the working directory.
    #' @param data One of three choices:
    #' * [PM_data] object 
    #' * Character vector with the filename of a Pmetrics data in the working directory.
    #' * If `data` is missing, there are two possibilities:
    #' **  When `poppar` is a [PM_result] with a valid `$data` field,  
    #' the data in `poppar` will be used. 
    #' ** In the absence of a `poppar` with a valid data field, 
    #' look for a file called "data.csv" in the working directory.
    #' In either choice, the value for outputs
    #' can be coded as any number(s) other than -99.  
    #' The number(s) will be replaced in the simulator output with the simulated values.
    #' @param nsamp The number of MM-optimal sample times to compute; default 
    #' is 1, but can be any number.  Values >4 will take an exponentially longer time.
    #' @param weight List whose names indicate the type of weighting, and 
    #' values indicate the relative weight. Values should sum to 1.  
    #' Names can be any of the following:
    #' * **none** The default. MMopt times will be chosen to maximally discriminate all responses at all times.
    #' * **AUC** MMopt times will be chosen to maximally discriminate AUC, regardless of the shape of the response profile.
    #' * **max** MMopt times will be chosen to maximally discriminate maximum, regardless of the shape of the response profile.
    #' * **min** MMopt times will be chosen to maximally discriminate minimum, regardless of the shape of the response profile.
    #'
    #' Any combination of AUC, max, and min can be chosen.  If "none" is specified, other
    #' weight types will be ignored and the relative value will be set to 1.
    #' For example,`list(auc = 0.5,max = 0.5)` or `list(auc = 0.2, min = 0.8)`.
    #' The default is `list(none = 1)`.
    #' @param predInt The interval in fractional hours for simulated predicted outputs at times other than those specified in the template `data`.
    #' The default is 0.5, which means there will be simulated outputs every 30 minutes from time 0 up
    #' to the maximal time in the template file.  You may also specify `predInt`
    #' as a vector of 3 values, e.g. `c(1,4,1)`, similar to the R command [seq], where the
    #' first value is the start time, the second is the stop time, and the third is the
    #' step value.  Outputs for times specified in the template file will also be simulated.
    #' To simulate outputs *only* at the output times in the template data (i.e. EVID=0 events), use `predInt = 0`.
    #' Note that the maximum number of predictions total is 594, so the interval must be sufficiently large to accommodate this for a given
    #' number of output equations and total time to simulate over.  If `predInt` is set so that this cap is exceeded, predictions will be truncated.
    #' @param mmInt Specify the time intervals from which MMopt times can be selected.
    #' These should only include simulated times specified by `predInt`.
    #' @param algorithm Optimal sampling algorithm. Currently not modifiable and
    #' the only option is "mm".
    #' @param outeq Output equation to optimize
    #' @param clean Boolean parameter to specify whether temporary files made in the
    #' course of the simulation run should be deleted. Defaults to `TRUE`.
    #' This is primarily used for debugging.
    #' @param ... Other parameters to pass to [PM_sim]$new(). Most are not necessary,
    #' but `usePost = TRUE` can be used to calculate individual MMopt times. 
    #' In this case, the number of posterior distributions contained in 
    #' `poppar$final$postPoints` needs to match the number of subjects in `data`.
    #' You can also pass `include` and `exclude` to limit the subjects used in 
    #' `data`. This will work whether `usePost` is `TRUE` or `FALSE`. 
    #' Note that the following arguments to [PM_sim]$new cannot be modified.
    #' * `nsim` is zero
    #' * `outname` is "MMsim"
    #' * `combine` is `TRUE`
    initialize = function(poppar, model, data, nsamp = 1, weight = list(none = 1),
                          predInt = 0.5, mmInt, algorithm = "mm",
                          outeq = 1, ...) {
      if(missing(poppar)){
        cat(crayon::red("Error:"), "poppar is required.\n")
        return(NULL)
      }
      if(missing(model)){
        model <- NULL
      }
      if(missing(data)){
        data <- NULL
      }
      if(missing(mmInt)){
        mmInt <- NULL
      }
      
      tryCatch(private$make(poppar = poppar, 
                            model = model, 
                            data = data, 
                            nsamp = nsamp, 
                            weight = weight,
                            predInt = predInt, 
                            mmInt = mmInt, 
                            algorithm = algorithm,
                            outeq = outeq, 
                            ...,), error = function(e){
                              cat(crayon::red("Error:"), e$message, "\n")
                            }
      )
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PM_opt].
    #' @param ... Arguments passed to [plot.PM_opt]
    plot = function(...) {
      tryCatch(plot.PM_opt(self, ...), error = function(e){
        cat(crayon::red("Error:"), e$message, "\n")
      }
      )
    },
    #' @description
    #' Print method
    #'
    #' @return Prints the optimal sampling times and Bayes Risk.
    print = function() {
      cat("Multiple model optimal sample times\n")
      cat("-----------------------------------\n")
      for (i in 1:length(self$sampleTime)) {
        cat(paste("Sample ", i, ": ", self$sampleTime[i], "\n", sep = ""))
      }
      cat(paste("\nBayes Risk: ", self$bayesRisk, "\n", sep = ""))
    }
  ), #end public
  private = list(
    make = function(poppar, model, data, nsamp = 1, weight = list(none = 1),
                    predInt = 0.5, mmInt, outeq = 1, 
                    algorithm = "mm", clean = TRUE,...) {
      
      #get defaults for PM_sim$new() arguments
      #browser()
      arglist <- list(...)
      arglist$usePost <- ifelse(is.null(arglist$usePost), FALSE, arglist$usePost)
      arglist$quiet <- ifelse(is.null(arglist$quiet), TRUE, arglist$quiet)
      arglist$obsNoise <- ifelse(is.null(arglist$obsNoise), NA, arglist$obsNoise)
      if(!is.null(arglist$clean)){
        clean_opt <- arglist$clean
        arglist$clean <- FALSE
      } else {
        clean_opt <- TRUE
        arglist$clean <- FALSE
      }
      
      if (inherits(poppar, "PM_result")) {
        if (!inherits(poppar$final, "NPAG")) {
          cat(crayon::red("Error:"), "Prior run must be NPAG.")
          return(NULL)
        }
        popPoints <- if(usePost){
          poppar$final$postPoints
        } else {
          poppar$final$popPoints
        }  
        model <- if(is.null(model)){
          poppar$model
        } else {
          "model.txt"
        } 
        data <- if(is.null(data)){
          poppar$data
        } else {
          "data.csv"
        }
      } else if (all(c("NPAG", "PM_final") %in% class(poppar))) { 
        popPoints <- if(usePost){
          poppar$postPoints
        } else {
          poppar$popPoints
        }  
        model <- if(is.null(model)){
          "model.txt"
        } else {
          model
        } 
        data <- if(is.null(data)){
          "data.csv"
        } else {
          data
        }
      } else { #poppar was a list
        
        model <- if(is.null(model)){
          "model.txt"
        } else {
          model
        } 
        data <- if(is.null(data)){
          "data.csv"
        } else {
          data
        }
      }
      
      # remove prior simulations if they exist
      old <- Sys.glob("MMsim*.txt")
      invisible(file.remove(old))
      # simulate each point
      simdata <- do.call(PM_sim$new, (c(poppar = poppar,
                            model = model,
                            data = data, nsim = 0, 
                            predInt = predInt,
                            outname = "MMsim",
                            combine = TRUE,
                            arglist #the other args
                            ))
      )
      
      
      simdata$obs <- simdata$obs %>% filter(outeq == !!outeq)
      
      # transform into format for MMopt
      # nsubs is the number of subjects
      nsubs <- length(unique(simdata$obs$id))
      # parse mmInt
      if (!missing(mmInt) && !is.null(mmInt)) {
        simdata_full <- simdata
        if (!inherits(mmInt, "list")) {
          mmInt <- list(mmInt)
        } # mmInt was a single vector; make a list of 1
        simdata$obs <- purrr::map(mmInt, function(t) {
          dplyr::filter(simdata$obs, time >= t[1] & time <= t[2])
        }) %>%
          dplyr::bind_rows() %>%
          dplyr::arrange(id, time)
      } else {
        mmInt <- NULL
        simdata_full <- simdata
      }
      
      # time is the simulated times
      time <- unique(simdata$obs$time)
      # nout is the number of simulated times (outputs)
      nout <- length(time)
      # Mu is a matrix of nout rows x nsubs columns containing the outputs at each time
      Mu <- t(matrix(simdata$obs$out, nrow = nsubs, byrow = T))
      
      # pH is the vector of probabilities of each population point
      pH <- popPoints[, ncol(popPoints)]
      # replicate pH and normalize based on number of simulation templates
      ntemp <- nsubs / nrow(popPoints)
      pH <- rep(pH, ntemp)
      pH <- pH / ntemp
      numeqt <- max(simdata$obs$outeq)
      # get the assay error from the simulated output
      simout <- readLines("MMsim1.txt")
      errLine <- grep(" EQUATIONS, IN ORDER, WERE:", simout)
      cassay <- scan("MMsim1.txt", n = 4, skip = errLine + numeqt - 1, quiet = T)
      
      # make the weighting Matrix
      wtnames <- names(weight)
      Cbar0 <- array(NA,
                     dim = c(nsubs, nsubs, 4),
                     dimnames = list(a = 1:nsubs, b = 1:nsubs, type = c("none", "auc", "cmax", "cmin"))
      )
      
      # default is no penalties (diag=0, off-diag=1)
      if ("none" %in% wtnames) {
        Cbar0[, , 1] <- matrix(1, nrow = nsubs, ncol = nsubs)
        diag(Cbar0[, , 1]) <- 0
      } else {
        if (sum(unlist(weight)) != 1) {
          stop("Relative weights do not sum to 1.\n")
        } else {
          if ("auc" %in% wtnames) {
            auc <- makeAUC(simdata)
            sqdiff <- matrix(sapply(1:nsubs, function(x) (auc$tau[x] - auc$tau)^2), nrow = nsubs)
            cbar <- cbar_make1(sqdiff)
            Cbar0[, , 2] <- weight$auc * cbar / mean(cbar)
          }
          
          if ("max" %in% wtnames) {
            maxi <- unlist(tapply(simdata$obs$out, simdata$obs$id, max))
            sqdiff <- matrix(sapply(1:nsubs, function(x) (maxi[x] - maxi)^2), nrow = nsubs)
            cbar <- cbar_make1(sqdiff)
            Cbar0[, , 3] <- weight$max * cbar / mean(cbar)
          }
          
          if ("min" %in% wtnames) {
            mini <- unlist(tapply(simdata$obs$out, simdata$obs$id, min))
            sqdiff <- matrix(sapply(1:nsubs, function(x) (mini[x] - mini)^2), nrow = nsubs)
            cbar <- cbar_make1(sqdiff)
            Cbar0[, , 4] <- weight$min * cbar / mean(cbar)
          }
          notWt <- which(!wtnames %in% c("auc", "min", "max", "none"))
          if (length(notWt) > 0) {
            cat(paste("The following parameters are not valid weighting factors and were ignored: ", paste(wtnames[notWt], collapse = ", "), ".\n", sep = ""))
          }
        }
      }
      # find max value over all selected weights (condense to nsubs x nsubs matrix)
      Cbar <- apply(Cbar0, c(1, 2), max, na.rm = T)
      
      # Call MMMOPT1 routine to compute optimal sampling times
      mmopt1 <- private$wmmopt1(Mu, time, pH, cassay, nsamp, nsubs, nout, Cbar)
      optsamp <- mmopt1$optsamp
      brisk <- mmopt1$brisk_cob
      optindex <- mmopt1$optindex
      
      
      
      # -------------------------
      
      
      if (clean_opt) {
        invisible(file.remove(Sys.glob(c("fort.*", "*.Z3Q", "*.ZMQ", 
                                         "montbig.exe", "ZMQtemp.csv", 
                                         "simControl.txt", "seedto.mon", 
                                         "abcde*.csv",
                                         "MMsim*.txt",
                                         "*.for")),
                              "simmodel.txt",
                              "simdata.csv"))
      }
      
      self <- list(
        sampleTime = optsamp[1:nsamp, nsamp],
        bayesRisk = brisk[nsamp],
        simdata = simdata_full, mmInt = mmInt
      )
      return(self)
    },
    #' @details
    #' This routine computes the MMOPT 1,2,3 and 4-sample optimal sample designs taking into account an additional weighting matrix C
    #' @author David S. Bayard, February 22,2015, Alona Kryshchenko, Michael Neely
    #' @param Mu (nt)x(nsubs), Simulated output responses for all models (no noise)
    #'            nt=# time points
    #'              nsubs=# subjects
    #'              Matrix Structure: Time response down, model index across
    #' @param time (nt)x1, time axis
    #' @param pH (ns)x1, Bayesian Prior probabilities (sum(pH)=1)
    #' @param cassay 4x1, coefficients in assay polynomial:
    #'        1-sigma assay error = \eqn{c0 + c1 * y + c2 * y^2 + c3 * y^3}
    #' @param nsamp desired # of samples in experiment design
    #' @param nsubs Number of subjects
    #' @param nout Number of output equations
    #' @param Cbar (nsubs)x(nsubs), Matrix of elements [cbar_ij] derived from matrix where c_ij is cost incurred from
    #'        mistaking i'th support point (truth) to be j'th support point
    #'        (wrong classification). Intuitively, you are giving jth subject's
    #'         dose with response a_j to
    #'         ith subject with response a_ij, so that the control cost is
    #'              c_ij=w_ij*(a_ij-a_j)^2,  where w_ij can be an arbitrary
    #'              additional weighting function of i and j
    #'         Key property: c_ii=0 for 1=1,...,nsubs, i.e., there is no cost for
    #'           getting classification correct
    #'         cbar_ij is the max(cbar_ij, t(cbar_ij)), or max(cbar_ij,cbar_ji)
    #' @return A list with the following elements
    #' * optsamp - 4x4, optimal samples times by column. Column i contains the optimal design for i samples, "-1" indicates "not applicable"
    #' * brisk_cob - 4x1, Bayes risk cost overbound; brisk_cob(i) is the Bayes Risk cost overbound associated with using the optimal design having i samples; "-1" indicates "not applicable".
    #' * ptindex - 4x4, indices of optimal sample times from time=(nt)x1
    #' @noRd
    wmmopt1 = function(Mu, time, pH, cassay, nsamp, nsubs, nout, Cbar) {
      
      # Initialize all entries with -1
      optsamp <- matrix(-1, nsamp, nsamp)
      optindex <- matrix(-1, nsamp, nsamp)
      brisk <- matrix(-1, nsamp, 1)
      nopt <- matrix(-1, nsamp, 1)
      
      
      # -------------------------------
      # Extract needed quantities
      
      c0 <- cassay[1]
      # additive noise
      c1 <- cassay[2]
      c2 <- cassay[3]
      c3 <- cassay[4]
      
      
      # BEGIN MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
      # FULL SCRIPT VERSION OF MMOPT ALGORITHM
      # -----------------------------------
      # Compute Kall
      kallijn <- private$kall_ijn(Mu, c0, c1, c2, c3, nsubs, nout)
      Kall <- kallijn$Kall
      skall <- kallijn$skall
      
      
      # Vectorized optimization for any number of samples
      search_grid <- data.frame(t(combn(1:nout, nsamp))) %>% dplyr::rowwise()
      pb <- progress::progress_bar$new(total = nrow(search_grid))
      Perror <- search_grid %>%
        dplyr::summarise(val = private$perrorc1(pH, Kall, nvec = dplyr::c_across(dplyr::everything()), Cbar, pb))
      
      nopt <- search_grid[which(Perror$val == min(Perror$val)), ] %>%
        purrr::as_vector(nopt[1, ]) %>%
        sort()
      # vctrs::vec_sort()
      
      # Compute Output Values
      
      optsamp[1:nsamp, nsamp] <- time[nopt]
      optindex[1:nsamp, nsamp] <- nopt
      brisk_cob <- rep(-1, nsamp)
      brisk_cob[nsamp] <- min(Perror)
      return(list(optsamp = optsamp, brisk_cob = brisk_cob, optindex = optindex))
    },
    kall_ijn = function(Mu, c0, c1, c2, c3, nsubs, nout) {
      # KALL_IJN.R
      #
      # Routine to Create Full K matrix
      # K=Kall(i,j,n) where,
      #
      # Kall(i,j,n) -(nsubs)x(nsubs)x(nt) 3D matrix, Risk coefficient as function of i,j and n
      # i = i'th patient
      # j = j'th patient
      # n = n'th time in horizon, n=1,...,nt
      # nsubs = # subjects
      # nout=nt = # times in time horizon
      
      # INPUTS
      # -------
      # Mu = (nout)X(nsubs), Mean of Assay for nout=# output values, nsubs=#subjects
      # n - time index to evaluate Kmat on
      # c0,c1,c2,c3 - assay error polynomial coefficients
      #
      # OUTPUTS
      # -------
      # Kall - (nsubs)x(nsubs)x(nt) 3D matrix
      # skall - (nout)x1, Frobenius norm over i,j of Kall(i,j,n) becomes function of
      #                 n=1:nout (stacked as vector)
      #
      # Written by David S. Bayard, October 14, 2013
      #
      #
      # --------------
      
      # Make full K matrix
      Kall <- array(0, dim = c(nsubs, nsubs, nout))
      skall <- matrix(0, nout, 1)
      # to store norm as function of n=1:nout
      # fill Kall
      for (n in 1:nout) {
        Kn <- private$kmat_ijn(Mu, n, c0, c1, c2, c3, nsubs, nout)
        skall[n] <- norm(Kn, "F")
        # Frobenious norm
        Kall[, , n] <- Kn
      }
      return(list("Kall" = Kall, "skall" = skall))
    },
    kmat_ijn = function(Mu, n, c0, c1, c2, c3, nsubs, nout) {
      # KMAT_IJN.R
      #
      # This routine is used to construct 3D matrix K(i,j,n) by routine
      # kall_ijn.m which calls current routine once for each n=1,...,nt
      # --------------------------------------
      # Routine to compute Kn=K(i,j,n)|_n,
      # i.e., evaluation of  K(i,j,n) on n'th time in horizon
      # where
      # K(i,j,n) -(nsubs)x(nsubs) matrix, Lars risk coefficient as function of i,j and n
      # i = i'th patient
      # j = j'th patient
      # n = n'th time in horizon, n=1,...,nt
      # nsubs = # subjects
      # nt = # times in time horizon
      # -------------
      #
      # INPUTS
      # -------
      # Mu = (nout)x(ns) (e.g., 96x10), true responses for nout=# output values, ns=#subjects
      # n - time index to evaluate Kn on
      # c0,c1,c2,c3 - assay error polynomial coefficients
      #
      # OUTPUTS
      # -------
      # Kn -(nsubs)x(nsubs), Kn=K(i,j,n)|_n, i.e., evaluation of K(i,j,n) on n'th time in horizon
      #
      # Written by David S. Bayard, October 14, 2013
      # -----------------------------------
      #
      # Make Sig2 by evaluating noise on Mu using assay polynomial
      yout_n <- Mu[n, ] # yout_n= (nsubs)x1
      Sig2 <- (c0 * matrix(1, nsubs, 1) + c1 * yout_n + c2 * yout_n^2 + c3 * yout_n^3)^2
      # CCCCCCCCCCCCCCCCCCCCCCCCCCCC
      # CHECKPOINT
      # yout_n - (nsubs)x1, contains means of each subject (at n'th assay time)
      # Sig2 = (nsubs)x1, contains covariance Sig2 of noise for each subject
      # CCCCCCCCCCCCCCCCCCCCCCCCCCCC
      #
      # --------------------
      # @@ SLOW APPROACH
      # Make Kijn
      #   if (1==0){
      #     # Create storage
      #     Kijn<-matrix(0,nsubs,nsubs);
      #     # Stuff Kijn for each i,j
      #     for (i in 1:nsubs){
      #       for (j in 1:nsubs){
      #         Sig2plus<-Sig2[i]+Sig2[j];
      #         Sig2prod<-Sig2[i]*Sig2[j];
      #         Kijn[i,j]<- (1/4)*( yout_n[j]-yout_n[i] )^2/Sig2plus+ (1/2)*log(Sig2plus/2)- (1/4)*log(Sig2prod);
      #       }
      #     }
      #     # --
      #   }
      # -------------
      # @@ FAST VECTORIZED REPLACEMENT
      
      Sig2plus <- Sig2 %*% matrix(1, 1, nsubs) + matrix(1, nsubs, 1) %*% t(Sig2)
      Sig2prod <- (Sig2 %*% matrix(1, 1, nsubs)) * (matrix(1, nsubs, 1) %*% t(Sig2))
      Mun <- yout_n
      # column vector
      Mun_minus <- Mun %*% matrix(1, 1, nsubs) - matrix(1, nsubs, 1) %*% t(Mun)
      Kijn <- (1 / 4) * (Mun_minus^2) / Sig2plus + (1 / 2) * log(.5 * Sig2plus) - (1 / 4) * log(Sig2prod)
      
      # ------------------------
      # Create output variable
      Kn <- Kijn
      return(Kn)
    },
    perrorc1 = function(pH, Kall, nvec, Cbar, pb) {
      # PERRORC1.R
      #
      # Routine to compute Bayes Risk Overbound, evaluated on
      # vector of candidate sample times specified in "nvec"
      #
      # INPUTS
      # ------
      # pH - (nsub)x1 prior on all hypotheses (i.e., sum(pH)=1))
      # Kall - (nsub)x(nsub)x(nout)
      # nvec - vector of candidate sample times to evaluate Perror on
      #            nvec = [n1
      #                    n2
      #                    :
      #                    nsamp]
      # Cbar - (nsub)x(nsub), matrix of control overbound error weights {cbar_ij}
      #    where cbar_ij=max(cij,cji)
      #
      # OUTPUTS
      # -------
      # Perror - MMopt upper bound on Bayes Risk, evaluated on the
      #          candidate sample times specified in nvec
      #
      # Written by David S. Bayard, October 14, 2013
      # --------------------
      pb$tick() # increment progress bar
      nsubs <- dim(Kall)[1]
      nxx <- dim(Kall)[2]
      nout <- dim(Kall)[3]
      nsamp <- length(nvec)
      pH5 <- sqrt(pH)
      #
      # Create Kallall
      # Example: For nsamp=4 sampling times [n1,n2,n3,n4] we have
      #         Kallall=Kall(:,:,n1)+Kall(:,:,n2)+Kall(:,:,n3)+Kall(:,:,n4);
      
      # Replace following statement with ONE statement below
      # ----------------
      # @@ LOOP APPROACH
      Kallsum <- matrix(0, nsubs, nsubs)
      for (n in 1:nsamp) {
        nn <- nvec[n]
        Kallsum <- Kallsum + Kall[, , nn]
      }
      # --
      # ----------------
      # @@  VECTORIZED APPROACH
      #   if (fast){
      #     # Replace with: (extracts nvec indices of Kall(:,:,nvec) and sums
      # Kallsum <- apply(Kall[ , ,nvec],c(1,2), sum);
      #   }
      # ---------------------------------
      ExpKallsum <- exp(-Kallsum)
      ExpKallsum0 <- ExpKallsum - diag(diag(ExpKallsum))
      # remove diagonal
      # Evaluate Lar upper bound
      # (note- Lars' formula has sum over the upper half of
      # the symmetric matrix ExpKallsum0, while I am summing over ENTIRE matrix, hence the
      # factor of 1/2 below:
      Perror <- .5 * t(pH5) %*% (ExpKallsum0 * Cbar) %*% pH5
      return(Perror)
    },
    cbar_make1 = function(C) {
      # CBAR_MAKE1.R
      #
      # Routine to make Cbar matrix from C matrix
      # used for MMopt control-relevant expt design
      #
      # INPUTS
      # ------
      # C    - (nsubs)x(nsubs), Matrix of elements (c_ij) where c_ij is cost incurred from
      #        mistaking i'th support point (truth) to be j'th support point
      #        (wrong classification). Specifically, you are giving the jth dose
      #        dose_j to the ith subject, giving response a_ij when the desired
      #        response is a_j. Hence, elements of C are given by the formula:
      #         c_ij=w_ij*(a_ij-a_j)^2
      #         Key property: c_ii=0 for 1=1,...,nsubs
      #
      # OUTPUTS
      # ------
      # Cbar - (nsubs)x(nsubs), Matrix of elements (cbar_ij} where
      #               cbar_ij=max(c_ij,c_ji)
      #       and c_ij is an element of the C matrix defined next
      # -------------------
      #
      nsubs <- dim(C)[1]
      Cbar <- apply(array(c(C, t(C)), dim = c(nsubs, nsubs, 2)), c(1, 2), max)
      return(Cbar)
    }
  ) #end private
) #end PM_opt


# PLOT --------------------------------------------------------------------


#' @title Plot Pmetrics Multiple-Model Optimal Sampling Objects
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Plots [PM_opt] objects
#' @details
#' Simulated observations are plotted on the y-axis vs. time on the x.axis.
#' Optimal sampling times are indicated as vertical lines. Defaults for optimal
#' sample times are red, dash, width 2. Defaults for the `line` format are as
#' for [plot.PM_sim].
#'
#' @method plot PM_opt
#' @param x A [PM_opt] object
#' @param probs Default is NA. See [plot.PM_sim] for details.
#' @param line Passed to [plot.PM_sim] with default as `list(probs = NA)`.
#' @param times Format the vertical lines for optimal times. Default is
#' dashed red line. r template("line")`
#' @param ... Other parameters to pass to [plot.PM_sim].
#' @return Plots the simulation profiles with MM optimal times indicated as vertical lines.
#' @author Michael Neely
#' @seealso [plot.PM_sim]
#' @export
#' @family PMplots


plot.PM_opt <- function(x, line = list(probs = NA), times = T, ...) {
  mm_format <- amendLine(times, default = list(color = "red", dash = "dash", width = 2))
  
  # parse dots
  arglist <- list(...)
  arglist$quiet <- T
  arglist$line <- line
  
  p <- do.call(plot.PM_sim, c(list(x$simdata), arglist))$p
  
  
  if (!is.null(x$mmInt)) { # add MM interval times
    shapeList <- lapply(x$mmInt, function(m) {
      list(
        type = "rect",
        x0 = m[1],
        y0 = 0,
        x1 = m[2],
        y1 = 1,
        xref = "x",
        yref = "paper",
        fillcolor = "grey",
        opacity = 0.1,
        line = list(width = 0)
      )
    })
  } else {
    shapeList <- list()
  }
  
  shapeList <- append(shapeList, lapply(
    x$sampleTime,
    function(t) {
      ab_line(v = t, line = mm_format)
    }
  ))
  
  p <- p %>% layout(shapes = shapeList)
  
  print(p)
}


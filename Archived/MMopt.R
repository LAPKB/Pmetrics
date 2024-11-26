# Old, not fully vectorized MMopt functions ---------------------------------------------------------

wmmopt1 <- function(Mu, time, pH, cassay, nsamp, nsubs, nout, Cbar) {
  # WMMOPT1.M
  #
  # This routine computes the MMOPT 1,2,3 and 4-sample optimal sample designs
  # taking into account an additional weighting matrix C
  #
  # Note: Created from mmopt1.m in order to incorporate control weighting
  #
  # Written by David S. Bayard, February 22,2015 and Alona Kryshchenko, and Michael Neely
  #
  # INPUTS
  # --------
  # Mu - (nout)x(nsubs), Simulated output responses for all models (no noise)
  #              @ nout=# time points
  #              @ nsubs=# subjects
  #              @ Matrix Structure: Time response down, model index across
  # time - (noutt)x1, time axis
  # pH - (nsubs)x1, Bayesian Prior probabilities (sum(pH)=1)
  # cassay - 4x1, coefficients in assay polynomial:
  #        1-sigma assay error = c0+c1*y+c2*y^2+c3*y^3
  # nsamp - desired # of samples in experiment design
  # Cbar    - (nsubs)x(nsubs), Matrix of elements [cbar_ij] derived from matrix where c_ij is cost incurred from
  #        mistaking i'th support point (truth) to be j'th support point
  #        (wrong classification). Intuitively, you are giving jth subject's
  #         dose with response a_j to
  #         ith subject with response a_ij, so that the control cost is
  #              c_ij=w_ij*(a_ij-a_j)^2,  where w_ij can be an arbitrary
  #              additional weighting function of i and j
  #         Key property: c_ii=0 for 1=1,...,nsubs, i.e., there is no cost for
  #           getting classification correct
  #         cbar_ij is the max(cbar_ij, t(cbar_ij)), or max(cbar_ij,cbar_ji)
  #
  # Note: All MMOPT designs are computed having less than or equal to nsamp
  # samples
  #
  # OUTPUTS
  # --------
  # optsamp - 4x4, optimal samples times by column
  #             @ Column i contains the optimal design for i samples
  #             @ "-1" indicates "not applicable"
  # brisk_cob   - 4x1, Bayes risk cost overbound
  #            brisk_cob(i) is the Bayes Risk cost overbound associated with
  #            using the optimal design having i samples
  #                "-1" indicates "not applicable"
  # optindex - 4x4, indices of optimal sample times from time=(nt)x1
  
  #
  
  # -------------------------------------
  # Initialize all entries with -1
  optsamp <- matrix(-1, 4, 4)
  optindex <- matrix(-1, 4, 4)
  brisk <- matrix(-1, 4, 1)
  nopt1 <- -1
  nopt2 <- matrix(-1, 2, 1)
  nopt3 <- matrix(-1, 3, 1)
  nopt4 <- matrix(-1, 4, 1)
  Perror1 <- -1
  Perror2 <- -1
  Perror3 <- -1
  Perror4 <- -1
  Perror1_min <- -1
  Perror2_min <- -1
  Perror3_min <- -1
  Perror4_min <- -1
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
  kallijn <- kall_ijn(Mu, c0, c1, c2, c3, nsubs, nout)
  Kall <- kallijn$Kall
  skall <- kallijn$skall
  
  # ------------------------------
  # SINGLE SAMPLE OPTIMIZATION
  if (nsamp == 1) {
    # 111111111111111111111111111111111111111111111111111111111111111
    # 111111111111111111111111111111111111111111111111111111111111111
    # 1-SAMPLE SEARCH: FULL COMBINATORIAL SEARCH
    # Perror1_stor=zeros(nout); # to save intermediary results
    #
    for (n1 in 1:nout) {
      nvec <- n1
      Perror1 <- perrorc1(pH, Kall, nvec, Cbar)
      # keep running optimal
      if (n1 == 1) {
        Perror1_min <- Perror1
        nopt1 <- n1
      }
      if (Perror1 < Perror1_min) {
        Perror1_min <- Perror1
        nopt1 <- n1
      }
      # Store Perror
      # Perror1_stor(n1)=Perror1;
    }
    Perror2 <- Perror2_min
    Perror3 <- Perror3_min
    # 111111111111111111111111111111111111111111111111111111111111111
  }
  # endif
  
  
  # ------------------------------
  # TWO SAMPLE OPTIMIZATION
  if (nsamp == 2) {
    # 222222222222222222222222222222222222222222222222222222222222222
    # 222222222222222222222222222222222222222222222222222222222222222
    # 222222222222222222222222222222222222222222222222222222222222222
    # 2-SAMPLE SEARCH: FULL COMBINATORIAL SEARCH
    # Perror2_stor=zeros(nout,nout); # to save intermediary results
    #
    for (n1 in 1:nout) {
      for (n2 in n1:nout) {
        nvec <- c(n1, n2)
        Perror2 <- perrorc1(pH, Kall, nvec, Cbar)
        # keep running optimal
        if ((n1 == 1) && (n2 == 1)) {
          Perror2_min <- Perror2
          nopt2 <- c(n1, n2)
        }
        if (Perror2 < Perror2_min) {
          Perror2_min <- Perror2
          nopt2 <- c(n1, n2)
        }
        #          # Store Perror
        #          Perror2_stor(n1,n2)=Perror2;
      }
    }
    Perror1 <- Perror1_min
    Perror3 <- Perror3_min
    # 222222222222222222222222222222222222222222222222222222222222222
  }
  # endif
  #
  # ------------------------------
  # THREE SAMPLE OPTIMIZATION
  if (nsamp == 3) {
    # 333333333333333333333333333333333333333333333333333333333333333
    # 333333333333333333333333333333333333333333333333333333333333333
    # 333333333333333333333333333333333333333333333333333333333333333
    # 3-SAMPLE SEARCH: FULL COMBINATORIAL SEARCH
    # Perror3_stor=zeros(nout,nout,nout); # to save intermediary results
    nout10 <- floor(nout / 10)
    #
    cat("\nComputing 3-sample design\n")
    pb <- txtProgressBar(min = 0, max = nout, style = 3)
    for (n1 in 1:nout) {
      setTxtProgressBar(pb, n1)
      for (n2 in n1:nout) {
        for (n3 in n2:nout) {
          nvec <- c(n1, n2, n3)
          Perror3 <- perrorc1(pH, Kall, nvec, Cbar)
          # keep running optimal
          if ((n1 == 1) && (n2 == 1) && (n3 == 1)) {
            Perror3_min <- Perror3
            nopt3 <- c(n1, n2, n3)
          }
          if (Perror3 < Perror3_min) {
            Perror3_min <- Perror3
            nopt3 <- c(n1, n2, n3)
          }
          # Store Perror
          #             Perror3_stor(n1,n2,n3)=Perror3;
        }
      }
    }
    close(pb)
    Perror1 <- Perror1_min
    Perror2 <- Perror2_min
    # 333333333333333333333333333333333333333333333333333333333333333
  }
  # endif
  #
  # ------------------------------
  # FOUR SAMPLE OPTIMIZATION
  if (nsamp == 4) {
    # 444444444444444444444444444444444444444444444444444444444444444
    # 444444444444444444444444444444444444444444444444444444444444444
    # 4-SAMPLE SEARCH: FULL COMBINATORIAL SEARCH
    # Careful: don't store anything or you will blow memory!!!!
    # Perror4_stor=zeros(nout,nout,nout,nout); # to save intermediary results
    cat("\nComputing 4-sample design\n")
    pb <- txtProgressBar(min = 0, max = nout, style = 3)
    for (n1 in 1:nout) {
      setTxtProgressBar(pb, n1)
      for (n2 in n1:nout) {
        for (n3 in n2:nout) {
          for (n4 in n3:nout) {
            nvec <- c(n1, n2, n3, n4)
            Perror4 <- perrorc1(pH, Kall, nvec, Cbar)
            # keep running optimal
            if ((n1 == 1) && (n2 == 1) && (n3 == 1) && (n4 == 1)) {
              Perror4_min <- Perror4
              nopt4 <- c(n1, n2, n3, n4)
            }
            if (Perror4 < Perror4_min) {
              Perror4_min <- Perror4
              nopt4 <- c(n1, n2, n3, n4)
            }
            # Store Perror
            #             Perror4_stor(n1,n2,n3,n4)=Perror4;
          }
        }
      }
    }
    close(pb)
    Perror1 <- Perror1_min
    Perror2 <- Perror2_min
    Perror3 <- Perror3_min
    # 444444444444444444444444444444444444444444444444444444444444444
  }
  # endif
  # -----------------------------------
  # Compute Output Values
  if (nsamp == 1) {
    optsamp[1, 1] <- time[nopt1]
  }
  if (nsamp == 2) {
    optsamp[1:2, 2] <- time[nopt2]
  }
  if (nsamp == 3) {
    optsamp[1:3, 3] <- time[nopt3]
  }
  if (nsamp == 4) {
    optsamp[1:4, 4] <- time[nopt4]
  }
  # --
  optindex[1, 1] <- nopt1
  optindex[1:2, 2] <- nopt2
  optindex[1:3, 3] <- nopt3
  optindex[1:4, 4] <- nopt4
  # --
  brisk_cob <- c(Perror1, Perror2, Perror3, Perror4)
  return(list(optsamp = optsamp, brisk_cob = brisk_cob, optindex = optindex))
}




#' Print a Pmetrics Simulation Diagnostic Object
#'
#' Includes the mean, variance, kurtosis, skewness of npde, as well as statistical tests.
#'
#' Print a summary of a PMdiag object made by \code{\link{PMdiag}}.
#'
#' @title Print a Pmetrics Simulation Diagnostic Object
#' @method print PMdiag
#' @param x A PMdiag object made by \code{\link{PMdiag}}.
#' @param \dots Other parameters which are not necessary.
#' @return A printed object.
#' @author Michael Neely
#' @seealso \code{\link{summary.PMop}}
#' @S3method print PMdiag
#' @references 
#' Brendel K, Comets E, Laffont CM, Laveille C, Mentre F (2006) Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, \bold{23}:2036-49


print.PMdiag <- function (x,...)
{

    kurtosis <- function (x) 
    {
        x <- x[!is.na(x)]
        m4 <- sum((x - mean(x))^4)
        m2 <- sum((x - mean(x))^2)
        kurt <- m4 * length(x)/(m2^2) - 3
        return(kurtosis = kurt)
    }
    
    skewness <- function (x) 
    {
       x <- x[!is.na(x)]
       m3 <- sum((x - mean(x))^3)
       m2 <- sum((x - mean(x))^2)
       skew <- m3/(m2 * sqrt(m2/length(x)))
       return(skewness = skew)
    }
    
    agostino.test <- function (x, alternative = c("two.sided", "less", "greater")){
        DNAME <- deparse(substitute(x))
        x <- sort(x[complete.cases(x)])
        n <- length(x)
        s <- match.arg(alternative)
        alter <- switch(s, two.sided = 0, less = 1, greater = 2)
        if ((n < 8 || n > 46340)) 
            stop("sample size must be between 8 and 46340")
        s3 <- (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
        y <- s3 * sqrt((n + 1) * (n + 3)/(6 * (n - 2)))
        b2 <- 3 * (n * n + 27 * n - 70) * (n + 1) * (n + 3)/((n - 
            2) * (n + 5) * (n + 7) * (n + 9))
        w <- sqrt(-1 + sqrt(2 * (b2 - 1)))
        d <- 1/sqrt(log10(w))
        a <- sqrt(2/(w * w - 1))
        z <- d * log10(y/a + sqrt((y/a)^2 + 1))
        pval <- pnorm(z, lower.tail = FALSE)
        if (alter == 0) {
            pval <- 2 * pval
            if (pval > 1) 
                pval <- 2 - pval
            alt <- "data have a skewness"
        }
        else if (alter == 1) {
            alt <- "data have positive skewness"
        }
        else {
            pval <- 1 - pval
            alt <- "data have negative skewness"
        }
        RVAL <- list(statistic = c(skew = s3, z = z), p.value = pval, 
            alternative = alt, method = "D'Agostino skewness test", 
            data.name = DNAME)
        class(RVAL) <- "htest"
        return(RVAL)
    }
    
    
    npde <- x$npde
    cat("---------------------------------------------\n")
    cat("Distribution of npde:\n")
    sev <- var(npde) * sqrt(2/(length(npde) - 1))
    sem <- sd(npde)/sqrt(length(npde))
    cat("           mean=", format(mean(npde), digits = 4), "  (SE=", 
        format(sem, digits = 2), ")\n")
    cat("       variance=", format(var(npde), digits = 4), "  (SE=", 
        format(sev, digits = 2), ")\n")
    cat("       skewness=", format(skewness(npde), digits = 4), 
        "\n")
    cat("       kurtosis=", format(kurtosis(npde), digits = 4), 
        "\n")
    cat("---------------------------------------------\n\n")
    myres <- rep(0, 4)
    y <- wilcox.test(npde)
    myres[1] <- y$p.val
    y <- agostino.test(npde)
    myres[3] <- y$p.val
    semp <- sd(npde)
    n1 <- length(npde)
    chi <- (semp^2) * (n1 - 1)
    y <- 2 * min(pchisq(chi, n1 - 1), 1 - pchisq(chi, n1 - 1))
    myres[2] <- y
    xcal <- 3 * min(myres[1:3])
    myres[4] <- min(1, xcal)
    names(myres) <- c("  Wilcoxon signed rank test   ", "  Fisher variance test        ", 
        "  D'Agostino test of normality", "Global adjusted p-value       ")
    cat("Statistical tests\n")
    for (i in 1:4) {
        cat(names(myres)[i], ": ")
        cat(format(myres[i], digits = 3))
        if (as.numeric(myres[i]) < 0.1 & as.numeric(myres[i]) >= 
            0.05) 
            cat(" .")
        if (as.numeric(myres[i]) < 0.05) 
            cat(" *")
        if (as.numeric(myres[i]) < 0.01) 
            cat("*")
        if (as.numeric(myres[i]) < 0.001) 
            cat("*")
        cat("\n")
    }
    cat("---\n")
    cat("Signif. codes: '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 \n")
    cat("---------------------------------------------\n")
}

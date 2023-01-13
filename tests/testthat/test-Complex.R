library(tidyverse)

### 1/13/23 Beginning of complex model/data test
### Not for automated testing yet.

mod <- PM_model$new("cmplxMod.txt")
#TODO: write out model in lists

dat <- PM_data$new("cmplxDat.csv")

means <- sapply(mod$model_list$pri,function(x) x$mean)
covMat <- diag(sapply(mod$model_list$pri,function(x) x$sd^2))

poppar <- list(1, means, covMat)
sim1 <- PM_sim$run(poppar = poppar, data = dat, model = mod, predInt = 1, nsim = 5, 
                   makecsv = "simtest.csv", obsNoise = NA, limits = NA, clean = T)     


###### only execute after running simulator above
# the code does not write the file properly
df <- readr::read_csv("simtest.csv", skip = 1, na = ".")
names(df)[1] <- "ID"
names(df) = tolower(names(df))
dat2 <- PM_data$new(df)
dat2$write("simtest.csv")
##################################################

dat2 <- PM_data$new("simtest.csv")

dat2$plot(marker = F, legend = T)

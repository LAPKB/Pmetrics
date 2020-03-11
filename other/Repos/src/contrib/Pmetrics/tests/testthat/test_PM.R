library(Pmetrics)
#testing Pmetrics


#retain current wd
currwd <- getwd()
Sys.setenv(env = "test")

#copy files from testthat to testFolder
copyNeededFiles <- function(files, testFolder) {
  #if tsstFolder exists, erase it
  if (file.exists(testFolder)) {
    unlink(testFolder, recursive = T)
  }
  #create a new testFolder
  dir.create(testFolder)
  #copy the files
  file.copy(from = files, to = testFolder)
}

#Skippers
# skip_if_not_interactive <- function() {
#   if (!interactive()) {
#     skip("Not in interactive mode")
#   }
# }

# test_that("NPAG on rifapentine", {
#   testFolder <- "Runs"
#   copyNeededFiles(files = c("model.txt", "ex.csv"), testFolder)
#   setwd(testFolder)
#   NPrun(data = "ex.csv", include = 1:4, cycles = 2, intern = T, silent = T)
#   PMload(1)
#   # skip_if_not_interactive()
#   expect_equal(nrow(final.1$popPoints), 4)
#   # skip_if_not_interactive()
#   expect_equal(round(final.1$popPoints$prob[4], 7), 0.2495361)
#   # skip_if_not_interactive()
#   expect_equal(round(cycle.1$ll[2], 5), 77.89823)
#   setwd(currwd)
#   unlink(testFolder, recursive = T)

# }) #end NPAG on rifapentine test



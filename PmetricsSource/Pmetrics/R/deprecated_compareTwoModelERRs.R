# BUG FIX wmy.2017.04.12 The Femke error polynomial quandry.
#
# wmy.2017.04.12
#
# Symptoms:
# For NPrun(...data=RunNo...) the RunNo/wrk/working_files are copied to the new
# NPAG run. The working files include an error polynomial. NPAG
# uses this error polynomial instead of the error function requested in the 
# model.txt file for the new optimization.
#
# Relevant observations:
# 1) NPprep() reads the name of the model.txt file, just prior to
# determining if data=RunNo or a new *.csv file.
#
# Solution: Immediately after determining that data=RunNo AND is.integer(RunNo) == T,
# compare the #ERR block of /RunNo/input/model.txt to the model.txt for the current
# run. If equivalent, no problem -- if different, then use the /RunNo/input/*.csv
# as a "new" dataset and generate working files from it.  Soution reqs the following
# two utility functions:
# 1) compareTwoModelERRs <- function(mod1,mod2)
# 2) 

#
# This function compares two model.txt file #ERR blocks and returns T/F if
# the same or different, respectively.
#
deprecated_wmy20190327_compareTwoModelERRs <- function (mod1,mod2)
{
  err.matches = F
  
# Initialize mod1 error block
mod1CommentLines <- grep("#",readLines(mod1))
mod1ErrBlockStart <- (grep("#err",readLines(mod1),ignore.case = T))
mod1ErrBlockStop <- mod1CommentLines[which(mod1CommentLines == mod1ErrBlockStart) + 1]
mod1ErrBlockStart = 1 + mod1ErrBlockStart
n1 = mod1ErrBlockStop - mod1ErrBlockStart
mod1ErrBlockStop = mod1ErrBlockStop - 1
read1 <- readLines(mod1,n=mod1ErrBlockStop)
err.block1 <- read1[mod1ErrBlockStart:mod1ErrBlockStop]

# Setup read mod2 error block
mod2CommentLines <- grep("#",readLines(mod2))
mod2ErrBlockStart <- (grep("#err",readLines(mod2),ignore.case = T))
mod2ErrBlockStop <- mod2CommentLines[which(mod2CommentLines == mod2ErrBlockStart) + 1]
mod2ErrBlockStart = 1 + mod2ErrBlockStart
n2 = mod2ErrBlockStop - mod2ErrBlockStart
mod2ErrBlockStop = mod2ErrBlockStop - 1
read2 <- readLines(mod2,n=mod2ErrBlockStop)
err.block2 <- read2[mod2ErrBlockStart:mod2ErrBlockStop]

# Collapse the blocks to strings w/no spaces, tabs, newlines, etc.
for (i in 1: n1)
{
  err.block1[i]<-paste(unlist(strsplit(err.block1[i]," ")),collapse="") 
}
for (i in 1: n2)
{
  err.block2[i]<-paste(unlist(strsplit(err.block2[i]," ")),collapse="") 
}

# Now compare and return
lineno=1
if (length(err.block1) == length(err.block2))
{ # same no. of lines; but is each line the same?
  if (length(err.block1) == sum(err.block1 == err.block2))
  {
    err.matches = T # All lines are identical
  }
  else
  { # we need to check line by line because numerically identical models might
    # differ w.r.t. extra zeros after decimals, exp vs decimal notation, etc.
    lineno = 1
    if ((grepl("=",err.block1[1]) == T) & (grepl("=",err.block2[1]) == T)) 
    {# Both blocks have an "=" on the first line, thus are G/L models
      if (err.block1[1] == err.block2[1])
      {
        err.matches = T
      }
      else
      {
        err.matches = F
      }
      lineno = lineno + 1 # C0,C1,C2,C3 input begins on lineno 2
    }
    else
    { 
      if ((grepl("=",err.block1[1]) == T) != (grepl("=",err.block2[1]) == T))
      {
        err.matches = F # b/c only one of the err.blocks is a G/L model
      } # else do not have to do anything b/c both blocks are NOT G/L models, lineno=1
    }
    # Remaining lines are C0, C1, C2, C3
    if (err.matches == T) # if FALSE, there is no need to check the Cs
    {
      polyCons1 <- read.csv(text=err.block1[lineno:n1],header=F)
      polyCons2 <- read.csv(text=err.block2[lineno:n2],header=F)
      ss <- dim(polyCons1); AA <- ss[1] * ss[2]
      if (sum(polyCons1 == polyCons2) == AA)
      {
        err.matches = T
      }
      else
      {
        err.matches = F
      }
    }
  }
}
else
{ # different no. lines; so obviously not the same
  error.matches = F
}
return(err.matches)
}

# This utility gets a block of information from a past run instr.inx file.
#
# TODO
#  1) add parameter for pathToInstFile -- currently just assumes this is "../
#    which should be fine b/c the only reason to use this utility is
#    to compare old instructions to a new NPAG run instructions.  Therefore, 
#    the user should be calling this utility from a /.../Runs/new_run/ directory,
#    and the old instructions will be in ../data/etc/instr.inx, where data is
#    the interger identifier of the old run. 
#  2) Write error checking code for: if a file or directory exists,
#    if an instr is a valid phrase, if old.data is an integer, REM* is the
#    first line and is not preceded by a HEADER line, PAR(I) is treated as
#    regex instead of as plain text,
#
getNPinstr <- function (old.data,instr) {
  Ifile = paste("../",old.data,"/etc/instr.inx",sep="")
  # The header lines for all instruction blocks in old.data (lines that begin w/a " ")
  HLs <- grep(" ",readLines(Ifile))
  lines <- readLines(Ifile)
  FirstChar <- substring(lines, 1, 1)
  HLs <- which(FirstChar == " ")
  # The header line for the requested instruction block
  dataHeaderLine <- (grep(instr,readLines(Ifile),ignore.case = T))
  # The desired block is in instr.inx[dataHeaderLine:HLs[dataHeaderLine+1]]
  instrBlockStart = HLs[which(HLs == dataHeaderLine)] + 1
  instrBlockStop = HLs[which(HLs == dataHeaderLine) + 1] - 1
  if ((instrBlockStop - instrBlockStart) > -1)
  {
    read1 <- readLines(Ifile,n=instrBlockStop)
    InstrBlock <- read1[instrBlockStart:instrBlockStop] 
  }
  else
  {
    InstrBlock <- -99
  }
  return(InstrBlock)
}

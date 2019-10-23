#first export all emails to Pmetrics smart folder as 
#a plain text document to the desktop


PMlist <- readLines("~/Desktop/Pmetrics.txt")

froms <- grep("From: ",PMlist)
OS <- grep("Subject:",PMlist)
dates <-  grep("Date: ",PMlist)

fromlines <- PMlist[froms]
OSlines <- PMlist[OS]
datelines <- PMlist[dates]


fromnames <- sapply(fromlines,function(x) unlist(strsplit(x,":"))[2])
# pattern
email_pat = "([a-z0-9_\\.-]+)@([\\da-z\\.-]+)\\.([a-z\\.]{2,6})"
emailMatches <- regexpr(email_pat,tolower(fromlines))
emails <- tolower(substr(fromlines,emailMatches,emailMatches+attr(emailMatches,"match.length")-1))

year_pat <- "[[:digit:]]{4}"
yearMatches <- regexpr(year_pat,datelines)
years <- as.numeric(substr(datelines,yearMatches,yearMatches+attr(yearMatches,"match.length")-1))

plot(table(years),xlab="Year",ylab="Number")

# names(tonames) <- NULL
# tonames <- gsub("^ *","",tonames)
# tonames <- gsub("\\\"","",tonames)
# tonames <- tonames[-grep("neely|Neely",tonames)]
# uniquenames <- tonames[!duplicated(tonames)]
uniqueOS <- OSlines[grep("Subject: Pmetrics",OSlines)]
uniqueOS <- sub("^Subject: Pmetrics for ","",uniqueOS)

#since August 6, 2013
DL <- data.frame(email=emails,year=years)

#number of unique downloaders
length(unique(emails))
#table of OS downloads
table(uniqueOS,dnn="OS")



PMdata <- read.csv("~/Desktop/Pmetrics.csv",stringsAsFactors=F)
PMdata <- PMdata[!duplicated(PMdata$From),]
yearMatches <- regexpr("/[[:digit:]]{2} ",PMdata$Date)
year <- as.numeric(paste("20",substr(PMdata$Date,yearMatches+1,yearMatches+attr(yearMatches,"match.length")-1),sep=""))
PMdata$year <- year
year <- c(year,rep(2015,109/0.75-109))
plot(table(year),xlab="Year",ylab="Number")

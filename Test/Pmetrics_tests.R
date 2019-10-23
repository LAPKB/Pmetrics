library(Pmetrics)

#RUN 1 - tlag, ka, kel, vol - test of  NPAG
setwd("~/LAPK/PmetricsSource/Test/NPAG")
file.copy(from=c("../src/model.txt","../src/ex.csv"),to=getwd(),overwrite=T)
NPrun(data="ex.csv",cycles=100)
PMload(1)
plot(mdata.1,xlim=c(120,148),pch=3)
plot(mdata.1,xlim=c(120,148),pred=post.1,overlay=F,join=F,pch=3)
plot(final.1,Ke~V,scale=200,bg="red",grid=F)
plot(final.1,density=T)
plot(op.1,pred.type="post",icen="mean")
plot(op.1,pred.type="pop",icen="mean")

auc <- makeAUC(post.1,start=0,end=24)
nca.1 <- makeNCA(1,start=120,end=146)
nca.1 <- makeNCA(1,first=5,end=6)

plot(op.1,pred.type="pop",resid=T,x.stat=0.3)


PMstep(cov.1)

# Example of Pmetrics pd - be sure to have executed RUN #1 above and used NPload(1)
setwd("~/LAPK/PmetricsSource/Test/pd")
file.copy(from=c("../src/model.txt","../src/ex1.csv"),to=getwd(),overwrite=T)
diag <- PMdiag("ex1.csv",poppar=final.1)
plot(diag)
# do this if you don't want to rerun above
save(diag,file="diag.Rdata")
load("diag.Rdata")
plot(diag)

#Example of a simulator run
setwd("~/LAPK/PmetricsSource/Test/SIM")
file.copy(from=c("../src/model.txt","../src/ex.csv"),to=getwd(),overwrite=T)
SIMrun(poppar=final.1,data="ex1.csv",nsim=0,split=T,include=1,limits=NA,predInt=0.25,obsNoise=c(0,0,0,0),clean=F)
simdata1 <- SIMparse("simout1.txt")
simdata <- SIMparse("simout?.txt")
plot(simdata1,log=F)
plot(simdata1,probs=0.5,ci=0,log=F,pch=3)
plot(simdata1,log=F,probs=NA)


# Example of visual predictive check based on subject 4 simulation
simdata <- SIMparse("simout1.txt")
predcheck <- plot(simdata,obspred=op.1$post1,log=F,ocol="blue")
predcheck$npc

# Examples of percent target attainment
setwd("~/LAPK/PmetricsSource/Test/PTA")
file.copy(from=c("../src/model.txt","../src/ptaex1.csv"),to=getwd(),overwrite=T)
SIMrun(poppar=final.1,limits=matrix(c(0,5,0,5,0,100,0,5),nrow=4,ncol=2,byrow=T),data="ptaex1.csv",nsub=4,nsim=100,seed=c(-17,10,5,-3))

simlist <- "simout*"
pta.1 <- makePTA(simdata=simlist,targets=c(0.25,0.5,1,2,4,8,16,32),target.type="time",success=0.6, start=120, end=144)
summary(pta.1)
plot(pta.1,ylab="Proportion with %T>MIC of at least 60%",grid=T,legend=list(x="bottomleft",legend=c("600 mg daily","1200 mg daily","300 mg bid","600 mg bid")))

pta.2 <- makePTA(simdata=simlist,targets=c(0.25,0.5,1,2,4,8,16,32),target.type="auc",success=100, start=120, end=144)
summary(pta.2)
plot(pta.2,ylab="Proportion with AUC/MIC of at least 100",legend=list(x="bottomleft",legend=c("600 mg daily","1200 mg daily","300 mg bid","600 mg bid")))

pta.3 <- makePTA(simdata=simlist,targets=c(0.25,0.5,1,2,4,8,16,32),target.type="peak",success=10, start=120, end=144)
summary(pta.3)
plot(pta.3,ylab="Proportion with peak/MIC of at least 10",grid=T,legend=list(legend=c("600 mg daily","1200 mg daily","300 mg bid","600 mg bid")))

pta.4 <- makePTA(simdata=simlist,targets=c(0.25,0.5,1,2,4,8,16,32),target.type="min",success=1, start=120, end=144)
summary(pta.4)
plot(pta.4,ylab="Proportion with Cmin/MIC of at least 1",grid=T,legend=list(x="bottomleft",legend=c("600 mg daily","1200 mg daily","300 mg bid","600 mg bid")))

pta.5 <- makePTA(simdata=simlist,targets=c(0.25,0.5,1,2,4,8,16,32),target.type=123,success=2, start=120, end=144)
summary(pta.5)
plot(pta.5,ylab="Proportion with C3/MIC of at least 1",grid=T,legend=list(x="bottomleft",legend=c("600 mg daily","1200 mg daily","300 mg bid","600 mg bid")))

#RUN 2 - tlag, ka, kel, vol - Example of Pmetrics IT2B
setwd("~/LAPK/PmetricsSource/Test/IT2B")
file.copy(from=c("../src/model.txt","../src/ex.csv"),to=getwd(),overwrite=T)
ITrun(data="ex.csv",cycles=100)
PMload(1)

mdata.1$gender <- factor(mdata.2$gender,labels=c("female","male"))

plot(mdata.2,overlay=T,xlim=c(120,144))
plot(mdata.2,overlay=T,group=mdata.2$gender,xlim=c(120,144),lwd=3,legend=T)

par(mfrow=c(1,2))
plot(op.2$pop1,main="Population",square=T,ref=T,reg=T)
plot(op.2$post1,main="Posterior",log=F,square=T,ref=T,reg=F,lowess=T)
par(mfrow=c(1,1))

plot(op.2$post1,reg=F,lowess=T,col="black")
plot(op.2$post1,resid=T)

plot(final.2)
plot(final.2,ke~v)
plot(final.2,standard=F)

plot(cycle.2)
plot(cov.2,Kel~AGE,lowess=F,reg=T,pch=3)
plot(cov.2,Vol~WT)

#RUN 3 - tlag, ka, kel, vol - Example of Pmetrics ERRrun
setwd("~/LAPK/PmetricsSource/Test/IT2B")
file.copy(from=c("../src/model.txt","../src/ex1.csv"),to=getwd(),overwrite=T)
ERRrun(data="ex1.csv",intern=T)
ERRrun(data=1,model=1)

#example of a run with a non-uniform density, tlag, ka, kel, vol - test of  NPAG
setwd("~/LAPK/PmetricsSource/Test/NPAG")
NPrun(data=1,model=1,prior=1)
setwd("/Users/neely/LAPK/PmetricsSource/Test/NPAG/out-2012Oct31-1315/outputs")
PMload(2)

#test bootstrap


setwd("~/LAPK/JAGS/LAPK/NPAG/")
NPrun(instr="data.inx")
setwd("/Users/neely/LAPK/JAGS/LAPK/NPAG/out-2012Jul20-1600/outputs")
NPload(2)


boot.2 <- makeBoot(n=100,instr="data.inx",prior=NPdata.2)
boot.2$summary
boot.2$summary$mean + 1.96*boot.2$summary$meanSE
boot.2$summary$mean - 1.96*boot.2$summary$meanSE

# KEL 0.76 (0.68 - 0.84), VOL 2.03 (1.93 - 2.12)  by bootstrap
# KEL 0.76 (0.73 - 0.79), VOL 1.98 (1.92 - 2.03)  by NPB


NPdata <- NPparse()

setwd("~/LAPK/PmetricsSource/Test/NPAG")
NPrun(model="modelnew1.txt",data="znew.csv",instr="tonpag14.dat")

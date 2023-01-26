##### section 1: simulate a dataset

install.packages("profvis")
library(profvis)

# profvis({

file.sources = list.files(path = "NEMo-main/", pattern="*.R", full.names = T)
sapply(file.sources,source,.GlobalEnv)





iii <- 1
set.seed(iii)

r <- 0.03
d <- 1
roota <- -1
rootb <- -1
rootu <- 1
sd <- 2
rootx <- 1
N <- 10
pois.x <- 1
sp <- 0.1
mu <- 0.03
nspecies <- 100
bias <- NULL
area <- 1000
nBK <- 1000
A1 <- 20
A2 <- 20
sim <- simcode(sp,mu,nspecies,A1,A2,rootu,sd,roota,rootb,rootx,d,N,r,pois.x,area,nBK)
while(inherits(sim,"try-error")) {
  sim <- simcode(sp,mu,nspecies,A1,A2,rootu,sd,roota,rootb,rootx,d,N,r,pois.x,area,nBK)
}
integrand <- function (tt) {mu*(1-exp(-(sp-mu)*tt))/(sp-mu*exp(-(sp-mu)*tt))}

##### section 2: setting up the output files for results from NEMo

#two chains per simulation
for (aaa in 1:2) {
  eventsfilename <- paste0(getwd(),"/simulation/events",iii,"_",aaa)
  postfilename <- paste0(getwd(),"/simulation/post",iii,"_",aaa)
  filename <- paste0(getwd(),"/simulation/work",iii,"_",aaa)
  
  ##### section 3: setting up the initial values NEMo
  
  tT1 <- sim$tT1
  tT2 <- sim$tT2
  t <- sim$t
  PA <- sim$PA
  PO <- sim$PO
  BK <- sim$BK
  edge <- sim$tree$edge
  edge.length <- sim$tree$edge.length
  edge.prob1 <- sim$edge.prob1
  edge.prob2 <- sim$edge.prob2
  tuning.t <- min(edge.length)
  A <- c(0.1/(nspecies-1+sp*tT1),0.1)
  
  norm.roota.m <- norm.rootb.m <- -1
  norm.rootu.m <- 1
  sd <- lnorm.sd.m <- 2
  norm.roota.sd <- norm.rootb.sd <- 1
  norm.rootu.sd <- 3
  lnorm.sd.sd <- 0.5
  exp.d <- 0.5
  exp.r <- 10
  exp.A <- c(10,10)
  
  tuning.roota <- tuning.rootb <- 0.1
  tuning.rootu <- 1
  tuning.sd <- 0.1
  tuning.r <- 0.1
  tuning.d <- 0.1
  tuning.A <- c(0.1,0.1)
  
  events <- matrix(NA,0,7)
  events <- data.frame(events)
  names(events) <- c("node","type","time","a","b","n","xmin")
  
  #calculate initial likelihood
  rootniche <- exp(-((0:(N-1))/(exp(roota)+1e-6))^(exp(rootb)+1e-6))
  rootback <- pnorm(seq(-(N-1),N,1),mean=rootu,sd=sd)-pnorm(seq(-N,N-1,1),mean=rootu,sd=sd)
  rootback <- rootback/sum(rootback)
  rootdistro <- c(rep(1,N),rootniche)*rootback
  rootdistro <- rootdistro/sum(rootdistro)
  rootdistr <- rootdistro[(N+1):(2*N)]
  rootdistr[1] <- rootdistr[1]+sum(rootdistro[1:N])
  lik.old <- likcal(node=0,root.included=F,events=events,edge=edge,edge.length=edge.length,nspecies=nspecies,PO=PO,PA=PA,N=N,nodeu=rootu,sd=sd,nodea=roota,nodeb=rootb,nodex=rootx,nodedistr=rootdistr,nodedistro=rootdistro,nodeniche=rootniche,nodeback=rootback,d=d,bias=bias,area=area)
  
  #number of MCMC generations to run
  #ngen <- 3*10^6
  ngen <- 10 #temporarily using a smaller number than 3*10^6 for testing
  k <- c(0,0)
  
  ##### section 4: running the NEMo analysis on simulate data
  
  niche_mcmc(ngen)
}

# end of profvis

saveRDS(lik.old, file = "lik.old.RData")

saveRDS(sim, file = "sim.RData")

#############################################################################
#function of producing contamination in the population
#popsize:population size; group:number of regions; n.con.group:number of contaminated regions
#con.rate.group:contaminated rate per region
#############################################################################
population.heter <- function (popsize,group,n.con.group,con.rate.group){
  con.group=sample(1:group,n.con.group)
  popind<-matrix(rep(0,popsize),ncol = group)
  
  colnames(popind)<-paste0('zone',1:group)
  
  for (g in 1:n.con.group){
    
    con.number<-as.integer(nrow(popind)*con.rate.group[g])
    
    con.ind<-sample(1:nrow(popind),con.number)
    
    popind[,con.group[g]][con.ind]=1 
  }
  popind
}
############################################################################


############################################################################
#contamination in farm population###########################################
#popsize:number of farms; n.con.group:number of contaminated regions;
#con.rate.group: contaminated rate per region
############################################################################
#contamination 0.1
farmpopulation0.1 <- function (popsize,group,n.con.group,con.rate.group){
  con.group=sample(1:group,n.con.group)
  popind<-matrix(rep(0,popsize),ncol = group)
  colnames(popind)<-paste0('feed',1:group)
  for (g in 1:n.con.group){
    con.number<-nrow(popind)*con.rate.group[g]
    con.ind<-sample(1:nrow(popind),con.number)
    popind[,con.group[g]][con.ind]=1 
  }
  aa <- c(popind[,1])
  for(j in 2:10){
    aa <- c(aa,popind[,j])
  }
  data.frame(data=aa, zone=rep(paste0('zone',1:10), each=2000))
}
#example
farm0.1<-farmpopulation0.1(20000,10,2,c(0.2,0.8))
#contamination 0.05
farmpopulation0.05 <- function (popsize,group,n.con.group,con.rate.group){
  con.group=sample(1:group,n.con.group)
  popind<-matrix(rep(0,popsize),ncol = group)
  colnames(popind)<-paste0('feed',1:group)
  for (g in 1:n.con.group){
    con.number<-nrow(popind)*con.rate.group[g]
    con.ind<-sample(1:nrow(popind),con.number)
    popind[,con.group[g]][con.ind]=1 
  }
  aa <- c(popind[,1])
  for(j in 2:20){
    aa <- c(aa,popind[,j])
  }
  data.frame(data=aa, zone=rep(paste0('zone',1:10), each=2000))
}
#example
farm0.05<-farmpopulation0.1(20000,20,1,1)
#contamination 0.01
farmpopulation0.01 <- function (popsize,group,n.con.group,con.rate.group){
  con.group=sample(1:group,n.con.group)
  popind<-matrix(rep(0,popsize),ncol = group)
  colnames(popind)<-paste0('feed',1:group)
  for (g in 1:n.con.group){
    con.number<-nrow(popind)*con.rate.group[g]
    con.ind<-sample(1:nrow(popind),con.number)
    popind[,con.group[g]][con.ind]=1 
  }
  aa <- c(popind[,1])
  for(j in 2:100){
    aa <- c(aa,popind[,j])
  }
  data.frame(data=aa, zone=rep(paste0('zone',1:10), each=2000))
}
farm0.01<-farmpopulation0.1(20000,100,1,1)
############################################################################


############################################################################
#simple random sampling;SRSDF(probability of sampling at least one contamination in dariy farms by SRS)
#popsize:number of farms; group: number of regions; n.con.group:number of contaminated regions;
#con.rate.group:contaminated rate per region; sample.size: number of sample collected
#iteration: itration times for each sampling scenario
############################################################################
## simple random sampling (SRS);x:population size;y:number of sample size
SRS<- function(x, y){
  result <- sample(x,size=y, replace=TRUE)
  sum(result)
  if (sum(result) > 0) {
    result2 =1
  }else {
    result2 =0
  }
  return(c(sum(result), result2))
}
##sample contamination fraction 0.1
SRSDF0.1<-function(popsize, group,n.con.group,con.rate.group,sample.size,iteration=1000) {
  set.seed(0)
  population.frame<-farmpopulation0.1(popsize, group,n.con.group,con.rate.group)
  all.result<-data.frame(n.com=NULL,if.sucess=NULL )
  for(i in 1:iteration){
    one.result<-SRS(population.frame$data,sample.size)
    all.result[i,1]<-one.result[[1]]
    all.result[i,2]<-one.result[[2]]
  }
  colnames(all.result)<-c('n.com','if.sucess')
  return(sum(all.result$if.sucess)/length(all.result$if.sucess))
}

##sample contamination fraction 0.05
SRSDF0.05<-function(popsize, group,n.con.group,con.rate.group,sample.size,iteration=1000) {
  set.seed(0)
  population.frame<-farmpopulation0.05(popsize, group,n.con.group,con.rate.group)
  all.result<-data.frame(n.com=NULL,if.sucess=NULL )
  for(i in 1:iteration){
    one.result<-SRS(population.frame$data,sample.size)
    all.result[i,1]<-one.result[[1]]
    all.result[i,2]<-one.result[[2]]
  }
  colnames(all.result)<-c('n.com','if.sucess')
  return(sum(all.result$if.sucess)/length(all.result$if.sucess))
}
##sample contamination fraction 0.01
SRSDF0.01<-function(popsize, group,n.con.group,con.rate.group,sample.size,iteration=1000) {
  set.seed(0)
  population.frame<-farmpopulation0.01(popsize, group,n.con.group,con.rate.group)
  all.result<-data.frame(n.com=NULL,if.sucess=NULL )
  for(i in 1:iteration){
    one.result<-SRS(population.frame$data,sample.size)
    all.result[i,1]<-one.result[[1]]
    all.result[i,2]<-one.result[[2]]
  }
  colnames(all.result)<-c('n.com','if.sucess')
  return(sum(all.result$if.sucess)/length(all.result$if.sucess))
}

#EXAMPLE with contamniation fraction 0.1-0.05-0.01 sample size 10
SRSDF0.1(popsize=20000, group=10,n.con.group=2,con.rate.group=c(0.2,0.8),sample.size=10)
SRSDF0.05(popsize=20000, group=20,n.con.group=1,con.rate.group=1,sample.size=10)
SRSDF0.01(popsize=20000, group=100,n.con.group=1,con.rate.group=1,sample.size=10)
#result based on fraction 0.1 with sample size from 1 to 100
#example for fration 0.1
SRSfarm0.1<-NULL
for (i in 1:100) {
  SRSfarm0.1[i]<-SRSDF(popsize=20000, group = 10,n.con.group=2,con.rate.group=c(0.2,0.8),sample.size=i) 
}

############################################################################


############################################################################
#Stratified sampling; STRDF (propability of sampling at least one contamination in dariy farms by STR)
#popsize:number of farms; group: number of regions; n.con.group:number of contaminated regions;
#con.rate.group:contaminated rate per region;
#sample.fraction: number of samples collected in one stratified group;10 regions were assumed as groups
#iteration:itration times for each sampling scenario
############################################################################
install.packages("splitstackshape")
library(splitstackshape)

#contamination fraction 0.1
STRDF0.1 <- function (popsize, group,n.con.group,con.rate.group,sample.fraction,iteration=1000){
  set.seed(0)
  strata <- NULL
  for (i in 1:iteration) { 
    print(paste0('sample size = ',sample.fraction,' and repeat =', i))
    if (sum ( stratified(farmpopulation0.1(popsize,group,n.con.group,con.rate.group), "zone",sample.fraction)$data)>0) {
      strata[i]=1} else {strata[i]=0}
    strata
  }
  sum(strata)/length(strata)
}
#contamination fraction 0.05
STRDF0.05 <- function (popsize, group,n.con.group,con.rate.group,sample.fraction,iteration=1000){
  set.seed(0)
  strata <- NULL
  for (i in 1:iteration) { 
    print(paste0('sample size = ',sample.fraction,' and repeat =', i))
    if (sum ( stratified(farmpopulation0.05(popsize,group,n.con.group,con.rate.group), "zone",sample.fraction)$data)>0) {
      strata[i]=1} else {strata[i]=0}
    strata
  }
  sum(strata)/length(strata)
}
#contamination fraction 0.01
STRDF0.01 <- function (popsize, group,n.con.group,con.rate.group,sample.fraction,iteration=1000){
  set.seed(0)
  strata <- NULL
  for (i in 1:iteration) { 
    print(paste0('sample size = ',sample.fraction,' and repeat =', i))
    if (sum ( stratified(farmpopulation0.01(popsize,group,n.con.group,con.rate.group), "zone",sample.fraction)$data)>0) {
      strata[i]=1} else {strata[i]=0}
    strata
  }
  sum(strata)/length(strata)
}
#### example with contamination fraction 0.1-0.05-0.01, and each group 10 samples collected
STRDF0.1(popsize=20000,group=10,n.con.group=2,con.rate.group=c(0.8,0.2),sample.fraction=1)
STRDF0.05(popsize=20000,group=20,n.con.group=1,con.rate.group=1,sample.fraction=1)
STRDF0.01(popsize=20000,group=100,n.con.group=1,con.rate.group=1,sample.fraction=1)
#### stratified result with contamination fraction (0.01,0.05,0.1) sample size 10,20...100
#example for fraction 0.1
STRfarm0.1<-NULL
for (i in 1:10){
  STRfarm0.1[i]<-STRDF(popsize=20000,group=100,n.con.group=1,con.rate.group=1,sample.fraction=i)
}

############################################################################



############################################################################
#systematic sampling; circular.sys (circular systematic sampling function)
#x:population size;n:sample size
#sys_DF:probability of sampling at least one contamination in dairy famrs ny SS
#popsize:number of farms; group: number of regions; n.con.group:number of contaminated regions;
#con.rate.group:contaminated rate per region; sample.size: number of sample collected
#iter:itration times for each sampling scenario
############################################################################
circular.sys <- function(x,n){
  N = length(x)    
  k =as.integer(N/n)
  r = sample(N,1)
  sam = 0
  for(j in 1: n)
  {
    if (r + (j-1)*k <= N)
    {sam[j] = r + (j-1)*k}
    else sam[j] = min(r + (j-1)*k-N,r + (j-1))
  }
  x[sam]
}
#application in dairy farms
#contamination fraction 0.1
sys_DF0.1 <- function(popsize,group,n.con.group,con.rate.group,sample.size,iter=1000){
  set.seed(0)
  sys <- NULL
  for (i in 1:iter) { 
    
    print(paste0('sample size = ',sample.size,' and repeat =', i))
    
    if (sum ( circular.sys(farmpopulation0.1(popsize,group,n.con.group,con.rate.group)$data, sample.size))>0) {
      sys[i]=1} else {sys[i]=0}
    sys
  }
  sum(sys)/length(sys)
}
#contamination fraction 0.05
sys_DF0.05 <- function(popsize,group,n.con.group,con.rate.group,sample.size,iter=1000){
  set.seed(0)
  sys <- NULL
  for (i in 1:iter) { 
    
    print(paste0('sample size = ',sample.size,' and repeat =', i))
    
    if (sum ( circular.sys(farmpopulation0.05(popsize,group,n.con.group,con.rate.group)$data, sample.size))>0) {
      sys[i]=1} else {sys[i]=0}
    sys
  }
  sum(sys)/length(sys)
}
#contamination fraction 0.01
sys_DF0.01 <- function(popsize,group,n.con.group,con.rate.group,sample.size,iter=1000){
  set.seed(0)
  sys <- NULL
  for (i in 1:iter) { 
    
    print(paste0('sample size = ',sample.size,' and repeat =', i))
    
    if (sum ( circular.sys(farmpopulation0.01(popsize,group,n.con.group,con.rate.group)$data, sample.size))>0) {
      sys[i]=1} else {sys[i]=0}
    sys
  }
  sum(sys)/length(sys)
}
#example with contamination fraction 0.1-0.05-0.01 sample size 10
sys_DF0.1(popsize=20000, group=10,n.con.group=2,con.rate.group=c(0.2,0.8),sample.size =10)
sys_DF0.05(popsize=20000, group=20,n.con.group=1,con.rate.group=1,sample.size =10)
sys_DF0.1(popsize=20000, group=100,n.con.group=1,con.rate.group=1,sample.size =10)


### systematic sampling results with contamination fraction (0.01, 0.05, 0.1) sample size (1 to 100)
#example with contamination fraction (0.01)
SYSfarm0.01<-NULL
for (i in 1:100) {
  # message(paste0('when sample size is ',i))
  SYSfarm0.01[i]<-sys_DF(popsize=20000, group=100,n.con.group=1,con.rate.group=1,sample.size = i) 
}

############################################################################


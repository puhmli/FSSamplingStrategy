######################################################
#contamination in farm population#####################
#popsize:number of farms; n.con.group:number of contaminated regions;
#con.rate.group: contaminated rate per region
######################################################
######################################################
#data for dutch farms from 2008-2016, samples collected from RIKILT monitoring plan
#year;	farms;	farms/region;	regions;	number of samples
#2011	19200	4800	4	39  choose this one as example
#2012	18600	4650	4	27
#2013	18700	4675	4	23
#2014	18600	4650	4	18
#2015	18300	4575	4	19
#2016	17900	4475	4	25
#2010	19800	4950	4	40
#2009	20300	5075	4	47
#2008	18500	4625	4	41

#example of monitoring dioxins in Dutch dairy farms in 2011
#contamination fraction 0.1
farmpopulation0.1 <- function (popsize,group,n.con.group,con.rate.group){
  con.group=sample(1:group,n.con.group)
  popind<-matrix(rep(0,popsize),ncol = group)
  colnames(popind)<-paste0('zone',1:group)
  for (g in 1:n.con.group){
    con.number<-nrow(popind)*con.rate.group[g]
    con.ind<-sample(1:nrow(popind),con.number)
    popind[,con.group[g]][con.ind]=1
  }
  aa <- c(popind[,1])
  for(j in 2:10){
    aa <- c(aa,popind[,j])
  }
  data.frame(data=aa, zone=rep(paste0('zone',1:4), each=4800))
}
#example
farm1<-farmpopulation0.1(19200, 10,1, 1)
####################################################
#contamination fraction 0.05
farmpopulation0.05 <- function (popsize,group,n.con.group,con.rate.group){
  con.group=sample(1:group,n.con.group)
  popind<-matrix(rep(0,popsize),ncol = group)
  colnames(popind)<-paste0('zone',1:group)
  for (g in 1:n.con.group){
    con.number<-nrow(popind)*con.rate.group[g]
    con.ind<-sample(1:nrow(popind),con.number)
    popind[,con.group[g]][con.ind]=1
  }
  aa <- c(popind[,1])
  for(j in 2:20){
    aa <- c(aa,popind[,j])
  }
  data.frame(data=aa, zone=rep(paste0('zone',1:4), each=4800))
}
#contamination fraction 0.01
farmpopulation0.01 <- function (popsize,group,n.con.group,con.rate.group){
  con.group=sample(1:group,n.con.group)
  popind<-matrix(rep(0,popsize),ncol = group)
  colnames(popind)<-paste0('zone',1:group)
  for (g in 1:n.con.group){
    con.number<-nrow(popind)*con.rate.group[g]
    con.ind<-sample(1:nrow(popind),con.number)
    popind[,con.group[g]][con.ind]=1
  }
  aa <- c(popind[,1])
  for(j in 2:100){
    aa <- c(aa,popind[,j])
  }
  data.frame(data=aa, zone=rep(paste0('zone',1:4), each=4800))
}

############################################################################
#simple random sampling;SRSDF(probability of collecting at least one contamination in dariy farms by SRS)
#popsize:number of farms; group: number of regions; n.con.group:number of contaminated regions;
#con.rate.group:contaminated rate per region; sample.size: number of sample collected
#iteration: itration times for each sampling scenario
############################################################################
## simple random sampling (SRS);x:population size;y:number of sample size
####################################################
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
##
SRSDF<-function(popsize, group,n.con.group,con.rate.group,sample.size,iteration=1000) {
 
  population.frame<-farmpopulation(popsize, group,n.con.group,con.rate.group)
  all.result<-data.frame(n.com=NULL,if.sucess=NULL )
  for(i in 1:iteration){
    one.result<-SRS(population.frame$data,sample.size)
    all.result[i,1]<-one.result[[1]]
    all.result[i,2]<-one.result[[2]]
  }
  colnames(all.result)<-c('n.com','if.sucess')
  return(sum(all.result$if.sucess)/length(all.result$if.sucess))
}
#EXAMPLE with contamniation fraction 0.1, with corresponding con.rate.group=0.4
#sample size = 40
#2011 Dutch dairy farms
#0.1
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
#0.05
SRSDF0.05<-function(popsize, group,n.con.group,con.rate.group,sample.size,iteration=1000) {
  
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
#0.01
SRSDF0.01<-function(popsize, group,n.con.group,con.rate.group,sample.size,iteration=1000) {
  
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
#results contamination fraction 0.1-0.05-0.1 sample size 39
SRSDF0.1(popsize=19200, group=10,n.con.group=1,con.rate.group=1,sample.size=39)
SRSDF0.05(popsize=19200, group=20,n.con.group=1,con.rate.group=1,sample.size=39)
SRSDF0.01(popsize=19200, group=100,n.con.group=1,con.rate.group=1,sample.size=39)
############################################################################



############################################################################
#Stratified sampling; STRDF (propability of collecting at least one contamination in dariy farms by STR)
#popsize:number of farms; group: number of regions; n.con.group:number of contaminated regions;
#con.rate.group:contaminated rate per region;
#sample.fraction: number of samples collected in one stratified group;10 regions were assumed as groups
#iteration:itration times for each sampling scenario
############################################################################
install.packages("splitstackshape")
library(splitstackshape)
STRDF <- function (popsize, group,n.con.group,con.rate.group,sample.fraction){
  strata <- NULL
  for (i in 1:1000) {
    print(paste0('sample size = ',sample.fraction,' and repeat =', i))
    if (sum ( stratified(farmpopulation(popsize,group,n.con.group,con.rate.group), "zone",sample.fraction)$data)>0) {
      strata[i]=1} else {strata[i]=0}
    strata
  }
  sum(strata)/length(strata)
}
#### stratified result with contamination fraction 0.1,0.05,0.01, sample size 40
#2011 dutch dairy farms
#contamination fraction 0.1
STRDF0.1 <- function (popsize, group,n.con.group,con.rate.group,sample.fraction){
  strata <- NULL
  set.seed(0)
  for (i in 1:1000) {
    print(paste0('sample size = ',sample.fraction,' and repeat =', i))
    if (sum ( stratified(farmpopulation0.1(popsize,group,n.con.group,con.rate.group), "zone",sample.fraction)$data)>0) {
      strata[i]=1} else {strata[i]=0}
    strata
  }
  sum(strata)/length(strata)
}
STRDF0.1(popsize=19600,group=10,n.con.group=1,con.rate.group=1,sample.fraction=10)
#contamination fraction 0.05
STRDF0.05 <- function (popsize, group,n.con.group,con.rate.group,sample.fraction){
  strata <- NULL
  for (i in 1:1000) {
    print(paste0('sample size = ',sample.fraction,' and repeat =', i))
    if (sum ( stratified(farmpopulation0.05(popsize,group,n.con.group,con.rate.group), "zone",sample.fraction)$data)>0) {
      strata[i]=1} else {strata[i]=0}
    strata
  }
  sum(strata)/length(strata)
}

STRDF0.05(popsize=19200,group=20,n.con.group=1,con.rate.group=1,sample.fraction=10)
#contamination fraction 0.01
STRDF0.01 <- function (popsize, group,n.con.group,con.rate.group,sample.fraction){
  strata <- NULL
  for (i in 1:1000) {
    print(paste0('sample size = ',sample.fraction,' and repeat =', i))
    if (sum ( stratified(farmpopulation0.01(popsize,group,n.con.group,con.rate.group), "zone",sample.fraction)$data)>0) {
      strata[i]=1} else {strata[i]=0}
    strata
  }
  sum(strata)/length(strata)
}
#results contamination fraction 0.1-0.05-0.01 sample size 40
STRDF0.01(popsize=19200,group=100,n.con.group=1,con.rate.group=1,sample.fraction=10)
STRDF0.05(popsize=19200,group=20,n.con.group=1,con.rate.group=1,sample.fraction=10)
STRDF0.1(popsize=19200,group=10,n.con.group=1,con.rate.group=1,sample.fraction=10)
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
sys_DF <- function(popsize,group,n.con.group,con.rate.group,sample.size,iter=1000){
  sys <- NULL
  for (i in 1:1000) {
    print(paste0('sample size = ',sample.size,' and repeat =', i))
    if (sum ( circular.sys(farmpopulation(popsize,group,n.con.group,con.rate.group)$data, sample.size))>0) {
      sys[i]=1} else {sys[i]=0}
    sys
  }
  sum(sys)/length(sys)
}
#contamination fraction 0.1
sys_DF0.1 <- function(popsize,group,n.con.group,con.rate.group,sample.size,iter=1000){
  sys <- NULL
  for (i in 1:1000) {
    print(paste0('sample size = ',sample.size,' and repeat =', i))
    if (sum ( circular.sys(farmpopulation0.1(popsize,group,n.con.group,con.rate.group)$data, sample.size))>0) {
      sys[i]=1} else {sys[i]=0}
    sys
  }
  sum(sys)/length(sys)
}
#result
sys_DF0.1(popsize=19200, group=10,n.con.group=2,con.rate.group=c(0.2,0.8),sample.size = 39)
#contamination fraction 0.05
sys_DF0.05 <- function(popsize,group,n.con.group,con.rate.group,sample.size,iter=1000){
  sys <- NULL
  for (i in 1:1000) {
    print(paste0('sample size = ',sample.size,' and repeat =', i))
    if (sum ( circular.sys(farmpopulation0.05(popsize,group,n.con.group,con.rate.group)$data, sample.size))>0) {
      sys[i]=1} else {sys[i]=0}
    sys
  }
  sum(sys)/length(sys)
}
#result
sys_DF0.05(popsize=19200, group=20,n.con.group=1,con.rate.group=1,sample.size = 39)
#contamination fraction0.01
sys_DF0.01 <- function(popsize,group,n.con.group,con.rate.group,sample.size,iter=1000){
  sys <- NULL
  for (i in 1:1000) {
    print(paste0('sample size = ',sample.size,' and repeat =', i))
    if (sum ( circular.sys(farmpopulation0.01(popsize,group,n.con.group,con.rate.group)$data, sample.size))>0) {
      sys[i]=1} else {sys[i]=0}
    sys
  }
  sum(sys)/length(sys)
}
#results contamination fraction 0.1-0.05-0.01 sample size 39
sys_DF0.01(popsize=19200, group=100,n.con.group=1,con.rate.group=1,sample.size = 39)
####################################################################################





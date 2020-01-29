###############################################################################
#function of producing contamination in the population with no spatial correlation
#popsize:population size; com.per:contamination fraction
###############################################################################

population.homo<-function (size, com.per) {
  
  index<- 1:size
  
  population<-rep(0,size)
  
  com.index<-sample(1:size,size*com.per)
  
  data<-population
  
  data[com.index]<-1
  
  population.frame <- data.frame (zone=rep(paste0('zone',1:10), each=10), data)
  return(population.frame)
  
}
#example
pop1<-population.homo(100,0.1)
###############################################################################

###############################################################################
#silo population
#popsize:number of silos among the whole regions
#com.per:contamination fraction
###############################################################################
######silo population###############################################################
silopopulation<-function (popsize, com.per) {
  
  index<- 1:popsize
  
  population<-rep(0,popsize)
  
  # Simulation
  
  com.index<-sample(1:popsize,popsize*com.per)
  
  data<-population
  
  data[com.index]<-1
  
  population.frame <- data.frame (zone=rep(paste0('zone',1:10), each=250), data)
  return(population.frame)
  
}
#example with contamination of 0.8
silo1<-silopopulation(2500, 0.8)
###############################################################################


###############################################################################
#simple random sampling;SRSSI(probability of sampling at least one contamination in SILOS by SRS)
#popsize:number of silos; 
#com.per:contamination fraction
#iteration: itration times for each sampling scenario
## simple random sampling (SRS);x:population size;y:number of sample size
##################################################################################
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
##SRS silos
SRSSI<-function(popsize, com.per,sample.size,iteration=1000) {
  set.seed(0)
  population.frame<-truckpopulation(popsize, com.per)
  all.result<-data.frame(n.com=NULL,if.sucess=NULL )
  for(i in 1:iteration){
    one.result<-SRS(population.frame$data,sample.size)
    all.result[i,1]<-one.result[[1]]
    all.result[i,2]<-one.result[[2]]
  }
  colnames(all.result)<-c('n.com','if.sucess')
  return(sum(all.result$if.sucess)/length(all.result$if.sucess))
}
#EXAMPLE with contamniation fraction 0.8
SRSSI(2500,0.8,10)

#result based on fraction (0.08, 0.4, 0.8) with sample size (1 to 100)
#example for contamination fraction 0.08
SRSSI0.08<-NULL
for (i in 1:100) {
  SRSSI0.08[i]<-SRSDT(popsize=2500, com.per = 0.08,sample.size=i) 
}

plot(1:100,SRSSI0.08,type = 'l',xlab = 'sample size')
write.csv(SRSSI0.08,'SRSSI0.08.csv',row.names = F)
##################################################################################



#################################################################################
#Stratified sampling
#STRSI (propability of sampling at least one contamination in SILOS by STR)
#popsize:numer of SILOS; com.per:comtamination fraction
#sample.fraction: number of samples collected in one stratified group;10 regions were assumed as groups
#iter:itration times for each sampling scenario
#################################################################################
install.packages("splitstackshape")
library(splitstackshape)
STRSI <- function (popsize, com.per,sample.fraction,iter=1000){
  strata <- NULL
  for (i in 1:iter) { 
    print(paste0('sample size = ',sample.fraction,' and repeat =', i))
    if (sum ( stratified(silopopulation(popsize,com.per), "zone",sample.fraction)$data)>0) {
      strata[i]=1} else {strata[i]=0}
    strata
  }
  sum(strata)/length(strata)
}

#example with contamination fraction 0.8,sample size 10
STRSI(popsize=2500,com.per = 0.8,sample.fraction=1)
#### stratified result with contamination fraction (0.08,0.4,0.8) with sample size (10,20...100)
#example for contamination fraction 0.08
STRSI0.08<-NULL
for (i in 1:10){
  STRSI0.1[i]<-STRSI(popsize=2500,com.per = 0.08,sample.fraction=i)
}

write.csv(STRSI0.08,'STRSI0.08.csv',row.names = F)
#################################################################################




##################################################################################
#systematic sampling
#sys_silo(probability of sampling at least one contamination in silos by SS)
#popsize:numer of silos; com.per:comtamination fraction
#iter:itration times for each sampling scenario
#circular.sys (circular systematic sampling function)
#x:population size;n:sample size
##################################################################################
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

#silos
sys_silos <- function(popsize,com.per,sample.size,iter=1000){
  set.seed(0)
  sys <- NULL
  for (i in 1:iter) { 
    
    print(paste0('sample size = ',sample.size,' and repeat =', i))
    
    if (sum ( circular.sys(truckpopulation(popsize,com.per)$data, sample.size))>0) {
      sys[i]=1} else {sys[i]=0}
    sys
  }
  sum(sys)/length(sys)
}
#example with contamination fraction 0.8 sample size 10
sys_silos(popsize=2500, com.per = 0.8,sample.size = 10)
### systematic sampling results with contamination fraction (0.08,0.4,0.8) with sample size (1 to 100)
# example for contamination fraction 0.08
syssi0.08<-NULL
for (i in 1:100) {
  # message(paste0('when sample size is ',i))
  syssi0.08[i]<-sys_silos(popsize=2500, com.per = 0.04,sample.size = i) 
}
plot(1:10,syssi0.08,type = 'l')
write.csv(syssi0.08,'systr0.04wst.csv',row.names = F)
##################################################################################
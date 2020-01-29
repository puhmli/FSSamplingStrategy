###############################################################################
#function of producing contamination in the population 
#popsize:population size; group:number of regions; n.con.group:number of contaminated regions
#con.rate.group:contaminated rate per region
###############################################################################

population.heter <- function (popsize,group,n.con.group,con.rate.group){
  con.group=sample(1:group,n.con.group)
  popind<-matrix(rep(0,popsize),ncol = group)
  
  colnames(popind)<-paste0('zone',1:group)
  
  for (g in 1:n.con.group){
    
    con.number<-nrow(popind)*con.rate.group[g]
    
    con.ind<-sample(1:nrow(popind),con.number)
    
    popind[,con.group[g]][con.ind]=1 
  }
  popind
}
###############################################################################

###############################################################################
####feedpopulation (producing contamination in the feed mills)
#popsize:population size; n.con.group:contaminated regions;con.rate.group:contaminated rate per region
###############################################################################
feedpopulation <- function (popsize,group,n.con.group,con.rate.group){
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
  data.frame(data=aa, zone=rep(paste0('zone',1:10), each=10))
}
#example with contamination fraction 0.01
feed1<-feedpopulation(100, 10,2, c(0.1,0))
##############################################################################




############################################################################
#simple random sampling;SRSDF(probability of sampling at least one contamination in feed mills by SRS)
#popsize:number of feed mills; group: number of regions; n.con.group:number of contaminated regions;
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
##
SRSFM<-function(popsize, group,n.con.group,con.rate.group,sample.size,iteration=1000) {
set.seed(0)
  population.frame<-feedpopulation(popsize, group,n.con.group,con.rate.group)
  all.result<-data.frame(n.com=NULL,if.sucess=NULL )
  for(i in 1:iteration){
    one.result<-SRS(population.frame$data,sample.size)
    all.result[i,1]<-one.result[[1]]
    all.result[i,2]<-one.result[[2]]
  }
  colnames(all.result)<-c('n.com','if.sucess')
  return(sum(all.result$if.sucess)/length(all.result$if.sucess))
}
#EXAMPLE with contamniation fraction 0.1 sample size=10
SRSFM(popsize=100, group=10,n.con.group=2,con.rate.group=c(0.8,0.2),sample.size=10)

#result based on fraction (0.01, 0.05, 0.1) with sample size (1 to 100)
#example for contamination fraction 0.01
SRSFM0.01<-NULL
for (i in 1:100) {
  SRSFM0.01[i]<-SRSFM(popsize=100, group = 10,n.con.group=2,con.rate.group=c(0,0.1),sample.size=i) 
}

plot(1:100,SRSFM0.01,type = 'l',xlab = 'sample size')

############################################################################


############################################################################
#stratified sampling
#STRFM (probability of Stratified sampling can sample at least one contamination in feed mills)
#popsize:number of feed mills; group: number of regions; n.con.group:number of contaminated regions;
#con.rate.group:contaminated rate per region;
#sample.fraction: number of samples collected in one stratified group;10 regions were assumed as groups
#iteration:itration times for each sampling scenario
###############################################################################
install.packages("splitstackshape")
library(splitstackshape)
STRFM <- function (popsize, group,n.con.group,con.rate.group,sample.fraction){
  set.seed(0)
  strata <- NULL
  for (i in 1:1000) { 
    print(paste0('sample size = ',sample.fraction,' and repeat =', i))
    if (sum ( stratified(feedpopulation(popsize,group,n.con.group,con.rate.group), "zone",sample.fraction)$data)>0) {
      strata[i]=1} else {strata[i]=0}
    strata
  }
  sum(strata)/length(strata)
}

#example with contamination fraction 0.1 sample size=10 with corresponding sample.fraction=1
STRFM(popsize=100,group=10,n.con.group=2,con.rate.group=c(0.2,0.8),sample.fraction=1)
#### stratified result with contamination fraction (0.01,0.05,0.1) with sample size (10,20...100)
#example for contamination fraction 0.01
STRFM0.1<-NULL
for (i in 1:10){
  STRFM0.1[i]<-STRFM(popsize=100,group=10,n.con.group=2,con.rate.group=c(0.2,0.8),sample.fraction=i)
}

write.csv(STRFM0.1,'STRFM0.1.csv',row.names = F)

##############################################################################



###############################################################################systematic sampling
#circular.sys function of systematic sampling
#x:population size;n:sample size
#sys_feed (probability of systematic sampling can sample at least one contamination in feed mills)
#popsize:number of feed mills; group: number of regions; n.con.group:number of contaminated regions;
#con.rate.group:contaminated rate per region; sample.size: number of sample collected
#iter:itration times for each sampling scenario
###############################################################################
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
#feed mills
sys_feed <- function(popsize,group,n.con.group,con.rate.group,sample.size,iter=1000){

  sys <- NULL
  for (i in 1:1000) { 
    
    print(paste0('sample size = ',sample.size,' and repeat =', i))
    
    if (sum ( circular.sys(feedpopulation(popsize,group,n.con.group,con.rate.group)$data, sample.size))>0) {
      sys[i]=1} else {sys[i]=0}
    sys
  }
  sum(sys)/length(sys)
}
#example with contamination fraction 0.1 sample size 10
sys_feed(popsize=100, group=10,n.con.group=2,con.rate.group=c(0.2,0.8),sample.size = 10)

### systematic sampling results with contamination fraction (0.01,0.05,0.1) sample size (1 to 100)
#example for contamination fraction 0.01
SYSfeed0.01<-NULL
for (i in 1:100) {
  # message(paste0('when sample size is ',i))
  SYSfeed0.01[i]<-sys_feed(popsize=100, group=10,n.con.group=2,con.rate.group=c(0.1,0),sample.size = i) 
}
plot(1:10,SYSfeed0.01,type = 'l')
write.csv(SYSfeed0.01,'sysfeed0.01.csv',row.names = F)
###############################################################################




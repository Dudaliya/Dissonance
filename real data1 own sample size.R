
 library(plyr)
 library(dbplyr)
 library(tidyverse)
 library(readxl)
 library(purrr)
 "rows 1,2,3 full sample (sw statistic, p value, significance)
 rows 4,5,6,7 full sample (size,sw statistic, p value, significance)
 "

 data1 <- read_excel("C:/Users/darshi/Downloads/NIST data1 only.xlsx")
  View(data1)
 
  mat1<-function(n1,n2, data1){
   m1<-matrix(0,nrow=n2,ncol=7)
   
    for(j in 1:n2){ 
      "set.seed(j)"
    n<-n1
    method<-1
    data<-data1
    sample1 <- sample(data, n, replace=TRUE)
   
    #sample1 <-sample(data1, n)
  
   if(length(sample1)<=3 |length(unique(sample1))==1) {
    
     v01<-0
     v02<-0
     v03<--1
   } else {
   v01<-shapiro.test(sample1)$statistic
   v02<-shapiro.test(sample1)$p.value
   if(v02<=0.05) {v03<-1} else {v03<-0}
   }
   m1[j,1]<-v01
   m1[j,2]<-v02
   m1[j,3]<-v03
   data2<-subset(data1,data1>mean(data1))
   sample2<-data2
   if(length(sample2)<=3 | method > 7 |length(unique(sample1))==1)  {
     v0<-length(sample2)
     v1<-0
     v2<-0
     v3<--1
   } else {
   
   v0<-length(sample2)
   
   v1<-shapiro.test(sample2)$statistic
   v2<-shapiro.test(sample2)$p.value
   if(v2<=0.05) {v3<-1} else {v3<-0}
   }
  
   m1[j,4]<-v0
   m1[j,5]<-v1
   m1[j,6]<-v2
   m1[j,7]<-v3
    }
   m1
  }
   
  mat2<-function(n2,method){
    out<-matrix(0,nrow=20,ncol=8)
    trial<-c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
    row_vector<-as.array(8)
   count0<-as.array(8)
   count1<-as.array(8)
   count2<-as.array(8)
   
   sig_prop_orig<-as.array(8)
   sig_prop_samp<-as.array(8)
    for (i in 1: length(trial))
    {
      n1<-trial[i] 
    m<-mat1(n1,n2,method)
   
    sig_prop_orig[i]<-sum(m[,3]==1)/(sum(m[,3]==0)+sum(m[,3]<0)+sum(m[,3]==1))
    
    count0[i]<-sum(m[,7]==0)
    count1[i]<-sum(m[,7]<0)
    count2[i]<-sum(m[,7]==1)
    sig_prop_samp[i]<-count2[i]/(count0[i]+count1[i]+count2[i])
    
   
   out[i,1]<-n1
   out[i,2]<-n2
   out[i,3]<-method
   out[i,4]<-count1[i]
   out[i,5]<-count2[i]
   out[i,6]<-(count0[i]+count2[i]+count1[i])
   out[i,7]<-sig_prop_orig[i]
   out[i,8]<-sig_prop_samp[i]
    }
    
    out
  }
  
  
  mat3<-function(n2){
    
    
    mn<-array(0,dim=c(20,8,15))
    for (k in 1: 15)
    {out1<-mat2(n2,k)
    mn[,,k]<-out1
    }
    mn
  }
  
  ##Just to get last two columns
  mat4<-function(n2){
    
    mn<-array(0,dim=c(20,1,15))
    for (k in 1: 10)
    {out1<-mat2(n2,k)
    
    mn[,,k]<-out1[,c(7,8)]
    }
    mn
  }   
  ##############
  # Step 2: Convert the list to a data frame
  ma_df <- as.data.frame(ma)
  
  # Step 3: Export the data frame to a CSV file
  write.csv(ma_df, file = "d:\\sample own size itereate.csv", row.names = FALSE)
  
  
  
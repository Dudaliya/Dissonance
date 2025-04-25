This runs Shapiro Wilk test - Select random samples from 1000 datapoints
 library(plyr)
 library(dbplyr)
 library(tidyverse)
 library(readxl)
 "rows 1,2,3 full sample (sw statistic, p value, significance)
 rows 4,5,6,7 full sample (size,sw statistic, p value, significance)
 "
  mat1<-function(n1,n2,method){
   m1<-matrix(0,nrow=n2,ncol=7)
   
    for(j in 1:n2){ 
      "set.seed(j)"
    n<-n1
    if(method==1){
   data1<-rnorm(1000,mean=0,sd=1)} else if (method==2)
   {data1<-runif(1000,min=0, max=1)}
    else if (method==3)
    {data1<-
      rlnorm(1000,meanlog=0,sdlog=1)}
 
    else if (method==4)
    {data1<-
      rbeta(1000,2,1,0)}
    
    else if (method==5)
    {data1<-
      rweibull(1000,shape=2,scale=1)}
    else if (method==6)
    {data1<-
      rweibull(1000,shape=1,scale=1)}
    else if (method==7)
    {data1<-
      rbeta(1000,2,2)}
    else if (method==8)
    {data1<-
      rpois(1000,lambda=1)}
    else if (method==9)
    {data1<-
      rbinom(1000,500,0.5)}
    else if (method==10)
    {data1<-
      rchisq(1000,1,0)}
    else if (method==11)
    {data1<-
      rchisq(1000,10,0)}
    else if (method==12)
    {data1<-
      rt(1000,df=4)}
    else if (method==13)
        {data1<-
      rt(1000,df=10)}
    else if (method==14)
      {data1<-
        rbinom(1000,500,0.95)}
    else if (method==15)
    {data1<-
      rweibull(1000,shape=5,scale=2)}
    
    sample1 <-sample(data1, n)
   sample1 <-sample(data1, n)
   if((length(sample1)<=3)| (length(unique(sample1))==1)){
    
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
   sample2<-sample(data2, n)
   if(length(sample2)<=3 | method > 7 |(length(unique(sample2))==1))  {
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
     
  # Step 1: Install and load the openxlsx package
  install.packages("openxlsx")
  library(openxlsx)
  
  # Step 2: Create a workbook
  wb <- createWorkbook()
  
  # Step 3: Add a worksheet
  addWorksheet(wb, "Sheet1")
  
  # Step 4: Write data to the worksheet
  writeData(wb, "Sheet1", ma)
  
  # Step 5: Save the workbook to an Excel file
  saveWorkbook(wb, file = "d:\sample Original itereate.xlsx", overwrite = TRUE)
  
  # Step 1: Install and load the openxlsx package
  install.packages("openxlsx")
  library(openxlsx)
  
  # Step 2: Create a workbook
  wb <- createWorkbook()
  
  # Step 3: Iterate through the list and add each element to a new worksheet
  for (i in seq_along(ma)) {
    sheet_name <- paste("Sheet", i)
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, ma[[i]])
  }
  
  # Step 4: Save the workbook to an Excel file
  saveWorkbook(wb, file = "d:\\sample Original itereate.xlsx", overwrite = TRUE)
 
  
  # Step 2: Convert the list to a data frame
  ma_df <- as.data.frame(ma)
  
  # Step 3: Export the data frame to a CSV file
  write.csv(ma_df, file = "d:\\sample Original itereate.csv", row.names = FALSE)
  
  

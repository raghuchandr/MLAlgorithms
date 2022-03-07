

## Step 0 clean up!!!
## remove all objects
rm(list=ls())


######### Read and load Brest cancer data.  ######### 
  setwd("~/")
  dsa_raw<-read.csv("breast-cancer-wisconsin.data.csv", stringsAsFactors = TRUE, na.strings="?")
  View(dsa_raw)
  
######### Check package validate is installed if not do installation and load. ######### 
  if ("validate" %in% rownames(installed.packages()) ) {
       library(validate) # Load Library
     } else {
       install.packages("validate") # Install package
       library(validate) # Load Library
       }
      #detach("package:validate", unload=TRUE)

######### Run data completeness validations.  ######### 
  cf <- check_that(dsa_raw, Sample > 1, F1 > 0, F2 > 0, F3 > 0, F4 > 0, F5 > 0, F6 > 0, F7 > 0, F8 > 0, F9 > 0, Class > 0 )
  summary(cf)
    #Results:
    #     name items passes fails nNA error warning expression
    # 7   V07   699    683     0  16 FALSE   FALSE     F6 > 0
    # 16 records has NA values
  
#### Normilize data - Devide by 10 Feature columns and translate Class values to Bening and Malignant####
  dsa_tmp<-dsa_raw
  dsa_tmp[,2:10]=dsa_tmp[,2:10]/10
  dsa_tmp$Class[dsa_tmp$Class == 2 ] <-"Bening"
  dsa_tmp$Class[dsa_tmp$Class == 4 ] <-"Malignant"
  
## Add titles to Features columns ##
  names(dsa_tmp)<-c("Sample Code","Clump Thickness","Uniformity of Cell Size","Uniformity of Cell Shape","Marginal Adhesion","Single Epithelial Cell Size","Bare Nuclei","Bland Chromatin","Normal Nucleoli","Mitoses","Diagnosis Class")   
  View(dsa_tmp)
  
#### Filter rows with NA and initialize dsa_norm for analysis ####  
  dsa_norm_noNa<-na.omit(dsa_tmp)
  View(dsa_norm_noNa)
  
#?hclust 
#### Hierachical Cluster - 638 NA Filtered - Complete ####
  hclust_resutls<-hclust(dist(dsa_norm_noNa[,2:10]), method = "complete")  
  plot(hclust_resutls, cex=0.3, main = "Cluster Dendogram - complete")  
  rect.hclust(hclust_resutls, k = 2, border = 2:20 ) #Rectangular clusters  
  hclust_cut3<-cutree(hclust_resutls,2) #Cut the data to split it in two clusters.
  cm <- table(dsa_norm_noNa[,11],hclust_cut3) # Cluster seems to make sense. Confusion/Error Matrix.
  cm
  cat("Accuracy with Hierachichal Cluster (2 clsuters) - 699 - Complete: ", sum(diag(cm)) / sum(cm) * 100, "/", 100 - ( (sum(diag(cm)) / sum(cm) * 100 ) ) )
  
#### Hierachical Cluster - 683 NA Filtered - ward.D ####
  hclust_resutls<-hclust(dist(dsa_norm_noNa[,2:10]), method = "ward.D")  
  plot(hclust_resutls, cex=0.3, main = "Cluster Dendogram - ward.D")  
  rect.hclust(hclust_resutls, k = 2, border = 2:20 ) #Rectangular clusters  
  hclust_cut3<-cutree(hclust_resutls,2) #Cut the data to split it in two clusters.
  cm <- table(dsa_norm_noNa[,11],hclust_cut3) # Cluster seems to make sense. Confusion/Error Matrix.
  cm
  cat("Accuracy with Hierachical Cluster (2 clsuters) - 699 - ward.D: ", sum(diag(cm)) / sum(cm) * 100, "/", 100 - ( (sum(diag(cm)) / sum(cm) * 100 ) ) )
  
#### Hierachical Cluster - 683 NA Filtered - average ####
  hclust_resutls<-hclust(dist(dsa_norm_noNa[,2:10]), method = "average")  
  plot(hclust_resutls, cex=0.3, main = "Cluster Dendogram - average")  
  rect.hclust(hclust_resutls, k = 2, border = 2:20 ) #Rectangular clusters  
  hclust_cut3<-cutree(hclust_resutls,2) #Cut the data to split it in two clusters.
  cm <- table(dsa_norm_noNa[,11],hclust_cut3) # Cluster seems to make sense. Confusion/Error Matrix.
  cm
  cat("Accuracy with Hierachical Cluster (2 clsuters) - 699 - average: ", sum(diag(cm)) / sum(cm)*100, "/", 100 - ( (sum(diag(cm)) / sum(cm) * 100 ) ) )

    
#### EXTRA - NOT REQUIRED: Identify and fix Data Quality issues. ####
  #Funtion to get the statistical mode.
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  summary(dsa_raw[,7])
  #Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #1.000   1.000   1.000   3.545   6.000  10.000      16 
  mode=getmode(na.omit(dsa_raw[,7]))
  print(mode/10)
  print(mean(dsa_raw[,7], na.rm = TRUE)/10)
  # After testing bellow the Hcust Overall Accuracy with both using Mean doesnt decrease much the accuracy.

  dsa_tmp2<-dsa_tmp #To keep dsa_tmp unchaged.
  #dsa_tmp[is.na(dsa_tmp)] <- mode/10 #Replacing NA with mode Normilize that is 0.1
  dsa_tmp2[is.na(dsa_tmp2)] <- mean(dsa_raw[,7], na.rm = TRUE)/10 #Replacing NA with Mean Normilize that is 0.3544656
  
#### NA Rows using Mean and initialize dsa_norm for analysis ####  
  dsa_norm<-dsa_tmp2
  View(dsa_norm)
  
#### Hierachical Cluster - 699 Fixing missing measures  - ward.D ####
  hclust_resutls<-hclust(dist(dsa_norm[,2:10]), method = "ward.D")  
  plot(hclust_resutls, cex=0.3, main = "Cluster Dendogram - ward.D - NA < mean")  
  rect.hclust(hclust_resutls, k = 2, border = 2:20 ) #Rectangular clusters  
  hclust_cut3<-cutree(hclust_resutls,2) #Cut the data to split it in two clusters.
  cm <- table(dsa_norm[,11],hclust_cut3) # Cluster seems to make sense. Confusion/Error Matrix.
  cm
  cat("Accuracy with Hierachical Cluster (2 clsuters) - 699 - ward.D : ", sum(diag(cm)) / sum(cm) * 100, "/", 100 - ( (sum(diag(cm)) / sum(cm) * 100 ) ) )

#### K-means Cluster - 683 NA Filtered ####
  #?kmeans
  
  kmeans_2<- kmeans(dsa_norm_noNa[,2:10],2,nstart = 10) # 3 clausters, not including las col, just numeric values. Nstart give the option to test centroids
  
  cm <- table(dsa_norm_noNa[,11],kmeans_2$cluster)
  cm
  cat("Accuracy with Hierachical Cluster (2 clsuters) - 683 - Kmeans : ",  sum(diag(cm)) / sum(cm) * 100, "/", 100 - ( (sum(diag(cm)) / sum(cm) * 100 ) ) )
  


    
#### Detaching Packages, close plots and clean Global Enviroment variables ####
detach("package:validate", unload=TRUE)
dev.off()  
rm(list=ls())  

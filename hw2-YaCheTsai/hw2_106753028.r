query_func<-function(query_m, i, d, name)
{
  if(query_m == "male"){
   d <- d[order(d[,4], decreasing = TRUE), ]
    sensitivity <- i[4]/(i[4]+i[3])
    specificity <- i[1]/(i[1]+i[2])
    recall <- specificity
    precision <- i[4]/(i[4]+i[2])
    F1 <- 2*i[4]/(2*i[4]+i[3]+i[2])
   
    nrank <- 0
    M <- i[4]+i[3]
    N <- i[1]+i[2]
   
    for(j in c(1:length(d$reference))){
      if(d$reference[j] == query_m){
         nrank <- nrank + (length(d$reference)+1 - j )
      }
    }
    auc = (nrank-(M*(M+1)/2))/(M*N)
   
    
    return (c(name, round(sensitivity, digits=2),round(specificity, digits=2),round(F1, digits=2),round(auc, digits=2)))
    
    
    
  }
  else if (query_m == "female") {
   d <- d[order(d[,4]), ]
    sensitivity <- i[1]/(i[1]+i[2])
    specificity <- i[4]/(i[4]+i[3])
    recall <- specificity
    precision <- i[1]/(i[1]+i[3])
    F1 <- 2*i[1]/(2*i[1]+i[2]+i[3])
   
    nrank <- 0
    M <- i[1]+i[2]
    N <- i[4]+i[3]
   
    for(j in c(1:length(d$reference))){
      if(d$reference[j] == query_m){
         nrank <- nrank + (length(d$reference)+1 - j )
      }
    }
    auc = (nrank-(M*(M+1)/2))/(M*N)
    
    return (c(name, round(sensitivity, digits=2),round(specificity, digits=2),round(F1, digits=2),round(auc, digits=2)))
    
  } else {
    stop(paste("ERROR: unknown query function", query_m))
  }
}

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_yourID.R --target male|female --inputs file1 file2 ... filen --output out.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m<-args[i+1]
    i<-i+1
  }else if(args[i] == "--inputs"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--output"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}


# read files
names<-c()
result<-c()
for(file in files)
{
  name<-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  
  
  xtab <- table(d$prediction, d$reference)

  x<- query_func(query_m, xtab, d, name)
  result <- rbind(result, x)
  
  names<-c(names,name)
}
h<-c("highedt")

colnames(result) <- c("method","sensitivity","specificity", "F1", "AUC")
for(i in c(2:5)){
 d <- result[order(result[,i], decreasing = TRUE), ]
 h<- cbind (h,d[1:1])
 }
 result <- rbind(result, h)


 write.table(result, file = out_f, sep = ",", row.names = FALSE,)
































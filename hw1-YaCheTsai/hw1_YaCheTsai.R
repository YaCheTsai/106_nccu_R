args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_YaCheTsai.R input ", call.=FALSE)
} 

if(args[1] == "-input"){
  inputfile <-  args[2]
  outputfile <- args[4]
}else if (args[1] == "-out"){
  inputfile <-  args[4]
  outputfile <- args[2]
}else{
  stop("input error", call.=FALSE)
}


data = read.csv(file=inputfile)

data[2:3]<- round(data[2:3],2)

data <- apply(data,2,max)

result <-t(data.matrix(data)[-4,])
result[1] <- substr(inputfile,start=1,stop=6)

colnames(result)[colnames(result)=="persons"] <- "set" 

write.table(result, file = outputfile, sep = ",", row.names = FALSE,)

# Build features for PFA models
ech<-FALSE
# Read script parameters
args <- commandArgs(trailingOnly = TRUE)
# Enable if debugging

#print(args)4444r44r

# initialize variables
inputFile = NULL


deedef    jfnjf  c  fgtggyhyh
workingDirectory = NULL
componentDirectory = NULL
flags = NULL

# parse commandline args
i = 1
while (i <= length(args)) {
    if (args[i] == "-file0") {
       if (length(args) == i) {
          stop("input file name must be specified")
       }
       inputFile0 = args[i+1]
       i = i+1
    }  else if (args[i] == "-file1") {
       if (length(args) == i) {
          stop("input file name must be specified")
       }
       inputFile1 = args[i+1]
       i = i+1
    }else if (args[i] == "-workingDir") {
       if (length(args) == i) {
          stop("workingDir name must be specified")
       }
# This dir is the working dir for the component instantiation.
       workingDirectory = args[i+1]
       i = i+1
    } else if (args[i] == "-programDir") {
       if (length(args) == i) {
          stop("programDir name must be specified")
       }
# This dir is the root dir of the component code.
       componentDirectory = args[i+1]
       i = i+1
    } 
    i = i+1
}

if (is.null(inputFile) || is.null(workingDirectory) || is.null(componentDirectory) ) {
   if (is.null(inputFile)) {
      warning("Missing required input parameter: -file0")
   }
   if (is.null(workingDirectory)) {
      warning("Missing required input parameter: -workingDir")
   }
   if (is.null(componentDirectory)) {
      warning("Missing required input parameter: -programDir")
   }


   stop("Usage: -programDir component_directory -workingDir output_directory -file0 input_file0 -file1 input_file0")
}

# This dir contains the R program or any R helper scripts
programLocation<- paste(componentDirectory, "/program/", sep="")

# Get data
outputFilePath<- paste(workingDirectory, "transaction file with covariate feature.txt", sep="")

# Get data
datalocation<- paste(componentDirectory, "/program/", sep="")
val<-read.table(inputFile0,sep="\t", header=TRUE,na.strings="",quote="",comment.char = "")
val2<-read.table(inputFile1,sep="\t", header=TRUE,na.strings="",quote="",comment.char = "")

# Creates output log file (use .wfl extension if you want the file to be treated as a logging file and hide from user)
clean <- file(paste(workingDirectory, "CopyCovariate-log.wfl", sep=""))
sink(clean,append=TRUE)
sink(clean,append=TRUE,type="message") # get error reports also
options(width=120)

# cat(length(val$Outcome))yhhhyyhyhy
                               for (i in unique(val2$Anon.Student.Id)){
                                 val$covariate[val$Anon.Student.Id==i,]<-val2$value[val2$Anon.Student.Id==i]}
                               
 
# Feature creation
val$CF..ansbin.<-ifelse(tolower(val$Outcome)=="correct",1,ifelse(tolower(val$Outcome)=="incorrect",0,-1))
#val$CF..KCindex.<-  paste(val$Anon.Student.Id,val$KC..Default.,sep="-")
val$CF..KCindex.<-  paste(val$Anon.Student.Id,eval(parse(text=paste("val$",KCmodel,sep=""))),sep="-")
val<-val[order(val$Anon.Student.Id,]

# cat("\nnow adding cor\n")
val$CF..cor.<-corcount(val,val$CF..KCindex.)
# cat("now adding incor\n")
val$CF..incor.<-incorcount(val,val$CF..KCindex.)
# cat("now adding study\n")
val$CF..study.<-studycount(val,val$CF..KCindex.)
#remove no KC lines
eval(parse(text=paste("val<-val[!is.na(val$",KCmodel,"),]",sep="")))
# cat("now writing table\n")

# Export modified data frame for reimport after header attachment
headers<-gsub("Unique[.]step","Unique-step",colnames(val))
headers<-gsub("[.]1","",headers)
headers<-gsub("[.]2","",headers)
headers<-gsub("[.]3","",headers)
headers<-gsub("Single[.]KC","Single-KC",headers)
headers<-gsub("[.][.]"," (",headers)
headers<-gsub("[.]$",")",headers)
headers<-gsub("[.]"," ",headers)
headers<-paste(headers,collapse="\t")
write.table(headers,file=outputFilePath,sep="\t",quote=FALSE,na = "",append=FALSE,col.names=FALSE,row.names = FALSE)
write.table(val,file=outputFilePath,sep="\t",quote=FALSE,na = "",append=TRUE,col.names=FALSE,row.names = FALSE)

# Stop logging
sink()
sink(type="message")

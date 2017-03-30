### functions ###
stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

deg2rad <- function(deg) {(deg * pi) / (180)}

raw2clean <-function(files){
  #files <- list.files(pattern = "[.]txt$") #how to list files
  results = data.frame(run=1:length(files),sub=0,x=0,y=0,conf=0,rt=0,dir=0,lang=0,sent=0,xi=0,yi=0)
  for (run in 1:length(files)){
    print(run)
    file <- read.delim(files[run], sep="\t", header=FALSE)
    results[run,]$rt = tail(file)[6,2]
    results[run,]$sub=as.numeric(as.character(unlist(file[2,]$V1)))
    results[run,]$x=as.numeric(unlist(file[2,]$V2))
    results[run,]$y=as.numeric(unlist(file[2,]$V3))
    results[run,]$conf=as.numeric(unlist(file[2,]$V4))
    results[run,]$lang=as.character(unlist(file[2,]$V5))
    results[run,]$dir=as.character(unlist(file[2,]$V6))
    results[run,]$sent=as.character(unlist(file[2,]$V7))
  }
  return (results)
}


# nolint start
#setwd("/usr/people/bakker/KNMI14/transformatie/droogte")
setwd("/nobackup/users/photiado/KNMI14_scenarios_2/transformatie/droogte")

rm(list=ls(all=TRUE))

source("../station_transform_functions.R")

var="evmk"

version   <- "v1.0"
scenarios <- c("","GL","GH","WL","WH")
periodes  <- c("ref","2030","2050","2085")
range="19810101-20101231"

products <- data.frame("sum"=1)
drempels <- vector()

nametabel <- as.matrix(data.frame(short=c("sum"),long=c("seasonal.sum")))

# reference
ifile       <- paste("../klimatologie/klimaat_",file.name(var=var,v=version),sep="")
obs         <- read.table(ifile)
h.ids       <- which(obs[,1]=="00000000")                # welke rijen niet?
names(obs)  <- c("00000000",as.numeric(obs[1,-1]))
obs         <- obs[-h.ids,]
nr          <- nrow(obs)
ns          <- ncol(obs)-1

## DEFINIEER SCENARIOTABEL ##
tabel.260        <- as.data.frame(matrix(data=NA,nrow=nr,ncol=12))
names(tabel.260) <- c("season","variabeles","reference","2030","GL2050","GH2050","WL2050","WH2050","GL2085","GH2085","WL2085","WH2085")

varnames    <- as.character(as.matrix(obs[,1]))
lastchar    <- nchar(varnames)
season      <- as.numeric(substr(varnames,lastchar,lastchar))
if(sum(season==0)>0) {tabel.260$season[which(season==0)] <- "year  "}
if(sum(season==1)>0) {tabel.260$season[which(season==1)] <- "winter"}
if(sum(season==2)>0) {tabel.260$season[which(season==2)] <- "spring"}
if(sum(season==3)>0) {tabel.260$season[which(season==3)] <- "summer"}
if(sum(season==4)>0) {tabel.260$season[which(season==4)] <- "autumn"}
varnames    <- substr(varnames,1,(lastchar-1))
longnames   <- varnames
for(i in 1:nrow(nametabel)) {
  longnames[which(longnames==nametabel[i,1])] <- nametabel[i,2]
}
tabel.260$variabeles <- longnames
for(ss in 0:4) {tabel.260[which(season==ss),"reference"] <- round(obs[season==ss,"260"],2)}
## ##

## DEFINIEER TABEL.ALL ##
tabel.all <- vector()

tabel            <- as.data.frame(matrix(data=NA,nrow=nr,ncol=3+ns))
names(tabel)     <- c("scenario","season","variabeles",names(obs[,-1]))

varnames    <- as.character(as.matrix(obs[,1]))
lastchar    <- nchar(varnames)
season      <- as.numeric(substr(varnames,lastchar,lastchar))
if(sum(season==0)>0) {tabel$season[which(season==0)] <- "year  "}
if(sum(season==1)>0) {tabel$season[which(season==1)] <- "winter"}
if(sum(season==2)>0) {tabel$season[which(season==2)] <- "spring"}
if(sum(season==3)>0) {tabel$season[which(season==3)] <- "summer"}
if(sum(season==4)>0) {tabel$season[which(season==4)] <- "autumn"}
varnames    <- substr(varnames,1,(lastchar-1))
longnames   <- varnames
for(i in 1:nrow(nametabel)) {
  longnames[which(longnames==nametabel[i,1])] <- nametabel[i,2]
}
tabel$variabeles <- longnames
tabel$scenario   <- "reference"

tabel[,-1:-3] <- round(obs[,-1],2)

tabel.all <- rbind(tabel.all,tabel)
## ##

for(p in periodes) {
  for(sc in scenarios) {

    if(( (p == "2030" | p == "ref") & sc == "") | (p != "2030" & p != "ref" & sc != "")) {

      sc_p       <- paste(sc,p,sep="")
      print(c(sc_p,paste("version:",version)))

      ifile      <- paste("../klimatologie/klimaat_",file.name(sc=sc,p=p,var=var,v=version),sep="")
      fut        <- read.table(ifile)
      names(fut) <- names(obs)
      fut        <- fut[-h.ids,]
      delta      <- obs

      # relative change
      delta[,-1]    <- 100 * (fut[,-1] - obs[,-1]) / obs[,-1]

      # scenario tabellen (alles op basis van De Bilt)
      for(ss in 0:4) {tabel.260[which(season==ss),sc_p] <- round(delta[season==ss,"260"],2)}

      # scenario tabel (alles)
      tabel[,-1:-3]  <- round(delta[,-1],2)
      tabel$scenario <- sc_p

      tabel.all <- rbind(tabel.all,tabel)


    } # if(p == "MOC" | p == "EOC" | sc == "G") {
  }   # sc
}     # period

ofile <- paste("scenario_tabel_verdamping_trans_",version,"_DeBilt.txt",sep="")
write.table(format(rbind(colnames(tabel.260),tabel.260),width=10,digits=1,justify="right"), ofile,col.names=F,row.names=F,quote=F)

ofile <- paste("scenario_tabel_verdamping_trans_",version,"_ALL.txt",sep="")
write.table(format(rbind(colnames(tabel.all),tabel.all),width=10,digits=1,justify="right"), ofile,col.names=F,row.names=F,quote=F)

# nolint end

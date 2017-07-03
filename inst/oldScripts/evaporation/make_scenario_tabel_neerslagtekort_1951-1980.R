# nolint start
#setwd("/usr/people/bakker/KNMI14/transformatie/droogte")
setwd("/nobackup/users/photiado/KNMI14_scenarios_2/transformatie/droogte")

rm(list=ls(all=TRUE))

source("/nobackup/users/photiado/nobackup_1/bakker_home_bakup/Programma/R-functions/general_functions.R")
source("../station_transform_functions.R")

#versions  <- c("v0.1","v0.2")
#scenarios <- c("","GL","GH","WL","WH")
#periodes  <- c(2030,2050,2085)
range="19510101-19801231"

subset="P102"
sset <- as.vector(t(read.table(paste("../neerslag/",subset,".txt",sep=""))))
sset <- as.character(sset)

# verdamping
ev <- read.table("../JulesBeersma/P13_Er_DeBilt.dat",skip=4)
dt <- ev[,1]
id <- which(dt>=19510101 & dt<=19801231)
ev <- ev[id,3]
dt <- dt[id]

mm        <- (dt%/%100)%%100
amjjas    <- which(mm>=4 & mm<=9)
yy        <- (dt%/%10000)[amjjas]
ev        <- ev[amjjas]

# neerslag
rr        <- read.table(paste("../tijdreeksen/",file.name(sc="",p="ref",var="rr",range=range,v="v1.0"),sep=""))
names(rr) <- c("date",round(rr[1,-1],0))
h.ids     <- which(rr[,1]==0)
rr        <- rr[-h.ids,sset][-1,]
rr        <- apply(rr[amjjas,],1,mean)

# maximaal potentieel neerslagtekort
Xsum      <- tapply(ev-rr,yy,max.pos.cumsum) # bereken maximaal potentieel neerslagtekort
nr        <- length(Xsum)
Xstat     <- c(mean(Xsum),sd(Xsum),sort(as.numeric(Xsum)))          # mean,sd,ranks Xsum

## DEFINIEER SCENARIOTABEL EN JAARREEKSTABEL ##
tabel.sc              <- as.data.frame(matrix(data=NA,nrow=nr+2,ncol=3),col.names=kolommen)
names(tabel.sc)       <- c("season","variabeles","1951-1980")

tabel.sc$season       <- "year"
tabel.sc$variabeles   <- c("mean","stdev",paste("rank",1:nr,sep="."))
tabel.sc["1951-1980"] <- round(Xstat,0)

tabel.yr              <- as.data.frame(matrix(data=NA,nrow=nr,ncol=2),col.names=kolommen)
names(tabel.yr)       <- c("year","1951-1980")
tabel.yr$year         <- as.numeric(names(Xsum))
tabel.yr["1951-1980"] <- round(as.numeric(Xsum),0)

# WEGSCHRIJVEN
ofile <- "scenario_tabel_neerslagtekort_1951-1980_P102.txt"
write.table(format(rbind(colnames(tabel.sc),tabel.sc),width=10,digits=1,justify="right"), ofile,col.names=F,row.names=F,quote=F)

ofile <- "jaarlijks_neerslagtekort_1951-1980_P102.txt"
write.table(format(rbind(colnames(tabel.yr),tabel.yr),width=10,digits=1,justify="right"), ofile,col.names=F,row.names=F,quote=F)

# nolint end

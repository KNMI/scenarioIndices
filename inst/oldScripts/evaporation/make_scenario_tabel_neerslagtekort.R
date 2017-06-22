# nolint start
#setwd("/usr/people/bakker/KNMI14/transformatie/droogte")
setwd("/nobackup/users/photiado/KNMI14_scenarios_2/transformatie/droogte")

rm(list=ls(all=TRUE))

source("/nobackup/users/photiado/nobackup_1/bakker_home_bakup/Programma/R-functions/general_functions.R")
source("../station_transform_functions.R")

#versions  <- c("v0.1","v0.2")
versions  <- c("v0.1","v0.2","v1.1","v1.2")
scenarios <- c("","GL","GH","WL","WH")
periodes  <- c(2030,2050,2085)
range="19810101-20101231"

subset="P102"
sset <- as.vector(t(read.table(paste("../neerslag/",subset,".txt",sep=""))))
sset <- as.character(sset)

# reference
ev        <- read.table(paste("../tijdreeksen/",file.name(sc="",p="ref",var="evmk",range=range,v="v1.0"),sep=""))
names(ev) <- c("date",round(ev[1,-1],0))
h.ids     <- which(ev[,1]==0)
dt        <- ev[-h.ids,1]
ev        <- ev[-h.ids,"260"]

mm        <- (dt%/%100)%%100
amjjas    <- which(mm>=4 & mm<=9)
yy        <- (dt%/%10000)[amjjas]
ev        <- ev[amjjas]
# rr
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
tabel.sc            <- as.data.frame(matrix(data=NA,nrow=nr+2,ncol=12),col.names=kolommen)
names(tabel.sc)     <- c("season","variabeles","reference","2030","GL2050","GH2050","WL2050","WH2050","GL2085","GH2085","WL2085","WH2085")

tabel.sc$season     <- "year"
tabel.sc$variabeles <- c("mean","stdev",paste("rank",1:nr,sep="."))
tabel.sc$reference  <- round(Xstat,1)

tabel.yr            <- as.data.frame(matrix(data=NA,nrow=nr,ncol=11),col.names=kolommen)
names(tabel.yr)     <- c("year","reference","2030","GL2050","GH2050","WL2050","WH2050","GL2085","GH2085","WL2085","WH2085")
tabel.yr$year       <- as.numeric(names(Xsum))
tabel.yr$reference  <- round(as.numeric(Xsum),1)

for(version in versions) {
  for(scaling in c("lower","upper","centr")) {

    for(p in periodes) {
      for(sc in scenarios) {
        if(( (p == "2030" | p == "ref") & sc == "") | (p != "2030" & p != "ref" & sc != "")) {
          sc_p <- ifelse(p=="2030","2030",paste(sc,p,sep=""))
          print(c(sc,p,scaling,paste("version:",version),subset))

          # verdamping
          ev        <- read.table(paste("../tijdreeksen/",file.name(sc=sc,p=p,var="evmk",range=range,v="v1.0"),sep=""))
          names(ev) <- c("date",round(ev[1,-1],0))
          h.ids     <- which(ev[,1]==0)
          dt        <- ev[-h.ids,1]
          ev        <- ev[-h.ids,"260"]
          mm        <- (dt%/%100)%%100
          amjjas    <- which(mm>=4 & mm<=9)
          yy        <- (dt%/%10000)[amjjas]
          ev        <- ev[amjjas]

          # neerslag
          rr        <- read.table(paste("../tijdreeksen/",file.name(sc=sc,p=p,var="rr",range=range,v=version,scaling=scaling),sep=""))
          names(rr) <- c("date",round(rr[1,-1],0))
          h.ids     <- which(rr[,1]==0)
          rr        <- rr[-h.ids,sset][-1,]
          rr        <- apply(rr[amjjas,],1,mean)

          # maximaal potentieel neerslagtekort
          Ysum      <- tapply(ev-rr,yy,max.pos.cumsum) # bereken maximaal potentieel neerslagtekort
          Ystat     <- c(mean(Ysum),sd(Ysum),sort(as.numeric(Ysum)))          # mean,sd,ranks Ysum
          delta     <- 100*(Ystat-Xstat)/Xstat
          ## DEFINIEER SCENARIOTABEL EN JAARREEKSTABEL ##
          tabel.sc[,sc_p]  <- round(delta,1)
          tabel.yr[,sc_p]  <- round(as.numeric(Ysum),1)

        } # if(p == "MOC" | p == "EOC" | sc == "G") {
      }   # sc
    }     # period

    ofile <- paste("scenario_tabel_neerslagtekort_scaling-",scaling,"_trans_",version,"_",subset,".txt",sep="")
    write.table(format(rbind(colnames(tabel.sc),tabel.sc),width=10,digits=1,justify="right"), ofile,col.names=F,row.names=F,quote=F)

    ofile <- paste("jaarlijks_neerslagtekort_scaling-",scaling,"_trans_",version,"_",subset,".txt",sep="")
    write.table(format(rbind(colnames(tabel.yr),tabel.yr),width=10,digits=1,justify="right"), ofile,col.names=F,row.names=F,quote=F)
  }
}

# nolint end

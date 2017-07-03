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
yrange=paste(substring(range,1,4),substring(range,10,13),sep="-")

products <- data.frame("sum"=1)
drempels <- vector()

for(p in periodes) {
  for(sc in scenarios) {
    if(( (p == "2030" | p == "ref") & sc == "") | (p != "2030" & p != "ref" & sc != "")) {
      print(paste(sc,p))

      ev        <- read.table(paste("../tijdreeksen/",file.name(sc=sc,p=p,var=var,range=range,v=version),sep=""))
      h.ids     <- which(ev[,1]==0)
      header    <- ev[h.ids,]; header[,1] <- "00000000"
      names(ev) <- c("date",round(ev[1,-1],0))
      ev        <- ev[-h.ids,]

      mm <- (ev[,1]%/%100)%%100
      ss <- as.integer((mm/3)%%4+1)
      yy <-  ev[,1]%/%10000
      wy <- ifelse(mm<12,yy,yy+1)


      # seasonal variables
      for(season in 0:4) {
        season.name <- c("year","winter","spring","summer","autumn")[season+1]
        if(season==0) {
          id  <- 1:length(yy)
          idy <- yy
        } else {
          if(season==1) {
            id  <- which(ss==1 & wy > min(wy) & wy < max(wy))
            idy <- wy[id]
          } else {
            id  <- which(ss==season)
            idy <- yy[id]
          }
        }

        if(length(drempels>0)) {
          for(j in 1:length(drempels)) {
            X     <- aggregate(ev[id,-1]>=drempels[j],by=list(idy),  sum)
            stat  <- paste("N_",drempels[j],"mm",sep="")
            ofile <- paste("../jaarreeksen/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version),sep="")

            schrijf.header.tabel(ofile,header,X)
          }
        }

        prod="sum"
        if(products[prod]==1) {
          X     <- aggregate(ev[id,-1],by=list(idy),  sum)
          stat  <- prod
          ofile <- paste("../jaarreeksen/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version),sep="")

          schrijf.header.tabel(ofile,header,X)
        }
      } # season
    } # if(p == "MOC" | p == "EOC" | sc == "G") {
  }   # sc
}     # period


# nolint end

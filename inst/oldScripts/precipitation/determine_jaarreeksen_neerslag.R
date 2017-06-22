# nolint start
setwd("/usr/people/bakker/KNMI14/transformatie/neerslag")
rm(list=ls(all=TRUE))

source("../station_transform_functions.R")

var="rr"

#versions  <- c("v0.1","v0.2")
versions  <- c("v1.1","v1.2")
scenarios <- c("","GL","GH","WL","WH")
periodes  <- c("ref","2030","2050","2085")
range="19810101-20101231"
yrange=paste(substring(range,1,4),substring(range,10,13),sep="-")

products <- data.frame("sum"=1)

drempels <- c(0.1,0.3,1.0,5.0,10.0,15.0,20.0,30.0)

for(version in versions) {
  for(p in periodes) {
    for(sc in scenarios) {
      if(( (p == "2030" | p == "ref") & sc == "") | (p != "2030" & p != "ref" & sc != "")) {
        for(scaling in c("lower","upper","centr")) {

          print(c(sc,p,scaling,paste("version:",version)))

          # read data
          ifile <- ifelse(p!="ref",
            paste("../tijdreeksen/",file.name(sc=sc,p=p,var=var,range=range,v=version,scaling=scaling),sep=""),
            paste("../tijdreeksen/",file.name(sc=sc,p=p,var=var,range=range,v="v1.0",scaling=""),sep=""))

          rr    <- read.table(ifile)
          h.ids     <- which(rr[,1]==0)
          header    <- rr[h.ids,]; header[,1] <- "00000000"
          names(rr) <- c("date",round(rr[1,-1],0))
          rr        <- rr[-h.ids,]

          mm <- (rr[,1]%/%100)%%100
          ss <- as.integer((mm/3)%%4+1)
          yy <- rr[,1]%/%10000
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
                X     <- aggregate(rr[id,-1]>=drempels[j],by=list(idy),  sum)
                stat  <- paste("N_",drempels[j],"mm",sep="")

                ofile <- ifelse(p!="ref",
                                paste("../jaarreeksen/neerslag/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version,scaling=scaling),sep=""),
                                paste("../jaarreeksen/neerslag/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v="v1.0",scaling=""),sep=""))

                schrijf.header.tabel(ofile,header,X)
              }
            }

            prod="sum"
            if(products[prod]==1) {
              X     <- aggregate(rr[id,-1],by=list(idy),  sum)
              stat  <- prod

              ofile <- ifelse(p!="ref",
                              paste("../jaarreeksen/neerslag/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version,scaling=scaling),sep=""),
                              paste("../jaarreeksen/neerslag/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v="v1.0",scaling=""),sep=""))

              schrijf.header.tabel(ofile,header,X)
            }
          } # end seasonal variables

        }   # scaling
      }     # if(( (p == "2030" | p == "ref") & sc == "") | (p != "2030" & p != "ref" & sc != ""))
    }       # scenario
  }         # period
}           # version




# nolint end

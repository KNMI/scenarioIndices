# nolint start
setwd("/usr/people/bakker/KNMI14/transformatie/temperatuur")
rm(list=ls(all=TRUE))

source("../station_transform_functions.R")
version   <- "v1.0_T25" #"v1.0"
scenarios <- c("","GL","GH","WL","WH")
periodes  <- c("ref","2030","2050","2085")
range="19810101-20101231"
yrange=paste(substring(range,1,4),substring(range,10,13),sep="-")

# variabelen in tabel (=1)
# variabelen per seizoen (ijs, strenge vorst, vorst, warme, zomer en tropische dagen, warme nachten, koudste en warmste dag, sd)
sproducts <- data.frame("N_ijs"   =1,
                        "N_str.v" =1,
                        "N_vorst" =1,
                        "N_warm"  =1,
                        "N_zomer" =1,
                        "N_trop"  =1,
                        "N_wn"    =1,
                        "tg.av"   =1,
                        "tn.av"   =1,
                        "tx.av"   =1,
                        "tg.mn"   =1,
                        "tg.mx"   =1)

# variabelen per jaar (graaddagen, zero-crossings, hittegolg ! nog niet uitgewerkt)
yproducts <- data.frame("d.days"  =0,
                        "zero.cr" =0,
                        "heatwave"=0)

for(p in periodes) {
  for(sc in scenarios) {
    if(( (p == "2030" | p == "ref") & sc == "") | (p != "2030" & p != "ref" & sc != "")) {
      print(c(sc,p,paste("version:",version)))

      # read data
      var="tg"; tg <- read.table(paste("../tijdreeksen/",file.name(sc=sc,p=p,var=var,range=range,v=version),sep=""))
      var="tn"; tn <- read.table(paste("../tijdreeksen/",file.name(sc=sc,p=p,var=var,range=range,v=version),sep=""))
      var="tx"; tx <- read.table(paste("../tijdreeksen/",file.name(sc=sc,p=p,var=var,range=range,v=version),sep=""))

      h.ids        <- which(tg[,1]==0)
      header       <- tg[h.ids,]; header[,1] <- "00000000"
      names(tg)    <- c("date",round(tg[1,-1],0))
      names(tn)    <- names(tg)
      names(tx)    <- names(tg)
      tg           <- tg[-h.ids,]
      tn           <- tn[-h.ids,]
      tx           <- tx[-h.ids,]

      mm <- (tg[,1]%/%100)%%100
      ss <- as.integer((mm/3)%%4+1)
      yy <-  tg[,1]%/%10000
      wy <- ifelse(mm<12,yy,yy+1)

      # afgeleiden per seizoen
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

        prod="N_ijs" # aantal ijsdagen
        if(sproducts[prod]==1) {
          X     <- aggregate(tx[id,-1] < 0,by=list(idy),  sum)
          var   <- "tx"
          stat  <- "N_ijsdagen"
          ofile <- paste("../jaarreeksen/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version),sep="")

          schrijf.header.tabel(ofile,header,X)
        }

        prod="N_str.v" # dagen met strenge vorst
        if(sproducts[prod]==1) {
          X     <- aggregate(tn[id,-1] < -10,by=list(idy),  sum)
          var   <- "tn"
          stat  <- "N_strenge.vorst"
          ofile <- paste("../jaarreeksen/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version),sep="")

          schrijf.header.tabel(ofile,header,X)
        }

        prod="N_vorst" # vorstdagen
        if(sproducts[prod]==1) {
          X     <- aggregate(tn[id,-1] < 0,by=list(idy),  sum)
          var   <- "tn"
          stat  <- "N_vorstdagen"
          ofile <- paste("../jaarreeksen/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version),sep="")

          schrijf.header.tabel(ofile,header,X)
        }

        prod="N_warm" # warme dagen
        if(sproducts[prod]==1) {
          X     <- aggregate(tx[id,-1] >= 20,by=list(idy),  sum)
          var   <- "tx"
          stat  <- "N_warme.dagen"
          ofile <- paste("../jaarreeksen/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version),sep="")

          schrijf.header.tabel(ofile,header,X)
        }

        prod="N_zomer" # zomerse dagen
        if(sproducts[prod]==1) {
          X     <- aggregate(tx[id,-1] >= 25,by=list(idy),  sum)
          var   <- "tx"
          stat  <- "N_zomerse.dagen"
          ofile <- paste("../jaarreeksen/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version),sep="")

          schrijf.header.tabel(ofile,header,X)
        }

        prod="N_trop" # tropische dagen
        if(sproducts[prod]==1) {
          X     <- aggregate(tx[id,-1] >= 30,by=list(idy),  sum)
          var   <- "tx"
          stat  <- "N_tropische.dagen"
          ofile <- paste("../jaarreeksen/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version),sep="")

          schrijf.header.tabel(ofile,header,X)
        }

        prod="N_wn" # warme nachten
        if(sproducts[prod]==1) {
          X     <- aggregate(tn[id,-1] >= 20,by=list(idy),  sum)
          var   <- "tn"
          stat  <- "N_warme.nachten"
          ofile <- paste("../jaarreeksen/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version),sep="")

          schrijf.header.tabel(ofile,header,X)
        }

        prod="tg.av" # gemiddelde temperatuur per seizoen
        if(sproducts[prod]==1) {
          X     <- aggregate(tg[id,-1],by=list(idy),  mean)
          var   <- "tg"
          stat  <- "mean"
          ofile <- paste("../jaarreeksen/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version),sep="")

          schrijf.header.tabel(ofile,header,X)
        }

        prod="tn.av" # gemiddelde minimumtemperatuur per seizoen
        if(sproducts[prod]==1) {
          X     <- aggregate(tn[id,-1],by=list(idy),  mean)
          var   <- "tn"
          stat  <- "mean"
          ofile <- paste("../jaarreeksen/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version),sep="")

          schrijf.header.tabel(ofile,header,X)
        }

        prod="tx.av" # gemiddelde maximumtemperatuur per seizoen
        if(sproducts[prod]==1) {
          X     <- aggregate(tx[id,-1],by=list(idy),  mean)
          var   <- "tx"
          stat  <- "mean"
          ofile <- paste("../jaarreeksen/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version),sep="")

          schrijf.header.tabel(ofile,header,X)
        }

        prod="tg.mn" # gemiddelde minimale dagtemperatuur per seizoen
        if(sproducts[prod]==1) {
          X     <- aggregate(tg[id,-1],by=list(idy),  min)
          var   <- "tg"
          stat  <- "min"
          ofile <- paste("../jaarreeksen/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version),sep="")

          schrijf.header.tabel(ofile,header,X)
        }

        prod="tg.mx" # gemiddelde maximale dagtemperatuur per seizoen
        if(sproducts[prod]==1) {
          X     <- aggregate(tg[id,-1],by=list(idy),  max)
          var   <- "tg"
          stat  <- "max"
          ofile <- paste("../jaarreeksen/",annual.file(sc=sc,p=p,var=var,range=yrange,s.name=season.name,stat=stat,v=version),sep="")

          schrijf.header.tabel(ofile,header,X)
        }

      } # end seasonal variables

    } # if
  }   # sc
}     # period



# nolint end

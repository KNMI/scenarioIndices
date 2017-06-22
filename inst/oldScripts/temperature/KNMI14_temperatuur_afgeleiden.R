# nolint start
#'CP: change path
#setwd("/usr/people/bakker/KNMI14/transformatie/temperatuur")

setwd("/nobackup/users/photiado/KNMI14_scenarios_2/transformatie/temperatuur")
rm(list=ls(all=TRUE))

#'CP: added version_out
source("../station_transform_functions.R")
version   <- "v1.0_T25" #"v1.0"
#version_out   <- "v2.0_T25" #"v1.0"

scenarios <- c("","GL","GH","WL","WH")
periodes  <- c("ref","2030","2050","2085")
range="19810101-20101231"

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
                        "tg.mx"   =1,
                        "tg.g.sd" =1)

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
      tabel <- as.data.frame(matrix(NA,5*sum(sproducts),ncol(tg)))
      names(tabel) <- names(tg)

      i=0

      for(season in 0:4) {
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
          i=i+1
          name <- paste(prod,season,sep="")
          tabel[i, 1] <- paste(name,substr("        ",1,8-nchar(name)))
          tabel[i,-1] <- apply(tx[id,-1]<0,2,sum,na.rm=T) / length(unique(yy))
        }

        prod="N_str.v" # dagen met strenge vorst
        if(sproducts[prod]==1) {
          i=i+1
          name <- paste(prod,season,sep="")
          tabel[i, 1] <- paste(name,substr("        ",1,8-nchar(name)))
          tabel[i,-1] <- apply(tn[id,-1]<(-10),2,sum,na.rm=T) / length(unique(yy))
        }

        prod="N_vorst" # vorstdagen
        if(sproducts[prod]==1) {
          i=i+1
          name <- paste(prod,season,sep="")
          tabel[i, 1] <- paste(name,substr("        ",1,8-nchar(name)))
          tabel[i,-1] <- apply(tn[id,-1]<0,2,sum,na.rm=T) / length(unique(yy))
        }

        prod="N_warm" # warme dagen
        if(sproducts[prod]==1) {
          i=i+1
          name <- paste(prod,season,sep="")
          tabel[i, 1] <- paste(name,substr("        ",1,8-nchar(name)))
          tabel[i,-1] <- apply(tx[id,-1]>=20,2,sum,na.rm=T) / length(unique(yy))
        }

        prod="N_zomer" # zomerse dagen
        if(sproducts[prod]==1) {
          i=i+1
          name <- paste(prod,season,sep="")
          tabel[i, 1] <- paste(name,substr("        ",1,8-nchar(name)))
          tabel[i,-1] <- apply(tx[id,-1]>=25,2,sum,na.rm=T) / length(unique(yy))
        }

        prod="N_trop" # tropische dagen
        if(sproducts[prod]==1) {
          i=i+1
          name <- paste(prod,season,sep="")
          tabel[i, 1] <- paste(name,substr("        ",1,8-nchar(name)))
          tabel[i,-1] <- apply(tx[id,-1]>=30,2,sum,na.rm=T) / length(unique(yy))
        }

        prod="N_wn" # warme nachten
        if(sproducts[prod]==1) {
          i=i+1
          name <- paste(prod,season,sep="")
          tabel[i, 1] <- paste(name,substr("        ",1,8-nchar(name)))
          tabel[i,-1] <- apply(tn[id,-1]>=20,2,sum,na.rm=T) / length(unique(yy))
        }

        prod="tg.av" # gemiddelde temperatuur per seizoen
        if(sproducts[prod]==1) {
          i=i+1
          name <- paste(prod,season,sep="")
          tabel[i, 1] <- paste(name,substr("        ",1,8-nchar(name)))
          X           <- aggregate(tg[id,-1],by=list(idy),mean,na.rm=T)
          tabel[i,-1] <- apply(X[,-1]       , 2          ,mean,na.rm=T)
        }

        prod="tn.av" # gemiddelde minimumtemperatuur per seizoen
        if(sproducts[prod]==1) {
          i=i+1
          name <- paste(prod,season,sep="")
          tabel[i, 1] <- paste(name,substr("        ",1,8-nchar(name)))
          X           <- aggregate(tn[id,-1],by=list(idy),mean,na.rm=T)
          tabel[i,-1] <- apply(X[,-1]       , 2          ,mean,na.rm=T)
        }

        prod="tx.av" # gemiddelde maximumtemperatuur per seizoen
        if(sproducts[prod]==1) {
          i=i+1
          name <- paste(prod,season,sep="")
          tabel[i, 1] <- paste(name,substr("        ",1,8-nchar(name)))
          X           <- aggregate(tx[id,-1],by=list(idy),mean,na.rm=T)
          tabel[i,-1] <- apply(X[,-1]       , 2          ,mean,na.rm=T)
        }

        prod="tg.mn" # gemiddelde minimale dagtemperatuur per seizoen
        if(sproducts[prod]==1) {
          i=i+1
          name <- paste(prod,season,sep="")
          tabel[i, 1] <- paste(name,substr("        ",1,8-nchar(name)))
          X           <- aggregate(tg[id,-1],by=list(idy), min,na.rm=T)
          tabel[i,-1] <- apply(X[,-1]       , 2          ,mean,na.rm=T)
        }

        prod="tg.mx" # gemiddelde maximale dagtemperatuur per seizoen
        if(sproducts[prod]==1) {
          i=i+1
          name <- paste(prod,season,sep="")
          tabel[i, 1] <- paste(name,substr("        ",1,8-nchar(name)))
          X           <- aggregate(tg[id,-1],by=list(idy), max,na.rm=T)
          tabel[i,-1] <- apply(X[,-1]       , 2          ,mean,na.rm=T)
        }

        prod="tg.g.sd" # standaard deviatie seizoensgemiddelde temperatuur
        if(sproducts[prod]==1) {
          i=i+1
          name <- paste(prod,season,sep="")
          tabel[i, 1] <- paste(name,substr("        ",1,8-nchar(name)))
          X           <- aggregate(tg[id,-1],by=list(idy), mean,na.rm=T)
          tabel[i,-1] <- apply(X[,-1]       , 2          ,   sd,na.rm=T)
        }
      } # end seasonal variables

      #'CP: changed v=version to v=version_out
      # schrijf weg naar ASCII bestand
      ofile <- paste("../klimatologie/klimaat_",file.name(sc=sc,p=p,var="temp",range=range,v=version),sep="")

      sink(ofile)
      # comments
      writeLines("# Climatologies derived from transformed daily temperature observations")
      writeLines("# Royal Netherlands Meteorological Institute (KNMI)")
      writeLines("# processed for the KNMI14 climate change scenarios")
      if(p == "ref") {
        writeLines("# reference data")
      } else {
        if(p != "2030") writeLines(paste("# scenario",sc))
        writeLines(paste("# time horizon",p))
        writeLines(paste("# transformation = ",version))
      }

      # header
      write.table(format(header[ 1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
      write.table(format(header[-1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
      # transformed data
      write.table(format(tabel,width=10,digits=2,justify="right"),row.names=F,col.names=F,quote=F)
      sink()
    } # if(p == "MOC" | p == "EOC" | sc == "G") {
  }   # sc
}     # period

# nolint end

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
      
      tabel <- as.data.frame(matrix(NA,5*(length(drempels)+sum(products)),ncol(ev)))
      names(tabel) <- names(ev)
      
      i=0
      
      # seasonal variables
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

        if(length(drempels>0)) {
          for(j in 1:length(drempels)) {
            i=i+1
            dname <- paste("N_",drempels[j],"mm",season,sep="")
            tabel[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
            tabel[i,-1] <- apply(ev[id,-1]>=drempels[j],2,sum) / length(unique(yy))
          }
        }
                
        prod="sum"   # sd seizoenssom EV
        if(products[prod]==1) {
          dname=paste(prod,season,sep="")
          i=i+1
          tabel[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
          #X           <- aggregate(ev[id,-1],by=list(idy),  sum)
          #tabel[i,-1] <- apply(X[,-1]       , 2          ,   sd)
          tabel[i,-1]  <- apply(ev[id,-1],2,sum)/30
        }  
      } # end seasonal variables
      
      # schrijf weg naar ASCII bestand
      ofile <- paste("../klimatologie/klimaat_",file.name(sc=sc,p=p,var=var,range=range,v="v1.0"),sep="")
      
      
      sink(ofile)    
      # comments
      writeLines("# Climatologies derived from transformed Makkink crop reference evaporation")
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
  



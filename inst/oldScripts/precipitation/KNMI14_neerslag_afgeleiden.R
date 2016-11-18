#'CP: change path
#setwd("/usr/people/bakker/KNMI14/transformatie/neerslag")

setwd("/nobackup/users/photiado/nobackup_1/KNMI14_scenarios/transformatie/neerslag")
rm(list=ls(all=TRUE))

source("../station_transform_functions.R")

var="rr"

#versions  <- c("v0.1","v0.2")
versions  <- c("v1.1","v1.2")
scenarios <- c("","GL","GH","WL","WH")
periodes  <- c("ref","2030","2050","2085")
range="19810101-20101231"

products <- data.frame("sd.Psum"=1,"Psum"=1)

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
          
          tabel <- as.data.frame(matrix(NA,5*(length(drempels)+sum(products)),ncol(rr)))
          names(tabel) <- names(rr)
          
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
            
            if(length(drempels)>0) {
              for(j in 1:length(drempels)) {
                i=i+1
                dname <- paste("N_",drempels[j],"mm",season,sep="")
                tabel[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
                tabel[i,-1] <- apply(rr[id,-1]>=drempels[j],2,sum) / length(unique(yy))
              }
            }
            
            prod="Psum"   # mean seizoenssom RR
            if(products[prod]==1) {
              dname=paste(prod,season,sep="")
              i=i+1
              tabel[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
              X           <- aggregate(rr[id,-1],by=list(idy),  sum)
              tabel[i,-1] <- apply(X[,-1]       , 2          , mean)
            }  

            prod="sd.Psum"   # sd seizoenssom RR
            if(products[prod]==1) {
              dname=paste(prod,season,sep="")
              i=i+1
              tabel[i, 1] <- paste(dname,substr("        ",1,8-nchar(dname)))
              X           <- aggregate(rr[id,-1],by=list(idy),  sum)
              tabel[i,-1] <- apply(X[,-1]       , 2          ,   sd)
            }  
            
            } # end seasonal variables
          
          # schrijf weg naar ASCII bestand
          ofile <- ifelse(p!="ref",
                          paste("../klimatologie/klimaat_",file.name(sc=sc,p=p,var=var,range=range,v=version,scaling=scaling),sep=""),
                          paste("../klimatologie/klimaat_",file.name(sc=sc,p=p,var=var,range=range,v="v1.0",scaling=""),sep=""))
          
          sink(ofile)    
          # comments
          writeLines("# Climatologies derived from transformed daily (homogenised) precipitation observations")
          writeLines("# Royal Netherlands Meteorological Institute (KNMI)")
          writeLines("# processed for the KNMI14 climate change scenarios")
          if(p == "ref") {
            writeLines("# reference data")
          } else {
            if(p != "2030") writeLines(paste("# scenario",sc))
            writeLines(paste("# time horizon",p))
            writeLines(paste("# wet day scaling",scaling))
            writeLines(paste("# transformation = ",version))
          }
          
          # header
          write.table(format(header[ 1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
          write.table(format(header[-1,],width=10,justify="right"),row.names=F,col.names=F,quote=F)
          # transformed data
          write.table(format(tabel,width=10,digits=2,justify="right"),row.names=F,col.names=F,quote=F)
          sink()
        
          }   # scaling
      }     # if(( (p == "2030" | p == "ref") & sc == "") | (p != "2030" & p != "ref" & sc != ""))
    }       # scenario
  }         # period
}           # version            
            
              



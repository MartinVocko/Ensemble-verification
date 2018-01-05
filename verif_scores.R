#### nacteni dat ####
setwd("~/Plocha/DATA/tab_RDS")
v=readRDS("vertab.RDS")
##### Korelace ######
#meatoda A
dta=v
obl=unique(as.character(v$ID))
tab=list()
tab2=data.frame()
for (ii in obl)
{ 
  dt=dta[ID==ii]
#dta=dta[AHEAD==54]
#ens=dt[,!c( "TIME","ID","EVE", "tps", "tps_all","ORIGIN","AHEAD", "PR"),with=FALSE]
pred=dt$ALADIN
obs=dt$PR
x=cor(pred,obs)
#Corr(ens,obs)
tab=rbind(tab,CORR)

}

tab2=data.table(cbind(tab,obl))
rankobl=rename(rankobl,c("V1"="CORA"))
rankobl=cbind(rankobl,tab)
#tab3 <- data.frame(lapply(tab2, as.character), stringsAsFactors=FALSE)
write.xls(tab3, file='CORR_obl.xls')
          
#metoda B
ob=as.vector(v[["PR"]])
en=data.matrix(ens$ALADIN, rownames.force=NA)
EnsCorr(en,ob )
en=data.matrix(v[,9:28,with=FALSE])
#### Korelace tps ####
ens=v[,!c( "TIME","AHEAD","EVE", "tps_all","ORIGIN"),with=FALSE]
TPS=unique(as.character(c("Bp","B","C","SWc2", "Ec", "NEc")))
obl=unique(as.character(v$ID))
tab=data.table()
tab2=data.table()
for (i in obl)
{ 
  en=ens[ID== i]
  for (jj in TPS)
  { 
    e=en[tps== jj]
    
    
    CORR=data.table(cor(e$`1`,e$PR),cor(e$`2`,e$PR),cor(e$`3`,e$PR),cor(e$`4`,e$PR),cor(e$`5`,e$PR),cor(e$`6`,e$PR),cor(e$`7`,e$PR),cor(e$`8`,e$PR),cor(e$`9`,e$PR),cor(e$`10`,e$PR),cor(e$`11`,e$PR),cor(e$`12`,e$PR),cor(e$`13`,e$PR),cor(e$`14`,e$PR),cor(e$`15`,e$PR),cor(e$`16`,e$PR),cor(e$`MEAN`,e$PR),cor(e$`MEDIAN`,e$PR),cor(e$`CF`,e$PR),cor(e$`ALADIN`,e$PR))
    
    tab=cbind(CORR, TPS= jj,ID=i)#AHEAD= i
    tab2=rbind(tab2, tab)
  }
}
tab2=rename(tab2,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12","V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))
saveRDS (tab2,"CORR_tps_obl")

#### BIAS ######
bin=readRDS("binary_1")
ahead=c(6,12,18,24,30,36,42,48,54)
dat=list()
obl=unique(as.character(bin$ID))
for(j in ahead){ 
  
  b=bin[AHEAD==j]
  
  for (i in obl){
    b=bin[ID==i]
    ct=data.table(table.stats(b$PR,b$X1),table.stats(b$PR,b$X2),table.stats(b$PR,b$X3),table.stats(b$PR,b$X4),table.stats(b$PR,b$X5), table.stats(b$PR,b$X6),table.stats(b$PR,b$X7),table.stats(b$PR,b$X8),table.stats(b$PR,b$X9),table.stats(b$PR,b$X10),table.stats(b$PR,b$X11),table.stats(b$PR,b$X12),table.stats(b$PR,b$X13),table.stats(b$PR,b$X14),table.stats(b$PR,b$X15),table.stats(b$PR,b$X16),table.stats(b$PR,b$MEAN),table.stats(b$PR,b$MEDIAN),table.stats(b$PR,b$CF),table.stats(b$PR,b$ALADIN))
    
    ct=ct[18,]
    ct=cbind(ct, OBL=i)
    ct=cbind(ct, AHEAD=j)
    
    dat=rbind(dat,ct)
  }
}

saveRDS(dat,"BIAS1")
##### Percent Bias
pbias(ens$MEAN,obs)

e=v[AHEAD==54]
PB=data.table(pbias(e$`1`,e$PR),pbias(e$`2`,e$PR),pbias(e$`3`,e$PR),pbias(e$`4`,e$PR),pbias(e$`5`,e$PR),pbias(e$`6`,e$PR),pbias(e$`7`,e$PR),pbias(e$`8`,e$PR),pbias(e$`9`,e$PR),pbias(e$`10`,e$PR),pbias(e$`11`,e$PR),pbias(e$`12`,e$PR),pbias(e$`13`,e$PR),pbias(e$`14`,e$PR),pbias(e$`15`,e$PR),pbias(e$`16`,e$PR),pbias(e$`MEAN`,e$PR),pbias(e$`MEDIAN`,e$PR),pbias(e$`CF`,e$PR),pbias(e$`ALADIN`,e$PR))

#### ME ####
ens=v[,!c( "TIME","EVE", "tps", "tps_all","ORIGIN"),with=FALSE]
ahead=c(6,12,18,24,30,36,42,48,54)
obl=unique(as.character(v$ID))
tab=data.table()
tab2=data.table()
for (jj in obl)
{ 
  en=ens[ID==jj] #pozor na zamenu e x en
  for (i in ahead)
  { 
e=en[AHEAD==i]
ME=data.table(me(e$`1`,e$PR),me(e$`2`,e$PR),me(e$`3`,e$PR),me(e$`4`,e$PR),me(e$`5`,e$PR),me(e$`6`,e$PR),me(e$`7`,e$PR),me(e$`8`,e$PR),me(e$`9`,e$PR),me(e$`10`,e$PR),me(e$`11`,e$PR),me(e$`12`,e$PR),me(e$`13`,e$PR),me(e$`14`,e$PR),me(e$`15`,e$PR),me(e$`16`,e$PR),me(e$`MEAN`,e$PR),me(e$`MEDIAN`,e$PR),me(e$`CF`,e$PR),me(e$`ALADIN`,e$PR))

tab=cbind(ME,AHEAD= i,obl=jj)
tab2=rbind(tab2, tab)
  }
  
}

#dat_avg=tab2[,.(th_mean=mean(tab2[,1:20,with=FALSE])),by=.(obl)] #nejde/ubrat cyklus
write.xls(tab2, file='ME_obl.xls')

##### MAE ######

ens=v[,!c( "TIME","EVE", "tps_all","ORIGIN"),with=FALSE]
ahead=c(6,12,18,24,30,36,42,48,54)
obl=unique(as.character(v$ID))
tab=data.table()
tab2=data.table()
for (jj in obl)
{ 
  en=ens[ID==jj] 
      for (i in TPS)
      { 
 e=en[tps==i]
 #e=ens[AHEAD==i]
MAE=data.table(mae(e$`1`,e$PR),mae(e$`2`,e$PR),mae(e$`3`,e$PR),mae(e$`4`,e$PR),mae(e$`5`,e$PR),mae(e$`6`,e$PR),mae(e$`7`,e$PR),mae(e$`8`,e$PR),mae(e$`9`,e$PR),mae(e$`10`,e$PR),mae(e$`11`,e$PR),mae(e$`12`,e$PR),mae(e$`13`,e$PR),mae(e$`14`,e$PR),mae(e$`15`,e$PR),mae(e$`16`,e$PR),mae(e$`MEAN`,e$PR),mae(e$`MEDIAN`,e$PR),mae(e$`CF`,e$PR),mae(e$`ALADIN`,e$PR))

MAE=data.table(SCORE="MAE",ALADIN=mae(e$`ALADIN`,e$PR),MEAN=mae(e$`MEAN`,e$PR))

tab=cbind(MAE, TPS=i)#,AHEAD= i)
tab2=rbind(tab2, tab)
      }
}
saveRDS(tab2,file="MAE_obl")
write.xls(tab2, file='MAE_obl.xls')
tab2=rename(tab2,c("V1"="MEAN","V2"="ALADIN"))
MAE=melt(tab2)
####RMSE vypise pouze 1 hodnotu pro cely ensemble
ens=v[,!c( "TIME","ID","PR","EVE", "tps", "tps_all", "AHEAD","ORIGIN", "CF", "ALADIN","MEAN","MEDIAN"),with=FALSE]
e=data.matrix(ens, rownames.force=NA)
obs=v[,!c("TIME", "ID","EVE","tps","tps_all", "AHEAD","ORIGIN","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","CF","ALADIN","MEAN","MEDIAN"),with=FALSE]
ob=as.vector(obs[["PR"]])
#vypise pouze 1 hodnotu pro cely ensemble
rmse=EnsRmse(en,ob)

######RMSE ##########
ens=v[,!c( "TIME","AHEAD","EVE", "tps_all","ORIGIN"),with=FALSE]
TPS=unique(as.character(c("Bp","B","C","SWc2", "Ec", "NEc")))
obl=unique(as.character(v$ID))
#ahead=c(6,12,18,24,30,36,42,48,54)
tab=data.table()
tab2=data.table()
for (i in obl)
{ 
en=ens[ID== i]
  for (jj in TPS)
  { 
    e=en[tps== jj]
    

RMSE=data.table(rmse(e$`1`,e$PR),rmse(e$`2`,e$PR),rmse(e$`3`,e$PR),rmse(e$`4`,e$PR),rmse(e$`5`,e$PR),rmse(e$`6`,e$PR),rmse(e$`7`,e$PR),rmse(e$`8`,e$PR),rmse(e$`9`,e$PR),rmse(e$`10`,e$PR),rmse(e$`11`,e$PR),rmse(e$`12`,e$PR),rmse(e$`13`,e$PR),rmse(e$`14`,e$PR),rmse(e$`15`,e$PR),rmse(e$`16`,e$PR),rmse(e$`MEAN`,e$PR),rmse(e$`MEDIAN`,e$PR),rmse(e$`CF`,e$PR),rmse(e$`ALADIN`,e$PR))

tab=cbind(RMSE, TPS= jj,ID=i)#AHEAD= i
tab2=rbind(tab2, tab)
}
}
tab2=rename(tab2,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12","V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))
saveRDS (tab2,"RMSE_tps_obl")
tab3=tab2[!is.na(V1),]
tab3=tab2[order(TPS)]

x=v$tps
table(x)

write.xls(tab2,file= 'RMSE_tps.xls')
#x=factor(c(en$ID))
#en=en[,OBL:=revalue(x, c("r"="1","s"="2","m"="3","n"="4","h"="5","j"="6","l"="7","o"="8","t"="9","q"="10","u"="11","g"="12","a"="13","b"="14","f"="15","p"="16","k"="17","i"="18","e"="19","d"="20","c"="21","N"="22","G"="23","O"="24","I"="25","P"="26","J"="27","K"="28","H"="29","F"="30","E"="31","D"="32","M"="33","L"="34","A"="35","B"="36","C"="37"))]
h=54  #AHEAD
dat=data.table()
#en=v[AHEAD==h]
en=v
obl=unique(as.character(en$ID))
for (i in obl){
  e=en[ID==i]
 # RMSE=list(rmse(e$`1`,e$PR),rmse(e$`2`,e$PR),rmse(e$`3`,e$PR),rmse(e$`4`,e$PR),rmse(e$`5`,e$PR),rmse(e$`6`,e$PR),rmse(e$`7`,e$PR),rmse(e$`8`,e$PR),rmse(e$`9`,e$PR),rmse(e$`10`,e$PR),rmse(e$`11`,e$PR),rmse(e$`12`,e$PR),rmse(e$`13`,e$PR),rmse(e$`14`,e$PR),rmse(e$`15`,e$PR),rmse(e$`16`,e$PR),rmse(e$`MEAN`,e$PR),rmse(e$`MEDIAN`,e$PR),rmse(e$`CF`,e$PR),rmse(e$`ALADIN`,e$PR))
  
RMSE=data.table(mae(e$`MEAN`,e$PR),mae(e$`ALADIN`,e$PR))
 
# RMSE=data.table(rmse(e$`MEAN`,e$PR),rmse(e$`ALADIN`,e$PR)) 
dat=rbind(dat,RMSE) 
}
dat=cbind(dat, OBL=obl)
dat=cbind(dat, AHEAD=h)
saveRDS(dat,"RMSE_avrg")

dat=rename(dat,c("V1"="MAEmean","V2"="MAEaladin"))
rankobl=cbind(rankobl,dat)
saveRDS(dt,"RMSE_polygons")

#Brier score
#vypise hodnotu skore pro kazdy radek, tresholdy se musi pocitat postupne
ens=v[,!c( "TIME","ID","EVE", "tps", "tps_all","ORIGIN", "CF", "ALADIN","MEAN", "MEDIAN"),with=FALSE]
en=ens[AHEAD==6]
ob=as.vector(en[["PR"]])
en=en[,!c("AHEAD","PR"),with=FALSE]
e=data.matrix(en, rownames.force=NA)
bs=EnsBrier(e,ob,tau=0.5)
mean(bs)

##### urceni tresholdu ######
tab=v
tab=data.frame(tab)
tab=tab[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,2)]
tab=data.table(tab)
#tab[,above:=as.integer(tab[,9,with=FALSE]>0.1)]
for (i in 8:28){
  tab[,above:=as.integer(tab[,i,with=FALSE]>20)]
  tab[,i:=above,with=FALSE]
}
tab$above=NULL
saveRDS(tab,"binary_20")

###### Urceni contingency table
bin=readRDS("binary_5")
ahead=c(6,12,18,24,30,36,42,48,54)
dat=data.table()
obl=unique(as.character(bin$ID))
for(j in ahead){ 

bi=bin[AHEAD==j]

for (i in obl){
  b=bin[ID==i]
#ct=data.table(table.stats(b$PR,b$X1),table.stats(b$PR,b$X2),table.stats(b$PR,b$X3),table.stats(b$PR,b$X4),table.stats(b$PR,b$X5), table.stats(b$PR,b$X6),table.stats(b$PR,b$X7),table.stats(b$PR,b$X8),table.stats(b$PR,b$X9),table.stats(b$PR,b$X10),table.stats(b$PR,b$X11),table.stats(b$PR,b$X12),table.stats(b$PR,b$X13),table.stats(b$PR,b$X14),table.stats(b$PR,b$X15),table.stats(b$PR,b$X16),table.stats(b$PR,b$MEAN),table.stats(b$PR,b$MEDIAN),table.stats(b$PR,b$CF),table.stats(b$PR,b$ALADIN))
ct=data.table(table.stats(b$PR,b$MEAN)) #, table.stats(b$PR,b$ALADIN))
ct=ct[2,]
#ct=cbind(ct, OBL=i)
#ct=cbind(ct, AHEAD=j)

dat=rbind(dat,ct)
}


}

saveRDS(dat,"ETSpol_1")

##### SCORES TABLE #####
e1=readRDS("binary_1")
e1=e1[,TH:=1]
e2=readRDS("binary_2.5")
e2=e2[,TH:=2.5]
e3=readRDS("binary_5")
e3=e3[,TH:=5]
e4=readRDS("binary_10")
e4=e4[,TH:=10]
binar=rbind(e1,e2,e3,e4)

dat=data.table()
ahead=c(6,12,18,24,30,36,42,48,54)
obl=unique(as.character(binar$ID))
ens=unique(as.character(names(binar[,8:27])))
#for (gg in ws){ 
#bin= binar[tps==gg]
dt=data.table()
    for (ii in ahead){
      b=binar[AHEAD==ii]
           ct=data.table(table.stats(b$PR,b$ALADIN))
           ct=ct[7,]
     # ctm=data.table(table.stats(b$PR,b$MEAN))
     # ctm=ctm[7,]
     # dt=cbind(ct,ctm)
     # dat=cbind(dat,dt)
     dt=rbind(dt,ct) 
     #TS=cbind(TS,dat)
     #TS=cbind(dat,TPS= ws ,MODEL="ALADIN",AHEAD= ahead)
      }
      
 dat=cbind(dat,AHEAD=ahead) 
}

eds[TS == 0] <- NaN
POD=rename(POD,c("V1"="value", "MODEL"="variable"))
POD=rbind(POD,TS)
POD$value=as.numeric(POD$value)
POD$variable=as.factor(POD$variable)
saveRDS(POD,"POD_tps6ahead")
POD=data.table(readRDS("POD_tps6"))
#}
      
rankobl=cbind(rankobl, dat)
######################### verify #######################
b=readRDS("binary_1")
#b=b[AHEAD==6]
obs=(b$PR)
obs=as.vector(obs)
pred=((b[,10:25,with=FALSE]))
pred=data.matrix(pred, rownames.force=NA)
ref=(b$ALADIN)
ref=as.vector(ref)

#obs=(v[,2, with=FALSE])
#pred=(v[,11:26,with=FALSE])
ver=verify(obs,pred,frcst.type = "binary",obs.type = "binary" ,baseline=NULL, p=NULL)
ver=verify(obs,ens,baseline=ens.ref, p=NULL)
summary(ver)
#A=verify(ob, e,frcst.type="cont",obs.type = "cont",baseline=NULL,p=NULL,treshold=1)
#summary(A)


####### Brier skill score - oblasti #######
ahead=c(6,12,18,24,30,36,42,48,54)
th=c(1,2.5,5,10)
obl=unique(as.character(v$ID))
tps=unique(as.character(c("Bp","B","C","SWc2", "Ec", "NEc")))
dat=data.table()
#for (h in obl)
# { 
 # v1=v[ID==h]
 # for (i in th) #pro pripad bez oblasti zacit zde
 #{ 
   for (xx in tps)
    {
     vx=v[tps== xx]
    
   for(jj in ahead)
    { 
     
     v2=vx[AHEAD==jj]  # v2=v1[AHEAD==j]
    ens=v2 [,!c( "TIME","ID","EVE", "tps_all","ORIGIN", "ALADIN","MEAN", "MEDIAN","CF","tps"),with=FALSE]
    ens=ens[,!c("AHEAD","PR"),with=FALSE]
   # e=v2$MEAN
    ens=as.matrix(ens, rownames.force=NA)
    
    obs=as.vector(v2[["PR"]])
    ens.ref=as.matrix(v2[["ALADIN"]])

dt=SkillScore(EnsBrier(ens,obs),EnsBrier(ens.ref,obs),handle.na = "na.fail")
#ct=EnsBrierSs(e,ens.ref,ob,tau = i)
   if (nrow(ens) == 0) next
dt=EnsBrierSs(ens,ens.ref,obs,tau=NA)
    val=mean(EnsBrier(e,ob))

#ct=ct[1]
#ct=data.table(cbind(value,TH=(1), ID=jj, TPS= (xx), variable= ("ALADIN"), score="BS"))#,obl=h)
ct=data.table(cbind(dt, variable= ("ALADIN")))
dat=rbind(dat,ct)
}
  }
    }
dat
dt=dat
dt=rbind(dt,dat)
dt$variable=as.factor(dt$variable)
z[,1]=as.numeric(z[,1])
saveRDS(dt,"BS_tps6")
#dat[gsub('bss','',dat)]
dat_avg=aggregate(ct~AHEAD+obl, dat, mean)

ddt = data.table(ct = unlist(dat[, 'ct']), AHEAD = unlist(dat[, 'AHEAD']), TH = unlist(dat[, 'TH'])) #,obl = unlist(dat[, 'obl']) )
dat_avg=ddt[,.(th_mean=mean(ct)),by=.(obl,AHEAD)]

saveRDS(dat_avg,"BSS_obl")
datavg=data.frame(dat_avg)
datavg=dcast(datavg,AHEAD~obl,fill=0) #transponuje tabulku
write.xls(datavg,file= 'BSS_OBL.xls')
write.xls(ddt,file= 'xx.xls')
  
ens=(v[,9,with=FALSE])
ens.ref=ens+2

#### BSS v.2 #####
bin=readRDS("binary_1")
bin2=readRDS("binary_2.5")
bin5=readRDS("binary_5")
bin10=readRDS("binary_10")
bi=data.table(rbind(bin,bin2,bin5,bin10))

ahead=c(6,12,18,24,30,36,42,48,54)
tab=data.table()
for (i in ahead)
{ 
  b=bi[AHEAD== i]
ens=as.matrix(b[,10:25])
obs=as.vector(as.numeric(b$PR))
obs1=as.numeric(b$PR)
enref=as.matrix(b$ALADIN)
enref=cbind(enref,enref)

ss=data.table(ALADIN=SkillScore(EnsBrier(ens,obs),EnsBrier(enref,obs1)))
tab=rbind(tab,ss[1,])
}
dt=cbind(dt,tab)
dtt=cbind(dt, AHEAD=ahead)
saveRDS(dtt,file="BSS_ahead")
########  RPSS  #########
### v.1
bin=readRDS("binary_1")
bin2=readRDS("binary_2.5")
bin5=readRDS("binary_5")
bin10=readRDS("binary_10")
bin$P1 <- rowMeans(subset(bin, select = c(10:25)), na.rm = TRUE)
bin2$P2 <- rowMeans(subset(bin2, select = c(10:25)), na.rm = TRUE)
bin5$P5 <- rowMeans(subset(bin5, select = c(10:25)), na.rm = TRUE)
bin10$P10 <- rowMeans(subset(bin10, select = c(10:25)), na.rm = TRUE)
pdftab=data.frame(bin$P1,bin2$P2,bin5$P5,bin10$P10,bin$ALADIN,bin2$ALADIN,bin5$ALADIN,bin10$ALADIN,bin$PR,bin2$PR,bin5$PR,bin10$PR)
mata=pdftab[,9:12]
mata=abs(mata-1)

dt=list()
b=brier(bin10$PR,bin10$ALADIN)
b=b$bs
dt=rbind(dt,b)
############# Urceni prsti ####################

#pro ensemble
 
#ens=v[,c(11:26,28)]


ahead=c(6,12,18,24,30,36,42,48,54)
obl=unique(as.character(v$ID))

tab=list()
tab2=list()
tab3=list()
#for (j in obl)
#{ 
 # vo=v[ID==j]

for (i in ahead)
  { 
  va=v[AHEAD== i]
ens=va[,!c( "TIME","PR","ID", "EVE", "tps", "tps_all", "AHEAD","ORIGIN", "ALADIN","MEAN","MEDIAN","CF"),with=FALSE]  #obsahuje CF
ens[, id:=1:.N]
mens = melt(ens, id.vars = 'id')
mens[value<0, value := 0]
mens[, cls := cut(value, breaks = c(0, 1, 2.5, 5, 10, Inf), include.lowest = TRUE)]
res = mens[, .(prst = .N/16), by = .(id, cls)]
res = dcast.data.table(res, id ~ cls)
res[is.na(res)] <- 0
res$id <- NULL
#res=matrix(res)

# pro referencni predpoved

ref=va$ALADIN
ref=data.table(ref)
ref[, id:=1:.N]
mref=melt(ref, id.vars='id')
mref[value<0, value:=0]
mref[,cls:= cut(value, breaks = c (0,1,2.5,5,10,Inf), include.lowest=TRUE)]
pref=mref[,.(prst= .N/1), by = .(id,cls)]
pref=dcast.data.table(pref, id~cls)
pref[is.na(pref)] <- 0
pref$id=NULL

#pro merena data
#obstab=data.table(bin$PR,bin2$PR,bin5$PR,bin10$PR)
#obstab=obstab[,CAT:= (V1+V2+V3+V4+1)]
#pobs=as.vector(obstab$CAT)

obs=va$PR
obs=data.table(obs)
obs[, id:=1:.N]
mobs=melt(obs, id.vars='id')
mobs[value<0, value:=0]
mobs[,cls:= cut (value, breaks = c(0,1,2.5,5,10,Inf),include.lowest= TRUE)]
pobs=mobs[, .(prst = .N/1), by = .(id, cls)] 
pobs = dcast.data.table(pobs, id ~ cls)
pobs[is.na(pobs)] <- 0
pobs$id <- NULL

RPS <- mean( apply( ( res - pobs)^2,1, sum)  )/ ( ncol(res) -1 )
RPS.climo <- mean( apply( ( pref - pobs)^2,1, sum)  )/ ( ncol(pref) -1 )
RPSS <- 1 - RPS/RPS.climo
#RPSS=EnsRpss(res,pref,pobs)
#RPS=rps(pobs, res, nrow=16938)

#tab=cbind(RPS, AHEAD=i,obl=j)

tab=rbind(tab,RPSS)
#tab2=cbind(tab,AHEAD=ahead)
#tab2=rbind(tab2,RPS.climo)
}

}
tab2=cbind(tab2,tab)
tab3 = data.table( AHEAD = unlist(tab2[, 'AHEAD']), RPS = unlist(tab2[, 'RPS']), obl = unlist(tab2[, 'obl']) )
tab3 = data.table( AHEAD = unlist(tab2[, 'AHEAD']),MEAN=unlist(tab2[,'MEAN']),MEDIAN=unlist(tab2[,'MEDIAN']), CF=unlist(tab2[,'CF']), ALADIN=unlist(tab2[,'ALADIN']))
write.xls(tab3,file= 'RPS_OBL.xls')

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

matb=pdftab[,1:8]
matb=1-matb
pdftab=data.frame(matb,mata)
ens=pdftab[,1:4]
ens.ref=pdftab[,5:8]
obs=pdftab[,9:12]
A=EnsRpss(ens,ens.ref,obs)

### v.2
ahead=c(6,12,18,24,30,36,42,48,54)
bin=readRDS("binary_1")
bin2=readRDS("binary_2.5")
dat=list()
for (i in ahead){
  #b=bin
  b=bin[AHEAD==i]
  obs=(b$PR)
  obs=as.vector(obs)
  ens=((b[,10:25,with=FALSE]))
  ens=data.matrix(ens, rownames.force=NA)
  ens.ref=(b$ALADIN)
  ens.ref=as.vector(ens.ref)
  A=rps(obs,pred=ens,baseline = ens.ref)
A= EnsRpss(ens, ens.ref, obs)
A=A$rpss
A=cbind(A, AHEAD=i)
dat=rbind(dat,A)
}
dat

# Reliability diagram ######

#nacteni matic prsti z kapitoly Urceni prsti
#par(mfrow=c(2,2))
layout(matrix(1:4,ncol=2))
ReliabilityDiagram(res$`[0,1]`,pobs$`[0,1]`,plot=TRUE, attributes=TRUE)
ReliabilityDiagram(res$`(1,2.5]`,pobs$`(1,2.5]`,plot=TRUE,attributes=TRUE)
ReliabilityDiagram(res$`(2.5,5]`,pobs$`(2.5,5]`,plot=TRUE,attributes=TRUE)
ReliabilityDiagram(res$`(5,10]`,pobs$`(5,10]`,plot=TRUE,attributes=TRUE)
ReliabilityDiagram(res$`(10,Inf]`,pobs$`(10,Inf]`,plot=TRUE,attributes=TRUE)



# ROC #######
bin=readRDS("binary_1")
bin2=readRDS("binary_2.5")
bin5=readRDS("binary_5")
bin10=readRDS("binary_10")
x=bin[,10:25]
pred=bin$PR
par(mfrow=c(3,2))
roc.plot(pobs$`[0,1]`,res$`[0,1]`,main="Interval 0-1 mm")
roc.plot(pobs$`(1,2.5]`,res$`(1,2.5]`,main="Interval 1-2.5 mm")
roc.plot(pobs$`(2.5,5]`,res$`(2.5,5]`,main="Interval 2.5-5 mm")
roc.plot(pobs$`(5,10]`,res$`(5,10]`,main="Interval 5-10 mm")
roc.plot(pobs$`(10,Inf]`,res$`(10,Inf]`,main="Interval 10-Inf mm")

roc.plot(pobs,res)

#### Rank histogram ####
rh=Rankhist(ens,obs)
rh
PlotRankhist(rh,mode="raw")
#PlotRankhist(rh,mode="prob.paper")
TestRankhist(rh)
# verze 5 ruznych histogramu
v1=v[PR<1]
v2=v[PR<2.5]
v5=v[PR<5]
v10=v[PR<10]
vi=v[PR>10]
par(mfrow(c=2,1))
verifRankHist(v1[,11:26,with=FALSE],v1$PR)
verifRankHist(v2[,11:26,with=FALSE],v2$PR)
verifRankHist(v5[,11:26,with=FALSE],v5$PR)
verifRankHist(v10[,11:26,with=FALSE],v10$PR)
verifRankHist(vi[,11:26,with=FALSE],vi$PR)

## verze vse v 1 histogramu  ##

v1=v[PR<1]
v2=v[PR<2.5]
v5=v[PR<5]
v10=v[PR<10]
vi=v[PR>10]


observations=v1$PR
forecasts=v1[,11:26,with=F]
rank1 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])

observations=v2$PR
forecasts=v2[,11:26,with=F] 
rank2 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])
observations=v5$PR
forecasts=v5[,11:26,with=F] 
rank5 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])
observations=v10$PR
forecasts=v10[,11:26,with=F] 
rank10 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                    ties = "random")[1])
observations=vi$PR
forecasts=vi[,11:26,with=F] 
ranki <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])
l=list(rank1, rank2, rank5, rank10, ranki)

multhist(l,freq=F,breaks = seq(0,17,by=1),names.arg=rep("",17), prob = TRUE, xaxt = "n",col=c( "#252525", "#636363", "#969696", "#CCCCCC", "#F7F7F7"),main="Rank Histogram")
axis(1, at = seq(3.2, to = 104 + 0.5, by = 6), labels = 1:(16 + 1))
abline(h = 1/(k + 1), lty = 2)
legend("topleft", c("  < 1 mm"," < 2 mm","  < 5 mm","  < 10 mm","  > 10 mm"), fill=c( "#252525", "#636363", "#969696", "#CCCCCC", "#F7F7F7") )

gsub(' ', ',', paste(c(rev(brewer.pal(7, 'Greys')) [2:6]), collapse = ' '))
pocet_barev <- 5
barvicka <- unlist(strsplit(gsub(' ', ', ', paste(c(rev(brewer.pal(pocet_barev, 'Greys'))), collapse = ' ')), split = ' '))
barvicka

## v tehle variante se grafy prekryvaji (add=TRUE)
verifRankHist<- function (forecasts, observations) 
{
  rank <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                    ties = "random")[1])
  k <- ncol(forecasts)
  hist(rank, breaks = 0:(k + 1), prob = TRUE, xaxt = "n", xlab = "", 
       ylab = "", col="blue", ylim=c(0,0.8), main = "Rank Histogram Decomposition")
  axis(1, at = seq(0.5, to = k + 0.5, by = 1), labels = 1:(k + 
                                                             1))
  abline(h = 1/(k + 1), lty = 2)
  invisible(rank)
}


histrank<-function (forecasts, observations) 
{
  rank <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                    ties = "random")[1])
  k <- ncol(forecasts)
  hist(rank, breaks = 0:(k + 1), prob = TRUE,xlab = "", 
       ylab = "", col="green", main = "",add=T)
  axis(1, at = seq(0.5, to = k + 0.5, by = 1), labels = 1:(k + 
                                                             1))
  abline(h = 1/(k + 1), lty = 2)
  invisible(rank)
}

v1=v[PR<1]
v2=v[PR<2.5]
v5=v[PR<5]
v10=v[PR<10]
vi=v[PR>10]
par(mfrow(c=2,1))
verifRankHist(v1[,11:26,with=FALSE],v1$PR)
histrank(v2[,11:26,with=FALSE],v2$PR)
histrank(v5[,11:26,with=FALSE],v5$PR)
histrank(v10[,11:26,with=FALSE],v10$PR)
histrank(vi[,11:26,with=FALSE],vi$PR)


## # # #
Rankhist(vt[,11:26,with=FALSE],vt$PR)
rh=as.vector(rh)
rh=data.table(rh)

g=ggplot(data=rh, aes(x= y=rh))
g=g+ ylab("COUNT")+xlab("WEATHER SITUATIONS")+ggtitle("WEATHER SITUATIONS")
g=g + scale_fill_brewer(palette = "Set1")
g+ geom_bar(stat='identity',fill='darkblue')

##### Tabulka prehled ######
v=readRDS("vertab.RDS")
v[v<0]=0   #odstraneni zapornych hodnot
obl=unique(as.character(v$ID))
eve=unique(as.numeric(v$EVE))

#tab=v[v$AHEAD<13]
#tab=tab[,!c("tps","tps_all","AHEAD","ORIGIN"),with=FALSE]
#tab=data.frame(tab)
#tab=tab[,c(1,3:4,2,7:22,5:6,24,23)]
#tab=data.table(tab)

tab=list()
for (i in obl)
  { 
  u=v[ID== i]
  
  for (j in eve)
    {
    uu=u[EVE ==j]
    
    #tabu=uu[, .(sumO = sum(PR), sumP = sum(MEAN)), by = .(ID, EVE, ORIGIN)]
    muu = melt(uu, id.vars = c('ID', 'PR', 'TIME', 'EVE', 'tps', 'tps_all', 'AHEAD', 'ORIGIN'))
   t= muu[, .(sumO = sum(PR), sumP = sum(value), maxO = max(PR), maxP = max(value), maxtimeO = which.max(PR), maxtimeP = which.max(value)), by = .(ID, EVE, ORIGIN, variable)] #max intenz, 
tab=rbind(tab,t)
    
  }
}


tabl=transform(tab,diffsum = sumO-sumP) #rozdil mezi pozorovanymi a merenymi sumami
tabl=transform(tabl,diffmax = maxO-maxP) #rozdil mezi maximy
tabl=transform(tabl,difftime = maxtimeO-maxtimeP)#rozdil mezi casy vyskytu max int
tabl=transform(tabl,timeratio=maxtimeO/maxtimeP) #pomer mezi casy vyskytu max int
View(tabl)

table=tabl[,.(RMSE=sqrt(mean((sumO-sumP)^2))) , by = .(ID, EVE, variable)]  # RMSE
setkey(tabl,ID,EVE,ORIGIN,variable)
setkey(table,ID, EVE,variable)
tabulka=merge(table,tabl,all=TRUE)
table=tabl[,.(RMSEmax=sqrt(mean((maxO-maxP)^2))) , by = .(ID, EVE, variable)]
setkey(tabulka,ID,EVE,ORIGIN,variable)
setkey(table,ID, EVE,variable)
tabulka=merge(table,tabulka,all=TRUE)
View(tabulka)

tc=tabulka[,.(COR=cor(sumO,sumP)), by=.(ID, EVE, variable)]    # CORRELATION
setkey(tabulka,ID,EVE,ORIGIN,variable)
setkey(tc,ID, EVE,variable)
tabulka=merge(tc,tabulka,all=TRUE)
tc=tabulka[,.(CORmax=cor(maxO,maxP)), by=.(ID, EVE, variable)]
setkey(tabulka,ID,EVE,ORIGIN,variable)
setkey(tc,ID, EVE,variable)
tabulka=merge(tc,tabulka,all=TRUE)

setcolorder(tabulka,c("ID","EVE","variable","ORIGIN","sumO","sumP","maxO","maxP","diffsum","diffmax","maxtimeO","maxtimeP","difftime","timeratio","RMSE","RMSEmax","COR","CORmax"))

#v=rename(v,c("X1"="E1","X2"="E2","X3"="E3","X4"="E4","X5"="E5","X6"="E6","X7"="E7","X8"="E8","X9"="E9","X10"="E10","X11"="E11","X12"="E12","X13"="E13","X14"="E14","X15"="E15","X16"="E16"))
saveRDS(tabulka,file='EVENT_summary')

#tabl=transform(tabl,RMSE=sqrt(mean((sumO-sumP)^2)))

setwd("~/Plocha/DATA/tab_RDS")
tabulka=readRDS('EVENT_summary')


data=ty[variable %in% c("MEAN","ALADIN")]
t1=v[,.(MEAN=sqrt(mean((PR-MEAN)^2))) , by = .( ORIGIN)]
t2=v[,.(ALADIN=sqrt(mean((PR-ALADIN)^2))) , by = .( ORIGIN)]
ty=cbind(t1,t2$ALADIN)
ty=rename(ty,c("V2"="ALADIN")
melt(ty,id.vars = "ORIGIN")


tab=melt(tab,id.vars="TIME")
tab1=tab[c(1:24, 153:168)]
tab2=tab[25:152,]



ggplot(data=tab1,aes(x=factor(TIME),y=value, group=variable))+
  geom_line(data=tab2,aes(x=factor(TIME),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(0.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("mm")+xlab("TIME")+ggtitle("EVENT")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)
 
####  PS Prumerny Uhrn  ####
#prehled udalosti uhrn/doba trvani/cas pocatku


dta=readRDS("dtaival")

dta = dta[, .(VYB = any(VYB), PR = sum(PR), TIME = DTM[1] - 3600, EVE=EVE[!is.na(EVE)][1], tps = tps[1], tps_all = paste(unique(tps), collapse = '-')), by = .(IVAL, ID)]
dta[!is.na(EVE), EID:=paste(ID, EVE, sep = '_')]
dta=dta[VYB=='TRUE']
dta=dta[,HOUR:=6]



d=dta[ID=='P']
suma=dta[,.(PR=sum(PR),DURATION=sum(HOUR), TIMEstart=TIME[1],EVE=EVE[1],tps=tps[1],ID=ID[1]), by=.(EID)]


TPS=unique(as.character(v$tps))
tab=list()
for (i in TPS)
{
    sum=suma[tps==i]
    rf=sum[,.(MEANRAIN=mean(PR), tps=tps[1], MEANDURATION=mean(DURATION),COUNT=length(EID))]
   
    tab= rbind(tab,rf)
}
rainmean=tab[!is.na(tps)]
saveRDS(rainmean, file = 'TPSrain')
write.xls(rainmean,'TPSrain.xls')

#### RANK POLYGONS ####

### definice vertab kategorii dle uhrnu
vc=v
vc$CAT<-findInterval(vc$PR, c(0, 1, 2.5, 5, 10), rightmost.closed = TRUE)

### Vypocet score do tabulky

# CORR
dta=vc[CAT %in% c("5")]
obl=unique(as.character(v$ID))
tab=list()
tab2=data.frame()
for (ii in obl)
{ 
  dt=dta[ID==ii]
  #dta=dta[AHEAD==54]
  #ens=dt[,!c( "TIME","ID","EVE", "tps", "tps_all","ORIGIN","AHEAD", "PR"),with=FALSE]
  pred=dt$MEAN
  obs=dt$PR
  CORR=data.table(CORRm=cor(pred,obs))
  #Corr(ens,obs)
  tab=rbind(tab,CORR)
  
}

tab2=data.table(cbind(tab,obl))
robl10=tab2
robl10=cbind(robl10,tab)

#RMSE a MAE
dat=data.table()
for (i in obl){
  e=dta[ID==i]
  
   #RMSE=data.table(MAEm=mae(e$`MEAN`,e$PR),MAEa=mae(e$`ALADIN`,e$PR))
  RMSE=data.table(RMSEm=rmse(e$`MEAN`,e$PR),RMSEa=rmse(e$`ALADIN`,e$PR)) 
   
  dat=rbind(dat,RMSE) 
}

robl10=cbind(robl10,dat)

####  POD
binar=readRDS("binary_10")

dat=data.table()
for (ii in obl){
  b=binar[ID==ii]
  ct=data.table( PODa = table.stats(b$PR,b$ALADIN))
  ct=ct[4,]
  ctm=data.table( PODm = table.stats(b$PR,b$MEAN))
  ctm=ctm[4,]
  dt=cbind(ct,ctm)
  dat=rbind(dat,dt)
}

robl10=cbind(robl10, dat)

# TS
dat=data.table()
for (ii in obl){
  b=binar[ID==ii]
  ct=data.table( TSa = table.stats(b$PR,b$ALADIN))
  ct=ct[2,]
  ctm=data.table( TSm = table.stats(b$PR,b$MEAN))
  ctm=ctm[2,]
  dt=cbind(ct,ctm)
  dat=rbind(dat,dt)
}

robl10=cbind(robl10, dat)

saveRDS(robl10, 'rankobl10')

## Rankovani
r=readRDS('rankobl10')
r=data.frame(r)
r <- as.data.frame(lapply(r, unlist))

nam=unique(as.character(names(r)))

#for (i in nam)
{ 
x=r$TSa
rx=rank(-x)
r$rank_TSa<-rx


}
r=r[c(2,1,3,4,5,6,7,8,9,10,11,13,15,17,19,21,12,14,16,18,20)]
r=r[c(1:11,12,14,16,18,20,13,15,17,19,21)]
output <- data.frame( RANKmean = apply(r[,12:16], 1, sum) ,RANKaladin = apply(r[,17:21], 1, sum) ) #soucet ranku
r=cbind(r,output)

saveRDS(r,"rankobl10")

#### Def predpovedi pred a po pocatku eventu ####
eve=unique(as.numeric(v$EVE))
etab=data.table()
for (i in eve){ 
ev=v[EVE== i]
t=min(ev$TIME)
ev=ev[ ,tdif:=(t-ORIGIN)]
ev=ev[ ,tindex:=ifelse(ev$tdif >=0, "pre", "aft")]
ev=ev[,tdif:=NULL]
etab=rbind(etab,ev)
}

####  RMSE
tim=unique(as.character(etab$tindex))

tab=data.table()
tab2=data.table()
for (i in tim)
{ 
  et=etab[tindex== i]
  
    for (j in ahead )
    { 
      e=et[AHEAD== j]
  
  
  RMSE=data.table("1"=rmse(e$`1`,e$PR),"2"=rmse(e$`2`,e$PR),"3"=rmse(e$`3`,e$PR),"4"=rmse(e$`4`,e$PR),"5"=rmse(e$`5`,e$PR),"6"=rmse(e$`6`,e$PR),"7"=rmse(e$`7`,e$PR),"8"=rmse(e$`8`,e$PR),"9"=rmse(e$`9`,e$PR),"10"=rmse(e$`10`,e$PR),"11"=rmse(e$`11`,e$PR),"12"=rmse(e$`12`,e$PR),"13"=rmse(e$`13`,e$PR),"14"=rmse(e$`14`,e$PR),"15"=rmse(e$`15`,e$PR),"16"=rmse(e$`16`,e$PR),"MEAN"=rmse(e$`MEAN`,e$PR),"MEDIAN"=rmse(e$`MEDIAN`,e$PR),"CF"=rmse(e$`CF`,e$PR),"ALADIN"=rmse(e$`ALADIN`,e$PR),score="RMSE")
  
  tab=cbind(RMSE, tindex= i, AHEAD= as.factor(j))
  tab2=rbind(tab2, tab)
    }
}
tab3=melt(tab2)


## Korelace
ahead=c(6,12,18,24,30,36,42,48,54)
tab4=data.table()
for (i in tim)
{ 
  et=etab[tindex== i]
  
    for ( j in ahead)
    { 
      e=et[AHEAD== j]
      
pred=e[,9:28,with=FALSE]
obs=e$PR
x=data.table(value =cor(pred,obs),tindex= i,score="CORR",variable=colnames(pred), AHEAD= as.factor(j))
tab4=rbind(tab4,x)
    }
}
tab4=melt(tab4)
tab4=tab4[,-5]

### MAE 
tab=data.table()
tab2=data.table()
for (i in tim)
{ 
  et=etab[tindex== i]
     for (j in ahead )
  { 
    e=et[AHEAD== j]
  
  MAE=data.table("1"=mae(e$`1`,e$PR),"2"=mae(e$`2`,e$PR),"3"=mae(e$`3`,e$PR),"4"=mae(e$`4`,e$PR),"5"=mae(e$`5`,e$PR),"6"=mae(e$`6`,e$PR),"7"=mae(e$`7`,e$PR),"8"=mae(e$`8`,e$PR),"9"=mae(e$`9`,e$PR),"10"=mae(e$`10`,e$PR),"11"=mae(e$`11`,e$PR),"12"=mae(e$`12`,e$PR),"13"=mae(e$`13`,e$PR),"14"=mae(e$`14`,e$PR),"15"=mae(e$`15`,e$PR),"16"=mae(e$`16`,e$PR),"MEAN"=mae(e$`MEAN`,e$PR),"MEDIAN"=mae(e$`MEDIAN`,e$PR),"CF"=mae(e$`CF`,e$PR),"ALADIN"=mae(e$`ALADIN`,e$PR),score="MAE")
  
  tab=cbind(MAE, tindex= i,AHEAD= as.factor(j))
  tab2=rbind(tab2, tab)
     }
}
tab5=melt(tab2)

# POD  a  TS  
eve=unique(as.numeric(binar$EVE))
btab=data.table()
for (i in eve){ 
  ev=binar[EVE== i]
  t=min(ev$TIME)
  ev=ev[ ,tdif:=(t-ORIGIN)]
  ev=ev[ ,tindex:=ifelse(ev$tdif >=0, "pre", "aft")]
  ev=ev[,tdif:=NULL]
  btab=rbind(btab,ev)
}


#nacteni dat z tabulek 
tab6=data.table()
for (i in tim){ 
  bt= btab[tindex== i]
  for (j in ahead){
    b=bt[AHEAD==j]
    ct=data.table("1"=table.stats(b$PR,b$X1), "2"=table.stats(b$PR,b$X2),"3"=table.stats(b$PR,b$X3),"4"=table.stats(b$PR,b$X4),"5"=table.stats(b$PR,b$X5),"6"=table.stats(b$PR,b$X6),"7"=table.stats(b$PR,b$X7),"8"=table.stats(b$PR,b$X8),"9"=table.stats(b$PR,b$X9),"10"=table.stats(b$PR,b$X10),"11"=table.stats(b$PR,b$X11),"12"=table.stats(b$PR,b$X12),"13"=table.stats(b$PR,b$X13),"14"=table.stats(b$PR,b$X14),"15"=table.stats(b$PR,b$X15),"16"=table.stats(b$PR,b$X16),"MEAN"=table.stats(b$PR,b$MEAN),"MEDIAN"=table.stats(b$PR,b$MEDIAN),"CF"=table.stats(b$PR,b$CF),"ALADIN"=table.stats(b$PR,b$ALADIN),score= "TS",tindex= i,AHEAD=as.factor(j))
    dt=ct[2,]
    tab6=rbind(tab6,dt)
  
  }
}
tab6$'1'=as.numeric(tab6$'1')
tab6$'2'=as.numeric(tab6$'2')
tab6$'3'=as.numeric(tab6$'3')
tab6$'4'=as.numeric(tab6$'4')
tab6$'5'=as.numeric(tab6$'5')
tab6$'6'=as.numeric(tab6$'6')
tab6$'7'=as.numeric(tab6$'7')
tab6$'8'=as.numeric(tab6$'8')
tab6$'9'=as.numeric(tab6$'9')
tab6$'10'=as.numeric(tab6$'10')
tab6$'11'=as.numeric(tab6$'11')
tab6$'12'=as.numeric(tab6$'12')
tab6$'13'=as.numeric(tab6$'13')
tab6$'14'=as.numeric(tab6$'14')
tab6$'15'=as.numeric(tab6$'15')
tab6$'16'=as.numeric(tab6$'16')
tab6$'MEAN'=as.numeric(tab6$'MEAN')
tab6$'MEDIAN'=as.numeric(tab6$'MEDIAN')
tab6$'CF'=as.numeric(tab6$'CF')
tab6$'ALADIN'=as.numeric(tab6$'ALADIN')

tab6=melt(tab6)

tab7=data.table()
for (i in tim){ 
  bt= btab[tindex== i]
  for (j in ahead){
    b=bt[AHEAD==j]
  ct=data.table("1"=table.stats(b$PR,b$X1), "2"=table.stats(b$PR,b$X2),"3"=table.stats(b$PR,b$X3),"4"=table.stats(b$PR,b$X4),"5"=table.stats(b$PR,b$X5),"6"=table.stats(b$PR,b$X6),"7"=table.stats(b$PR,b$X7),"8"=table.stats(b$PR,b$X8),"9"=table.stats(b$PR,b$X9),"10"=table.stats(b$PR,b$X10),"11"=table.stats(b$PR,b$X11),"12"=table.stats(b$PR,b$X12),"13"=table.stats(b$PR,b$X13),"14"=table.stats(b$PR,b$X14),"15"=table.stats(b$PR,b$X15),"16"=table.stats(b$PR,b$X16),"MEAN"=table.stats(b$PR,b$MEAN),"MEDIAN"=table.stats(b$PR,b$MEDIAN),"CF"=table.stats(b$PR,b$CF),"ALADIN"=table.stats(b$PR,b$ALADIN),score= "POD",tindex= i, AHEAD=as.factor(j))
  dt=(ct[4,])

  tab7=rbind(tab7,dt)
}
} 
  tab7$'1'=as.numeric(tab7$'1')
  tab7$'2'=as.numeric(tab7$'2')
  tab7$'3'=as.numeric(tab7$'3')
  tab7$'4'=as.numeric(tab7$'4')
  tab7$'5'=as.numeric(tab7$'5')
  tab7$'6'=as.numeric(tab7$'6')
  tab7$'7'=as.numeric(tab7$'7')
  tab7$'8'=as.numeric(tab7$'8')
  tab7$'9'=as.numeric(tab7$'9')
  tab7$'10'=as.numeric(tab7$'10')
  tab7$'11'=as.numeric(tab7$'11')
  tab7$'12'=as.numeric(tab7$'12')
  tab7$'13'=as.numeric(tab7$'13')
  tab7$'14'=as.numeric(tab7$'14')
  tab7$'15'=as.numeric(tab7$'15')
  tab7$'16'=as.numeric(tab7$'16')
  tab7$'MEAN'=as.numeric(tab7$'MEAN')
  tab7$'MEDIAN'=as.numeric(tab7$'MEDIAN')
  tab7$'CF'=as.numeric(tab7$'CF')
  tab7$'ALADIN'=as.numeric(tab7$'ALADIN')

tab7=melt(tab7)

dtindex=rbind(tab4,tab3,tab5,tab6,tab7)


### charaktristiky udalosti####
obl=unique (as.character(v$ID))
e=data.table()
ev=data.table()

for (i in obl){
  vv=v[ID == i]
  x=unique(vv$EVE)
  e=cbind(x,ID=i)
  ev=rbind(e,x)
  
}

  
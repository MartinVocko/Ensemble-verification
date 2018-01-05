setwd("~/Plocha/DATA/cosmotabs")
vc=readRDS('vertabcos18')
v=na.omit(vc)

#### RMSE ALADIN ####
ahead=c(6,12,18)
tab=data.table()
tab2=data.table()
for (i in ahead)
    { 
  ens=v[AHEAD== i]
  
  RMSE=data.table(rmse(ens$`1`,ens$PR),rmse(ens$`2`,ens$PR),rmse(ens$`3`,ens$PR),rmse(ens$`4`,ens$PR),rmse(ens$`5`,ens$PR),rmse(ens$`6`,ens$PR),
                  rmse(ens$`7`,ens$PR),rmse(ens$`8`,ens$PR),rmse(ens$`9`,ens$PR),rmse(ens$`10`,ens$PR),rmse(ens$`11`,ens$PR),rmse(ens$`12`,ens$PR),
                  rmse(ens$`13`,ens$PR),rmse(ens$`14`,ens$PR),rmse(ens$`15`,ens$PR),rmse(ens$`16`,ens$PR),rmse(ens$`MEAN`,ens$PR),rmse(ens$`MEDIAN`,ens$PR),
                  rmse(ens$`CF`,ens$PR),rmse(ens$`ALADIN`,ens$PR))
  
  tab=cbind(RMSE, AHEAD= i)
  tab2=rbind(tab2,tab)
}
tab2=rename(tab2,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12",
                   "V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))


#### RMSE COSMO ####

ahead=c(6,12,18)
tab=data.table()
tab3=data.table()
for (i in ahead)
{ 
  ens=v[AHEADcosmo== i]
  
  RMSE=data.table(rmse(ens$`pf01`,ens$PR),rmse(ens$`pf02`,ens$PR),rmse(ens$`pf03`,ens$PR),rmse(ens$`pf04`,ens$PR),rmse(ens$`pf05`,ens$PR),rmse(ens$`pf06`,ens$PR),
                  rmse(ens$`pf07`,ens$PR),rmse(ens$`pf08`,ens$PR),rmse(ens$`pf09`,ens$PR),rmse(ens$`pf10`,ens$PR),rmse(ens$`pf11`,ens$PR),rmse(ens$`pf12`,ens$PR),
                  rmse(ens$`pf13`,ens$PR),rmse(ens$`pf14`,ens$PR),rmse(ens$`pf15`,ens$PR),rmse(ens$`pf16`,ens$PR),rmse(ens$`MEANcosmo`,ens$PR),rmse(ens$`MEDIANcosmo`,ens$PR))
      
  
  tab=cbind(RMSE, AHEADcosmo= i)
  tab3=rbind(tab3,tab)
}

tab3=rename(tab3,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12",
                   "V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEAN","V18"="MEDIAN"))
#rm=data.table(read.xls("rmse_lin.xls"))
alme=melt(tab2,id.vars="AHEAD")
ensr=alme[49:51,]

cm=melt(tab3, id.vars="AHEADcosmo")
modr=cm[49:51,]


####line colour 

ggplot(data=ensr, aes(x=AHEAD, y=value, colour=variable)) +
  geom_line()+
  geom_line(data=modr,aes(x=AHEADcosmo,y=value,colour=variable))
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("RMSE")+xlab("HOURS")+ggtitle("RMSE")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')

  
  
  #### ME ALADIN ####
  en=v[,!c( "TIME","EVE", "tps", "tps_all","ORIGIN"),with=FALSE]
  ahead=c(6,12,18)
  obl=unique(as.character(v$ID))
  tab=data.table()
  tab2=data.table()
  #for (jj in obl)
 # { 
  #  en=ens[ID==jj] #pozor na zamenu ens x en
    for (i in ahead)
    { 
      ens=en[AHEAD==i]
      ME=data.table(me(ens$`1`,ens$PR),me(ens$`2`,ens$PR),me(ens$`3`,ens$PR),me(ens$`4`,ens$PR),me(ens$`5`,ens$PR),
                    me(ens$`6`,ens$PR),me(ens$`7`,ens$PR),me(ens$`8`,ens$PR),me(ens$`9`,ens$PR),me(ens$`10`,ens$PR),
                    me(ens$`11`,ens$PR),me(ens$`12`,ens$PR),me(ens$`13`,ens$PR),me(ens$`14`,ens$PR),me(ens$`15`,ens$PR),
                    me(ens$`16`,ens$PR),me(ens$`MEAN`,ens$PR),me(ens$`MEDIAN`,ens$PR),me(ens$`CF`,ens$PR),me(ens$`ALADIN`,ens$PR))
      
      tab=cbind(ME,AHEAD= i)#obl=jj
      tab2=rbind(tab2, tab)
    }  
  
  tab2=rename(tab2,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12",
                     "V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))
  
  
  #### ME COSMO ####
  en=v[,!c( "TIME","EVE", "tps", "tps_all","ORIGIN"),with=FALSE]
  ahead=c(6,12,18)
  obl=unique(as.character(v$ID))
  tab=data.table()
  tab3=data.table()
  #for (jj in obl)
  # { 
  #  en=ens[ID==jj] #pozor na zamenu ens x en
  for (i in ahead)
  { 
    ens=en[AHEADcosmo==i]
    ME=data.table(me(ens$`pf01`,ens$PR),me(ens$`pf02`,ens$PR),me(ens$`pf03`,ens$PR),me(ens$`pf04`,ens$PR),me(ens$`pf05`,ens$PR),me(ens$`pf06`,ens$PR),
                    me(ens$`pf07`,ens$PR),me(ens$`pf08`,ens$PR),me(ens$`pf09`,ens$PR),me(ens$`pf10`,ens$PR),me(ens$`pf11`,ens$PR),me(ens$`pf12`,ens$PR),
                    me(ens$`pf13`,ens$PR),me(ens$`pf14`,ens$PR),me(ens$`pf15`,ens$PR),me(ens$`pf16`,ens$PR),me(ens$`MEANcosmo`,ens$PR),me(ens$`MEDIANcosmo`,ens$PR))
    
    tab=cbind(ME,AHEADcosmo= i)#obl=jj
    tab3=rbind(tab3, tab)
  }  
  
  tab3=rename(tab3,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12",
                     "V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEANcosmo","V18"="MEDIANcosmo"))
  
  
  
  alme=melt(tab2,id.vars="AHEAD")
  ensr=alme[49:51,]
  
  cm=melt(tab3, id.vars="AHEADcosmo")
  modr=cm[49:51,]
  
  
  ####line colour 
  
  ggplot(data=ensr, aes(x=AHEAD, y=value, colour=variable)) +
    geom_line()+
    geom_line(data=modr,aes(x=AHEADcosmo,y=value,colour=variable))+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
    ylab("ME")+xlab("HOURS")+ggtitle("ME")+
    scale_colour_brewer(name = 'MODEL', palette = 'Set1')
  
 
  ####  MAE ALADIN #### 
  en=v[,!c( "TIME","EVE", "tps", "tps_all","ORIGIN"),with=FALSE]
  ahead=c(6,12,18)
  obl=unique(as.character(v$ID))
  tab=data.table()
  tab2=data.table()
  #for (jj in obl)
  # { 
  #  en=ens[ID==jj] #pozor na zamenu ens x en
  for (i in ahead)
  { 
    ens=en[AHEAD==i]
    MAE=data.table(mae(ens$`1`,ens$PR),mae(ens$`2`,ens$PR),mae(ens$`3`,ens$PR),mae(ens$`4`,ens$PR),mae(ens$`5`,ens$PR),
                  me(ens$`6`,ens$PR),me(ens$`7`,ens$PR),mae(ens$`8`,ens$PR),mae(ens$`9`,ens$PR),mae(ens$`10`,ens$PR),
                  mae(ens$`11`,ens$PR),mae(ens$`12`,ens$PR),mae(ens$`13`,ens$PR),mae(ens$`14`,ens$PR),mae(ens$`15`,ens$PR),
                  mae(ens$`16`,ens$PR),mae(ens$`MEAN`,ens$PR),mae(ens$`MEDIAN`,ens$PR),mae(ens$`CF`,ens$PR),mae(ens$`ALADIN`,ens$PR))
    
    tab=cbind(MAE,AHEAD= i)#obl=jj
    tab2=rbind(tab2, tab)
  }  
  
  tab2=rename(tab2,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12",
                     "V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))
  
  
  #### MAE COSMO ####
  en=v[,!c( "TIME","EVE", "tps", "tps_all","ORIGIN"),with=FALSE]
  ahead=c(6,12,18)
  obl=unique(as.character(v$ID))
  tab=data.table()
  tab3=data.table()
  #for (jj in obl)
  # { 
  #  en=ens[ID==jj] #pozor na zamenu ens x en
  for (i in ahead)
  { 
    ens=en[AHEADcosmo==i]
    MAE=data.table(mae(ens$`pf01`,ens$PR),mae(ens$`pf02`,ens$PR),mae(ens$`pf03`,ens$PR),mae(ens$`pf04`,ens$PR),mae(ens$`pf05`,ens$PR),mae(ens$`pf06`,ens$PR),
                  mae(ens$`pf07`,ens$PR),mae(ens$`pf08`,ens$PR),mae(ens$`pf09`,ens$PR),mae(ens$`pf10`,ens$PR),mae(ens$`pf11`,ens$PR),mae(ens$`pf12`,ens$PR),
                  mae(ens$`pf13`,ens$PR),mae(ens$`pf14`,ens$PR),mae(ens$`pf15`,ens$PR),mae(ens$`pf16`,ens$PR),mae(ens$`MEANcosmo`,ens$PR),mae(ens$`MEDIANcosmo`,ens$PR))
    
    tab=cbind(MAE,AHEADcosmo= i)#obl=jj
    tab3=rbind(tab3, tab)
  }  
  
  tab3=rename(tab3,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12",
                     "V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEANcosmo","V18"="MEDIANcosmo"))
  
  
  
  alme=melt(tab2,id.vars="AHEAD")
  ensr=alme[49:51,]
  
  cm=melt(tab3, id.vars="AHEADcosmo")
  modr=cm[49:51,]
  
  
  ####line colour 
  
  ggplot(data=ensr, aes(x=AHEAD, y=value, colour=variable)) +
    geom_line()+
    geom_line(data=modr,aes(x=AHEADcosmo,y=value,colour=variable))+
    theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
    ylab("MAE")+xlab("HOURS")+ggtitle("MAE")+
    scale_colour_brewer(name = 'MODEL', palette = 'Set1')
  
  
  
  ##### urceni tresholdu ######
  tab=v
  tab=data.frame(tab)
  tab=tab[,c(1,3:28,2,30:48,29)]
  tab=data.table(tab)
  #tab[,above:=as.integer(tab[,9,with=FALSE]>0.1)]
  for (i in 8:47){
    tab[,above:=as.integer(tab[,i,with=FALSE]>10)]
    tab[,i:=above,with=FALSE]
  }
  tab$above=NULL
  saveRDS(tab,"binarycos_10")
  
  
  #### BIAS ALADIN####
  bin=readRDS("binarycos_5")
  ahead=c(6,12,18)
  dat=list()
  obl=unique(as.character(bin$ID))
  for(j in ahead){ 
    
    b=bin[AHEAD==j]
    
   # for (i in obl){
     # b=bin[ID==i]
      ct=data.table(table.stats(b$PR,b$X1),table.stats(b$PR,b$X2),table.stats(b$PR,b$X3),table.stats(b$PR,b$X4),
                    table.stats(b$PR,b$X5), table.stats(b$PR,b$X6),table.stats(b$PR,b$X7),table.stats(b$PR,b$X8),
                    table.stats(b$PR,b$X9),table.stats(b$PR,b$X10),table.stats(b$PR,b$X11),table.stats(b$PR,b$X12),
                    table.stats(b$PR,b$X13),table.stats(b$PR,b$X14),table.stats(b$PR,b$X15),table.stats(b$PR,b$X16),
                    table.stats(b$PR,b$MEAN),table.stats(b$PR,b$MEDIAN),table.stats(b$PR,b$CF),table.stats(b$PR,b$ALADIN))
      
      ct=ct[18,]
     # ct=cbind(ct, OBL=i)
      ct=cbind(ct, AHEAD=j)
      
      dat=rbind(dat,ct)
    }
#  }
  dat=rename(dat,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12",
                     "V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))
#### BIAS COSMO ####
dat2=list()
for(j in ahead){ 
  
  b=bin[AHEAD==j]
  
  # for (i in obl){
  # b=bin[ID==i]
  ct2=data.table(table.stats(b$PR,b$pf01),table.stats(b$PR,b$pf02),table.stats(b$PR,b$pf03),table.stats(b$PR,b$pf04),
                table.stats(b$PR,b$pf05), table.stats(b$PR,b$pf06),table.stats(b$PR,b$pf07),table.stats(b$PR,b$pf08),
                table.stats(b$PR,b$pf09),table.stats(b$PR,b$pf10),table.stats(b$PR,b$pf11),table.stats(b$PR,b$pf12),
                table.stats(b$PR,b$pf13),table.stats(b$PR,b$pf14),table.stats(b$PR,b$pf15),table.stats(b$PR,b$pf16),
                table.stats(b$PR,b$MEANcosmo),table.stats(b$PR,b$MEDIANcosmo))
  
  ct2=ct2[18,]
  # ct=cbind(ct, OBL=i)
  ct2=cbind(ct2, AHEAD=j)
  
  dat2=rbind(dat2,ct2)
}
#}
  dat2=rename(dat2,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12",
                     "V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEANcosmo","V18"="MEDIANcosmo"))
  
  
  alme=melt(dat,id.vars="AHEAD")
  ensr=alme[49:51,]
  ala=alme[58:60,]
  
  cm=melt(dat2, id.vars="AHEAD")
  modr=cm[49:51,]
  
  dt=rbind(ensr,ala,modr)
  dt$value=unlist(dt$value)
  
  ####line colour 
  
  ggplot(data=dt,aes(x=factor(AHEAD),y=value, group=variable))+
    geom_line(aes(group=factor(variable),color=variable),size=2)+
   # geom_line(data=ens,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
    theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
    ylab("BIAS")+xlab("HOURS")+ggtitle("BIAS")+
    scale_colour_brewer(name = 'MODEL', palette = 'Set1') 
   
  
  #### BIAS avrg ####
  b1=readRDS('binarycos_1')
  b2=readRDS('binarycos_2.5')
  b3=readRDS('binarycos_5')
  b4=readRDS('binarycos_10')
  bin=rbind(b1,b2,b3,b4)
  
  for(j in ahead){ 
    
    b=bin[AHEAD==j]
    
    # for (i in obl){
    # b=bin[ID==i]
    ct2=data.table(table.stats(b$PR,b$pf01),table.stats(b$PR,b$pf02),table.stats(b$PR,b$pf03),table.stats(b$PR,b$pf04),
                   table.stats(b$PR,b$pf05), table.stats(b$PR,b$pf06),table.stats(b$PR,b$pf07),table.stats(b$PR,b$pf08),
                   table.stats(b$PR,b$pf09),table.stats(b$PR,b$pf10),table.stats(b$PR,b$pf11),table.stats(b$PR,b$pf12),
                   table.stats(b$PR,b$pf13),table.stats(b$PR,b$pf14),table.stats(b$PR,b$pf15),table.stats(b$PR,b$pf16),
                   table.stats(b$PR,b$MEANcosmo),table.stats(b$PR,b$MEDIANcosmo))
    
    ct2=ct2[18,]
    # ct=cbind(ct, OBL=i)
    ct2=cbind(ct2, AHEAD=j)
    
    dat2=rbind(dat2,ct2)
  }
  dat=rename(dat,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12",
                   "V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))
  
  dat2=list()
  for(j in ahead){ 
    
    b=bin[AHEAD==j]
    
    # for (i in obl){
    # b=bin[ID==i]
    ct2=data.table(table.stats(b$PR,b$pf01),table.stats(b$PR,b$pf02),table.stats(b$PR,b$pf03),table.stats(b$PR,b$pf04),
                   table.stats(b$PR,b$pf05), table.stats(b$PR,b$pf06),table.stats(b$PR,b$pf07),table.stats(b$PR,b$pf08),
                   table.stats(b$PR,b$pf09),table.stats(b$PR,b$pf10),table.stats(b$PR,b$pf11),table.stats(b$PR,b$pf12),
                   table.stats(b$PR,b$pf13),table.stats(b$PR,b$pf14),table.stats(b$PR,b$pf15),table.stats(b$PR,b$pf16),
                   table.stats(b$PR,b$MEANcosmo),table.stats(b$PR,b$MEDIANcosmo))
    
    ct2=ct2[18,]
    # ct=cbind(ct, OBL=i)
    ct2=cbind(ct2, AHEAD=j)
    
    dat2=rbind(dat2,ct2)
  }
  #}
  dat2=rename(dat2,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12",
                     "V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEANcosmo","V18"="MEDIANcosmo"))
  
  
  alme=melt(dat,id.vars="AHEAD")
  ensr=alme[49:51,]
  ala=alme[58:60,]
  
  cm=melt(dat2, id.vars="AHEAD")
  modr=cm[49:51,]
  
  dt=rbind(ensr,ala,modr)
  dt$value=unlist(dt$value)
  
  ####line colour 
  
  ggplot(data=dt,aes(x=factor(AHEAD),y=value, group=variable))+
    geom_line(aes(group=factor(variable),color=variable),size=2)+
    # geom_line(data=ens,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
    theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
    ylab("BIAS")+xlab("HOURS")+ggtitle("BIAS")+
    scale_colour_brewer(name = 'MODEL', palette = 'Set1') 
  
  ##### SCORES TABLE #####
  
  
  e1=readRDS("binarycos_1")
  e1=e1[,TH:=1]
  e2=readRDS("binarycos_2.5")
  e2=e2[,TH:=2.5]
  e3=readRDS("binarycos_5")
  e3=e3[,TH:=5]
  e4=readRDS("binarycos_10")
  e4=e4[,TH:=10]
  binar=rbind(e1,e2,e3,e4)
  
  
  ahead=c(6,12,18)
  obl=unique(as.character(binar$ID))
  ens=unique(as.character(names(binar[,8:27])))
  
  #### POD ####  
  
  # efektivneji vypocteno pres skript /verif_scores_binar.R/
  dat=data.table()
  dt=data.table()
  for (ii in ahead){
    b=binar[AHEAD==ii]
    cta=data.table(table.stats(b$PR,b$ALADIN))
    cta=cta[2,]
    ctm=data.table(table.stats(b$PR,b$MEAN))
    ctm=ctm[2,]
  
    
    dt=rbind(cta,ctm) 
    dt=cbind(dt,AHEAD=ii,MODEL=c("ALADIN","MEAN"))
    dat=rbind(dat,dt)
  
  }
  
  datc=data.table()
  dtc=data.table()
  for (ii in ahead){
    b=binar[AHEADcosmo==ii]
    
    ctc=data.table(table.stats(b$PR,b$MEANcosmo))
    ctc=ctc[2,]
   
  
    dtc=cbind(ctc,AHEAD=ii,MODEL="MEANcosmo")
    datc=rbind(datc,dtc) 
  }
  
 #udelej plot pro POD
  

dt=rbind(dat,datc)
dt$V1=unlist(dt$V1)
  
  ggplot(data=dt,aes(x=factor(AHEAD),y=V1, group=MODEL))+
    geom_line(aes(group=factor(MODEL),color=MODEL),size=2)+
    # geom_line(data=ens,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
    theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1,0.4), legend.justification = c(1, 1))+
    ylab("POD")+xlab("HOURS")+ggtitle("POD")+
    scale_colour_brewer(name = 'MODEL', palette = 'Set1') 

  # plot pro POD i s ensembly
  
  pod=data.table(readRDS("POD_18"))
  pod$POD=unlist(pod$POD)
  mod=pod[1:18,]
  ensa=pod[19:66,]
  ensc=pod[67:114,]
  
  
  ggplot(data=mod,aes(x=factor(AHEAD),y=POD, group=MODEL))+
    geom_line(data=ensa,aes(x=factor(AHEAD),y=POD ,group=MODEL),alpha=0.5)+
    theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
    ylab("POD")+xlab("HOURS")+ggtitle("POD")+
    scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
    geom_line(data=ensc,aes(group=factor(MODEL)),size=1) 
  
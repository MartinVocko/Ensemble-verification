### nastaveni cesty ####
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
tab2=rename(tab2,c("V1"="X1","V2"="X2","V3"="X3","V4"="X4","V5"="X5","V6"="X6","V7"="X7","V8"="X8","V9"="X9","V10"="X10","V11"="X11","V12"="X12",
                   "V13"="X13","V14"="X14","V15"="X15","V16"="X16","V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))


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
      
  
  tab=cbind(RMSE, AHEAD= i)
  tab3=rbind(tab3,tab)
}

tab3=rename(tab3,c("V1"="pf01","V2"="pf02","V3"="pf03","V4"="pf04","V5"="pf05","V6"="pf06","V7"="pf07","V8"="pf08","V9"="pf09","V10"="pf10","V11"="pf11","V12"="pf12",
                   "V13"="pf13","V14"="pf14","V15"="pf15","V16"="pf16","V17"="MEANcosmo","V18"="MEDIANcosmo"))
#rm=data.table(read.xls("rmse_lin.xls"))
alme=melt(tab2,id.vars="AHEAD")
cm=melt(tab3, id.vars="AHEAD")

rmse=rbind(alme,cm)
rmse$variable=as.character(rmse$variable)
rmse=setorder(rmse,variable,AHEAD)



####line colour 
rmse$value=unlist(rmse$value)
mod=rmse[1:18,]
ensa=rmse[19:66,]
ensc=rmse[67:114,]


ggplot(data=ensa,aes(x=factor(AHEAD),y=value))+
  geom_line(aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(0.35, 0.35), legend.justification = c(1, 1))+
  ylab("RMSE")+xlab("HOURS")+ggtitle("ROOT MEAN SQUARE ERROR")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(data=ensc,aes(group=factor(variable)),linetype="dotted",alpha=1)+
  geom_line(data=mod,aes(group=variable, color=variable), size=1.5)

### RMSE Regions ####
modname=names (c(v[,9:28],v[,31:48])) 
obl=unique(as.character(v$ID))
tab=data.table()
tab2=data.table()
for (i in obl)
{ 
  ens=v[ID== i]
    for (m in modname) { 
  
  RMSE=data.table(rmse(ens[[m]],ens$PR))   #upgrade...projede sloupce s modely automaticky
 
  tab=cbind(RMSE, OBL= i, MODEL=m)
  tab2=rbind(tab2,tab)
  }
} 

ggplot(data=tab2, aes(x=OBL, y=V1)) + 
  geom_boxplot(fill='white', color="black")+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+
  ylab("RMSE")+xlab("MODEL")+ggtitle("RMSE")

  
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
  
  tab2=rename(tab2,c("V1"="X1","V2"="X2","V3"="X3","V4"="X4","V5"="X5","V6"="X6","V7"="X7","V8"="X8","V9"="X9","V10"="X10","V11"="X11","V12"="X12",
                     "V13"="X13","V14"="X14","V15"="X15","V16"="X16","V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))
  
  
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
    
    tab=cbind(ME,AHEAD= i)#obl=jj
    tab3=rbind(tab3, tab)
  }  
  
  tab3=rename(tab3,c("V1"="pf01","V2"="pf02","V3"="pf03","V4"="pf04","V5"="pf05","V6"="pf06","V7"="pf07","V8"="pf08","V9"="pf09","V10"="pf10","V11"="pf11","V12"="pf12",
                     "V13"="pf13","V14"="pf14","V15"="pf15","V16"="pf16","V17"="MEANcosmo","V18"="MEDIANcosmo"))
  
  
  
  alme=melt(tab2,id.vars="AHEAD")
  cm=melt(tab3, id.vars="AHEAD")
 
   me=rbind(alme,cm)
   me$variable=as.character(me$variable)
   me=setorder(me,variable,AHEAD)
    
  
  
  ####line colour 
   me$value=unlist(me$value)
   mod=me[1:18,]
   ensa=me[19:66,]
   ensc=me[67:114,]
   
   
   ggplot(data=ensa,aes(x=factor(AHEAD),y=value))+
     geom_line(aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
     theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(0.35, 0.9), legend.justification = c(1, 1))+
     ylab("ME")+xlab("HOURS")+ggtitle("MEAN ERROR")+
     scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
     geom_line(data=ensc,aes(group=factor(variable)),linetype="dotted",alpha=1)+
     geom_line(data=mod,aes(group=variable, color=variable), size=1.5)
  

   #### ME regions ####
   
   modname=names (c(v[,9:28],v[,31:48])) 
   obl=unique(as.character(v$ID))
   tab=data.table()
   tab2=data.table()
   for (i in obl)
   { 
     ens=v[ID== i]
     for (m in modname) { 
       
       ME=data.table(me(ens[[m]],ens$PR))   #upgrade...projede sloupce s modelz automaticky
       
       tab=cbind(ME, OBL= i, MODEL=m)
       tab2=rbind(tab2,tab)
     }
   } 
   
   ggplot(data=tab2, aes(x=OBL, y=V1)) + 
     geom_boxplot(fill='white', color="black")+
     stat_summary(fun.y=mean, geom="point", shape=18, size=4)+
     ylab("ME")+xlab("MODEL")+ggtitle("ME")
   
  
 
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
                  mae(ens$`6`,ens$PR),mae(ens$`7`,ens$PR),mae(ens$`8`,ens$PR),mae(ens$`9`,ens$PR),mae(ens$`10`,ens$PR),
                  mae(ens$`11`,ens$PR),mae(ens$`12`,ens$PR),mae(ens$`13`,ens$PR),mae(ens$`14`,ens$PR),mae(ens$`15`,ens$PR),
                  mae(ens$`16`,ens$PR),mae(ens$`MEAN`,ens$PR),mae(ens$`MEDIAN`,ens$PR),mae(ens$`CF`,ens$PR),mae(ens$`ALADIN`,ens$PR))
    
    tab=cbind(MAE,AHEAD= i)#obl=jj
    tab2=rbind(tab2, tab)
  }  
  
  tab2=rename(tab2,c("V1"="X1","V2"="X2","V3"="X3","V4"="X4","V5"="X5","V6"="X6","V7"="X7","V8"="X8","V9"="X9","V10"="X10","V11"="X11","V12"="X12",
                     "V13"="X13","V14"="X14","V15"="X15","V16"="X16","V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))
  
  
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
    
    tab=cbind(MAE,AHEAD= i)#obl=jj
    tab3=rbind(tab3, tab)
  }  
  
  tab3=rename(tab3,c("V1"="pf01","V2"="pf02","V3"="pf03","V4"="pf04","V5"="pf05","V6"="pf06","V7"="pf07","V8"="pf08","V9"="pf09","V10"="pf10","V11"="pf11","V12"="pf12",
                     "V13"="pf13","V14"="pf14","V15"="pf15","V16"="pf16","V17"="MEANcosmo","V18"="MEDIANcosmo"))
  
  
  
  alme=melt(tab2,id.vars="AHEAD")
  cm=melt(tab3, id.vars="AHEAD")
  
  mae=rbind(alme,cm)
  mae$variable=as.character(mae$variable)
  mae=setorder(mae,variable,AHEAD)
  
  
  
  ####line colour 
  mae$value=unlist(mae$value)
  mod=mae[1:18,]
  ensa=mae[19:66,]
  ensc=mae[67:114,]
  
  
  ggplot(data=ensa,aes(x=factor(AHEAD),y=value))+
    geom_line(aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
    theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(0.35, 0.35), legend.justification = c(1, 1))+
    ylab("MAE")+xlab("HOURS")+ggtitle("MEAN ABSOLUTE ERROR")+
    scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
    geom_line(data=ensc,aes(group=factor(variable)),linetype="dotted",alpha=1)+
    geom_line(data=mod,aes(group=variable, color=variable), size=1.5)
  
 #### MAE regions ####
  
  modname=names (c(v[,9:28],v[,31:48])) 
  obl=unique(as.character(v$ID))
  tab=data.table()
  tab2=data.table()
  for (i in obl)
  { 
    ens=v[ID== i]
    for (m in modname) { 
      
      MAE=data.table(mae(ens[[m]],ens$PR))   #upgrade...projede sloupce s modelz automaticky
      
      tab=cbind(MAE, OBL= i, MODEL=m)
      tab2=rbind(tab2,tab)
    }
  } 
  
  ggplot(data=tab2, aes(x=OBL, y=V1)) + 
    geom_boxplot(fill='white', color="black")+
    stat_summary(fun.y=mean, geom="point", shape=18, size=4)+
    ylab("MAE")+xlab("MODEL")+ggtitle("MAE")
  
  
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
  
  #### POD - pro ostatni skore skript verif_scores_binar.r ####  
  
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
  
  
  ggplot(data=ensa,aes(x=factor(AHEAD),y=POD))+
    geom_line(aes(x=factor(AHEAD),y=POD ,group=MODEL),alpha=0.5)+
    theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 0.35), legend.justification = c(1, 1))+
    ylab("POD")+xlab("HOURS")+ggtitle("POD")+
    scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
    geom_line(data=ensc,aes(group=factor(MODEL)),linetype="dotted",alpha=1)+
    geom_line(data=mod,aes(group=MODEL, color=MODEL), size=1.5)
  
  #plot pro threshold POD i s ensembly
  
  pod=data.table(readRDS("POD_18TH"))
  pod$POD=unlist(pod$POD)
  mod=pod[1:24,]
  ensa=pod[25:88,]
  ensc=pod[89:152,]
  
  
  ggplot(data=ensa,aes(x=factor(TH),y=POD))+
    geom_line(aes(x=factor(TH),y=POD ,group=MODEL),alpha=0.5)+
    theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
    ylab("POD")+xlab("THRESHOLD [mm]")+ggtitle("POD")+
    scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
    geom_line(data=ensc,aes(group=factor(MODEL)),linetype="dotted",alpha=1)+
    geom_line(data=mod,aes(group=MODEL, color=MODEL), size=1.5)
  
  #### Brier score ####
  ############################## AHEAD
  ahead=c(6,12,18)
  tab=data.table()
  tab2=data.table()
  
  for (i in ahead){
    b=binar[AHEAD== i]
  
    
    LAEF=as.matrix(b[,10:25])
    ALADIN=as.matrix(b[,26])
    MEANlaef=as.matrix(b[,8])
    obs=as.matrix(b[,28])
    
    C=list()
    C[[1]]=LAEF
   # C[[2]]=ALADIN
   # C[[3]]=MEANlaef
    
   # bs=lapply(ma,mean(EnsBrier(ma, obs, R.new=NA)))
    
   for (j in 1:length(C)){
    ens=C[[j]]  
      
    bs=mean(EnsBrier(ens, obs ))    #R.new=NA
    
    tab=data.table(BS=bs,AHEAD= i, MODEL= 'LAEF')
    tab2=rbind(tab2, tab)
      
    }
  
  }  
  
  for (i in ahead){ 
    bb=binar[AHEADcosmo==i]
 
  MEANcosmo=as.matrix(bb[,46])
  COSMO=as.matrix(bb[,30:45])
  obs=as.matrix(bb[,28])
  
  C=list()
  C[[1]]=COSMO
 # C[[2]]=MEANcosmo
  
     for (jj in 1:length(C)){ 
       ens=C[[jj]]  
       
       bs=mean(EnsBrier(ens, obs, R.new=NA))
       
       tab=data.table(BS=bs,AHEAD= i, MODEL='COSMO')
       tab2=rbind(tab2, tab)
       
     }
  }
 saveRDS(tab2,"BScosmolaef")     
 
 ggplot(data=tab2,aes(x=factor(AHEAD),y=BS,group=MODEL,color=MODEL))+
   geom_line()+
   theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
   ylab("BS")+xlab("HOURS")+ggtitle("BS")
  
####################################### TH
 
 th=c(1,2.5,5,10)
 tab=data.table()
 tab2=data.table()
 
 for (i in th){
   b=binar[TH== i]
   
   
   LAEF=as.matrix(b[,10:25])
   ALADIN=as.matrix(b[,26])
   MEANlaef=as.matrix(b[,8])
   obs=as.matrix(b[,28])
   
   C=list()
   C[[1]]=LAEF
   # C[[2]]=ALADIN
   # C[[3]]=MEANlaef
   
   # bs=lapply(ma,mean(EnsBrier(ma, obs, R.new=NA)))
   
   for (j in 1:length(C)){
     ens=C[[j]]  
     
     bs=mean(EnsBrier(ens, obs ))    #R.new=NA
     
     tab=data.table(BS=bs,TH= i, MODEL= 'LAEF')
     tab2=rbind(tab2, tab)
     
   }
   
 }  
 
 for (i in th){ 
   bb=binar[TH==i]
   
   MEANcosmo=as.matrix(bb[,46])
   COSMO=as.matrix(bb[,30:45])
   obs=as.matrix(bb[,28])
   
   C=list()
   C[[1]]=COSMO
   # C[[2]]=MEANcosmo
   
   for (jj in 1:length(C)){ 
     ens=C[[jj]]  
     
     bs=mean(EnsBrier(ens, obs, R.new=NA))
     
     tab=data.table(BS=bs,TH= i, MODEL='COSMO')
     tab2=rbind(tab2, tab)
     
   }
 }
 saveRDS(tab2,"BScosmolaef_th")     
 
 ggplot(data=tab2,aes(x=factor(TH),y=BS,group=MODEL,color=MODEL))+
   geom_line()+
   theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
   ylab("BS")+xlab("Threshold")+ggtitle("BS")
 
 
 

############################ Ranked Probability Score  ##########
 
 ############### AHEAD ALADIN LAEF
 
 ahead=c(6,12,18)
 obl=unique(as.character(v$ID))
 tab=data.table()
 tab2=data.table()
# tab=list()
# tab2=list()
 #tab3=list()
 
 #for (j in obl)
 #{ 
 # vo=v[ID==j]
 
 for (i in ahead)
 { 
   va=v[AHEAD== i]
   ens=va[,11:26]
   ens[, id:=1:.N]
   mens = melt(ens, id.vars = 'id')
   mens[value<0, value := 0]
   mens[, cls := cut(value, breaks = c(0, 1, 2.5, 5, 10, Inf), include.lowest = TRUE)]
   res = mens[, .(prst = .N/16), by = .(id, cls)]
   res = dcast.data.table(res, id ~ cls,value.var ="prst" )
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
  # RPS.climo <- mean( apply( ( pref - pobs)^2,1, sum)  )/ ( ncol(pref) -1 )
  # RPSS <- 1 - RPS/RPS.climo
   
   tab=cbind(RPS=RPS, AHEAD=i, MODEL= "LAEF")   #obl=j
   
   tab2=rbind(tab2,tab)
   #tab2=cbind(tab,AHEAD=ahead)
   #tab2=rbind(tab2,RPS.climo)
 }
 
 }


tab=data.table()
#tab2=data.table()
# tab=list()
# tab2=list()
#tab3=list()

#for (j in obl)
#{ 
# vo=v[ID==j]

for (i in ahead)
{ 
  va=v[AHEADcosmo== i]
  ens=va[,31:46]
  ens[, id:=1:.N]
  mens = melt(ens, id.vars = 'id')
  mens[value<0, value := 0]
  mens[, cls := cut(value, breaks = c(0, 1, 2.5, 5, 10, Inf), include.lowest = TRUE)]
  res = mens[, .(prst = .N/16), by = .(id, cls)]
  res = dcast.data.table(res, id ~ cls,value.var ="prst" )
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
  # RPS.climo <- mean( apply( ( pref - pobs)^2,1, sum)  )/ ( ncol(pref) -1 )
  # RPSS <- 1 - RPS/RPS.climo
  
  tab=cbind(RPS=RPS, AHEAD=i, MODEL= "COSMO")   #obl=j
  
  tab2=rbind(tab2,tab)
  #tab2=cbind(tab,AHEAD=ahead)
  #tab2=rbind(tab2,RPS.climo)
}

}


tab2$AHEAD=as.numeric(tab2$AHEAD)  #prevedeni cisel v DT z characters na numerics (nevim proc se to tak uklada)
tab2$RPS=as.numeric(tab2$RPS)
tab2[,lapply(.SD, round, 3),by=MODEL] #zaokrouhleni hodnot na tri destinna mista

saveRDS(tab2,"RPScosmolaef")     

ggplot(data=tab2,aes(x=factor(AHEAD),y=RPS,group=MODEL,color=MODEL))+
  geom_line()+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("RPS")+xlab("HOURS")+ggtitle("RPS")

###### Rank histogram #####

v1=v[PR<1]
v2=v[PR<2.5]
v5=v[PR<5]
v10=v[PR<10]
vi=v[PR>10]

### Rank LAEF
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

multhist(l,freq=F,breaks = seq(0,17,by=1),names.arg=rep("",17), prob = TRUE, xaxt = "n",col=c( "#252525", "#636363", "#969696", "#CCCCCC", "#F7F7F7"),main="Rank Histogram-LAEF")
axis(1, at = seq(3.2, to = 104 + 0.5, by = 6), labels = 1:(16 + 1))
abline(h = 1/(k + 1), lty = 2)
legend("topleft", c("  < 1 mm"," < 2 mm","  < 5 mm","  < 10 mm","  > 10 mm"), fill=c( "#252525", "#636363", "#969696", "#CCCCCC", "#F7F7F7") )


#### Rank COSMO

observations=v1$PR
forecasts=v1[,31:46,with=F]
rank1 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])

observations=v2$PR
forecasts=v2[,31:46,with=F] 
rank2 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])
observations=v5$PR
forecasts=v5[,31:46,with=F] 
rank5 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])
observations=v10$PR
forecasts=v10[,31:46,with=F] 
rank10 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                    ties = "random")[1])
observations=vi$PR
forecasts=vi[,31:46,with=F] 
ranki <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])
l=list(rank1, rank2, rank5, rank10, ranki)

multhist(l,freq=F,breaks = seq(0,17,by=1),names.arg=rep("",17), prob = TRUE, xaxt = "n",col=c( "#252525", "#636363", "#969696", "#CCCCCC", "#F7F7F7"),main="Rank Histogram-COSMO")
axis(1, at = seq(3.2, to = 104 + 0.5, by = 6), labels = 1:(16 + 1))
abline(h = 1/(k + 1), lty = 2)
legend("topleft", c("  < 1 mm"," < 2 mm","  < 5 mm","  < 10 mm","  > 10 mm"), fill=c( "#252525", "#636363", "#969696", "#CCCCCC", "#F7F7F7") )

### rank histogram pro COSMO x LAEF bez zavislosti na velikosti uhrnu

observations=v$PR
forecasts=v[,11:26,with=F]
rank1 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])

observations=v$PR
forecasts=v[,31:46,with=F]
rank2 <- apply(cbind(observations, forecasts), 1, function(x) rank(x, 
                                                                   ties = "random")[1])

l=list(rank1, rank2)

multhist(l,freq=F,breaks = seq(0,17,by=1),names.arg=rep("",17), prob = TRUE, xaxt = "n",col=c( "#252525", "#F7F7F7"),main="Rank Histogram-COSMO vs. LAEF")
axis(1,at= seq(1.5, to = 49 + 0.5, by = 3),labels = 1:(16 + 1))
abline(h = 1/(k + 1), lty = 2)
legend("topleft", c("LAEF","COSMO"), fill=c( "#252525", "#F7F7F7") )



#### RANK POLYGONS COSMO/LAEF ####

### definice vertab kategorii dle uhrnu
vc=v
#vc$CAT<-findInterval(vc$PR, c(0, 1, 2.5, 5, 10), rightmost.closed = TRUE)
dta=v
bin=binar
### Vypocet score do tabulky

# CORR
#dta=vc[CAT %in% c("5")]
obl=unique(as.character(v$ID))
ahead=c(6,12,18)
modname=names (c(v[,31:48])) #v[,9:28]
tab0=data.table()
tab=data.table()
tab1=data.table()
tab2=data.table()
tab3=data.table()
tab4=data.table()

for (i in ahead)
{ 
dt=dta         #   dt=dta[AHEADcosmo==i]
b=binar         #   b=binar[AHEADcosmo==i]
  
  for (m in modname) { 
    
    ME=data.table(me(dt[[m]],dt$PR))
    tab=cbind(ME,SCORE="ME", MODEL=m) #AHEAD= i
    tab0=rbind(tab0,tab)
    
    MAE=data.table(mae(dt[[m]],dt$PR))
    tab=cbind(MAE,SCORE="MAE", MODEL=m) #AHEAD= i
    tab1=rbind(tab1,tab)
  
  
    RMSE=data.table(rmse(dt[[m]],dt$PR))   
    tab=cbind(RMSE,SCORE="RMSE", MODEL=m)  #AHEAD= i
    tab2=rbind(tab2,tab)
   
    
    ct=data.table( POD = table.stats(b$PR,b[[m]]))
    ct=ct[4,]
    dct=cbind(V1=unlist(ct),SCORE="POD",MODEL=m)   #,AHEAD=i
    tab3=rbind(tab3,dct)
  
    t=data.table( TS = table.stats(b$PR,b[[m]]))
    t=t[2,]
    dtm=cbind(V1=unlist(t),SCORE="TS",MODEL=m)   #,AHEAD=i
    tab4=rbind(tab4,dtm)
 
  } 
  

}
x=gather

datulka1=rbind (tab0, tab1, tab2)
datulka2=rbind (tab3, tab4)
datulka1=dcast(datulka1,MODEL ~SCORE, value.var='V1')
datulka2=dcast(datulka2, MODEL~ SCORE, value.var='V1')


## Rankovani

r1=datulka1
nam=names(r1[c(2:ncol(r1))])

for (i in nam)
{ 
  x=r1[[i]]
  rx=(rank(x))
  r1=cbind(r1,rx)
  r1=rename(r1,c("rx"=i))

}

r2=datulka2
nam2=names(r2[c(2:ncol(r2))])

for (i in nam2)
{ 
  x=r2[[i]]
  rx=rank(desc(x))
  r2=cbind(r2,rx)
  r2=rename(r2,c("rx"=i))
}

r=cbind(r1,r2)
output <- data.frame( RANKsum = apply(r[,c(5,6,7,11,12)], 1, sum)) 

r=cbind(r,output)
r=r[-c(2,3,4,8,9,10)]
RANKall=rank(r$RANKsum)
r=cbind(r,RANKall)

saveRDS(r,'rank_cosmo')

###############################

output <- data.frame( RANKmean = apply(r[,c(5,6,7,11,12)], 1, sum) ,RANKmeancosmo = apply(r[,c(12,14,16,18,20)], 1, sum) ) #soucet ranku
r=cbind(r,output)
r$obl=r1$obl
saveRDS(r,fil="ranktab")


r$COSMO<-cut(r$RANKmeancosmo,breaks=c(0,44,76,108,140,Inf), labels=c(1,2,3,4,5),right=FALSE)
r$LAEF<-cut(r$RANKmean,breaks=c(0,44,76,108,140,Inf), labels=c(1,2,3,4,5),right=FALSE)
rnk=r[,c(23,24,25)]
rn = data.table(merge(obl, rnk, by.x = "id", by.y = "obl") )
RN =melt(rn, id.vars = c(1:7))#, ncol()))
RN$TH2 <- factor(RN$TH,levels=c("1","2.5","5","10","MEAN"))

ggplot(RN) + geom_polygon(aes(x= long, y = lat, fill = value, group = group)) + 
  facet_grid(~variable) + 
  geom_polygon(aes(x = long, y = lat, group = group), data = fvp, col = 'black', fill = NA) + 
  scale_fill_brewer("RANK",palette='Reds')+
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
        strip.background = element_rect( colour='black',fill="grey"), panel.border = element_rect(colour = "black"),strip.text.x = element_text(size = 12), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), strip.text.y = element_text(size=12))




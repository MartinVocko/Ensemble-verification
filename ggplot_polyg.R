#### Nacteni dat ####
setwd("~/Plocha/erko")
VP = spChFIDs(vp, as.character(vp@data$name))
obl = fortify(VP )
fvp = fortify(vp)

setwd("~/Plocha/DATA/tab_RDS")


#### ME #####
me=data.table(read.xls("ME.xls"))
me=melt(me,id.vars="AHEAD")
ensr=me[1:144,]
modr=me[145:180,]


ggplot(data=modr,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(aes(group=factor(variable),color=variable),size=2)+
  #scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))
  geom_line(data=ensr,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("ME [mm]")+xlab("HOURS")+ggtitle("ME")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')

#ggplot()+geom_line(data=ensr,aes(x=AHEAD,y=value ,group=variable,alpha= 0.8,linetype="ENSEMBLE")) + 
geom_line(data=ALAr,aes(x=AHEAD,y=value,size="ALADIN",linetype="ALADIN")) +
  geom_line(data=MEANr,aes(x=AHEAD,y=value, size = "MEAN", linetype='MEAN')) +
  geom_line(data=MEDIANr,aes(x=AHEAD,y=value, size = "MEDIAN", linetype = "MEDIAN")) +
  geom_line(data=CFr,aes(x=AHEAD,y=value),size= "CF",linetype="CF")+
  theme_bw() + ylab("ME")+xlab("HOURS")+ggtitle("ME")+
  scale_size_manual(name = "MODEL", values = c("ENSEMBLE"=2, "ALADIN"=2, "MEAN" = 2, "MEDIAN" = 1.5,"CF"=2))+ scale_linetype_manual(name="MODEL", values = c("ENSEMBLE"=1, "ALADIN" = 2, "MEAN" = 3, "MEDIAN" = 4, "CF"=5))

#### ME BOXPLOT ####

bx=data.table(read.xls("ME_obl.xls"))
box=melt(bx)
ggplot(data=box, aes(x=variable, y=value)) + 
  geom_boxplot(fill='lightgrey', color="black")+
  #stat_summary(fun.y=mean, geom="point", shape=23, size=3)+
  ylab("ME[mm]")+xlab("AREA")+ggtitle("ME")+
  theme_bw()

##### MAE ############

mae=data.table(readRDS("MAE"))
mae=rename(mae,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12","V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))
mae=melt(mae,id.vars="AHEAD")
ensr=mae[1:144,]
modr=mae[145:180,]


ggplot(data=modr,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(aes(group=factor(variable),color=variable),size=2)+
   geom_line(data=ensr,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("MAE [mm]")+xlab("HOURS")+ggtitle("MAE")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')

#### MAE BOXPLOT ####

bx=data.table(read.xls("MAE_obl.xls"))
box=melt(bx)
ggplot(data=box, aes(x=variable, y=value)) + 
  geom_boxplot(fill='lightgrey', color="black")+
  #stat_summary(fun.y=mean, geom="point", shape=23, size=3)+
  ylab("MAE [mm]")+xlab("AREA")+ggtitle("MAE")+
  theme_bw()

###################################### RMSE #############################################################
r=readRDS("RMSE_polygons")
ra=readRDS("RMSE_avrg")
ra = data.table(merge(obl, ra, by.x = "id", by.y = "OBL") )
ra=ra[,AHEAD:="AVERAGE"]
ra=rename(ra,c("V7"="ENS7","V16"="ENS16","V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))
ra=ra[,!c("V1","V2","V3","V4","V5","V6","V8", "V9","V10","V11","V12", "V13","V14","V15"),with=FALSE]

R = data.table(merge(obl, r, by.x = "id", by.y = "OBL") )
R[,AHEAD:= unlist(AHEAD)]
R=rename(R,c("V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))
#R=R[,(8:23):=NULL]
R=R[,!c("V1","V2","V3","V4","V5","V6","V8", "V9","V10","V11","V12", "V13","V14","V15"),with=FALSE]
colnames(R)[8] <- "ENS7"
colnames(R)[9] <- "ENS16"
R=R[AHEAD %in% c("6","48")]

RR=rbind(R,ra)
mR =melt(RR, id.vars = c(1:7, ncol(R)))
mR$AHEAD_b = factor(mR$AHEAD, levels=c('6','AVERAGE','48')) #zmena poradi pro facet_grid
#mR =melt(R, id.vars = 1:7)  #pro prumer RMSE

ggplot(R) + geom_polygon(aes(x= long, y = lat, fill = unlist(MEAN), group = group)) + facet_wrap(~AHEAD) + geom_polygon(aes(x = long, y = lat, group = group), data = fvp, col = 'black', fill = NA) + scale_fill_gradient(low = 'white', high = 'red')

ggplot(R[AHEAD=='24', ]) + geom_polygon(aes(x= long, y = lat, fill = unlist(V20), group = group)) #+ facet_wrap(~AHEAD)

ggplot(mR[AHEAD=='54' ]) + geom_polygon(aes(x= long, y = lat, fill = unlist(value), group = group)) + facet_wrap(~variable) + geom_polygon(aes(x = long, y = lat, group = group), data = fvp, col = 'black', fill = NA) + scale_fill_gradient(low = 'white', high = 'red')+coord_fixed()

#RMSE vysledne srovnani
ggplot(mR) + geom_polygon(aes(x= long, y = lat, fill = unlist(value), group = group)) + facet_grid(variable~AHEAD_b) + geom_polygon(aes(x = long, y = lat, group = group), data = fvp, col = 'black', fill = NA) + scale_fill_gradient(name="RMSE",low = 'white', high = 'black')+coord_fixed()+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank())

#RMSE vysledne srovnani barevne
ggplot(mR) + geom_polygon(aes(x= long, y = lat, fill = unlist(value), group = group)) + facet_grid(variable~AHEAD_b) + geom_polygon(aes(x = long, y = lat, group = group), data = fvp, col = 'black', fill = NA) + scale_fill_gradient(name="RMSE",low = 'white', high = 'red')+coord_fixed()+ theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank())


########################################### RMSE line ################
rm=data.table(read.xls("rmse_lin.xls"))
rms=melt(rm,id.vars="AHEAD")
ensr=rms[1:144,]
modr=rms[145:180,]

ggplot(data=modr,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(aes(group=factor(variable),linetype=factor(variable)),size=2)+
  #scale_linetype_manual(values=c("solid","dotdash","dotted","dashed"))+
  scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))+
  theme_bw()+theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  #labs(linetype=NULL)+
  geom_line(data=ensr,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  ylab("RMSE")+xlab("HOURS")+ggtitle("RMSE")

####line colour 
ggplot(data=modr,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(aes(group=factor(variable),color=variable),size=2)+
  #scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))
  geom_line(data=ensr,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("RMSE")+xlab("HOURS")+ggtitle("RMSE")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')

#ggplot()+geom_line(data=ensr,aes(x=AHEAD,y=value ,group=variable,alpha= 0.8,linetype="ENSEMBLE")) + 
  geom_line(data=ALAr,aes(x=AHEAD,y=value,size="ALADIN",linetype="ALADIN")) +
  geom_line(data=MEANr,aes(x=AHEAD,y=value, size = "MEAN", linetype='MEAN')) +
  geom_line(data=MEDIANr,aes(x=AHEAD,y=value, size = "MEDIAN", linetype = "MEDIAN")) +
  geom_line(data=CFr,aes(x=AHEAD,y=value),size= "CF",linetype="CF")+
  theme_bw() + ylab("RMSE")+xlab("HOURS")+ggtitle("RMSE")+
  scale_size_manual(name = "MODEL", values = c("ENSEMBLE"=2, "ALADIN"=2, "MEAN" = 2, "MEDIAN" = 1.5,"CF"=2))+ scale_linetype_manual(name="MODEL", values = c("ENSEMBLE"=1, "ALADIN" = 2, "MEAN" = 3, "MEDIAN" = 4, "CF"=5))

################################## RMSE BOXPLOT ########################
bx=data.table(read.xls("rmse_boxmodel.xls"))
bx=bx[,21:=NULL]  
bx=rename(bx,c("X1"="E1","X2"="E2","X3"="E3","X4"="E4","X5"="E5","X6"="E6","X7"="E7","X8"="E8","X9"="E9","X10"="E10","X11"="E11","X12"="E12","X13"="E13","X14"="E14","X15"="E15","X16"="E16"))
box=melt(bx,measure=1:20)
ggplot(data=box, aes(x=variable, y=value)) + 
  geom_boxplot(fill='white', color="black")+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+
  ylab("RMSE")+xlab("MODEL")+ggtitle("RMSE")


bx=data.table(read.xls("rmse_boxoblast.xls"))
box=melt(bx)
ggplot(data=box, aes(x=variable, y=value)) + 
  geom_boxplot(fill='lightgrey', color="black")+
  #stat_summary(fun.y=mean, geom="point", shape=23, size=3)+
  ylab("RMSE")+xlab("AREA")+ggtitle("RMSE")+
  theme_bw()

bx=data.table(read.xls("rmse_boxoblsort.xls"))
box=melt(bx)
ggplot(data=box, aes(x=variable, y=value)) + 
  geom_boxplot(fill='white', color="black")+
  #stat_summary(fun.y=mean, geom="point", shape=23, size=3)+
  ylab("RMSE")+xlab("AREA")+ggtitle("RMSE")
  theme_bw()

#RMSE BOX EVENTS  
  bx=data.table(readRDS("EVENT_summary"))
 
  #bx=rename(bx,c("X1"="E1","X2"="E2","X3"="E3","X4"="E4","X5"="E5","X6"="E6","X7"="E7","X8"="E8","X9"="E9","X10"="E10","X11"="E11","X12"="E12","X13"="E13","X14"="E14","X15"="E15","X16"="E16"))
  #box=melt(bx,measure=1:20)
  ggplot(data=table, aes(x=variable, y=RMSEmax)) + 
    geom_boxplot(fill='white', color="black")+
    stat_summary(fun.y=mean, geom="point", shape=18, size=4)+
    ylab("RMSE")+xlab("MODEL")+ggtitle("RMSE EVEN MAXIMUM")
    
#### RMSE TPS ####### 
  bx=data.table(read.xls("RMSE_tps.xls"))
  box=melt(bx)
  ggplot(data=box, aes(x=variable, y=value)) + 
    geom_boxplot(fill='white', color="black")+
    #stat_summary(fun.y=mean, geom="point", shape=23, size=3)+
    ylab("RMSE")+xlab("WEATHER SITUATIONS")+ggtitle("RMSE - WEATHER SITUATIONS")
  theme_bw()

  barplot(table(v$tps))

ws=table(v$tps)
ws=data.table(x)

g=ggplot(data=ws, aes(x=V1,y=N))
g=g+ ylab("COUNT")+xlab("WEATHER SITUATIONS")+ggtitle("WEATHER SITUATIONS")
g=g + scale_fill_brewer(palette = "Set1")
g+ geom_bar(stat='identity',fill='darkblue')

#### RMSE TPS Box ####
bx=readRDS("RMSE_tps_obl")
box=melt(bx)
tps=unique(as.character(c("Bp","B","C","SWc2", "Ec", "NEc")))

#b=box[TPS=="Bp"]
ggplot(data=box, aes(x=variable, y=value, fill= TPS)) + 
  geom_boxplot(color='darkblue')+
  #scale_fill_brewer(palette="Dark2")+
  ylab('RMSE')+xlab("MODELS")+ggtitle("WEATHER SITUATIONS")+labs(fill="")+
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))+
  facet_wrap(~TPS)


#################################### KORELACE ##############################################

cr=data.table(read.xls("korelace_line.xlsx"))
cr=melt(cr,id.vars="AHEAD")
cre=cr[1:144,]
crmod=cr[145:180,]

ggplot(data=crmod,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(aes(group=factor(variable),linetype=factor(variable)),size=2)+
  #scale_linetype_manual(values=c("solid","dotdash","dotted","dashed"))+
  scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))+
  theme_bw()+theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  #labs(linetype=NULL)+
  geom_line(data=cre,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  ylab("CORR")+xlab("HOURS")+ggtitle("CORRELATION")

#### corr line colour
ggplot(data=crmod,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(aes(group=factor(variable),color=variable),size=2)+
  #scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))
  geom_line(data=cre,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("CORR")+xlab("HOURS")+ggtitle("CORRELATION COEFFICIENT")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')

#### KORELACE BOXPLOT #####
bx=data.table(read.xls("CORR_obl.xls"))
box=melt(bx)
ggplot(data=box, aes(x=variable, y=value)) + 
  geom_boxplot(fill='lightgrey', color="black")+
  #stat_summary(fun.y=mean, geom="point", shape=23, size=3)+
  ylab("CORR [mm]")+xlab("AREA")+ggtitle("CORRELATION COEFFICIENT")+
  theme_bw()

#### KORELACE TPS Boxplot ####
bx=readRDS("CORR_tps_obl")
box=melt(bx)
tps=unique(as.character(c("Bp","B","C","SWc2", "Ec", "NEc")))

ggplot(data=box, aes(x=variable, y=value, fill= TPS)) + 
  geom_boxplot(color='darkblue')+
  #scale_fill_brewer(palette="Dark2")+
  ylab('CORRELATION')+xlab("MODELS")+ggtitle("WEATHER SITUATIONS")+labs(fill="")+
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))+
  facet_wrap(~TPS)


##################################### BIAS ############################################
b=data.table(read.xls("bias.xls"))
bias=melt(b,id.vars="AHEAD")
ens=bias[1:144,]
mod=bias[145:180,]

ggplot(data=mod,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(aes(group=factor(variable),linetype=factor(variable)),size=2)+
  scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))+
  theme_bw()+theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  geom_line(data=ens,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  ylab("BIAS")+xlab("HOURS")+ggtitle("BIAS")

### COLOUR BIAS
ggplot(data=mod,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(aes(group=factor(variable),color=variable),size=2)+
  geom_line(data=ens,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("BIAS")+xlab("HOURS")+ggtitle("BIAS")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1') 


b=data.table(read.xls("bias_TH.xlsx"))
b=rename(b,c("X1"="E1","X2"="E2","X3"="E3","X4"="E4","X5"="E5","X6"="E6","X7"="E7","X8"="E8","X9"="E9","X10"="E10","X11"="E11","X12"="E12","X13"="E13","X14"="E14","X15"="E15","X16"="E16"))
bia=melt(b,id.vars="TH")
bia[,variable:= unlist(variable)]
eb=bia[1:64,]
modb=bia[65:80,]

ggplot(data=modb,aes(x=factor(TH),y=value, group=variable))+
  geom_line(aes(group=factor(variable),linetype=factor(variable)),size=2)+
  #scale_linetype_manual(values=c("solid","dotdash","dotted","dashed"))+
  scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))+
   theme_bw()+theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  geom_line(data=eb,aes(x=factor(TH),y=value ,group=variable),alpha=0.5)+
  ylab("BIAS")+xlab("THRESHOLD [mm]")+ggtitle("BIAS")

####COLOUR TH BIAS
ggplot(data=modb,aes(x=factor(TH),y=value, group=variable))+
  geom_line(aes(group=factor(variable),color=variable),size=2)+
  scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))+theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  #scale_colour_manual(values =c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))+
  geom_line(data=eb,aes(x=factor(TH),y=value ,group=variable),alpha=0.5)+
  ylab("BIAS")+xlab("THRESHOLD [mm]")+ggtitle("BIAS")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')


###### POD ######
pod=data.table(read.xls("POD.xls"))
pod=melt(pod,id.vars="AHEAD")
ensr=pod[1:144,]
modr=pod[145:180,]


ggplot(data=modr,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(data=ensr,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("POD")+xlab("HOURS")+ggtitle("POD")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)
  #scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))
  

#### POD TH
pod=data.table(read.xls("pod_th.xls"))
pod=melt(pod,id.vars="TH")
ense=pod[1:64,]
emod=pod[65:80,]
ALAe=pod[variable=="ALADIN"]
MEANe=pod[variable=="MEAN"]
MEDIAN=pod[variable=="MEDIAN"]
CFe=pod[variable=="CF"]

ggplot(data=emod,aes(x=factor(TH),y=value, group=variable))+
    #scale_colour_manual(values =c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))+
  geom_line(data=ense,aes(x=factor(TH),y=value ,group=variable),alpha=0.5)+
  ylab("POD")+xlab("THRESHOLD [mm]")+ggtitle("POD")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)+
  scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))+theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))

####### FAR  ########
pod=data.table(read.xls("far.xls"))
pod=melt(pod,id.vars="AHEAD")
ensr=pod[1:144,]
modr=pod[145:180,]


ggplot(data=modr,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(data=ensr,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(0.35, 1), legend.justification = c(1, 1))+
  ylab("FAR")+xlab("HOURS")+ggtitle("FAR")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)

##### FAR TH

pod=data.table(read.xls("far_th.xls"))
pod=melt(pod,id.vars="TH")
ense=pod[1:64,]
emod=pod[65:80,]
ALAe=pod[variable=="ALADIN"]
MEANe=pod[variable=="MEAN"]
MEDIAN=pod[variable=="MEDIAN"]
CFe=pod[variable=="CF"]

ggplot(data=emod,aes(x=factor(TH),y=value, group=variable))+
  #scale_colour_manual(values =c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))+
  geom_line(data=ense,aes(x=factor(TH),y=value ,group=variable),alpha=0.5)+
  ylab("FAR")+xlab("THRESHOLD [mm]")+ggtitle("FAR")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)+
  scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))+theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 0.4), legend.justification = c(1, 1))
############################## ETS ############################################################
e2=readRDS("ETSpol_2.5avrg")
e2=e2[,TH:=2.5]
e3=readRDS("ETSpol_5avrg")
e3=e3[,TH:=5]
e4=readRDS("ETSpol_10avrg")
e4=e4[,TH:=10]
e1=readRDS("ETSpol_1avrg")
e1=e1[,TH:=1]
e=rbind(e1,e2,e3,e4)
e=e[,TH:=as.character(TH)]
e=rename(e,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12","V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))
e=e[,!c("E1","E3","E4","E5","E6","E7","E8","E9","E10","E11","E12","E13","E14","E15"),with=FALSE]
E = data.table(merge(obl, e, by.x = "id", by.y = "OBL") )
#E[, AHEAD:= unlist(AHEAD)]
#E=E[,(8:23):=NULL]
#mE =melt(E, id.vars = c(1:7, ncol(E)))
mE =melt(E, id.vars = c(1:7,ncol(E))) #pro avrg
mE$TH_b = factor(mE$TH, levels=c('1','2.5','5','10'))
#ggplot(mE[AHEAD=='24' ]) + geom_polygon(aes(x= long, y = lat, fill = unlist(value), group = group)) + facet(~variable) + geom_polygon(aes(x = long, y = lat, group = group), data = fvp, col = 'black', fill = NA) + scale_fill_gradient(low = 'red', high = 'white')+coord_fixed()

ggplot(mE) + geom_polygon(aes(x= long, y = lat, fill = unlist(value), group = group)) + facet_grid(variable~TH_b) + geom_polygon(aes(x = long, y = lat, group = group), data = fvp, col = 'black', fill = NA) + scale_fill_gradient(name="ETS",low = 'white', high = 'darkblue')+coord_fixed()+theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank())
########## ETS BOXPLOT ######
bx=e[,21:=NULL]  
box=melt(bx,measure=1:20)
ggplot(data=box, aes(x=factor(variable), y=as.numeric(value))) + 
  geom_boxplot(fill='white', color="black")+
  stat_summary(fun.y=mean, geom="point", shape=18, size=4)+
  ylab("ETS")+xlab("MODEL")+ggtitle("ETS")


bx=data.table(read.xls("etsboxobl.xlsx"))
box=melt(bx,measure=1:37)
ggplot(data=box, aes(x=variable, y=value)) + 
  geom_boxplot(fill='white', color="black")+
  #stat_summary(fun.y=mean, geom="point", shape=23, size=3)+
  ylab("ETS")+xlab("AREA")+ggtitle("ETS")
  
####
et=data.table(read.xls("ETS_lin.xls"))
ets=melt(et,id.vars="TH")
ense=ets[1:64,]
emod=ets[65:80,]
ALAe=ets[variable=="ALADIN"]
MEANe=ets[variable=="MEAN"]
MEDIANe=ets[variable=="MEDIAN"]
CFe=ets[variable=="CF"]

ggplot(data=emod,aes(x=factor(TH),y=value, group=variable))+
   geom_line(data=ense,aes(x=factor(TH),y=value ,group=variable),alpha=0.5)+
  ylab("ETS")+xlab("THRESHOLD [mm]")+ggtitle("ETS")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)+
  scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))+theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))
  #scale_colour_manual(values =c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))+
 

#(ggplot()+geom_line(data=ense,aes(x=factor(TH),y=value ,group=variable,alpha= 0.6))
+geom_line(data=ALAe,aes(x=factor(TH),y=value),size=2,linetype=2)
+geom_line(data=MEANe,aes(x=factor(TH),y=value),size=2)
+geom_line(data=MEDIANe,aes(x=factor(TH),y=value),size=1.5, linetype=3)
+geom_line(data=CFe,aes(x=factor(TH),y=value),size=2,linetype=4)
+theme_bw()
+ylab("ETS")+xlab("THRESHOLD")+ggtitle("ETS") )

+scale_x_log10()
####
et=data.table(read.xls("ETS_thavrg.xls"))
ets=melt(et,id.vars="AHEAD")
ense=ets[1:144,]
emod=ets[145:180,]

ggplot(data=emod,aes(x=factor(AHEAD),y=value, group=variable))+
   geom_line(data=ense,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  ylab("ETS")+xlab("HOURS")+ggtitle("ETS")+
scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
geom_line(aes(group=factor(variable),color=variable),size=2)+
  scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))
 
##### TS ##############

b=data.table(read.xls("TS.xls"))
ts=melt(b,id.vars="AHEAD")
ens=ts[1:144,]
mod=ts[145:180,]

ggplot(data=mod,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(data=ens,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(0.3, 0.4), legend.justification = c(1, 1))+
  ylab("TS")+xlab("HOURS")+ggtitle("TS")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
 geom_line(aes(group=factor(variable),color=variable),size=2)

### TS TH

t=data.table(read.xls("ts_th.xls"))
ts=melt(t,id.vars="TH")
#bia[,variable:= unlist(variable)]
eb=ts[1:64,]
modb=ts[65:80,]

ggplot(data=modb,aes(x=factor(TH),y=value, group=variable))+
  geom_line(data=eb,aes(x=factor(TH),y=value ,group=variable),alpha=0.5)+
  ylab("TS")+xlab("THRESHOLD [mm]")+ggtitle("TS")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)+
  scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3, "CF"=4))+theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))
 

##### BS ####### 
b=data.table(read.xls("BS.xls"))
bs=melt(b,id.vars="AHEAD")


ggplot(data=bs,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(data=bs,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 0.6), legend.justification = c(1, 1))+
  ylab("BS")+xlab("HOURS")+ggtitle("BS")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)

### BS TH
b=data.table(read.xls("bs_th.xls"))
bs=melt(b,id.vars="TH")

ggplot(data=bs,aes(x=factor(TH),y=value, group=variable))+
  geom_line(data=bs,aes(x=factor(TH),y=value ,group=variable),alpha=0.5)+
  ylab("BS")+xlab("THRESHOLD [mm]")+ggtitle("BS")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)+
  scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3))+theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))
#### BSS #####
b=data.table(read.xls("BSS.xls"))
bss=melt(b,id.vars="AHEAD")


ggplot(data=bss,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(data=bss,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("BSS")+xlab("HOURS")+ggtitle("BSS")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)

##### BSS TH
b=data.table(read.xls("BSS_th.xls"))
bss=melt(b,id.vars="TH")

ggplot(data=bss,aes(x=factor(TH),y=value, group=variable))+
  geom_line(data=bss,aes(x=factor(TH),y=value ,group=variable),alpha=0.5)+
  ylab("BSS")+xlab("THRESHOLD [mm]")+ggtitle("BSS")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)+
  scale_linetype_manual(name="MODEL", values = c( "ALADIN" = 2, "MEAN" = 1, "MEDIAN" = 3))+theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 0.9), legend.justification = c(1, 1))
###### BSS BOX ####
b=data.table(read.xls("bss_obl_ala.xls"))
box=melt(b)
ggplot(data=box, aes(x=variable, y=value)) + 
  geom_boxplot(fill='lightgrey', color="black")+
  #stat_summary(fun.y=mean, geom="point", shape=23, size=3)+
  ylab("BSS")+xlab("AREA")+ggtitle("BSS")+
  theme_bw()

##### RPS ######
r=data.table(read.xls("rps.xls"))
rps=melt(r,id.vars="AHEAD")


ggplot(data=rps,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(data=rps,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 0.6), legend.justification = c(1, 1))+
  ylab("RPS")+xlab("HOURS")+ggtitle("RPS")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)


#### RPSS ######
r=data.table(read.xls("RPSS.xls"))
rpss=melt(r,id.vars="AHEAD")


ggplot(data=rpss,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(data=rpss,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 0.4), legend.justification = c(1, 1))+
  ylab("RPSS")+xlab("HOURS")+ggtitle("RPSS")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)

#### RPSS BOX ######
r=data.table(read.xls("rpss_obl_ala.xls"))
box=melt(r)
ggplot(data=box, aes(x=variable, y=value)) + 
  geom_boxplot(fill='lightgrey', color="black")+
  #stat_summary(fun.y=mean, geom="point", shape=23, size=3)+
  ylab("RPSS")+xlab("AREA")+ggtitle("RPSS")+
  theme_bw()


#### ROC ####
b=data.table(read.xls("ROC.xls"))
#bs=melt(b,id.vars="AHEAD")


ggplot(data=b,aes(x=factor(F),y=(POD), group=variable))+
  geom_line(data=b,aes(x=factor(F),y=(POD) ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 0.6), legend.justification = c(1, 1))+
  ylab("POD")+xlab("F")+ggtitle("ROC")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)

##### Scatter plot ####

var=unique(as.character(tabulka$variable))
par(mfrow=c(4,5))
G = list()
for (i in var)
{ 
  t=tabulka[variable== i]
G[[i]]=ggplot(t, aes(x=sumO, y=sumP)) +
  geom_point(size=0.5) +    # Use hollow circles
  geom_smooth(method="lm",se=TRUE)+
  ggtitle(i)+
  labs(x="Observation", y="Forecast")# Add linear regression line 
                           #  (by default includes 95% confidence region)
}




#porovni udalostnich sum a maxim (pro maxima je potreba zmenit sum na max)
tt=tabulka[variable %in% c("MEAN","ALADIN")]
ggplot(tt, aes(x=sumO, y=sumP,color=variable)) +
  geom_point(size=0.5) +    # Use hollow circles
  geom_smooth(method="lm",se=TRUE)+
  ggtitle('EVENT RAINFALLS')+
  labs(x="Observation", y="Forecast")

ggplot(tt, aes(x=maxtimeO, y=maxtimeP,color=variable)) +
  geom_point(size=0.5) +    # Use hollow circles
  geom_smooth(method="lm",se=TRUE)+
  ggtitle('MAXIMUM INTENSITY OCCURRENCE')+
  labs(x="Observation", y="Forecast")+
  scale_x_continuous(breaks=seq(0, 9, 1))+
  scale_y_continuous(breaks=seq(0, 9, 1))


#Porovnani uspesnosti v case
tab1=ty[variable %in% c("MEAN","ALADIN")]
ggplot(data=tab1,aes(x=(ORIGIN),y=RMSE, color=variable))+
  theme(legend.key.width = unit(0.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("RMSE")+xlab("TIME")+ggtitle("RMSE")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_point(aes(group=factor(variable),color=variable),size=1)+
  geom_smooth(method="lm",show.legend=TRUE)


#### Clusters ####
setwd("~/Plocha/erko")
VP = spChFIDs(vp, as.character(vp@data$name))
obl = fortify(VP )
fvp = fortify(vp)

setwd("~/Plocha/DATA/tab_RDS")
r1=readRDS("rankobl1")
r1$TH=1
r1=r1[,c(1,22,23,24)]
r2=readRDS("rankobl2")
r2$TH=2.5
r2=r2[,c(2,22,23,24)]
r5=readRDS("rankobl5")
r5$TH=5
r5=r5[,c(2,22,23,24)]
r10=readRDS("rankobl10")
r10$TH=10
r10=r10[,c(2,22,23,24)]
r=readRDS("rankobl")
r$TH="MEAN"
r=r[,c(1,22,23,24)]
rnk=rbind(r,r1,r2,r5,r10)

#rnk=readRDS('rankobl')

rnk$ALADIN<-cut(rnk$RANKaladin,breaks=c(0,44,76,108,140,Inf), labels=c(1,2,3,4,5),right=FALSE)
rnk$ENS_MEAN<-cut(rnk$RANKmean,breaks=c(0,44,76,108,140,Inf), labels=c(1,2,3,4,5),right=FALSE)
rnk=rnk[,c(1,4,5,6)]
rn = data.table(merge(obl, rnk, by.x = "id", by.y = "obl") )
RN =melt(rn, id.vars = c(1:8))#, ncol()))
RN$TH2 <- factor(RN$TH,levels=c("1","2.5","5","10","MEAN"))

ggplot(RN) + geom_polygon(aes(x= long, y = lat, fill = value, group = group)) + 
facet_grid(TH2~variable) + 
geom_polygon(aes(x = long, y = lat, group = group), data = fvp, col = 'black', fill = NA) + 
#scale_fill_gradient(name="RMSE",low = 'white', high = 'red')+
#scale_fill_manual(values = c("1"="blue","2"="darkgreen","3"="orange", "4"="darkred"))+
scale_fill_brewer("RANK",palette='Reds')+
#coord_fixed()+ 
theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),
      strip.background = element_rect( colour='black',fill="grey"), panel.border = element_rect(colour = "black"),strip.text.x = element_text(size = 12), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), strip.text.y = element_text(size=12))
#do.call(grid.arrange, G)

#### BOX ALA MEAN TPS ####
b1=data.table(readRDS("RMSE_tps6"))
b2=data.table(readRDS('CORR_tps6'))
b3=data.table(readRDS("POD_tps6"),score="POD")
b4=data.table(readRDS("TS_tps6"),score="TS")
b5=data.table(readRDS("MAE_tps6"),score="MAE")
b6=data.table(readRDS("BS_tps6"))
b6[,c("TH","ID")] =NULL


box=rbind(b1,b2,b3,b4,b5,b6)


ggplot(data=box, aes(x=TPS, y=value, fill= variable)) + 
  geom_boxplot()+
  ylab('')+xlab("")+ggtitle("WEATHER SITUATIONS")+labs(fill="MODEL")+
  facet_wrap(~score, scales = "free")+
  theme_bw()


POD$variable <- factor(POD$variable, levels = rev(levels(POD$variable)))

ggplot(data=POD, aes(x=TPS, y=value, fill= variable)) + 
  geom_boxplot(color='darkblue')+
  ylab('POD')+xlab("WEATHER SITUATIONS")+ggtitle("")+labs(fill="MODEL")


POD$var <- paste(POD$TPS,POD$variable)

ggplot(data=POD, aes(x=(AHEAD), y=value,group=(var),color=var)) + 
  geom_line()
 
#### Scores pre/aft event ####
modr=dtindex[score=="CORR"]
pre=modr[tindex=="pre"]
aft=modr[tindex=="aft"]
alap=pre[variable=="ALADIN"]

ggplot(data=pre,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(aes(group=factor(variable),color='Pre-event'),alpha=0.8)+
  geom_line(data=aft,aes(x=factor(AHEAD),y=value ,group=variable,color='After-event'),alpha=0.8)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("")+xlab("HOURS")+ggtitle("SCORE")+
  scale_colour_brewer(name = 'MODEL', palette = 'Dark2')




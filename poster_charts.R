#### nacteni dat ####
setwd("~/Plocha/DATA/tab_RDS")
v=readRDS("vertab.RDS")

### ME ####
me=data.table(read.xls("ME.xls"))
me=melt(me,id.vars="AHEAD")
ensr=me[1:144,]
modr=me[145:180,]


ME=ggplot(data=modr,aes(x=factor(AHEAD),y=value,group=variable))+
  geom_line(data=ensr,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  geom_line(aes(group=factor(variable),color=variable),size=1.5)+
  theme( axis.text=element_text(size=16), plot.title = element_text(size = 16, face = "bold",hjust = 0))+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  theme(legend.key.width = unit(1.4, 'cm'),legend.direction = "horizontal", legend.position = c(0.5, 0.5), legend.text=element_text(size=12))+ #legend.justification = c(1, 1)
  ylab("")+xlab("")+ggtitle("(a)   ME")
  #scale_x_continuous(breaks=seq(0, 54, 6))+
  #scale_y_continuous(breaks=seq(-4, -1, 0.5))




#### RMSE ####
rm=data.table(read.xls("rmse_lin.xls"))
rms=melt(rm,id.vars="AHEAD")
ensr=rms[1:144,]
modr=rms[145:180,]

RMSE=ggplot(data=modr,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(data=ensr,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  geom_line(aes(group=factor(variable),color=variable),size=1.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("RMSE")+xlab("HOURS")+ggtitle("RMSE")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  theme( axis.text=element_text(size=16))+
  theme(legend.position="none", plot.title = element_text(size = 16, face = "bold",hjust = 0))+
 ylab("")+xlab("HOURS")+ggtitle("(c)   RMSE")
  #scale_x_continuous(breaks=seq(0, 54, 6))


#### MAE ####
mae=data.table(readRDS("MAE"))
mae=rename(mae,c("V1"="E1","V2"="E2","V3"="E3","V4"="E4","V5"="E5","V6"="E6","V7"="E7","V8"="E8","V9"="E9","V10"="E10","V11"="E11","V12"="E12","V13"="E13","V14"="E14","V15"="E15","V16"="E16","V17"="MEAN","V18"="MEDIAN","V19"="CF","V20"="ALADIN"))
mae=melt(mae,id.vars="AHEAD")
ensr=mae[1:144,]
modr=mae[145:180,]


MAE=ggplot(data=modr,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(data=ensr,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  geom_line(aes(group=factor(variable),color=variable),size=1.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("MAE [mm]")+xlab("HOURS")+ggtitle("MAE")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  theme( axis.text=element_text(size=16))+
  theme(legend.position="none", plot.title = element_text(size = 16, face = "bold",hjust = 0))+
  #geom_ribbon(data = ribb, aes(x = AHEAD, ymin = min, ymax = max), alpha = .2)+
  ylab("")+xlab("")+ggtitle("(b)   MAE")
  #scale_x_continuous(breaks=seq(0, 54, 6))


#### Correlation ####
cr=data.table(read.xls("korelace_line.xlsx"))
cr=melt(cr,id.vars="AHEAD")
cre=cr[1:144,]
crmod=cr[145:180,]

CORR=ggplot(data=crmod,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(data=cre,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  geom_line(aes(group=factor(variable),color=variable),size=1.5)+
    theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("CORR")+xlab("HOURS")+ggtitle("CORRELATION COEFFICIENT")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  theme( axis.text=element_text(size=16))+
  theme(legend.position="none", plot.title = element_text(size = 16, face = "bold",hjust = 0))+
  ylab("")+xlab("HOURS")+ggtitle("(d)  COR")
 # scale_x_continuous(breaks=seq(0, 54, 6))+
  #scale_y_continuous(breaks=seq(-0.10, 0.20, 0.10))


get_legend <- function(ME) {
  
  tmp <- ggplot_gtable(ggplot_build(ME))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == 'guide-box')
  legend <- tmp$grobs[[leg]]
  
  return(legend)
}
cor_leg <- get_legend(ME)

grid.arrange(ME+theme(legend.position="none"),MAE, RMSE, CORR, cor_leg, layout_matrix = matrix(c(1,2,3,4,5,5), nrow = 3, byrow = T), heights=c(4,4,1))

labels=c("A", "B","C","D")


### RPSS ####
r=data.table(readRDS("RPSS_ahead")) #pro spravnou legendu s "ref" nacist r y ggplot_polyg
rpss=melt(r,id.vars="AHEAD")

RPSS=ggplot(data=rpss,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(data=rpss,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(0.5, 0.5), legend.justification = c(1, 1))+
  ylab("RPSS")+xlab("HOURS")+ggtitle("RPSS")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)+theme( axis.text=element_text(size=12), plot.title = element_text(size = 16, face = "bold",hjust = 0))+
  theme(legend.key.width = unit(1.4, 'cm'),legend.direction = "horizontal", legend.position = c(0.75, 1), legend.text=element_text(size=12))+ #legend.justification = c(1, 1)
  ylab("")+ggtitle("(a)   RPSS")
  #scale_x_continuous(breaks=seq(0, 54, 6))

#### BSS ####

r=data.table(readRDS("BSS_ahead"))
bss=melt(r,id.vars="AHEAD")

BSS=ggplot(data=bss,aes(x=factor(AHEAD),y=value, group=variable))+
  geom_line(data=bss,aes(x=factor(AHEAD),y=value ,group=variable),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(1, 1), legend.justification = c(1, 1))+
  ylab("BSS")+xlab("HOURS")+ggtitle("BSS")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(aes(group=factor(variable),color=variable),size=2)+theme( axis.text=element_text(size=12), plot.title = element_text(size = 16, face = "bold",hjust = 0))+
  theme(legend.position="none")+
  ylab("")+ggtitle("(b)   BSS")
  #scale_x_continuous(breaks=seq(0, 54, 6))+
  

get_legend <- function(myggplot) {
  
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == 'guide-box')
  legend <- tmp$grobs[[leg]]
  
  return(legend)
}
cor_leg <- get_legend(RPSS)

grid.arrange(RPSS+theme(legend.position="none"),BSS, cor_leg, layout_matrix = matrix(c(1,2,3,3), nrow = 2, byrow = T), heights=c(6,6,1))


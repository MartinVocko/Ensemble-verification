#poladit do for cyklu, aby to projelo vsechnz cleny ensemblu automaticky


score<-function (obs, pred, fudge = 0.01, silent = FALSE) 
{
  if (is.null(pred) & length(obs) == 4) {
    if (!silent) {
      print(" Assume data entered as c(n11, n01, n10, n00) Obs*Forecast")
    }
    a <- as.numeric(obs[1])
    b <- as.numeric(obs[2])
    c <- as.numeric(obs[3])
    d <- as.numeric(obs[4])
    tab.out <- matrix(c(a, c, b, d), nrow = 2)
  }
  if (is.null(pred) & is.matrix(obs) & prod(dim(obs)) == 4) {
    if (!silent) 
      print(" Assume contingency table has observed values in columns, forecasts in rows")
    obs <- as.numeric(obs)
    a <- obs[1]
    b <- obs[3]
    c <- obs[2]
    d <- obs[4]
    tab.out <- matrix(c(a, c, b, d), nrow = 2)
  }
  if (!is.null(pred) & !is.null(obs)) {
    tab.out <- table(as.numeric(obs), as.numeric(pred))
    a <- tryCatch(tab.out["1", "1"], error = function(e) 0)
    b <- tryCatch(tab.out["0", "1"], error = function(e) 0)
    c <- tryCatch(tab.out["1", "0"], error = function(e) 0)
    d <- tryCatch(tab.out["0", "0"], error = function(e) 0)
  }
  n <- a + b + c + d
  if (n == 0) 
    n <- fudge
  s <- (a + c)/n
  TS <- a/(a + b + c + fudge)
  POD <- H <- a/(a + c + fudge)
  F <- b/(b + d + fudge)
  TS.se <- sqrt((TS^2) * ((1 - H)/(a + fudge) + b * (1 - F)/((a + 
                                                                b + c)^2 + fudge)))
  SH2 <- H * (1 - H)/(a + c + fudge)
  SF2 <- F * (1 - F)/(b + d + fudge)
  POD.se <- sqrt(SH2)
  F.se <- sqrt(SF2)
  M <- c/(a + c + fudge)
  FAR <- b/(a + b + fudge)
  FAR.se <- sqrt((FAR^4) * ((1 - H)/(a + fudge) + (1 - F)/(b + 
                                                             fudge)) * (a^2)/(b^2 + fudge))
  HSS <- 2 * (a * d - b * c)/(1 * (a + c) * (c + d) + 1 * (a + 
                                                             b) * (b + d) + fudge)
  SHSS2 <- SF2 * (HSS^2) * (1/(H - F + fudge) + (1 - s) * (1 - 
                                                             2 * s))^2 + SH2 * (HSS^2) * (1/(H - F + fudge) - s * 
                                                                                            (1 - 2 * s))^2
  HSS.se = sqrt(SHSS2)
  PSS <- 1 - M - F
  PSS.se <- sqrt(SH2 + SF2)
  KSS <- (a * d - b * c)/((a + c) * (b + d) + fudge)
  PC <- (a + d)/(a + b + c + d + fudge)
  PC.se <- sqrt(s * H * (1 - H)/n + (1 - s) * F * (1 - F)/n)
  if (a + c == 0) 
    BIAS <- (a + b)/fudge
  else BIAS <- (a + b)/(a + c)
  if (b * c == 0) 
    OR <- a * d/fudge
  else OR <- a * d/(b * c)
  if (a * b + b * c == 0) 
    ORSS <- (a * d - b * c)/fudge
  else ORSS <- (a * d - b * c)/(a * d + b * c)
  HITSrandom <- 1 * (a + c) * (a + b)/n
  p <- (a + c)/n
  if (a + b + c - HITSrandom == 0) 
    ETS <- (a - HITSrandom)/fudge
  else ETS <- (a - HITSrandom)/(a + b + c - HITSrandom)
  if (2 - HSS == 0) 
    ETS.se <- sqrt(4 * SHSS2/fudge)
  else ETS.se <- sqrt(4 * SHSS2/((2 - HSS)^4))
  if (b * c == 0) 
    theta <- a * d/fudge
  else theta <- (a * d)/(b * c)
  log.theta <- log(a) + log(d) - log(b) - log(c)
  if (a == 0) 
    a.z <- fudge
  else a.z <- a
  if (b == 0) 
    b.z <- fudge
  else b.z <- b
  if (c == 0) 
    c.z <- fudge
  else c.z <- c
  if (d == 0) 
    d.z <- fudge
  else d.z <- d
  if (1/a.z + 1/b.z + 1/c.z + 1/d.z == 0) 
    n.h <- 1/fudge
  else n.h <- 1/(1/a.z + 1/b.z + 1/c.z + 1/d.z)
  if (theta + 1 == 0) 
    yules.q <- (theta - 1)/fudge
  else yules.q <- (theta - 1)/(theta + 1)
  if (n.h == 0) 
    SLOR2 <- 1/fudge
  else SLOR2 <- 1/n.h
  LOR.se <- sqrt(SLOR2)
  if (OR + 1 == 0) 
    ORSS.se <- sqrt(SLOR2 * 4 * OR^2/fudge)
  else ORSS.se <- sqrt(SLOR2 * 4 * OR^2/((OR + 1)^4))
  if (log(a/n) == 0) {
    eds <- 2 * log((a + c)/n)/fudge - 1
    seds <- (log((a + b)/n) + log((a + c)/n))/fudge - 1
  }
  else {
    eds <- 2 * log((a + c)/n)/log(a/n) - 1
    seds <- (log((a + b)/n) + log((a + c)/n))/log(a/n) - 
      1
  }
  eds.se <- 2 * abs(log(p))/(H * (log(p) + log(H))^2) * sqrt(H * 
                                                               (1 - H)/(p * n))
  seds.se <- sqrt(H * (1 - H)/(n * p)) * (-log(BIAS * p^2)/(H * 
                                                              log(H * p)^2))
  if (log(F) + log(H) == 0) 
    EDI <- (log(F) - log(H))/fudge
  else EDI <- (log(F) - log(H))/(log(F) + log(H))
  EDI.se <- 2 * abs(log(F) + H/(1 - H) * log(H))/(H * (log(F) + 
                                                         log(H))^2) * sqrt(H * (1 - H)/(p * n))
  SEDI <- (log(F) - log(H) - log(1 - F) + log(1 - H))/(log(F) + 
                                                         log(H) + log(1 - F) + log(1 - H))
  SEDI.se <- 2 * abs(((1 - H) * (1 - F) + H * F)/((1 - H) * 
                                                    (1 - F)) * log(F * (1 - H)) + 2 * H/(1 - H) * log(H * 
                                                                                                        (1 - F)))/(H * (log(F * (1 - H)) + log(H * (1 - F)))^2) * 
    sqrt(H * (1 - H)/(p * n))
  
  
  
  
  return(list( FAR=FAR))   #zde menit, jake skore chci returnout (ostatni skore jsou na konci skriptu)
}



### Vypocet skore zvoleneho ve fci "score". Prvni for czklus pro ALADIN a LAEF, druhy pro COSMO (kvuli posunutym AHEAD)

ahead=c(6,12,18)
obs=binar$PR

#fudge = 0.01


#ALADIN a LAEF for cyklus
dt=data.table()
for (i in ahead){
  b=binar[AHEAD==i]
  

        modname= names (b[,8:27])    
        for (name in modname){
  
                sc=data.table(FAR=score(b$PR, b[[name]],0.01, silent=FALSE),MODEL=name, AHEAD=i)
                dt=rbind(dt,sc)
  }
}


#COSMO for cyklus

dtc=data.table()
for (i in ahead){ 
  bb=binar[AHEADcosmo==i]

        modname2=names (bb[,30:47]) 
        for (nam in modname2){ 
                   sc=data.table(FAR=score(bb$PR, bb[[nam]],0.01, silent=FALSE),MODEL=nam, AHEAD=i)
                   dtc=rbind(dtc,sc) 
        }
}


tab=rbind(dt,dtc)
tab=setorder(tab,MODEL,AHEAD)

saveRDS(tab, file='FAR_18')




#### Vypocet skore pomoci fce v zavislosti na thresholdu

th=c(1,2.5,5,10)
#fudge = 0.01


#ALADIN a LAEF for cyklus TH
dt=data.table()
for (i in th){
  b=binar[TH==i]
  
  
  modname= names (b[,8:27])    
  for (name in modname){
    
    sc=data.table(FAR=score(b$PR, b[[name]],0.01, silent=FALSE),MODEL=name, TH=i)
    dt=rbind(dt,sc)
  }
}


#COSMO for cyklus TH

dtc=data.table()
for (i in th){ 
  bb=binar[TH==i]
  
  modname2=names (bb[,30:47]) 
  for (nam in modname2){ 
    sc=data.table(FAR=score(bb$PR, bb[[nam]],0.01, silent=FALSE),MODEL=nam, TH=i)
    dtc=rbind(dtc,sc) 
  }
}


tab=rbind(dt,dtc)
tab=setorder(tab,MODEL,TH)

saveRDS(tab, file='FAR_18TH')



#### vykresleni vyse uvedenych hodnot zavislych na ahead a thresholdu

# plot pro ahead s ensembly

dt=data.table(readRDS("FAR_18"))
dt$FAR=unlist(dt$FAR)
mod=dt[1:18,]
ensa=dt[19:66,]
ensc=dt[67:114,]


ggplot(data=ensa,aes(x=factor(AHEAD),y=FAR))+
  geom_line(aes(x=factor(AHEAD),y=FAR ,group=MODEL),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(0.35, 1), legend.justification = c(1, 1))+
  ylab("FAR")+xlab("HOURS")+ggtitle("FAR")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(data=ensc,aes(group=factor(MODEL)),linetype="dotted",alpha=1)+
  geom_line(data=mod,aes(group=MODEL, color=MODEL), size=1.5)

#plot pro threshold s ensembly

dtt=data.table(readRDS("FAR_18TH"))
dtt$FAR=unlist(dtt$FAR)
mod=dtt[1:24,]
ensa=dtt[25:88,]
ensc=dtt[89:152,]


ggplot(data=ensa,aes(x=factor(TH),y=FAR))+
  geom_line(aes(x=factor(TH),y=FAR ,group=MODEL),alpha=0.5)+
  theme(legend.key.width = unit(2.5, 'cm'), legend.position = c(0.35, 1), legend.justification = c(1, 1))+
  ylab("FAR")+xlab("THRESHOLD [mm]")+ggtitle("FAR")+
  scale_colour_brewer(name = 'MODEL', palette = 'Set1')+
  geom_line(data=ensc,aes(group=factor(MODEL)),linetype="dotted",alpha=1)+
  geom_line(data=mod,aes(group=MODEL, color=MODEL), size=1.5)



########################################################################################################################
###########################################################################################################################
############# vyseparovano z fce



 #return(list(tab = tab.out, TS = TS, TS.se = TS.se, POD = POD, 
              POD.se = POD.se, M = M, F = F, F.se = F.se, FAR = FAR, 
              FAR.se = FAR.se, HSS = HSS, HSS.se = HSS.se, PSS = PSS, 
              PSS.se = PSS.se, KSS = KSS, PC = PC, PC.se = PC.se, BIAS = BIAS, 
              ETS = ETS, ETS.se = ETS.se, theta = theta, log.theta = log.theta, 
              LOR.se = LOR.se, n.h = n.h, orss = yules.q, orss.se = ORSS.se, 
              eds = eds, eds.se = eds.se, seds = seds, seds.se = seds.se, 
              EDI = EDI, EDI.se = EDI.se, SEDI = SEDI, SEDI.se = SEDI.se))
}

  
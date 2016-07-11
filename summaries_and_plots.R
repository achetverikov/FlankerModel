
### Descriptives

sim_data[[1]][[2]][,list(acc=mymean(accuracy), RTs=mymean(RTs), respN=mymean(respN),.N),keyby=congruency]

lapply(sim_data, function (x) mean(x$acc, na.rm = T))
lapply(sim_data, function (x) aggregate(rts ~ cong, x[[2]],FUN = mean))
lapply(sim_data, function (x) aggregate(acc ~ cong,x, FUN = mean))
lapply(sim_data, function (x) table(is.na(x[[2]]$ecr)))
lapply(sim_data, function (x) mean(x[[2]]$respN))


# flanker errors

fe <- function(x){
  flanker_errors <- ifelse(x$acc == 0 & sapply(names(x$acc), FUN = function(y) strsplit(y,'')[[1]][1]) == x$resp, 1, 0)
}

lapply(sim_data, function (x) mean(fe(x)))


# energies
# conflicts comparisons in correct and erroneous trials

lapply(sim_data, function (x) mean(colMeans(x$e[x$acc,])))
lapply(sim_data, function (x) mean(colMeans(x$e[!x$acc,])))



# conflicts comparisons in correct and erroneous trials
mean(colMeans(energies[!1 & accuracy,]))
mean(colMeans(energies[!accuracy,!1]))

plot(colMeans(energies[!accuracy & congruency == 'incompatible',], na.rm=T), type = 'l', ylim = c(0, 0.1), col = 2)
points(colMeans(energies[!accuracy & congruency == 'compatible',], na.rm=T),type = 'l', col = 3)
points(colMeans(energies[accuracy & congruency == 'incompatible',], na.rm=T),type = 'l', lty = 2, col = 2)
points(colMeans(energies[accuracy & congruency == 'compatible',], na.rm=T),type = 'l', lty = 2, col = 3)

legend("topright", legend = c('Compatible, correct','Incompatible, correct','Compatible, incorrect','Incompatible, incorrect'), col=c(3:2, 3:2), pch=19,lty = c(2,2,1,1))
grid.draw.np<-function (x){
  grid.newpage()
  grid.draw(x)
}

for (i in 1:length(sim_data)){
  energies <- sim_data[[i]][[3]]
  stimuli_sequence  <- sim_data[[i]][[2]]
  energies_dt<-data.table(melt(energies))
  
  setnames(energies_dt,old=c('Var1', 'Var2' ,'value'),new= c('trialN','cycleN','energy'))
  setorder(energies_dt,trialN,cycleN)
  energies_dt<-merge(stimuli_sequence, energies_dt, by='trialN')
  energies_dt[,resp_lockedN:=cycleN-respN]
  setkeyv(energies_dt,c('trialN','resp_lockedN'))
  energies_dt<-energies_dt[!is.na(accuracy)]
  
  fillup_dt<-data.table(expand.grid(trialN=1:n_trials, resp_lockedN=-50:50))
  fillup_dt[,energy_filler:=0]
  setkeyv(fillup_dt,c('trialN','resp_lockedN'))
  
  energies_dt<-energies_dt[fillup_dt, roll=T,rollends=c(T,T)]
  
  # energies_dt[,respN:=min(respN, na.rm=T),by=list(trialN)]
  # energies_dt[,respN:=min(respN, na.rm=T),by=list(trialN)]
  # # energies_dt[,rN:=rep(respN, each=54)]
  # energies_dt[,acc:=factor(rep(accuracy, each=54))]
  # energies_dt[,congr:=relevel(factor(rep(congruency, each=54)), ref='incompatible')]
  
  p1 <- qplot(data=data.frame(energies_dt[!is.na(respN),list(energy=mymean(energy)), by=list(resp_lockedN,congruency,accuracy)]), x=resp_lockedN,y=energy, linetype=factor(accuracy), color=congruency, geom='line', size=I(1))+scale_color_grey()+theme_minimal() + ggtitle('R-locked')+scale_x_continuous(breaks=seq(-50,50,by=10))
  p2 <- qplot(data=data.frame(energies_dt[!is.na(respN),list(energy=mymean(energy)), by=list(cycleN,congruency,accuracy)]), x=cycleN,y=energy, linetype=factor(accuracy), color=congruency, geom='line', size=I(1))+scale_color_grey()+theme_minimal()+scale_x_continuous(breaks=seq(0,50,by=10)) + ggtitle('S-locked')
  p3 <- qplot(data=data.frame(energies_dt[!is.na(respN),list(energy=mymean(energy)), by=list(resp_lockedN,accuracy)]), x=resp_lockedN,y=energy, linetype=factor(accuracy),  geom='line', size=I(1))+scale_color_grey()+theme_minimal()+scale_x_continuous(breaks=seq(-50,50,by=10)) + ggtitle('R-locked')
  p4 <- qplot(data=data.frame(energies_dt[!is.na(respN),list(energy=mymean(energy)), by=list(cycleN,accuracy)]), x=cycleN,y=energy, linetype=factor(accuracy),  geom='line', size=I(1))+scale_color_grey()+theme_minimal()+scale_x_continuous(breaks=seq(0,50,by=10)) + ggtitle('S-locked')
  
  assign(paste('plot', i, '1', sep="_"), p1)
  assign(paste('plot', i, '2', sep="_"), p2)
  assign(paste('plot', i, '3', sep="_"), p3)
  assign(paste('plot', i, '4', sep="_"), p4)
  assign(paste('plot', i, 'c', sep="_"), rbind.gtable(cbind.gtable(ggplotGrob(p1+theme(legend.position='none')),ggplotGrob(p2)),cbind.gtable(ggplotGrob(p3+theme(legend.position='none')),ggplotGrob(p4))))
  
}

grid.draw.np(plot_1_c)

ggsave('outputs/2_flanker_sim.png', plot_1_c, dpi=320, width=9, height=7,limitsize = F, type = "cairo-png")




#
# Error signalling according to RM theory
#

RT_ser_RM_fun <- function(x){
  source('first.R',local = TRUE)
  T_cycle * (x$ecr - x$respn) + T_nd3
}
RT_ser_RM <- lapply(sim_data, function (x) mean(RT_ser_RM_fun(x), na.rm = T))
RT_ser_RM


P_esr_RM <- lapply(sim_data, function (x) table(is.na(x$ecr))[1])

#
# Error signalling according to CM theory
#

sim_data[[1]]$e[666,]

E_cum_fun <- function(x, Threshold = 0.005, Delay = 6){
  
  source('first.R',local = TRUE)
  
  K = Threshold
  D = Delay
  
  esr_CM <- numeric(n_trials)
  RT_esr_CM <- numeric(n_trials)
  
  for (i in 1:n_trials){
    
    if (!is.na(x$respn[i]) & x$respn[i] + D < 54){
      z = x$respn[i] + D # cycle at which conflict cumulation starts
      while (z < 54 & sum(x$e[i,(x$respn[i]+D) : z]) <= K){
        z = z+1
      }
      
      esr_CM[i] <- (sum(x$e[i,(x$respn[i]+D) : 54]) > K)
      # RT_esr_CM[i] <- ifelse(esr_CM[i] == 1, T_cycle * (z - x$respn[i]) + T_nd3, NA)
      RT_esr_CM[i] <- ifelse(esr_CM[i] == 1, (z - x$respn[i]), NA) # cycles instead of ms
    } else {
      esr_CM[i] <- FALSE
      # RT_esr_CM[i] <- ifelse(esr_CM[i] == 1, T_cycle * (z - x$respn[i]) + T_nd3, NA)
      RT_esr_CM[i] <- NA # cycles instead of ms
    }
    
  }
  return(list(esr_CM, RT_esr_CM))
}

P_esr_CM <- lapply(sim_data, function (x) sum(E_cum_fun(x)[[1]][x$cong != 'compatible']))
RT_esr_CM <- lapply(sim_data, function (x) mean(E_cum_fun(x)[[2]][x$cong != 'compatible'], na.rm = T))

RT_esr_CM_high_K <- RT_esr_CM
RT_esr_CM_low_K <- RT_esr_CM

RT_esr_CM_high_K_D0 <- RT_esr_CM
RT_esr_CM_low_K_D0 <- RT_esr_CM

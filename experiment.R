#
# Experiment
#

#setwd("C:/Users/spaladin/OneDrive/Models/Flanker Model/3 response flanker")
# setwd("C:/Users/st001825/OneDrive/Models/Flanker Model/3 response flanker")
# setwd("D:/OneDrive/Flanker Model/3 response flanker")
library(parallel)
library(doParallel)
library(data.table)
library(ggplot2)
library(apastats)
library(grid)
library(gridExtra)
cl <- makeCluster(4)
registerDoParallel(cl)

n_trials_in_block = 500
n_blocks = 4
n_trials = n_blocks*n_trials_in_block

pb = txtProgressBar(min = 0, max = n_trials, initial = 0, style=3) 

sim_data<-foreach (criterion=seq(0.18, 0.18, 0.2), .packages=c('data.table')) %do% {
  
  print(criterion)
  # set stimulus
  stimuli_resp<-data.table(target=c('P', 'R', 'M', 'V', 'W', 'X', '&', '#', '%', '$'))
  stimuli_resp[,target_resp:=car::recode(target, "c('P','R')=1;c('M','V')=2;c('W','X')=3;c('&', '#', '%', '$')=0")]
  # 
  
  stimuli_resp<-data.table(target=c('H', 'S'))
  stimuli_resp[,target_resp:=car::recode(target, "c('H')=1;c('S')=2")]
  # 
  stimuli_resp<-setkey(stimuli_resp[,c(k=1,.SD)],k)[stimuli_resp[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL] #pairwise combinations
  names(stimuli_resp)[3:4]<-c('flanker','flanker_resp')
  stimuli_resp<-stimuli_resp[target_resp!=0&!(flanker_resp==target_resp&target!=flanker)]
  stimuli_resp[,stimuli:=paste0(flanker,target,flanker)]
  stimuli_resp[,congruency:=ifelse(target==flanker, 'compatible',ifelse(flanker_resp==0,'neutral','incompatible'))]
  # set needed variables
  stimuli_sequence <- stimuli_resp[sample(nrow(stimuli_resp), n_trials, replace = T)]
  stimuli_sequence[,block:=rep(1:n_blocks, each=n_trials_in_block)]
  
  sim_res_by_block<-foreach (cur_block = 1:n_blocks, .combine=function (a, b) list(rbind(a[[1]], b[[1]]),rbind(a[[2]], b[[2]])),.packages=c('data.table','apastats'), .export=c('stimuli_resp')) %dopar%{
    source('stimulus_code_function.R', local = T)
    responses <- c()
    RTs <- numeric(n_trials_in_block)
    respN <- numeric(n_trials_in_block)
    error_corrections <- numeric(n_trials_in_block)
    energies <- matrix(ncol = 54, nrow = n_trials_in_block)
    energies[1:n_trials_in_block,1:54] <- 0
    
    attention_c_act <- numeric(n_trials_in_block)
    total_energy <- numeric(n_trials_in_block)
    
    cur_stimuli_sequence<-stimuli_sequence[block==cur_block]
    # run experiment
    for (z in 1:n_trials_in_block){
      #setTxtProgressBar(pb,z)
      stimulus <- cur_stimuli_sequence[z,stimuli]
      
      source('first.R',local = T)
      #print(stimulus_layer)
      C<-0.18#criterion
      
      source('cycle.R',local = T)
      responses[z] <- first_response
      RTs[z] <- RT_first
      error_corrections[z] <- n_corr
      energies[z,] <- E
      respN[z]<-n_first
      
      # attention modulation
      total_energy[z] <- sum(energies[z, ])
      attention_c_act[z+1] <- gaMMa * attention_c_act[z] + ((1 - gaMMa)*(alpha * total_energy[z] + beta))
      
      
      
      
    }
    cur_stimuli_sequence[,response:=responses]
    cur_stimuli_sequence[,accuracy:=as.numeric(target_resp == response)]
    cur_stimuli_sequence[,respN:=respN] 
    cur_stimuli_sequence[,RTs:=RTs] 
    cur_stimuli_sequence[,total_energy:=total_energy]
    cur_stimuli_sequence[,attention_c_act:=attention_c_act[1:n_trials_in_block]]
    cur_stimuli_sequence[,error_corrections:=error_corrections] 
    list(cur_stimuli_sequence, e=energies)
  }
  stimuli_sequence <- sim_res_by_block[[1]]
  energies <- sim_res_by_block[[2]]
  stimuli_sequence[,trialN:=.I]
  
  list(C, stimuli_sequence, energies)
}


############ RUN CODE UNTIL HERE TO SIMULATE n_trials TRIALS ############


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
  # energies_dt[,rN:=rep(respN, each=54)]
  # energies_dt[,acc:=factor(rep(accuracy, each=54))]
  # energies_dt[,congr:=relevel(factor(rep(congruency, each=54)), ref='incompatible')]
  
  p1 <- qplot(data=data.frame(energies_dt[!is.na(accuracy),list(energy=mymean(energy)), by=list(cycleN=cycleN-respN,congruency,accuracy)]), x=cycleN,y=energy, linetype=factor(accuracy), color=congruency, geom='line', size=I(1))+scale_color_grey()+theme_minimal() + ggtitle('R-locked')
  p2 <- qplot(data=data.frame(energies_dt[!is.na(accuracy),list(energy=mymean(energy)), by=list(cycleN,congruency,accuracy)]), x=cycleN,y=energy, linetype=factor(accuracy), color=congruency, geom='line', size=I(1))+scale_color_grey()+theme_minimal()+scale_x_continuous(breaks=seq(0,50,by=10)) + ggtitle('S-locked')
  p3 <- qplot(data=data.frame(energies_dt[!is.na(accuracy),list(energy=mymean(energy)), by=list(cycleN=cycleN-respN,accuracy)]), x=cycleN,y=energy, linetype=factor(accuracy),  geom='line', size=I(1))+scale_color_grey()+theme_minimal()+scale_x_continuous(breaks=seq(0,50,by=10)) + ggtitle('R-locked')
  p4 <- qplot(data=data.frame(energies_dt[!is.na(accuracy),list(energy=mymean(energy)), by=list(cycleN,accuracy)]), x=cycleN,y=energy, linetype=factor(accuracy),  geom='line', size=I(1))+scale_color_grey()+theme_minimal()+scale_x_continuous(breaks=seq(0,50,by=10)) + ggtitle('S-locked')
  
  assign(paste('plot', i, '1', sep="_"), p1)
  assign(paste('plot', i, '2', sep="_"), p2)
  assign(paste('plot', i, '3', sep="_"), p3)
  assign(paste('plot', i, '4', sep="_"), p4)
  assign(paste('plot', i, 'c', sep="_"), rbind.gtable(cbind.gtable(ggplotGrob(p1+theme(legend.position='none')),ggplotGrob(p2)),cbind.gtable(ggplotGrob(p3+theme(legend.position='none')),ggplotGrob(p4))))
  
}

grid.draw.np(plot_1_c)
plot_1_1
plot_1_2
plot_1_3
plot_1_4

plot_5_1
plot_5_2
plot_5_3
plot_5_4



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

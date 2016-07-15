#
# Experiment
#

#setwd("C:/Users/spaladin/OneDrive/Models/Flanker Model/3 response flanker")
# setwd("C:/Users/st001825/OneDrive/Models/Flanker Model/3 response flanker")
# setwd("D:/OneDrive/Flanker Model/3 response flanker")
library(apastats)
library(parallel)
library(Cairo)
library(doParallel)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(sampling)

rm(list=ls())

cl <- makeCluster(4)
registerDoParallel(cl)

n_trials_in_block = 3000
n_blocks = 4
n_trials = n_blocks*n_trials_in_block

congruency_balanced = F # should the number of trials be balanced in respect to congruency levels?
congruency_levels <- c('incompatible','compatible','neutral') # congruency levels to include

# set stimuli
# uncomment these strings to simulate three-response flanker task
# stimuli_resp<-data.table(target=c('P', 'R', 'M', 'V', 'W', 'X', '&', '#', '%', '$'))
# stimuli_resp[,target_resp:=car::recode(target, "c('P','R')=1;c('M','V')=2;c('W','X')=3;c('&', '#', '%', '$')=0")]
# # 

# uncomment these strings to simulate two-response flanker task
stimuli_resp<-data.table(target=c('H', 'S'))
stimuli_resp[,target_resp:=car::recode(target, "c('H')=1;c('S')=2")]
# 
# # 
# stimuli_resp<-data.table(target=c('S','D','K','L','0'))
# stimuli_resp[,target_resp:=car::recode(target, "c('S')=1;c('D')=2;c('K')=3;'L'=4;'0'=0")]

stimuli_resp<-setkey(stimuli_resp[,c(k=1,.SD)],k)[stimuli_resp[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL] #pairwise combinations
names(stimuli_resp)[3:4]<-c('flanker','flanker_resp')
stimuli_resp<-stimuli_resp[target_resp!=0&!(flanker_resp==target_resp&target!=flanker)]

stimuli_resp[,stimuli:=paste0(flanker,target,flanker)]
stimuli_resp[,congruency:=ifelse(target==flanker, 'compatible',ifelse(flanker_resp==0,'neutral','incompatible'))]

stimuli_resp<-stimuli_resp[congruency %in% congruency_levels] 


sim_data<-foreach (criterion=seq(0.18, 0.18, 0.2), .packages=c('data.table','sampling')) %do% {
  
  print(criterion)
  if (congruency_balanced==T){
    rows_to_sample<-sampling::strata(stimuli_resp, stratanames = 'congruency', size=rep(n_trials/lengthu(stimuli_resp$congruency),lengthu(stimuli_resp$congruency)), method='srswr') # to make congruency balanced 
  } else{
    rows_to_sample<-sampling::strata(stimuli_resp, stratanames = 'congruency', size=prop.table(table(stimuli_resp$congruency))*n_trials, method='srswr') 
  }
  
  stimuli_sequence<-stimuli_resp[sample(rows_to_sample$ID_unit)]
  stimuli_sequence[,block:=rep(1:n_blocks, each=n_trials_in_block)]
  
  sim_res_by_block<-foreach (cur_block = 1:n_blocks, .combine=function (a, b) list(rbind(a[[1]], b[[1]]),rbind(a[[2]], b[[2]])),.packages=c('data.table','apastats'), .export=c('stimuli_resp')) %do%{
    source('stimulus_code_function.R', local = T)
    responses <- c()
    RTs <- numeric(n_trials_in_block)
    respN <- numeric(n_trials_in_block)
    error_corrections <- numeric(n_trials_in_block)
    error_corrections_rts <- numeric(n_trials_in_block)
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
      error_corrections_rts[z] <- RT_ecr
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
    cur_stimuli_sequence[,error_corrections_rts:=error_corrections_rts] 
    list(cur_stimuli_sequence, e=energies)
  }
  stimuli_sequence <- sim_res_by_block[[1]]
  energies <- sim_res_by_block[[2]]
  stimuli_sequence[,trialN:=.I]
  
  list(C, stimuli_sequence, energies)
}


############ RUN CODE UNTIL HERE TO SIMULATE n_trials TRIALS ############

#save('sim_data',file = 'outputs/4resp_flanker_6000_trials_per_condition_SDKL.RData')
#save('sim_data',file = 'outputs/2resp_flanker_300_trials_per_condition_SDKL.RData')
#save('sim_data',file = 'outputs/2resp_flankers_6000_trials_per_cond.RData')
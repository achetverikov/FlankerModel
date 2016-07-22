#
# Experiment
#

library(apastats) # available from https://github.com/ralfer/apa_format_and_misc - used for helper functions
library(parallel)
library(Cairo)
library(doParallel)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(sampling)

rm(list=ls()) # clean the workspace

cl <- makeCluster(4) # number of slaves for parallel computation
registerDoParallel(cl)

n_trials_in_block = 3000 # trials are split into blocks for parallel processing
n_blocks = 4
n_trials = n_blocks*n_trials_in_block

congruency_balanced = T # should the number of trials be balanced in respect to congruency levels?
congruency_levels <- c('incompatible','compatible','neutral') # congruency levels to include

criterion_values <- c(0.18) # you can set more than one value of criterion (response threshold)

# set stimuli
# uncomment these strings to simulate three-response flanker task
# stimuli_resp<-data.table(target=c('P', 'R', 'M', 'V', 'W', 'X', '&', '#', '%', '$'))
# stimuli_resp[,target_resp:=car::recode(target, "c('P','R')=1;c('M','V')=2;c('W','X')=3;c('&', '#', '%', '$')=0")]


# uncomment these strings to simulate two-response flanker task
# stimuli_resp<-data.table(target=c('H', 'S'))
# stimuli_resp[,target_resp:=car::recode(target, "c('H')=1;c('S')=2")]

# uncomment these strings to simulate four-response flanker task
stimuli_resp<-data.table(target=c('S','D','K','L','0'))
stimuli_resp[,target_resp:=car::recode(target, "c('S')=1;c('D')=2;c('K')=3;'L'=4;'0'=0")]

stimuli_resp<-setkey(stimuli_resp[,c(k=1,.SD)],k)[stimuli_resp[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL] #pairwise combinations
names(stimuli_resp)[3:4]<-c('flanker','flanker_resp')

# stimuli cannot include trials with different target and flankers but with the same response
stimuli_resp<-stimuli_resp[target_resp!=0&!(flanker_resp==target_resp&target!=flanker)]

stimuli_resp[,stimuli:=paste0(flanker,target,flanker)]
stimuli_resp[,congruency:=ifelse(target==flanker, 'compatible',ifelse(flanker_resp==0,'neutral','incompatible'))]

stimuli_resp<-stimuli_resp[congruency %in% congruency_levels] 

sim_data<-foreach (criterion=criterion_values, .packages=c('data.table','sampling')) %do% {
  
  # sample trial conditions from all possible conditions
  
  if (congruency_balanced==T){
    rows_to_sample<-sampling::strata(stimuli_resp, stratanames = 'congruency', size=rep(n_trials/lengthu(stimuli_resp$congruency),lengthu(stimuli_resp$congruency)), method='srswr') # to make congruency balanced 
  } else{
    rows_to_sample<-sampling::strata(stimuli_resp, stratanames = 'congruency', size=prop.table(table(stimuli_resp$congruency))*n_trials, method='srswr') 
  }
  
  stimuli_sequence<-stimuli_resp[sample(rows_to_sample$ID_unit)]
  stimuli_sequence[,block:=rep(1:n_blocks, each=n_trials_in_block)]
  
  # split by block, run each block in parallel, combine results
  sim_res_by_block<-foreach (cur_block = 1:n_blocks, .combine=function (a, b) list(rbind(a[[1]], b[[1]]),rbind(a[[2]], b[[2]])),.packages=c('data.table','apastats'), .export=c('stimuli_resp')) %dopar%{
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
      stimulus <- cur_stimuli_sequence[z,stimuli]
      
      # set initial conditions
      source('first.R',local = T)
      C <- criterion
      
      # run simulations
      source('cycle.R',local = T)
      
      # save data for later
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
    
    # merge results with trial conditions
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


# the results are saved to sim_data object that can be stored for later processing in outputs folder
# fname <- 'enter name'
# save(sim_data, file=paste0('outputs/', fname,'.RData'))
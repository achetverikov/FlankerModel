#
# cycle
#

# helper functions 
sum_pos<-function (x,...){
  sum(x[x>0],...)
}

pos_only<-function (x){
  x[x>0]
}



for(n in 2:stop_cycle){ # n cycles in one trial

#################### external input #######################

  #cycles 2-4 are preparatory - ext. input only for R, attention not activated

  if (n<5){
    extR <- 0.03 # external input to response layer in preparatoty cycles
    extS <- 0
    extA[n,] <- c(0,0,0)
    # Uncomment the following two lines to make Flanker task Module closer to that of Cohen, Servan-Schreiber & McClelland (1992)
    # extA[n,'C'] <- attention_c_act[z]
    # extA[n,'C'] <- ifelse(extA[n,'C'] > 3, 3, ifelse(extA[n,'C'] < 1, 1, extA[n,'C']))
  }
  else if (n>=5 & n < stop_input){
    # external inputs to all units
    extR <- 0.03
    extS <- 0.15

    extA[n,'C'] <- attention_c_act[z]
    extA[n,'C'] <- ifelse(extA[n,'C'] > 3, 3, ifelse(extA[n,'C'] < 1, 1, extA[n,'C']))
    extA[n,'L'] <- extA[n,'R'] <-  (3 - extA[n,'C'])/2

  }
  else {
    extR <- 0
    extS <- 0
    extA[n,] <- c(0,0,0)
  }


################### net input calculation ##################################
  ### stimulus layer:

  for (i in 1:ns){
    # compute activation to i'th stimulus unit from all other stimulus units
    stimulus_layer_units <- sum(pos_only(stimulus_act[n-1, -i]) *  wS * sc_i)

    # compute activation to i'th stimulus unit from attention units
    attention_layer_unit <- sum(pos_only(attention_act[n-1, ifelse(i <= ns/3, 1, ifelse(i <= (ns*2/3), 2, 3))]) *  wSA * sc_e)

    # summarize all sourses of stimulus unit inputs
    stimulus_input[n,i] <- stimulus_code(stimulus)[i]*extS*estr + stimulus_layer_units + attention_layer_unit + rnorm(1, mean = 0, sd = s_noise)
  }

  ### attention layer:
  for (i in c('L','C','R')){

    # compute activation to i'th attention unit from all other attention units
    attention_to_attention_inputs[n,i]<-sum(pos_only(attention_act[n-1, names(attention_act)!=i]) *  wA * sc_i)

    # compute activation to i'th attention unit from stimulus units
    stimulus_to_attention_inputs[n,i]<-sum(pos_only(stimulus_act[n-1,  grepl(i, colnames(stimulus_act))]) *  wSA * sc_e)

    # summarize all sourses of attention unit inputs
    attention_input[n,i] <-extA[n,i]*estr +attention_to_attention_inputs[n,i]+stimulus_to_attention_inputs[n,i] + rnorm(1, mean = 0, sd = s_noise)
  }
  
  ### response layer:
  for (k in 1:nr){

    # compute activation to k'th response unit from all other response units
    response_layer_units <- sum(pos_only(response_act[n-1, -k]) *  wR * sc_i)

    # compute activation to k'th response unit from stimulus units
    stimulus_layer_units <- sum(pos_only(stimulus_act[n-1, as.character(k) == stimulus_layer_resp]) *  wSR * sc_e)

    # summarize all sourses of stimulus unit inputs
    response_input[n,k] <- extR*estr + response_layer_units + stimulus_layer_units + rnorm(1, mean = 0, sd = s_noise)
  }





####################### activation ##############################

  ### stimulus layer:

  for (i in 1:ns){
    # computing change of unit activation
    if(stimulus_input[n,i] >= 0){
      stimulus_d_act[n,i] <- (act_max - stimulus_act[n-1,i])
    } else {
    	stimulus_d_act[n,i] <- (stimulus_act[n-1,i] - act_min)
    }

    # updating unit activation
    stimulus_act[n,i] <- stimulus_act[n-1,i] + stimulus_d_act[n,i]*stimulus_input[n,i] - (stimulus_act[n-1, i] - act_rest)*decay
    if (stimulus_act[n,i]>act_max) stimulus_act[n,i] <- act_max
    if (stimulus_act[n,i]<act_min) stimulus_act[n,i] <- act_min
  }


  ### attention layer:

  for (i in 1:ncol(attention_input)){
    # computing change of unit activation
    if(attention_input[n,i] >= 0){
      attention_d_act[n,i] <- (act_max - attention_act[n-1,i])
    } else {
		attention_d_act[n,i] <- (attention_act[n-1,i] - act_min)
	}

    # updating unit activation
    attention_act[n,i] <- attention_act[n-1,i] + attention_d_act[n,i]*attention_input[n,i] - (attention_act[n-1, i] - act_rest)*decay
    if (attention_act[n,i]>act_max) attention_act[n,i] <- act_max
    if (attention_act[n,i]<act_min) attention_act[n,i] <- act_min

  }


    ### response layer:

  for (i in 1:ncol(response_layer)){
    # computing change of unit activation
    if(response_input[n,i] >= 0){
      response_d_act[n,i] <- (act_max - response_act[n-1,i])
    } else {
    	response_d_act[n,i] <- (response_act[n-1,i] - act_min)
    }

    # updating unit activation
    response_act[n,i] <- response_act[n-1,i] + response_d_act[n,i]*response_input[n,i] - (response_act[n-1, i] - act_rest)*decay
    if (response_act[n,i]>act_max) response_act[n,i] <- act_max
    if (response_act[n,i]<act_min) response_act[n,i] <- act_min
  }


  # Conflict

  ## "A product of unit activations was set to 0 whenever one of the activations was negative" (Steinhauser et al., 2008)

  #uncomment "if ..." to disable conflict monitoring on preparatory cycles
  #if (n>5){
    response_act_for_e<-response_act[n,]
    response_act_for_e[response_act_for_e<0]<-0
    E_mat <- response_act_for_e%*%t(response_act_for_e) * (-1) * wR
    diag(E_mat) <- 0
    E[n] <- sum(E_mat)
  #}

  # response execution

  if(any(response_act[n,] > C) & is.na(n_first)){
    first_response <- colnames(response_act)[which(response_act[n,] == max(response_act[n,]))]
    RT_first <- T_cycle * n + T_nd1

    n_first <- n

    stop_input <- round(n + rnorm(1, d_stop, s_stop))
  }
    
  # response correction (ECR) - occurs when any new response reachs the threshold after the first one

  if(!is.na(n_first) & n > n_first & any(response_act[n,colnames(response_act) != first_response] > C) & is.na(n_corr)){
    second_response <- colnames(response_act)[which(response_act[n,colnames(response_act) != first_response] == max(response_act[n,colnames(response_act) != first_response]))]

    n_corr <- n
    RT_ecr <- T_cycle * (n_corr - n_first) + T_nd2
  }
}

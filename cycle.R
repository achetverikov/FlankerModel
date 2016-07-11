#
# cycle
#
# this version is based on cycle_ac.R

sum_pos<-function (x,...){
  sum(x[x>0],...)
}

pos_only<-function (x){
  x[x>0]
}



for(n in 2:stop_cycle){

#################### external input #######################

  #cycles 2-4 are preparatory - ext. input only for R

  if (n<5){
    extR <- 0.03
    extS <- 0
    extA[n,] <- c(0,0,0)
    #  extA[n,'C'] <- extAC_next
    #  extA[n,'C'] <- ifelse(extA[n,'C'] > 3, 3, ifelse(extA[n,'C'] < 1, 1, extA[n,'C']))
  }
  else if (n>=5 & n < stop_input){
    extR <- 0.03
    extS <- 0.15

    extA[n,'C'] <- extAC_next
    extA[n,'C'] <- ifelse(extA[n,'C'] > 3, 3, ifelse(extA[n,'C'] < 1, 1, extA[n,'C']))
    extA[n,'L'] <- extA[n,'R'] <-  (3 - extA[n,'C'])/2

  }
  else {
    extR <- 0
    extS <- 0
    extA[n,] <- c(0,0,0)
  }


################### net input ##################################
  ### stimulus layer:

  for (i in 1:ncol(stimulus_input)){
    # stimulus_without_i <- stimulus
    # stimulus_without_i[i] <- 0
    stimulus_layer_units <- sum(pos_only(stimulus_act[n-1, -i]) *  wS * sc_i)

    attention_layer_unit <- sum(pos_only(attention_act[n-1, ifelse(i < 11, 1, ifelse(i < 21, 2, 3))]) *  wSA * sc_e)

    stimulus_input[n,i] <- stimulus_code(stimulus)[i]*extS*estr + stimulus_layer_units + attention_layer_unit + rnorm(1, mean = 0, sd = s_noise)
  }

  ### attention layer:

  attention_layer_unitsC <- sum(pos_only(attention_act[n-1, -2]) *  wA * sc_i)
  attention_layer_unitsL <- sum(pos_only(attention_act[n-1, -1]) *  wA * sc_i)
  attention_layer_unitsR <- sum(pos_only(attention_act[n-1, -3]) *  wA * sc_i)

  stimulus_layer_unitsC <- sum(pos_only(stimulus_act[n-1, ((ns/3)+1): (2*(ns/3))]) *  wSA * sc_e)
  stimulus_layer_unitsL <- sum(pos_only(stimulus_act[n-1, 1:(ns/3)]) *  wSA * sc_e)
  stimulus_layer_unitsR <- sum(pos_only(stimulus_act[n-1, ((2*(ns/3))+1):ns]) *  wSA * sc_e)

  attention_input[n,'C'] <- extA[n,'C']*estr + attention_layer_unitsC + stimulus_layer_unitsC + rnorm(1, mean = 0, sd = s_noise)
  attention_input[n,'L'] <- extA[n,'L']*estr + attention_layer_unitsL + stimulus_layer_unitsL + rnorm(1, mean = 0, sd = s_noise)
  attention_input[n,'R'] <- extA[n,'R']*estr + attention_layer_unitsR + stimulus_layer_unitsR + rnorm(1, mean = 0, sd = s_noise)

  ### response layer:

  for (k in 1:ncol(response_input)){
    response_layer_units <- sum(pos_only(response_act[n-1, -k]) *  wR * sc_i)
    stimulus_layer_units <- sum(pos_only(stimulus_act[n-1, as.character(k) == stimulus_layer_resp]) *  wSR * sc_e)

    response_input[n,k] <- extR*estr + response_layer_units + stimulus_layer_units + rnorm(1, mean = 0, sd = s_noise)
  }





####################### activation ##############################

  ### stimulus layer:
  for (i in 1:ncol(stimulus_input)){
    if(stimulus_input[n,i] >= 0){
      stimulus_d_act[n,i] <- (act_max - stimulus_act[n-1,i])*stimulus_input[n,i] - (stimulus_act[n-1, i] - act_rest)*decay
    } else {
	stimulus_d_act[n,i] <- (stimulus_act[n-1,i] - act_min)*stimulus_input[n,i] - (stimulus_act[n-1, i] - act_rest)*decay
}

    stimulus_act[n,i] <- stimulus_act[n-1,i] + stimulus_d_act[n,i]
    if (stimulus_act[n,i]>act_max) stimulus_act[n,i] <- act_max
    if (stimulus_act[n,i]<act_min) stimulus_act[n,i] <- act_min
  }


  ### attention layer:

  for (i in 1:ncol(attention_input)){
    if(attention_input[n,i] >= 0){
      attention_d_act[n,i] <- (act_max - attention_act[n-1,i])*attention_input[n,i] - (attention_act[n-1, i] - act_rest)*decay
    } else {
		attention_d_act[n,i] <- (attention_act[n-1,i] - act_min)*attention_input[n,i] - (attention_act[n-1, i] - act_rest)*decay
	}

    attention_act[n,i] <- attention_act[n-1,i] + attention_d_act[n,i]
    if (attention_act[n,i]>act_max) attention_act[n,i] <- act_max
    if (attention_act[n,i]<act_min) attention_act[n,i] <- act_min

  }


    ### response layer:

  for (i in 1:ncol(response_layer)){
    if(response_input[n,i] >= 0){
      response_d_act[n,i] <- (act_max - response_act[n-1,i])*response_input[n,i] - (response_act[n-1, i] - act_rest)*decay
    } else {
	response_d_act[n,i] <- (response_act[n-1,i] - act_min)*response_input[n,i] - (response_act[n-1, i] - act_rest)*decay
}

    response_act[n,i] <- response_act[n-1,i] + response_d_act[n,i]
    if (response_act[n,i]>act_max) response_act[n,i] <- act_max
    if (response_act[n,i]<act_min) response_act[n,i] <- act_min
  }


  # Conflict

  ## A product of unit activations was set to 0 whenever one of the activations was negative.
  
  response_act_for_e<-response_act[n,]
  response_act_for_e[response_act_for_e<0]<-0
  E_mat <- response_act_for_e%*%t(response_act_for_e) * (-1) * wR
  #diag(E_mat) <- 0
  E[n] <- sum(E_mat)
  #E[n] <- ifelse(E[n] < 0, 0, E[n])



  # response execution

  if(any(response_act[n,] > C) & is.na(n_first)){
    first_response <- colnames(response_act)[which(response_act[n,] == max(response_act[n,]))]
    RT_first <- T_cycle * n + T_nd1

    n_first <- n

    stop_input <- round(n + rnorm(1, d_stop, s_stop))
  }
#   In the model of Yeung et al. (2004), Tcycle
#   and TND1 were set to 16 ms and 200 ms, respectively. In our
#   study, these parameters were estimated from the data to obtain an optimal fit.

  # response correction (ECR)

  if(!is.na(n_first) & n > n_first & any(response_act[n,colnames(response_act) != first_response] > C) & is.na(n_corr)){
    second_response <- colnames(response_act)[which(response_act[n,colnames(response_act) != first_response] == max(response_act[n,colnames(response_act) != first_response]))]

    n_corr <- n

    RT_ecr <- T_cycle * (n_corr - n_first) + T_nd2

  }


}

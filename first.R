#
# Flanker task computational model
#
source('stimulus_code_function.R')

# parameters
extS <- 0.15
extR <- 0.03
estr <- 0.4
sc_e <- 0.08
sc_i <- 0.12
s_noise <- 0.035
alpha <- 4.41
beta <- 1.08
gaMMa <- 0.5
act_min <- -0.2
act_max <- 1
act_rest <- -0.1
decay <- 0.1
C <- 0.18 # response criterion
wSR <- 1.5
wSA <- 2
wS <- -2
wR <- -3
wA <- -1
d_stop <- 6
s_stop <- 0.5
T_cycle <- 16
T_nd1 <- 200
T_nd2 <- 200
T_nd3 <- 200
D <- 6 #0
K <- 0.005 #0.0001

stop_cycle <- 54
stop_input <- stop_cycle # by default, input continues until the end of the trial unless response is made (see cycle.R)
n_first <- NA
first_response <- NA
n_corr <- NA

#network layers

  ### Net inputs were initialized to zero at the start of each trial

stimulus_layer <- matrix(data = 0, ncol = stimuli_resp[!duplicated(paste(flanker_resp, flanker)),.N]*3, nrow = stop_cycle)

colnames(stimulus_layer) <- apply(expand.grid(c('H','S'),c('L','C','R')), 1, paste, collapse=".")# rep(stimuli_resp[!duplicated(paste(flanker_resp, flanker)),flanker], 3)
stimulus_layer_resp <- as.character(rep(stimuli_resp[!duplicated(paste(flanker_resp, flanker)),flanker_resp], 3))

response_layer <- matrix(0, ncol = stimuli_resp[,lengthu(target_resp)], nrow = stop_cycle)
colnames(response_layer) <-  stimuli_resp[,unique(target_resp)]

attention_layer <- matrix(0, ncol = 3, nrow = stop_cycle)
colnames(attention_layer) <- c('L', 'C', 'R')

nr <- ncol(response_layer)
ns <- ncol(stimulus_layer)

# activations
response_act <- response_layer
stimulus_act <- stimulus_layer
attention_act <- attention_layer

# inputs
response_input <- response_layer
stimulus_input <- stimulus_layer
attention_input <- attention_layer

attention_to_attention_inputs <- attention_layer
stimulus_to_attention_inputs <- attention_layer

# d_act's
response_d_act <- response_layer
stimulus_d_act <- stimulus_layer
attention_d_act <- attention_layer

# external Attention information
extA <- matrix(0, ncol = 3, nrow = stop_cycle)
colnames(extA) <- c('L', 'C', 'R')

# conflict
E <- rep(0,stop_cycle)


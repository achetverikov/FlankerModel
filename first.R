#
# Flanker task computational model
#

source('stimulus_code_function.R')

# parameters
extS <- 0.15  # external input to stimulus layer
extR <- 0.03  # external input to response layer
estr <- 0.4   # scaling parameter
sc_e <- 0.08  # excitatory scaling parameter
sc_i <- 0.12  # inhibitory scaling parameter
s_noise <- 0.035  # normally distributed noise
alpha <- 4.41
beta <- 1.08
gaMMa <- 0.5
act_min <- -0.2 # minimum activation
act_max <- 1 # maximum activation
act_rest <- -0.1 # resting activation
decay <- 0.1
C <- 0.18 # response criterion
wSR <- 1.5  # feedforward excitatory connections between the stimulus and response layer
wSA <- 2  # bidirectional connections between the stimulus and attention layer
wS <- -2  # inhibitory connections in stimulus layer
wR <- -3  # inhibitory connections in response layer
wA <- -1  # inhibitory connections in attention layer
d_stop <- 6 # mean for number of cycles to stop external input to all units after reaching a Criterion in one of the response units
s_stop <- 0.5 # standart deviation for d_stop
T_cycle <- 16 # duration of a cycle in milliseconds
T_nd1 <- 200  # nondecisional time constant, which includes the duration of perceptual and motor processes related to the produc- tion of the first overt response
T_nd2 <- 200  # nondecisional component comprising processes related to the initiation and execution of the overt correction response
T_nd3 <- 200  # duration of processes related to the initiation and execution of the ESR-RM
D <- 6  # delay after which conflict accumulation starts following the initial response
# D <- 0  # uncomment this line to make delay = 0
K <- 0.005  # detection threshold K
# K <- 0.0001 # uncomment this line to make K = 0

stop_cycle <- 54  # the last possible cycle in trial
stop_input <- stop_cycle # by default, input continues until the end of the trial unless response is made (see cycle.R)
n_first <- NA  # cycle at which the first response was executed
first_response <- NA
n_corr <- NA  # cycle at which the internal response unit exceeds the threshold for the second time
RT_first<-NA  # latency of the first response
RT_ecr<-NA # latency of an overt correction response, error correction response (ECR)
#network layers

  ### Net inputs were initialized to zero at the start of each trial

# stimulus layer initialization
stimulus_layer <- matrix(data = 0, ncol = stimuli_resp[!duplicated(paste(flanker_resp, flanker)),.N]*3, nrow = stop_cycle)

colnames(stimulus_layer) <- apply(expand.grid(unique(stimuli_resp$flanker),c('L','C','R')), 1, paste, collapse=".")
stimulus_layer_resp <- as.character(rep(stimuli_resp[!duplicated(paste(flanker_resp, flanker)),flanker_resp], 3))

# response layer initialization
response_layer <- matrix(0, ncol = stimuli_resp[,lengthu(target_resp)], nrow = stop_cycle)
colnames(response_layer) <-  unique(stimulus_layer_resp[stimulus_layer_resp!=0])

# attention layer initialization
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

# d_act's: activations for update in next cycle
response_d_act <- response_layer
stimulus_d_act <- stimulus_layer
attention_d_act <- attention_layer

# external Attention information
extA <- matrix(0, ncol = 3, nrow = stop_cycle)
colnames(extA) <- c('L', 'C', 'R')

# conflict
E <- rep(0,stop_cycle)


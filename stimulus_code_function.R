
# Function for translating human readable stimulus (e.g. HSH) to units activation vector (e.g. 011001)

stimulus_code <- function(stimulus){
  n_units_in_layer<-ncol(stimulus_layer)
  flanker_letters <- unlist(strsplit(stimulus, ''))
  left_side <- paste0(flanker_letters[1],'.L') == colnames(stimulus_layer)[1:(n_units_in_layer/3)]
  center <- paste0(flanker_letters[2],'.C') == colnames(stimulus_layer)[(n_units_in_layer/3+1):(n_units_in_layer/3*2)]
  right_side <- paste0(flanker_letters[3],'.R') == colnames(stimulus_layer)[(n_units_in_layer*2/3+1):(n_units_in_layer)]
  return(c(left_side, center, right_side))
}



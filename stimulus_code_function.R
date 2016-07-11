
stimulus_code <- function(stimulus){
  n_units_in_layer<-ncol(stimulus_layer)
  flanker_letters <- unlist(strsplit(stimulus, ''))
  left_side <- flanker_letters[1] == colnames(stimulus_layer)[1:(n_units_in_layer/3)]
  center <- flanker_letters[2] == colnames(stimulus_layer)[(n_units_in_layer/3+1):(n_units_in_layer/3*2)]
  right_side <- flanker_letters[3] == colnames(stimulus_layer)[(n_units_in_layer*2/3+1):(n_units_in_layer)]
  return(c(left_side, center, right_side))
}



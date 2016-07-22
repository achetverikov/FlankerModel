# Flanker Model

This is the code for simulating conflict-monitoring and response-monitoring networks in the Eriksen flanker task (Eriksen & Eriksen, 1974) described in Yeung, Botvinick, & Cohen (2004) and Steinhauser, Maier, & Hübner (2008).

The main files are:

- experiment.R - runs the simulations

- first.R - governs the initialization

- cycle.R - governs the network update

- generate_outputs.R - generate outputs from the simulations results (after they are saved in outputs folder)

- summaries_and_plots.Rmd - template for outputs

Simulated data along with descriptive statistics and plots are saved in "outputs" folder.

The detailed description of the networks can be found in:

Steinhauser, M., Maier, M. E., & Hübner, R. (2008). Modeling behavioral measures of error detection in choice tasks: response monitoring versus conflict monitoring. Journal of Experimental Psychology: Human Perception and Performance, 34(1), 158–176. http://doi.org/10.1037/0096-1523.34.1.158

Yeung, N., Botvinick, M. M., & Cohen, J. D. (2004). The Neural Basis of Error Detection: Conflict Monitoring and the Error-Related Negativity. Psychological Review, 111(4), 931–959. http://doi.org/10.1037/0033-295X.111.4.931

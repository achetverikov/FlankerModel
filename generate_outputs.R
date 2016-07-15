library(tools)
library(knitr)
library(rmarkdown)
library(apastats)
library(parallel)
library(Cairo)
library(doParallel)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(sampling)
library(tables)


#for (ext in c('html','word')){
for (i in list.files('outputs','.RData',full.names = T)){
  print(i)
  prefix <- file_path_sans_ext(basename(i))
  env = new.env()
  assign('fname',i, env)
  #assign('sim_data', sim_data, envir = env)
  rmarkdown::render('summaries_and_plots.Rmd',output_dir	= 'outputs', output_file = paste0('outputs/',prefix,c('.html','.docx')), output_format=c('html_document', 'word_document'),output_options = list(keep_md = T), envir=env, quiet=F)
  
}
#}
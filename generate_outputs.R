library(tools)
library(knitr)
library(rmarkdown)
library(apastats)
library(Cairo)
library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
library(tables)

rm(list=ls())

# generate html&docx outputs for each of .RData objects in outputs folder
for (i in list.files('outputs','.RData',full.names = T)){
  print(i)
  prefix <- file_path_sans_ext(basename(i))
  env = new.env()
  assign('fname',i, env)
  assign('prefix',prefix, env)
  rmarkdown::render('summaries_and_plots.Rmd',output_dir	= 'outputs', output_file = paste0('outputs/',prefix,c('.html','.docx')), output_format=c('html_document', 'word_document'),output_options = list(keep_md = T), envir=env, quiet=F)
  
}
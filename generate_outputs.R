library(tools)
library(knitr)
library(rmarkdown)

fig_width=7
opts_chunk$set(results='hide',highlight=F, cache=F, tidy=F, echo=F, message=F, warning=F, comment=NA, external=F,split=F, fig.width=fig_width, fig.height=fig_width, dpi=320, dev='png', dev.args = list(type='cairo'))

for (i in list.files('outputs','.RData',full.names = T)){
  load(i)
  prefix <- file_path_sans_ext(basename(i))
  opts_chunk$set(fig.path=paste0('outputs/figures/',prefix))
  render('summaries_and_plots.Rmd',output_dir	= 'outputs',output_file=prefix,output_format = 'html_document',output_options	
=list(keep_md	=T))

  
  print(prefix)
}
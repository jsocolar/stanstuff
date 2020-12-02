parsummarise_draws <- function(draws, n_cores, n_chunks){
  npars <- dim(draws)[3]
  chunk_size <- ceiling(npars/n_chunks)
  chunk_list <- list()
  for(i in 1:n_chunks){
    if((chunk_size*(i - 1) + 1) <= npars){
      chunk_list[[i]] <- draws[,,(chunk_size*(i - 1) + 1):(min(c(chunk_size*i,npars)))]
    }
  }
  summary_list <- parallel::mclapply(chunk_list, posterior::summarise_draws, mc.cores = n_cores)
  output <- dplyr::bind_rows(summary_list)
  return(output)
}
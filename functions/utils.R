get_spred_token<-function(run_as_dev = TRUE){
  args = commandArgs(trailingOnly = TRUE)  
  if (length(args) != 0) {
    if ("dev" %in% args) {
      run_as_dev <- TRUE
    } else if ("production" %in% args) {
      run_as_dev <- FALSE
    }
  }
  
  if(run_as_dev){
    cat(crayon::yellow("in developer mode...\n"))
    return(Sys.getenv("SPRED_DEV_BOT"))
  }else{
    stop("production mode not implemented")
  }
}



collapse_commas_and <- function(x) {
  out <- paste(x, collapse = ", ")
  if (length(x) <= 0) {
    out <- ""
  }
  if (length(x) == 1) {
    out <- x
  }
  if (length(x) == 2) {
    out <- paste(x, collapse = " & ")
  }
  if (length(x) > 2) {
    part_with_commas <-  paste(x[-length(x)], collapse = ", ")
    out <- paste0(part_with_commas, " & ", x[length(x)])
  }
  out
}


format_distance<-function(x){
  if(x==0){return('0 m')}
  if(x<100){return(paste(x,'m'))}
  if(x<1000){return(paste(round(x*2,-2)/2,'m'))}
  if(x<=10000){return(paste(round(x/1000,1), ' km'))}
  if(x<=20000){return(paste(round(x/1000,0), ' km'))}
  paste(round(x/1000,-1), ' km')
}

extract_mentioned_username <- function(update){
  username <- str_extract_all(update$message$text,"@[A-z0-9_]*") %>% unlist
}






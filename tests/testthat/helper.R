

# config ----

# HADLEY. PLZ. U NO DOIN TIDY? 
# https://github.com/r-lib/testthat/issues/544
# https://github.com/r-lib/testthat/issues/730

# end config



# run before tests

message("Global setup - start")

# indices to create
single_index_name <- "aaa"
multiple_indice_names <- c("bbb", "ccc", "ddd")

# just in case
kc <- NULL
kc2 <- NULL

# args
es_args <- list(
  host = "elasticsearch", 
  port = 9200, 
  user = "elastic", 
  pwd = "changeme", 
  verbose = FALSE
)
es_args2 <- list(
  host = "elasticsearch2", 
  verbose = FALSE
)

# clients
kc <- do.call(Kibior$new, es_args)
kc2 <- do.call(Kibior$new, es_args2)
# deactivate progressbar printing
kc$quiet_progress <- TRUE
kc2$quiet_progress <- TRUE

# utils functions
remove_all_indices <- function(){
  message("Removing all indices")
  res <- kc$list()
  if(!purrr::is_null(res)){
    kc$delete(res)
  }
}


message("Global setup - stop")

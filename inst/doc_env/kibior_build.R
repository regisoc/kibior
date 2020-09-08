# extract from env var
get_from_sysenv <- function(x){
    y <- trimws(Sys.getenv(x))
    if(y == "") NULL else y
}
# get 
.kibior_get_elastic_var_from_env <- function(){
    # env vars
    env_es_endpoint <- "KIBIOR_BUILD_ES_ENDPOINT"
    env_es_port     <- "KIBIOR_BUILD_ES_PORT"
    env_es_username <- "KIBIOR_BUILD_ES_USERNAME"
    env_es_password <- "KIBIOR_BUILD_ES_PASSWORD"
    # endpoint
    es_endpoint <- get_from_sysenv(env_es_endpoint)
    if(purrr::is_null(es_endpoint)){
        msg <- "\nTests and vignettes building require some environment variable to be set.\n"
        msg <- paste0(msg, " - ", env_es_endpoint, ", default: NULL, REQUIRED\n")
        msg <- paste0(msg, " - ", env_es_port, ", default: 9200\n")
        msg <- paste0(msg, " - ", env_es_username, ", default: NULL\n")
        msg <- paste0(msg, " - ", env_es_password, ", default: NULL\n")
        msg <- paste0(msg, "The targeted ElasticSearch instance should be empty.\n")
        msg <- paste0(msg, "See documentation for more information.\n")
        stop(msg)
    }
    # port
    es_port <- tryCatch(
        expr = {
            x <- get_from_sysenv(env_es_port)
            if(purrr::is_null(x)) 9200L else as.integer(x)
        },
        warning = function(w){
            # coercion of str to int -> NA + warning
            stop(env_es_port, " must be a number.")
        }
    )
    # args
    args <- list(
        host = es_endpoint, 
        port = es_port
    )
    # if auth, add username and pwd in args
    es_username <- get_from_sysenv(env_es_username)
    es_password <- get_from_sysenv(env_es_password)
    message(es_username)
    message(es_password)
    if(!purrr::is_null(es_username)){
        args <- c(args, list(
            user = es_username, 
            pwd = es_password
        ))
    }
    # return
    args
}
#
.kibior_get_instance_from_env <- function(){
    ka <- .kibior_get_elastic_var_from_env()
    if(purrr::is_null(ka$host)){
        message("No host. Skipping KibioR init.")
        kc <- NULL
    } else {
        message("Host '", ka$host, ":", ka$port, "'. Trying KibioR init...")
        kc <- do.call(Kibior$new, ka)
        # test if targeted ES is empty before building project
        if(!purrr::is_null(kc$list())){
            msg <- paste0("\nTarget Elasticsearch '", kc$endpoint, "' is not empty.\n")
            msg <- paste0(msg, "Please, use an empty Elasticsearch instance to build project.\n")
            stop(msg)
        }
    }
    kc
}
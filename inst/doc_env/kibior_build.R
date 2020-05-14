# get 
.kibior_get_elastic_var_from_env <- function(){
    # env vars
    env_es_endpoint <- "KIBIOR_BUILD_ES_ENDPOINT"
    env_es_port     <- "KIBIOR_BUILD_ES_PORT"
    env_es_username <- "KIBIOR_BUILD_ES_USERNAME"
    env_es_password <- "KIBIOR_BUILD_ES_PASSWORD"
    # extract from env var
    eget <- function(x){
        y <- trimws(Sys.getenv(x))
        if(y == "") NULL else y
    }
    # endpoint
    es_endpoint <- eget(env_es_endpoint)
    if(purrr::is_null(es_endpoint)){
        msg <- "\nTry loading Kibior tests but lacks Elasticsearch endpoint.\n"
        msg <- paste0(msg, "Tests and vignettes building require some environment variable to be set.\n")
        msg <- paste0(msg, " - ", env_es_endpoint, ", default: NULL, REQUIRED\n")
        msg <- paste0(msg, " - ", env_es_port, ", default: 9200\n")
        msg <- paste0(msg, " - ", env_es_username, ", default: NULL\n")
        msg <- paste0(msg, " - ", env_es_password, ", default: NULL\n")
        msg <- paste0(msg, "It should be local and empty. See documentation for more information.\n")
        stop(msg)
    }
    # port
    es_port <- tryCatch(
        {
            x <- eget(env_es_port)
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
    es_username <- eget(env_es_username)
    es_password <- eget(env_es_password)
    message(es_username)
    message(es_password)
    if(!purrr::is_null(es_username)){
        args <- c(
            args, 
            list(
                user = es_username, 
                pwd = es_password
            )
        )
    }
    # return
    args
}

.kibior_get_instance_from_env <- function(){
    ka <- .kibior_get_elastic_var_from_env()
    kc <- do.call(Kibior$new, ka)
    # test if targeted ES is empty before building project
    if(!purrr::is_null(kc$list())){
        msg <- paste0("\nTarget Elasticsearch '", kc$endpoint, "' is not empty.\n")
        msg <- paste0(msg, "Please, use an empty Elasticsearch instance to build project.\n")
        stop(msg)
    }
    kc
}
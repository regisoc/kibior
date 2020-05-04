

# config ----

# HADLEY. PLZ. U NO DOIN TIDY? 
# https://github.com/r-lib/testthat/issues/544
# https://github.com/r-lib/testthat/issues/730

# end config



# run before tests

library(magrittr)
library(dplyr)


get_elastic_var_from_env <- function(){
    # env vars
    env_es_endpoint <- "KIBIOR_BUILD_ES_ENDPOINT"
    env_es_port     <- "KIBIOR_BUILD_ES_PORT"
    env_es_username <- "KIBIOR_BUILD_ES_USERNAME"
    env_es_password <- "KIBIOR_BUILD_ES_PASSWORD"
    # extract from env var
    eget <- function(x){
        x %>% 
            Sys.getenv() %>% 
            trimws() %>% 
            (function(y){ 
                if(y == "") NULL else y 
            })
    }
    # endpoint
    es_endpoint <- eget(env_es_endpoint)
    if(purrr::is_null(es_endpoint)){
        msg <- "Try loading Kibior tests but lacks Elasticsearch endpoint.\n"
        msg <- paste0(msg, "Tests and vignettes building require some environment variable to be set.\n")
        msg <- paste0(msg, " - ", env_es_endpoint, ", default: NULL, REQUIRED\n")
        msg <- paste0(msg, " - ", env_es_port, ", default: 9200\n")
        msg <- paste0(msg, " - ", env_es_username, ", default: NULL\n")
        msg <- paste0(msg, " - ", env_es_password, ", default: NULL\n")
        msg <- paste0(msg, "It should be local and empty. See documentation for more information.")
        stop(msg)
    }
    message("Elastic endpoint: ", es_endpoint)
    # port
    es_port <- tryCatch(
        {
            env_es_port %>%
                eget() %>% 
                (function(x){ if(purrr::is_null(x)) 9200L else as.integer(x) })
        },
        warning = function(w){
            # coercion of str to int -> NA + warning
            stop(env_es_port, " must be a number.")
        }
    )
    message("Elastic port: ", es_port)
    # args
    args <- list(
        host = es_endpoint, 
        port = es_port,  
        verbose = FALSE
    )
    # if auth, add username and pwd in args
    es_username <- eget(env_es_username)
    es_password <- eget(env_es_password)
    if(!purrr::is_null(es_username)){
        args <- c(
            args, 
            list(
                user = es_username, 
                pwd = es_password
            )
        )
        message("Elastic username: ", es_username)
        message("Elastic password: ", es_password)
    }
    # return
    args
}

message()
message("----------------------------------")
message("-- GLOBAL DEFINITION")
message()

message("Getting build variables from env...")
# as list
args <- get_elastic_var_from_env()

#
kc <- do.call(Kibior$new, args)
kc$quiet_progress <- TRUE
# test if targeted ES is empty before building project
if(!purrr::is_null(kc$list())){
    msg <- paste0("Target Elasticsearch '", kc$endpoint, "' is not empty. ")
    msg <- paste0(msg, "Please, use an empty Elasticsearch instance to build project.")
    stop(msg)
}
message("Kibior build instance: kc")



# change names to lower and with underscores
change_names <- function(dataset){
    # to lower
    names(dataset) <- tolower(names(dataset))
    # dots to underscores 
    names(dataset) <- gsub("\\.", "_", names(dataset))
    dataset
}
message("Function added: change_names()")


mutate_factors <- function(dataset){
    dataset %>% dplyr::mutate_if(is.factor, as.character)
}


# ------------------------------------------------
# utils variables



single_index_name <- "test_index_single"
message("Variable added: single_index_name")
multiple_indice_names <- c("test_index_a", "test_index_b", "test_index_c")
message("Variable added: multiple_indice_names")
cpt_loop <- c(10, 100, 1000, 10000)
message("Variable added: cpt_loop")
temp_filepath <- tempfile(fileext = ".csv")
message("Variable added: temp_filepath")
all_features <- c("aliases", "mappings", "settings")
message("Variable added: all_features")



# some datasets
ds <- list(
    # small, 87 records
    "starwars" = dplyr::starwars %>% change_names() %>% mutate_factors(),
    # medium, 10k records
    "storms" = dplyr::storms %>% change_names() %>% mutate_factors(),
    # large, 53k records
    "diamonds" = ggplot2::diamonds %>% change_names() %>% mutate_factors()
)
message("Variable added: ds - list of 3 datasets")



ds_random_lines <- ds %>% lapply(function(x){ 
    x[sample(nrow(x), 10), ]
})
message("Variable added: ds_random_lines - 10 random lines from each df of 'ds'")


# modifications on parts of datasets

# starwars
## 38 records modified to force them into female gender
s <- dplyr::starwars %>% 
    dplyr::filter(height > 180)
s["gender"] <- "female"

# storms
## 44 records modified to a new category that does not exist
st <- dplyr::storms %>% 
    dplyr::filter(pressure < 980 & status == "tropical storm")
st["category"] <- 18

# diamonds
## 74 records modified to a new color that does not exist
d <- ggplot2::diamonds %>% 
    filter(clarity == "VS1" & depth > 65)
d["color"] <- "W"

# only the updated records here, not all
ds_modified <- list(
    "starwars" = change_names(s),
    "storms" = change_names(st),
    "diamonds" = change_names(d)
)
message("Variable added: ds_modified - some modifications on df of 'ds'")



# | emp_name | emp_id | dept_name       |
# |----------|--------|-----------------|
# | Harry    | 3415   | Finance         |
# | Sally    | 2241   | Sales           |
# | George   | 3401   | Finance         |
# | Harriet  | 2202   | Sales           |
# | Mary     | 1257   | Human Resources |
# | Tim      | 1123   | Executive       |

employee <- list(
    emp_id = c(
        3415,
        2241,
        3401,
        2202,
        1257,
        1123
    ),
    emp_name = c(
        "Harry",
        "Sally",
        "George",
        "Harriet",
        "Mary",
        "Tim"
    ),
    dept_name = c(
        "Finance",
        "Sales",
        "Finance",
        "Sales",
        "Human Resources",
        "Executive"
    )
) %>% dplyr::as_tibble()


# | dept_id | name        | manager |
# |---------|-------------|---------|
# | 1       | Finance     | George  |
# | 2       | Sales       | Harriet |
# | 3       | Production  | Charles |

dept <- list(
    dept_id = 1:3,
    name = c(
        "Finance",
        "Sales",
        "Production"
    ),
    manager = c(
        "George",
        "Harriet",
        "Charles"
    )
) %>% dplyr::as_tibble()

join_fields <- c("dept_name" = "name")


# start <- 1
# end <- 10
# ds_join <- list(
#     gene = LETTERS[seq(from=start, to=end)], 
#     value = start:end
#     ) %>% dplyr::as_tibble()







# ------------------------------------------------
# utils functions


remove_all_indices <- function(){
    res <- kc$list()
    if(!purrr::is_null(res)) kc$delete(res)
}
message("Function added: remove_all_indices()")


count_nb_lines <- function(filepath){
    f <- file(filepath, open="rb")
    nlines <- 0L
    while(length(chunk <- readBin(f, "raw", 65536)) > 0) {
        nlines <- nlines + sum(chunk == as.raw(10L))
    }
    close(f)
    nlines
}
message("Function added: count_nb_lines()")


produce_str <- function(nb){
    replicate(nb, "a") %>% 
        unlist(use.names = FALSE) %>% 
        paste0(collapse = "")
}
message("Function added: produce_str()")


push_test_datasets <- function(recreate = TRUE){
    message("Pushing test datasets:")
    for(d in names(ds)){
        message(" - ", d, "...")
        # by default, field 'kid' is the field used as id
        res <- ds[[d]] %>% kc$push(d, mode = (if(recreate) "recreate" else "check"))
        expect_equal(res, d)
    }
}
message("Function added: push_test_datasets()")





message()
message("----------------------------------")
message()

message("Data will be push to Elasticsearch to setup test env")

message()
message("----------------------------------")
message()




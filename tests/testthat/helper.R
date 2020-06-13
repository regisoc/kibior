

# config ----

# HADLEY. PLZ. U NO DOIN TIDY? 
# https://github.com/r-lib/testthat/issues/544
# https://github.com/r-lib/testthat/issues/730

# end config



# run before tests

library(magrittr)
library(dplyr)

message()
message("----------------------------------")
message("-- GLOBAL DEFINITION")
message()
message("Getting build variables from env...")

# get kibior var from ".Renviron" file
dd <- system.file("doc_env", "kibior_build.R", package = "kibior")
source(dd, local = TRUE)
kc <- .kibior_get_instance_from_env()
kc$quiet_progress <- TRUE


# change names to lower and with underscores
change_names <- function(dataset){
    # to lower
    names(dataset) <- tolower(names(dataset))
    # dots to underscores 
    names(dataset) <- gsub("\\.", "_", names(dataset))
    dataset
}


mutate_factors <- function(dataset){
    dataset %>% dplyr::mutate_if(is.factor, as.character)
}


# ------------------------------------------------
# utils variables



single_index_name <- "test_index_single"
multiple_indice_names <- c("test_index_a", "test_index_b", "test_index_c")
cpt_loop <- c(10, 100, 1000, 10000)
temp_filepath <- tempfile(fileext = ".csv")
all_features <- c("aliases", "mappings", "settings")



# some datasets
ds <- list(
    # small, 87 records
    "starwars" = dplyr::starwars %>% change_names() %>% mutate_factors(),
    # medium, 10k records
    "storms" = dplyr::storms %>% change_names() %>% mutate_factors(),
    # large, 53k records
    "diamonds" = ggplot2::diamonds %>% change_names() %>% mutate_factors()
)



ds_random_lines <- ds %>% lapply(function(x){ 
    x[sample(nrow(x), 10), ]
})


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

# dplyr by
join_fields <- c("dept_name" = "name")

# use with quosure "!!"
query_local <- dplyr::quo(dept_name %in% c("Finance", "Sales"))
query_remote <- "dept_name:(finance || sales)"





# ------------------------------------------------
# utils functions


remove_all_indices <- function(){
    res <- kc$list()
    if(!purrr::is_null(res)) kc$delete(res)
}


count_nb_lines <- function(filepath){
    f <- file(filepath, open="rb")
    nlines <- 0L
    while(length(chunk <- readBin(f, "raw", 65536)) > 0) {
        nlines <- nlines + sum(chunk == as.raw(10L))
    }
    close(f)
    nlines
}


produce_str <- function(nb){
    replicate(nb, "a") %>% 
        unlist(use.names = FALSE) %>% 
        paste0(collapse = "")
}


push_test_datasets <- function(recreate = TRUE){
    message("Pushing test datasets:")
    for(d in names(ds)){
        message(" - ", d, "...")
        # by default, field 'kid' is the field used as id
        res <- ds[[d]] %>% kc$push(d, mode = (if(recreate) "recreate" else "check"))
        expect_equal(res, d)
    }
}



message("Data will be push to Elasticsearch to setup test env")
message("----------------------------------")
message()




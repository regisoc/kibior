

context("Search")


# ---------------------------------------------------------------


testthat::setup({
    # remove indices if they exist
    remove_all_indices()

    # multiple (starwars, storms, diamonds) with their name
    selected_fields <- list()
    selected_fields_with_source <- list()
    kc$quiet_progress <- TRUE
    for(d in names(ds)){
        # select two fields randomly
        selected_fields[[d]] <- ds[[d]] %>% 
            names() %>% 
            sample(2) 
        # force the name to be "_source.<field>" to cmp more easily after
        selected_fields_with_source[[d]] <- selected_fields[[d]] %>% 
            lapply(function(x){ 
                paste0("_source.", x) 
            }) %>% 
            unlist(use.names = FALSE)
    }
    expect_setequal(kc$list(), names(ds))
    
    # assign var to global env
    assign("selected_fields", selected_fields, envir = .GlobalEnv)
    assign("selected_fields_with_source", selected_fields_with_source, envir = .GlobalEnv)
})

testthat::teardown({
    remove_all_indices()
    # remove var from global env 
    rm(selected_fields, envir = .GlobalEnv)
    rm(selected_fields_with_source, envir = .GlobalEnv)
})


# ---------------------------------------------------------------



# start search ----


# test_that("kibior::search, wrong args", {
#     single_index_name <- ds[[1]]
#     # args to test
#     a = list(
#         NA, 
#         TRUE, 
#         FALSE, 
#         0, 
#         -1, 
#         -100,
#         NULL, 
#         c(),
#         c("w", "e", "s", "h"), 
#         c("name", "nope"),
#         list(), 
#         list("w", "e", "s", "h"), 
#         "NOPE" 
#     )
#     # index name
#     for(i in a){
#         if(!is.null(i)){
#         expect_error(kc$search(index_name = i, head = FALSE))
#         }
#     }
#     # bulk 
#     for(i in a){
#         expect_error(kc$search(single_index_name, bulk_size = i, head = FALSE))
#     }
#     # max size
#     for(i in a){
#         if(!is.null(i)){ # if max_size is not null, else it returns everything
#         expect_error(kc$search(single_index_name, max_size = i, head = FALSE))
#         }
#     }
#     # scroll timer
#     for(i in a){
#         expect_error(kc$search(single_index_name, scroll_timer = i, head = FALSE))
#     }
#     # keep metadata 
#     for(i in a){
#         if(!is.logical(i)){
#         expect_error(kc$search(single_index_name, keep_metadata = i, head = FALSE))
#         }
#     }
#     expect_error(kc$search(single_index_name, keep_metadata = NA, head = FALSE))
#     # fields filters 
#     for(i in a){
#         # if fields is not null or string, else it returns everything
#         if(!is.null(i) && !is.character(i)){ 
#         expect_error(kc$search(single_index_name, fields = i, head = FALSE))
#         }
#     }
#     # fields filters with metadata
#     for(i in a){
#         # if fields is not null or string, else it returns everything
#         if(!is.null(i) && !is.character(i)){ 
#         expect_error(kc$search(single_index_name, keep_metadata = TRUE, fields = i, head = FALSE))
#         }
#     }
#     # query
#     for(i in a){
#         if(is.character(i) && i != "NOPE"){ # this works but returns no results
#         expect_error(kc$search(single_index_name, query = i, head = FALSE))
#         }
#     }
#     # head
#     for(i in a){
#         if(!is.logical(i)){ 
#         expect_error(kc$search(single_index_name, head = i))
#         }
#     }
#     expect_error(kc$search(single_index_name, head = NA))
# })


# test_that("kibior::search, query size error", {
#     # produces string of length nb
#     produce_str <- function(nb){
#         replicate(nb, "a") %>% 
#             unlist(use.names = FALSE) %>% 
#             paste0(collapse = "")
#     }
#     # 
#     index <- names(ds)[[1]]
#     for(i in c(10, 100, 1000, 4000)){
#         p <- produce_str(i)
#         # full query under 4096 bytes, so this return something (empty)
#         res <- kc$search(index, query = p)[[index]]
#         expect_equal(typeof(res), "list")
#         expect_equal(class(res), "list")
#         expect_length(res, 0)
#     }
#     # by default, ES limit is 4096 bytes
#     err_q <- produce_str(10000)
#     expect_error(kc$search(index, query = err_q)[[index]])
    
# })




# test_that("kibior::search, nominal simple case, get one index", {
#     for(d in names(ds)){
#         r <- kc$search(d, head = FALSE)[[d]]
#         # fields
#         expected_fields <- c("kid", names(ds[[d]]))
#         expect_setequal(names(r), expected_fields)
#         # dim
#         expect_equal(nrow(r), nrow(ds[[d]]))
#     }
# })

# test_that("kibior::search, nominal simple case, get two indices", {
#     ds_names <- ds %>% 
#         head(2) %>% 
#         names()
#     # get "starwars" and "diamonds" datasets
#     r <- kc$search(ds_names, head = FALSE)
#     expect_length(r, 2)
#     expect_setequal(ds_names, names(r))
#     # test names
#     for(d in ds_names){
#         expect_setequal(c("kid", names(ds[[d]])), names(r[[d]]))
#     }
#     # test dimensions
#     for(d in ds_names){
#         expect_equal(nrow(ds[[d]]), nrow(r[[d]]))
#     }
# })

# test_that("kibior::search, nominal simple case, get indices via pattern", {
#     # get "starwars" and "storms" datasets
#     expected_ds <- c("starwars", "storms")
#     r <- kc$search("s*", head = FALSE)
#     expect_length(r, 2)
#     expect_setequal(expected_ds, names(r))
#     # test names
#     for(d in expected_ds){
#         expect_setequal(c("kid", names(ds[[d]])), names(r[[d]]))
#     }
#     # test dimensions
#     for(d in expected_ds){
#         expect_equal(nrow(ds[[d]]), nrow(r[[d]]))
#     }
# })

# test_that("kibior::search, wrong index names", {
#   #
#   false_indices <- c("aaaa", "bbbb", "cccc")
#   for(i in false_indices){
#       for(j in c(TRUE, FALSE)){
#           expect_error(kc$search(i, head = j))
#       }
#   }
# })

# test_that("kibior::search, nominal simple case, single index, no impact regarding bulk_size", {
#   # 
#   for(b in cpt_loop){
#     for(d in names(ds)){
#       r <- kc$search(d, bulk_size = b, head = FALSE)[[d]]
#       #
#       expect_setequal(names(r), c("kid", names(ds[[d]])))
#       expect_equal(nrow(r), nrow(ds[[d]]))
#     }
#   }
# })

# test_that("kibior::search, nominal simple case, multiple indices, no impact regarding bulk_size", {
#   # 
#   for(b in cpt_loop){
#     r <- kc$search(c("starwars", "diamonds"), bulk_size = b, head = FALSE)
#     expect_length(r, 2)
#     # dimension
#     expect_setequal(names(r$starwars), c("kid", names(dplyr::starwars)))
#     expect_setequal(names(r$diamonds), c("kid", names(ggplot2::diamonds)))
#     expect_equal(nrow(r$starwars), nrow(starwars))
#     expect_equal(nrow(r$diamonds), nrow(ggplot2::diamonds))
#   }
# })

# test_that("kibior::search, nominal simple case, get via pattern, no impact regarding bulk_size", {
#   # 
#   for(b in cpt_loop){
#     # get starwars and storms datasets
#     r <- kc$search("s*", bulk_size = b, head = FALSE)
#     expect_length(r, 2)
#     # dimension
#     expect_setequal(names(r$starwars), c("kid", names(starwars)))
#     expect_setequal(names(r$storms), c("kid", names(storms)))
#     expect_equal(nrow(r$starwars), nrow(starwars))
#     expect_equal(nrow(r$storms), nrow(storms))
#   }
# })

# test_that("kibior::search, error when bulk_size > max_size", {
#     # default value
#     expect_error(kc$search("s*", max_size = 10, head = FALSE))
#     expect_error(kc$search("s*", bulk_size = 500, max_size = 10, head = FALSE))
#     # no eror when identical values

#     index <- names(ds)[[1]]
#     m <- 50
#     r <- kc$search(index, bulk_size = m, max_size = m, head = FALSE)[[index]]
#     expect_setequal(names(r), c("kid", names(ds[[1]])))
#     s <- if(m > nrow(ds[[1]])) nrow(ds[[1]]) else m
#     expect_equal(nrow(r), s)
# })

# test_that("kibior::search, single index, nominal expected max_size asked", {
#   # arbitrary sizes
#   for(s in cpt_loop){
#     for(d in names(ds)){
#       r <- kc$search(d, bulk_size = min(cpt_loop), max_size = s, head = FALSE)[[d]]
#       expect_setequal(names(r), c("kid", names(ds[[d]])))
#       # size
#       co <- kc$count(d)[[d]]
#       if(s > co){
#         expect_equal(nrow(r), co)
#       } else {
#         expect_equal(nrow(r), s)
#       }
#     }
#   }
# })

# test_that("kibior::search, multiple indices, nominal expected max_size asked", {
#   # arbitrary sizes
#   for(s in cpt_loop){
#     r <- kc$search(c("starwars", "storms"), bulk_size = min(cpt_loop), max_size = s, head = FALSE)
#     expect_length(r, 2)
#     # dimensions
#     expect_setequal(names(r$starwars), c("kid", names(starwars)))
#     expect_setequal(names(r$storms), c("kid", names(storms)))
#     swco <- kc$count("starwars")[["starwars"]]
#     stco <- kc$count("storms")[["storms"]]
#     if(s > swco){
#       expect_equal(nrow(r$starwars), swco)
#     } else {
#       expect_equal(nrow(r$starwars), s)
#     }
#     if(s > stco){
#       expect_equal(nrow(r$storms), stco)
#     } else {
#       expect_equal(nrow(r$storms), s)
#     }
#   }
# })

# test_that("kibior::search, indices via pattern, nominal expected max_size asked", {
#   # arbitrary sizes
#   for(s in cpt_loop){
#     r <- kc$search("s*", bulk_size = min(cpt_loop), max_size = s, head = FALSE)
#     expect_length(r, 2)
#     # dimensions
#     expect_setequal(names(r$starwars), c("kid", names(starwars)))
#     expect_setequal(names(r$storms), c("kid", names(storms)))
#     swco <- kc$count("starwars")[["starwars"]]
#     stco <- kc$count("storms")[["storms"]]
#     if(s > swco){
#       expect_equal(nrow(r$starwars), swco)
#     } else {
#       expect_equal(nrow(r$starwars), s)
#     }
#     if(s > stco){
#       expect_equal(nrow(r$storms), stco)
#     } else {
#       expect_equal(nrow(r$storms), s)
#     }
#   }
# })

# test_that("kibior::search, nominal too short scroll timer", {
#   # really short timer 1 nanosecond so the scroll connection expires
#   # and cannot retrieve the data
#   for(d in names(ds)){
#     expect_error(kc$search(d, scroll_timer = "1ns", head = FALSE))
#   }
# })

# test_that("kibior::search, wrong scroll timer", {
#   for(d in names(ds)){
#     expect_error(kc$search(d, scroll_timer = "NOOOOOPE", head = FALSE))
#   }
# })

# test_that("kibior::search, keep metadata, single index", {
#   for(d in names(ds)){
#     # ask meta
#     r <- kc$search(d, keep_metadata = TRUE, head = FALSE)[[d]]
#     expect_equal(r[["_index"]][[1]], d)
#     expect_setequal(r[["_id"]], r[["_source.kid"]])
#     # compare colnames with no metadata result
#     rr <- kc$search(d, keep_metadata = FALSE, head = FALSE)[[d]]
#     expect_equal(nrow(rr), nrow(r))
#     colnames <- names(r) %>% 
#       lapply(function(x){ if(startsWith(x, "_source.")) x else NULL }) %>% 
#       lapply(function(x){ gsub("_source.", "", x) }) %>% 
#       unlist(use.names = FALSE)
#     expect_setequal(colnames, names(rr))
#   }
# })

# test_that("kibior::search, keep metadata, multiple indices", {
#   # ask meta
#   r <- kc$search(c("starwars", "diamonds"), keep_metadata = TRUE, head = FALSE)
#   expect_length(r, 2)
#   # dimensions
#   for(i in names(r)){
#     # test some metadata cols
#     expect_true(all(c("_index", "_version") %in% names(r[[i]])))
#     # select cols with "_source." in the name and remove the rest
#     colnames <- names(r[[i]]) %>% 
#       lapply(function(x){ if(startsWith(x, "_source.")) x else NULL }) %>% 
#       lapply(function(x){ gsub("_source.", "", x) }) %>% 
#       unlist(use.names = FALSE)
#     expect_true(all(names(ds[[i]]) %in% colnames))
#   }
# })

# test_that("kibior::search, keep metadata, indices via pattern", {
#   # ask meta
#   r <- kc$search("s*", keep_metadata = TRUE, head = FALSE)
#   expect_length(r, 2)
#   # dimensions
#   for(i in names(r)){
#     expect_true(all(c("_index", "_version") %in% names(r[[i]])))
#     # select cols with "_source." in the name and remove the rest
#     colnames <- names(r[[i]]) %>% 
#       lapply(function(x){ if(startsWith(x, "_source.")) x else NULL }) %>% 
#       lapply(function(x){ gsub("_source.", "", x) }) %>% 
#       unlist(use.names = FALSE)
#     expect_true(all(names(ds[[i]]) %in% colnames))
#   }
# })

# test_that("kibior::search, nominal, single index, fields NULL is complete", {
#   for(d in names(ds)){
#     r <- kc$search(d, fields = NULL, head = FALSE)[[d]]
#     expect_setequal(names(r), c("kid", names(ds[[d]])) )
#   }
# })

# test_that("kibior::search, nominal, multiple indices, fields NULL is complete", {
#   # 
#   r <- kc$search(c("starwars", "diamonds"), fields = NULL, head = FALSE)
#   expect_length(r, 2)
#   for(i in names(r)){
#     expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])) )
#   }
# })

# test_that("kibior::search, nominal, indices via pattern, fields NULL is complete", {
#   # 
#   r <- kc$search("s*", fields = NULL, head = FALSE)
#   expect_length(r, 2)
#   for(i in names(r)){
#     expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])) )
#   }
# })

# test_that("kibior::search, nominal, one index, select some fields, without metadata", {
#   for(d in names(ds)){
#     # ask for randomly selected fields
#     r <- kc$search(d, fields = selected_fields[[d]], head = FALSE)[[d]]
#     expect_setequal(names(r), selected_fields[[d]])
#   }
# })

# test_that("kibior::search, nominal, one index, select some fields, with metadata", {
#   for(d in names(ds)){
#     # select two mentionned fields
#     r <- kc$search(d, fields = selected_fields[[d]], keep_metadata = TRUE, head = FALSE)[[d]]
#     # compare with "_source.<field>"
#     expect_true(all(selected_fields_with_source[[d]] %in% names(r)))
#   }
# })

# test_that("kibior::search, nominal, all indices, select one field only present in two datasets, without metadata", {
#   # we want to test specific names of fields
#   # the field "name" is present in "starwars" and "storms" dataset only
#   # should no get anything from "diamonds" dataset
#   r <- kc$search("*", fields = "name", keep_metadata = FALSE, head = FALSE)
#   expect_length(r, length(names(ds)))

#   expect_true("name" %in% names(r$starwars))
#   expect_true(!("_index" %in% names(r$starwars)))

#   expect_true("name" %in% names(r$storms))
#   expect_true(!("_index" %in% names(r$storms)))

#   expect_true(!("name" %in% names(r$diamonds)))
#   expect_true(!("_index" %in% names(r$diamonds)))

# })

# test_that("kibior::search, nominal, all indices, select one field only present in two datasets, with metadata", {
#   # we want to test specific names of fields
#   # the field "name" is present in "starwars" and "storms" dataset only
#   # should no get anything from "diamonds" dataset
#   r <- kc$search("*", fields = "name", keep_metadata = TRUE, head = FALSE)
#   expect_length(r, length(names(ds)))

#   expect_true("_source.name" %in% names(r$starwars))
#   expect_true("_index" %in% names(r$starwars))

#   expect_true("_source.name" %in% names(r$storms))
#   expect_true("_index" %in% names(r$storms))

#   expect_true(!("_source.name" %in% names(r$diamonds)))
#   expect_true("_index" %in% names(r$diamonds))
# })


# # HEAD

# test_that("kibior::search, head search, one index", {
#   # head on
#   r <- kc$search("starwars", head = TRUE)
#   expect_length(r, 1)
#   expect_equal(nrow(r$starwars), kc$head_search_size)
#   expect_setequal(names(r$starwars), c("kid", names(starwars)))

#   # head off
#   r <- kc$search("starwars", head = FALSE)
#   expect_length(r, 1)
#   expect_equal(nrow(r$starwars), nrow(starwars))
#   expect_setequal(names(r$starwars), c("kid", names(starwars)))
# })

# test_that("kibior::search, head search, all indices", {
#   # head on
#   r <- kc$search("*", head = TRUE)
#   expect_length(r, length(ds))
#   for(i in names(r)){
#     expect_equal(nrow(r[[i]]), kc$head_search_size)
#     expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])))
#   }
#   # head off
#   r <- kc$search("*", head = FALSE)
#   expect_length(r, length(ds))
#   for(i in names(r)){
#     expect_equal(nrow(r[[i]]), nrow(ds[[i]]))
#     expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])))
#   }
# })

test_that("kibior::search, size to return", {
    # push a ds smaller than head size
    small <- ds[[1]] %>% head(kc$head_search_size - 2)
    small %>% kc$push("small")
    new_ds <- ds
    new_ds[["small"]] <- small
    # if head on, then 
    #     ds size > head size, ds are restricted to head size
    #     ds size < head size, ds are full
    r <- kc$search("*", head = TRUE)
    for(i in names(new_ds)){
        if(nrow(new_ds[[i]]) > kc$head_search_size){
            expect_equal(nrow(r[[i]]), kc$head_search_size)
        } else {
            expect_equal(nrow(r[[i]]), nrow(new_ds[[i]]))
        }
    }
    # if head off, and max size set them
    #     if max size >= ds size, then
    #         ds are full
    #     else
    #         ds are restricted to max size 
    for(m in c(2, 5, 100, 10500)){
        r <- kc$search("*", max_size = m, bulk_size = m, head = FALSE)
        if(nrow(new_ds[[i]]) > m){
            expect_equal(nrow(r[[i]]), m)
        } else {
            expect_equal(nrow(r[[i]]), nrow(new_ds[[i]]))
        }
    }

    # if head off, and no max_size then
    #     ds are full
    r <- kc$search("*", max_size = NULL, head = FALSE)
    for(i in names(new_ds)){
        expect_equal(nrow(r[[i]]), nrow(new_ds[[i]]))
    }
    # delete tmp
    kc$delete("small")
})

# end search

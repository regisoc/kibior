

context("Move data - pull")


testthat::setup({
    # remove indices if they exist
    remove_all_indices()
})

testthat::teardown({
    # remove indices if they exist
    remove_all_indices()
})







# start pull ----

test_that("kibior::pull, wrong args", {
    remove_all_indices()
    res <- kc$push(ds[["starwars"]], single_index_name)
    expect_equal(res, single_index_name)
    # args to test
    a = list(
        NA, 
        TRUE, 
        FALSE, 
        0, 
        -1, 
        -100,
        NULL, 
        c(),
        c("w", "e", "s", "h"), 
        c("name", "nope"),
        list(), 
        list("w", "e", "s", "h"), 
        "NOPE" 
    )
    # no args
    expect_error(kc$pull())
    # index name
    expect_error(kc$pull(NA))
    expect_error(kc$pull(TRUE))
    expect_error(kc$pull(FALSE))
    expect_error(kc$pull(0))
    expect_error(kc$pull(-1))
    expect_error(kc$pull(-10))
    # expect_error(kc$pull(NULL))
    # expect_error(kc$pull(c()))
    # expect_error(kc$pull(c("w", "e", "s", "h")))
    # expect_error(kc$pull(c("name", "nope")))
    expect_error(kc$pull(list()))
    expect_error(kc$pull(list("w", "e", "s", "h")))
    # expect_error(kc$pull("NOPE"))
    # bulk 
    for(i in a){
        expect_error(kc$pull(single_index_name, bulk_size = NA))
    }
    # max size
    for(i in a){
        if(!purrr::is_null(i)){ 
            # if max_size is not null, else it returns everything
            expect_error(kc$pull(single_index_name, max_size = i))
        }
    }
    # scroll timer
    for(i in a){
        expect_error(kc$pull(single_index_name, scroll_timer = i))
    }
    # keep metadata 
    for(i in a){
        if(!is.logical(i)){
        expect_error(kc$pull(single_index_name, keep_metadata = i))
        }
    }
    expect_error(kc$pull(single_index_name, keep_metadata = NA))
    # columns filters 
    for(i in a){
        # if columns is not null or string, else it returns everything
        if(!purrr::is_null(i) && !is.character(i)){ 
        expect_error(kc$pull(single_index_name, columns = i))
        }
    }
    # columns filters with metadata
    for(i in a){
        # if columns is not null or string, else it returns everything
        if(!purrr::is_null(i) && !is.character(i)){ 
        expect_error(kc$pull(single_index_name, keep_metadata = TRUE, columns = i))
        }
    }
    # query
    for(i in a){
        if(is.character(i) && i != "NOPE"){ # this works but returns no results
        expect_error(kc$pull(single_index_name, query = i))
        }
    }
})



test_that("kibior::pull, nominal simple case, get one index", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # 
    for(d in names(ds)){
        r <- kc$pull(d)[[d]]
        expect_setequal(names(r), c("kid", kc$columns(d)[[d]]))
        expect_equal(nrow(r), kc$count(d)[[d]])
    }
})

test_that("kibior::pull, nominal simple case, get two indices", {
    # push data
    remove_all_indices()
    selected_columns <- list()
    selected_columns_with_source <- list()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # get "starwars" and "storms" datasets
    r <- kc$pull(c("starwars", "storms"))
    expect_length(r, 2)
    expect_setequal(c("starwars", "storms"), names(r))
    # test names
    expect_setequal(c("kid", names(ds[["starwars"]])), names(r$starwars))
    expect_setequal(c("kid", names(dplyr::storms)), names(r$storms))
    # test dimensions
    expect_equal(nrow(ds[["starwars"]]), nrow(r$starwars))
    expect_equal(nrow(dplyr::storms), nrow(r$storms))
})

test_that("kibior::pull, nominal simple case, get indices via pattern", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # get "starwars" and "storms" datasets
    r <- kc$pull("s*")
    expect_length(r, 2)
    expect_setequal(c("starwars", "storms"), names(r))
    # test names
    expect_setequal(c("kid", names(ds[["starwars"]])), names(r$starwars))
    expect_setequal(c("kid", names(dplyr::storms)), names(r$storms))
    # test dimensions
    expect_equal(nrow(ds[["starwars"]]), nrow(r$starwars))
    expect_equal(nrow(dplyr::storms), nrow(r$storms))
})


test_that("kibior::pull, wrong index names", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        ds[[d]] %>% 
            kc$push(d) %>% 
            expect_equal(d)
    }
    expect_setequal(kc$list(), names(ds))
    #
    res <- kc$pull("aaaa")
    expect_true(length(res) == 0)
    expect_true(typeof(res) == "list")
})


test_that("kibior::pull, more than 10k results asked per bulk", {
    # push data
    remove_all_indices()
    sw <- "starwars"
    ds[[sw]] %>% kc$push(sw)
    #
    res <- kc$pull(sw, bulk_size = 10001L)[[sw]]
    res2 <- kc$pull(sw)[[sw]]
    #
    expect_setequal(names(res), names(res2))
    expect_equal(nrow(res), nrow(res2))
})



test_that("kibior::pull, nominal simple case, single index, no impact regarding bulk_size", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # 
    for(b in c(10, 5000, 10000000)){
        for(d in names(ds)){
            r <- kc$pull(d, bulk_size = b)[[d]]
            expect_setequal(names(r), kc$columns(d)[[d]])
            expect_equal(nrow(r), kc$count(d)[[d]])
        }
    }
})

test_that("kibior::pull, nominal simple case, multiple indices, no impact regarding bulk_size", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # 
    for(b in c(10, 5000, 10000000)){
        r <- kc$pull(c("starwars", "storms"), bulk_size = b)
        expect_length(r, 2)
        # dimension
        expect_setequal(names(r$starwars), c("kid", names(ds[["starwars"]])))
        expect_setequal(names(r$storms), c("kid", names(dplyr::storms)))
        expect_equal(nrow(r$starwars), nrow(ds[["starwars"]]))
        expect_equal(nrow(r$storms), nrow(dplyr::storms))
    }
})

test_that("kibior::pull, nominal simple case, get via pattern, no impact regarding bulk_size", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # 
    for(b in c(10, 5000, 10000000)){
        # get starwars and storms datasets
        r <- kc$pull("s*", bulk_size = b)
        expect_length(r, 2)
        # dimension
        expect_setequal(names(r$starwars), c("kid", names(ds[["starwars"]])))
        expect_setequal(names(r$storms), c("kid", names(dplyr::storms)))
        expect_equal(nrow(r$starwars), nrow(ds[["starwars"]]))
        expect_equal(nrow(r$storms), nrow(dplyr::storms))
    }
})



test_that("kibior::pull, single index, nominal expected max_size asked", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # arbitrary sizes
    for(s in c(10, 5000, 10000000)){
        for(d in names(ds)){
        r <- kc$pull(d, max_size = s)[[d]]
        expect_setequal(names(r), c("kid", kc$columns(d)[[d]]))
        # size
        co <- kc$count(d)[[d]]
        if(s > co){
            expect_equal(nrow(r), co)
        } else {
            expect_equal(nrow(r), s)
        }
        }
    }
})

test_that("kibior::pull, multiple indices, nominal expected max_size asked", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # arbitrary sizes
    for(s in c(10, 5000, 10000000)){
        r <- kc$pull(c("starwars", "storms"), max_size = s)
        expect_length(r, 2)
        # dimensions
        expect_setequal(names(r$starwars), c("kid", names(ds[["starwars"]])))
        expect_setequal(names(r$storms), c("kid", names(dplyr::storms)))
        swco <- kc$count("starwars")[["starwars"]]
        stco <- kc$count("storms")[["storms"]]
        if(s > swco){
            expect_equal(nrow(r$starwars), swco)
        } else {
            expect_equal(nrow(r$starwars), s)
        }
        if(s > stco){
            expect_equal(nrow(r$storms), stco)
        } else {
            expect_equal(nrow(r$storms), s)
        }
    }
})

test_that("kibior::pull, indices via pattern, nominal expected max_size asked", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # arbitrary sizes
    for(s in c(10, 5000, 10000000)){
        r <- kc$pull("s*", max_size = s)
        expect_length(r, 2)
        # dimensions
        expect_setequal(names(r$starwars), c("kid", names(ds[["starwars"]])))
        expect_setequal(names(r$storms), c("kid", names(dplyr::storms)))
        swco <- kc$count("starwars")[["starwars"]]
        stco <- kc$count("storms")[["storms"]]
        if(s > swco){
            expect_equal(nrow(r$starwars), swco)
        } else {
            expect_equal(nrow(r$starwars), s)
        }
        if(s > stco){
            expect_equal(nrow(r$storms), stco)
        } else {
            expect_equal(nrow(r$storms), s)
        }
    }
})




test_that("kibior::pull, nominal too short scroll timer", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # really short timer 1 nanosecond so the scroll connection expires
    # and cannot retrieve the data
    for(d in names(ds)){
        expect_error(kc$pull(d, scroll_timer = "1ns"))
    }
})

test_that("kibior::pull, wrong scroll timer", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    for(d in names(ds)){
        expect_error(kc$pull(d, scroll_timer = "NOOOOOPE"))
    }
})



test_that("kibior::pull, keep metadata, single index", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    for(d in names(ds)){
        # ask meta
        r <- kc$pull(d, keep_metadata = TRUE)[[d]]
        expect_equal(r[["_index"]][[1]], d)
        expect_setequal(r[["_id"]], r[["_source.kid"]])
        # compare colnames with no metadata result
        rr <- kc$pull(d, keep_metadata = FALSE)[[d]]
        expect_equal(nrow(rr), nrow(r))
        colnames <- names(r) %>% 
        lapply(function(x){ if(startsWith(x, "_source.")) x else NULL }) %>% 
        lapply(function(x){ gsub("_source.", "", x) }) %>% 
        unlist(use.names = FALSE)
        expect_setequal(colnames, names(rr))
    }
})

test_that("kibior::pull, keep metadata, multiple indices", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # ask meta
    r <- kc$pull(c("starwars", "storms"), keep_metadata = TRUE)
    expect_length(r, 2)
    # dimensions
    for(i in names(r)){
        # test some metadata cols
        expect_true(all(c("_index", "_version") %in% names(r[[i]])))
        # select cols with "_source." in the name and remove the rest
        colnames <- names(r[[i]]) %>% 
        lapply(function(x){ if(startsWith(x, "_source.")) x else NULL }) %>% 
        lapply(function(x){ gsub("_source.", "", x) }) %>% 
        unlist(use.names = FALSE)
        expect_true(all(names(ds[[i]]) %in% colnames))
    }
})

test_that("kibior::pull, keep metadata, indices via pattern", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # ask meta
    r <- kc$pull("s*", keep_metadata = TRUE)
    expect_length(r, 2)
    # dimensions
    for(i in names(r)){
        expect_true(all(c("_index", "_version") %in% names(r[[i]])))
        # select cols with "_source." in the name and remove the rest
        colnames <- names(r[[i]]) %>% 
        lapply(function(x){ if(startsWith(x, "_source.")) x else NULL }) %>% 
        lapply(function(x){ gsub("_source.", "", x) }) %>% 
        unlist(use.names = FALSE)
        expect_true(all(names(ds[[i]]) %in% colnames))
    }
})




test_that("kibior::pull, nominal, single index, columns NULL is complete", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    for(d in names(ds)){
        r <- kc$pull(d, columns = NULL)[[d]]
        expect_setequal(names(r), c("kid", names(ds[[d]])) )
    }
})

test_that("kibior::pull, nominal, multiple indices, columns NULL is complete", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # 
    r <- kc$pull(c("starwars", "storms"), columns = NULL)
    expect_length(r, 2)
    for(i in names(r)){
        expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])) )
    }
})

test_that("kibior::pull, nominal, indices via pattern, columns NULL is complete", {
    # push data
    remove_all_indices()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # 
    r <- kc$pull("s*", columns = NULL)
    expect_length(r, 2)
    for(i in names(r)){
        expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])) )
    }
})



test_that("kibior::pull, nominal, one index, select some columns, without metadata", {
    # push data
    remove_all_indices()
    selected_columns <- list()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
        # select two columns randomly
        selected_columns[[d]] <- names(ds[[d]]) %>% sample(2)
    }
    expect_setequal(kc$list(), names(ds))
    for(d in names(ds)){
        # ask for randomly selected columns
        r <- kc$pull(d, columns = selected_columns[[d]])[[d]]
        expect_setequal(names(r), selected_columns[[d]])
    }
})

test_that("kibior::pull, nominal, one index, select some columns, with metadata", {
    # push data
    remove_all_indices()
    selected_columns <- list()
    selected_columns_with_source <- list()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
        # select two columns randomly
        selected_columns[[d]] <- names(ds[[d]]) %>% sample(2) 
        # force the name to be "_source.<field>" to cmp more easily after
        selected_columns_with_source[[d]] <- selected_columns[[d]] %>% 
        lapply(function(x){ paste0("_source.", x) }) %>% 
        unlist(use.names = FALSE)
    }
    expect_setequal(kc$list(), names(ds))
    for(d in names(ds)){
        # select two mentionned columns
        r <- kc$pull(d, columns = selected_columns[[d]], keep_metadata = TRUE)[[d]]
        # compare with "_source.<field>"
        expect_true(all(selected_columns_with_source[[d]] %in% names(r)))
    }
})



test_that("kibior::pull, nominal, all indices, select one field only present in two datasets, without metadata", {
    # push data
    remove_all_indices()
    selected_columns <- list()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # we want to test specific names of columns
    # the field "name" is present in "starwars" and "storms" dataset only
    # should no get anything from "storms" dataset
    r <- kc$pull("*", columns = "name", keep_metadata = FALSE)
    expect_length(r, 2)
    expect_true("name" %in% names(r$starwars))
    expect_true(!("_index" %in% names(r$starwars)))
    expect_true("name" %in% names(r$storms))
    expect_true(!("_index" %in% names(r$storms)))

})

test_that("kibior::pull, nominal, all indices, select one field only present in two datasets, with metadata", {
    # push data
    remove_all_indices()
    selected_columns <- list()
    for(d in names(ds)){
        # by default, field 'kid' is the field used as id
        res <- kc$push(ds[[d]], d)
        expect_equal(res, d)
    }
    expect_setequal(kc$list(), names(ds))
    # we want to test specific names of columns
    # the field "name" is present in "starwars" and "storms" dataset only
    # should no get anything from "storms" dataset
    r <- kc$pull("*", columns = "name", keep_metadata = TRUE)
    expect_length(r, 2)
    expect_true("_source.name" %in% names(r$starwars))
    expect_true("_index" %in% names(r$starwars))
    expect_true("_source.name" %in% names(r$storms))
    expect_true("_index" %in% names(r$storms))
})

# end pull




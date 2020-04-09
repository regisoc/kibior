

context("Search")





# start search ----

test_that("kibior::search, wrong args", {
  remove_all_indices()
  res <- kc$push(starwars, single_index_name)
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
  # index name
  for(i in a){
    if(!is.null(i)){
      expect_error(kc$search(index_name = i, head = FALSE))
    }
  }
  # bulk 
  for(i in a){
    expect_error(kc$search(single_index_name, bulk_size = NA, head = FALSE))
  }
  # max size
  for(i in a){
    if(!is.null(i)){ # if max_size is not null, else it returns everything
      expect_error(kc$search(single_index_name, max_size = i, head = FALSE))
    }
  }
  # scroll timer
  for(i in a){
    expect_error(kc$search(single_index_name, scroll_timer = i, head = FALSE))
  }
  # keep metadata 
  for(i in a){
    if(!is.logical(i)){
      expect_error(kc$search(single_index_name, keep_metadata = i, head = FALSE))
    }
  }
  expect_error(kc$search(single_index_name, keep_metadata = NA, head = FALSE))
  # fields filters 
  for(i in a){
    # if fields is not null or string, else it returns everything
    if(!is.null(i) && !is.character(i)){ 
      expect_error(kc$search(single_index_name, fields = i, head = FALSE))
    }
  }
  # fields filters with metadata
  for(i in a){
    # if fields is not null or string, else it returns everything
    if(!is.null(i) && !is.character(i)){ 
      expect_error(kc$search(single_index_name, keep_metadata = TRUE, fields = i, head = FALSE))
    }
  }
  # query
  for(i in a){
    if(is.character(i) && i != "NOPE"){ # this works but returns no results
      expect_error(kc$search(single_index_name, query = i, head = FALSE))
    }
  }
  # head
  for(i in a){
    if(!is.logical(i)){ 
      expect_error(kc$search(single_index_name, head = i))
    }
  }
  expect_error(kc$search(single_index_name, head = NA))
})



test_that("kibior::search, nominal simple case, get one index", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # 
  for(d in names(ds)){
    r <- kc$search(d, head = FALSE)[[d]]
    expect_setequal(names(r), c("kid", kc$get_fields(d)[[d]]))
    expect_equal(nrow(r), kc$count(d)[[d]])
  }
})

test_that("kibior::search, nominal simple case, get two indices", {
  # push data
  remove_all_indices()
  selected_fields <- list()
  selected_fields_with_source <- list()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # get "starwars" and "diamonds" datasets
  r <- kc$search(c("starwars", "diamonds"), head = FALSE)
  expect_length(r, 2)
  expect_setequal(c("starwars", "diamonds"), names(r))
  # test names
  expect_equal(c("kid", names(starwars)), names(r$starwars))
  expect_equal(c("kid", names(ggplot2::diamonds)), names(r$diamonds))
  # test dimensions
  expect_equal(nrow(starwars), nrow(r$starwars))
  expect_equal(nrow(ggplot2::diamonds), nrow(r$diamonds))
})

test_that("kibior::search, nominal simple case, get indices via pattern", {
  # push data
  remove_all_indices()
  selected_fields <- list()
  selected_fields_with_source <- list()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # get "starwars" and "storms" datasets
  r <- kc$search("s*", head = FALSE)
  expect_length(r, 2)
  expect_setequal(c("starwars", "storms"), names(r))
  # test names
  expect_equal(c("kid", names(starwars)), names(r$starwars))
  expect_equal(c("kid", names(storms)), names(r$storms))
  # test dimensions
  expect_equal(nrow(starwars), nrow(r$starwars))
  expect_equal(nrow(storms), nrow(r$storms))
})


test_that("kibior::search, wrong index names", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  #
  expect_error(kc$search("aaaa"), head = FALSE)
  expect_error(kc$search("bbbb"), head = FALSE)
  expect_error(kc$search("cccc"), head = FALSE)
})



test_that("kibior::search, nominal simple case, single index, no impact regarding bulk_size", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # 
  for(b in c(10, 100, 1000, 10000)){
    for(d in names(ds)){
      r <- kc$search(d, bulk_size = b, head = FALSE)[[d]]
      expect_setequal(names(r), c("kid", kc$get_fields(d)[[d]]))
      expect_equal(nrow(r), kc$count(d)[[d]])
    }
  }
})

test_that("kibior::search, nominal simple case, multiple indices, no impact regarding bulk_size", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # 
  for(b in c(10, 100, 1000, 10000)){
    r <- kc$search(c("starwars", "diamonds"), bulk_size = b, head = FALSE)
    expect_length(r, 2)
    # dimension
    expect_setequal(names(r$starwars), c("kid", names(starwars)))
    expect_setequal(names(r$diamonds), c("kid", names(ggplot2::diamonds)))
    expect_equal(nrow(r$starwars), nrow(starwars))
    expect_equal(nrow(r$diamonds), nrow(ggplot2::diamonds))
  }
})

test_that("kibior::search, nominal simple case, get via pattern, no impact regarding bulk_size", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # 
  for(b in c(10, 100, 1000, 10000)){
    # get starwars and storms datasets
    r <- kc$search("s*", bulk_size = b, head = FALSE)
    expect_length(r, 2)
    # dimension
    expect_setequal(names(r$starwars), c("kid", names(starwars)))
    expect_setequal(names(r$storms), c("kid", names(storms)))
    expect_equal(nrow(r$starwars), nrow(starwars))
    expect_equal(nrow(r$storms), nrow(storms))
  }
})



test_that("kibior::search, single index, nominal expected max_size asked", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # arbitrary sizes
  for(s in c(10, 50, 100, 500, 1000, 5000, 10000, 50000, 10000000)){
    for(d in names(ds)){
      r <- kc$search(d, max_size = s, head = FALSE)[[d]]
      expect_setequal(names(r), c("kid", kc$get_fields(d)[[d]]))
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

test_that("kibior::search, multiple indices, nominal expected max_size asked", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # arbitrary sizes
  for(s in c(10, 50, 100, 500, 1000, 5000, 10000, 50000, 10000000)){
    r <- kc$search(c("starwars", "storms"), max_size = s, head = FALSE)
    expect_length(r, 2)
    # dimensions
    expect_setequal(names(r$starwars), c("kid", names(starwars)))
    expect_setequal(names(r$storms), c("kid", names(storms)))
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

test_that("kibior::search, indices via pattern, nominal expected max_size asked", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # arbitrary sizes
  for(s in c(10, 50, 100, 500, 1000, 5000, 10000, 50000, 10000000)){
    r <- kc$search("s*", max_size = s, head = FALSE)
    expect_length(r, 2)
    # dimensions
    expect_setequal(names(r$starwars), c("kid", names(starwars)))
    expect_setequal(names(r$storms), c("kid", names(storms)))
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




test_that("kibior::search, nominal too short scroll timer", {
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
    expect_error(kc$search(d, scroll_timer = "1ns", head = FALSE))
  }
})

test_that("kibior::search, wrong scroll timer", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  for(d in names(ds)){
    expect_error(kc$search(d, scroll_timer = "NOOOOOPE", head = FALSE))
  }
})



test_that("kibior::search, keep metadata, single index", {
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
    r <- kc$search(d, keep_metadata = TRUE, head = FALSE)[[d]]
    expect_equal(r[["_index"]][[1]], d)
    expect_setequal(r[["_id"]], r[["_source.kid"]])
    # compare colnames with no metadata result
    rr <- kc$search(d, keep_metadata = FALSE, head = FALSE)[[d]]
    expect_equal(nrow(rr), nrow(r))
    colnames <- names(r) %>% 
      lapply(function(x){ if(startsWith(x, "_source.")) x else NULL }) %>% 
      lapply(function(x){ gsub("_source.", "", x) }) %>% 
      unlist(use.names = FALSE)
    expect_setequal(colnames, names(rr))
  }
})

test_that("kibior::search, keep metadata, multiple indices", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # ask meta
  r <- kc$search(c("starwars", "diamonds"), keep_metadata = TRUE, head = FALSE)
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

test_that("kibior::search, keep metadata, indices via pattern", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # ask meta
  r <- kc$search("s*", keep_metadata = TRUE, head = FALSE)
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




test_that("kibior::search, nominal, single index, fields NULL is complete", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  for(d in names(ds)){
    r <- kc$search(d, fields = NULL, head = FALSE)[[d]]
    expect_setequal(names(r), c("kid", names(ds[[d]])) )
  }
})

test_that("kibior::search, nominal, multiple indices, fields NULL is complete", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # 
  r <- kc$search(c("starwars", "diamonds"), fields = NULL, head = FALSE)
  expect_length(r, 2)
  for(i in names(r)){
    expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])) )
  }
})

test_that("kibior::search, nominal, indices via pattern, fields NULL is complete", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # 
  r <- kc$search("s*", fields = NULL, head = FALSE)
  expect_length(r, 2)
  for(i in names(r)){
    expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])) )
  }
})



test_that("kibior::search, nominal, one index, select some fields, without metadata", {
  # push data
  remove_all_indices()
  selected_fields <- list()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
    # select two fields randomly
    selected_fields[[d]] <- names(ds[[d]]) %>% sample(2)
  }
  expect_setequal(kc$list(), names(ds))
  for(d in names(ds)){
    # ask for randomly selected fields
    r <- kc$search(d, fields = selected_fields[[d]], head = FALSE)[[d]]
    expect_setequal(names(r), selected_fields[[d]])
  }
})

test_that("kibior::search, nominal, one index, select some fields, with metadata", {
  # push data
  remove_all_indices()
  selected_fields <- list()
  selected_fields_with_source <- list()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
    # select two fields randomly
    selected_fields[[d]] <- names(ds[[d]]) %>% sample(2) 
    # force the name to be "_source.<field>" to cmp more easily after
    selected_fields_with_source[[d]] <- selected_fields[[d]] %>% 
      lapply(function(x){ paste0("_source.", x) }) %>% 
      unlist(use.names = FALSE)
  }
  expect_setequal(kc$list(), names(ds))
  for(d in names(ds)){
    # select two mentionned fields
    r <- kc$search(d, fields = selected_fields[[d]], keep_metadata = TRUE, head = FALSE)[[d]]
    # compare with "_source.<field>"
    expect_true(all(selected_fields_with_source[[d]] %in% names(r)))
  }
})



test_that("kibior::search, nominal, all indices, select one field only present in two datasets, without metadata", {
  # push data
  remove_all_indices()
  selected_fields <- list()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # we want to test specific names of fields
  # the field "name" is present in "starwars" and "storms" dataset only
  # should no get anything from "diamonds" dataset
  r <- kc$search("*", fields = "name", keep_metadata = FALSE, head = FALSE)
  expect_length(r, 3)
  expect_true("name" %in% names(r$starwars))
  expect_true(!("_index" %in% names(r$starwars)))
  expect_true("name" %in% names(r$storms))
  expect_true(!("_index" %in% names(r$storms)))
  expect_true(!("name" %in% names(r$diamonds)))
  expect_true(!("_index" %in% names(r$diamonds)))

})

test_that("kibior::search, nominal, all indices, select one field only present in two datasets, with metadata", {
  # push data
  remove_all_indices()
  selected_fields <- list()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # we want to test specific names of fields
  # the field "name" is present in "starwars" and "storms" dataset only
  # should no get anything from "diamonds" dataset
  r <- kc$search("*", fields = "name", keep_metadata = TRUE, head = FALSE)
  expect_length(r, 3)
  expect_true("_source.name" %in% names(r$starwars))
  expect_true("_index" %in% names(r$starwars))
  expect_true("_source.name" %in% names(r$storms))
  expect_true("_index" %in% names(r$storms))
  expect_true(!("_source.name" %in% names(r$diamonds)))
  expect_true("_index" %in% names(r$diamonds))
})




# HEAD

test_that("kibior::search, head search, one index", {
  # push data
  remove_all_indices()
  res <- kc$push(starwars, "starwars")
  expect_equal(res, "starwars")
  expect_setequal(kc$list(), "starwars")
  # head on
  r <- kc$search("starwars", head = TRUE)
  expect_length(r, 1)
  expect_equal(nrow(r$starwars), kc$head_search_size)
  expect_setequal(names(r$starwars), c("kid", names(starwars)))

  # head off
  r <- kc$search("starwars", head = FALSE)
  expect_length(r, 1)
  expect_equal(nrow(r$starwars), nrow(starwars))
  expect_setequal(names(r$starwars), c("kid", names(starwars)))
})

test_that("kibior::search, head search, all indices", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # head on
  r <- kc$search("*", head = TRUE)
  expect_length(r, length(ds))
  for(i in names(r)){
    expect_equal(nrow(r[[i]]), kc$head_search_size)
    expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])))
  }
  # head off
  r <- kc$search("*", head = FALSE)
  expect_length(r, length(ds))
  for(i in names(r)){
    expect_equal(nrow(r[[i]]), nrow(ds[[i]]))
    expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])))
  }
})







# end search




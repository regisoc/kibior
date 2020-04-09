

context("Move data")





# start export ----

test_that("kibior::export, wrong args", {
  expect_error(kc$export())
  expect_error(kc$export(data = NULL))
  expect_error(kc$export(data = starwars, filepath = NULL))
  expect_error(kc$export(starwars, NULL))
  expect_error(kc$export("sw*", filepath = temp_filepath)) # pattern search
})

test_that("kibior::export, force arg wrong arg", {
  expect_error(kc$export(starwars, temp_filepath, force = NULL))
  expect_error(kc$export(starwars, temp_filepath, force = c()))
  expect_error(kc$export(starwars, temp_filepath, force = list()))
  expect_error(kc$export(starwars, temp_filepath, force = 3))
  expect_error(kc$export(starwars, temp_filepath, force = 3.56))
  expect_error(kc$export(starwars, temp_filepath, force = "wesh"))
})

test_that("kibior::export, wrong elastic index name", {
  remove_all_indices()
  expect_error(kc$export("unknown-index", temp_filepath))
})

test_that("kibior::export, file already exists", {
  remove_temp_files()
  res <- kc$export(starwars, temp_filepath)
  expect_equal(res, temp_filepath)
  expect_true(file.exists(temp_filepath))
  expect_equal(count_nb_lines(temp_filepath), nrow(starwars) + 1)
  expect_error(kc$export(starwars, temp_filepath))
})

test_that("kibior::export, nominal case, export from memory", {
  remove_temp_files()
  res <- kc$export(starwars, temp_filepath)
  expect_equal(res, temp_filepath)
  expect_true(file.exists(temp_filepath))
  expect_equal(count_nb_lines(temp_filepath), nrow(starwars) + 1)
  expect_equal(kc$export(starwars, temp_filepath, force = TRUE), temp_filepath)
})

test_that("kibior::export, nominal case, export from elasticsearch", {
  remove_temp_files()
  remove_all_indices()
  iname <- "sw"
  res <- kc$push(starwars, iname)
  expect_equal(res, iname)
  # 
  res <- kc$export(iname, temp_filepath)
  expect_equal(res, temp_filepath)
  expect_true(file.exists(temp_filepath))
  expect_equal(count_nb_lines(temp_filepath), nrow(starwars) + 1)
})

# end export




# start import ----

test_that("kibior::import, wrong args", {
  remove_all_indices()
  expect_error(kc$export())
  expect_error(kc$export(c(), "ok"))
  expect_error(kc$export(list(), "ok"))
  expect_error(kc$export(list(123, "asd"), "ok"))
  expect_error(kc$export(123, "ok"))
  expect_error(kc$export(123.234, "ok"))
  expect_error(kc$export("filenotfound", "ok"))
  expect_error(kc$export(c("sadf", "sdf"), "ok"))
  # duplicate
  expect_error(kc$export(temp_filepath, duplicate_to = NULL))
  expect_error(kc$export(temp_filepath))
  expect_error(kc$export(temp_filepath, duplicate_to = 123))
  expect_error(kc$export(temp_filepath, duplicate_to = 123.234))
  expect_error(kc$export(temp_filepath, duplicate_to = c()))
  expect_error(kc$export(temp_filepath, duplicate_to = c("new1", "new2")))
  expect_error(kc$export(temp_filepath, duplicate_to = list(123, "sdf")))
})

test_that("kibior::import, nominal case", {
  remove_all_indices()
  remove_temp_files()
  kc$export(starwars, temp_filepath)
  # no push
  res <- kc$import(filepath = temp_filepath)
  expect_equal(nrow(res), nrow(starwars))
  expect_equal(ncol(res), ncol(starwars))
  # push 
  res <- kc$import(filepath = temp_filepath, duplicate_to = "starwars")
  expect_equal(nrow(res), nrow(starwars))
  expect_equal(ncol(res), ncol(starwars))
  expect_equal(kc$list(), "starwars")
  res <- kc$pull("starwars")[["starwars"]]
  expect_equal(nrow(res), nrow(starwars))
  expect_equal(ncol(res), ncol(starwars) + 1)
})

# end import



# start push ----

test_that("kibior::push, wrong args", {
  expect_error(kc$push())
  # data
  expect_error(kc$push(data = 3))
  expect_error(kc$push(data = 3.123))
  expect_error(kc$push(data = "sdfff"))
  expect_error(kc$push(data = c()))
  expect_error(kc$push(data = c("w", "e", "s", "h")))
  expect_error(kc$push(data = list()))
  expect_error(kc$push(data = list("w", "e", "s", "h")))
  expect_error(kc$push(data = starwars))
  # index name
  expect_error(kc$push(data = NULL, index_name = starwars))
  expect_error(kc$push(index_name = starwars))
  expect_error(kc$push(data = starwars, index_name = NA))
  expect_error(kc$push(data = starwars, index_name = 1))
  expect_error(kc$push(data = starwars, index_name = 1.234))
  expect_error(kc$push(data = starwars, index_name = c()))
  expect_error(kc$push( starwars, index_name = c("w", "e", "s", "h")))
  expect_error(kc$push( starwars, index_name = list()))
  expect_error(kc$push( starwars, index_name = list("w", "e", "s", "h")))
  # bulk 
  expect_error(kc$push( starwars, "sw", bulk_size = NA))
  expect_error(kc$push( starwars, "sw", bulk_size = TRUE))
  expect_error(kc$push( starwars, "sw", bulk_size = FALSE))
  expect_error(kc$push( starwars, "sw", bulk_size = NULL))
  expect_error(kc$push( starwars, "sw", bulk_size = 0))
  expect_error(kc$push( starwars, "sw", bulk_size = -1))
  expect_error(kc$push( starwars, "sw", bulk_size = -100))
  expect_error(kc$push( starwars, "sw", bulk_size = c()))
  expect_error(kc$push( starwars, "sw", bulk_size = c("w", "e", "s", "h")))
  expect_error(kc$push( starwars, "sw", bulk_size = list()))
  expect_error(kc$push( starwars, "sw", bulk_size = list("w", "e", "s", "h")))
  expect_error(kc$push( starwars, "sw", bulk_size = "NOPE"))
  # mode args
  a <- list(
    NA,
    TRUE,
    FALSE,
    0,
    -1,
    2.4543451,
    -100,
    c("w", "e", "s", "h"),
    list(),
    list("w", "e", "s", "h"),
    "NOPE"
  )
  # mode
  expect_error(kc$push( starwars, "sw", mode = NULL))
  for(i in a){
    expect_error(kc$push( starwars, "sw", mode = i))
  }
  # mode = check
  for(i in a){
    expect_error(kc$push( starwars, "sw", mode = "check", id_col = i))
  }
  # mode = update
  expect_error(kc$push( starwars, "sw", mode = "update", id_col = NULL))
  for(i in a){
    expect_error(kc$push( starwars, "sw", mode = "update", id_col = i))
  }
  # mode = recreate
  for(i in a){
    expect_error(kc$push( starwars, "sw", mode = "recreate", id_col = i))
  }
})

test_that("kibior::push, nominal case, single index, no id_col", {
  remove_all_indices()
  res <- kc$push(starwars, "sw")
  expect_equal(res, "sw")
  # dim ok
  expect_equal(length(kc$list()), 1)
  res <- kc$count("sw")[["sw"]]
  expect_equal(res, nrow(starwars))
  res <- kc$get_fields("sw")[["sw"]]
  expect_setequal(res, c("kid", names(starwars)))
})

test_that("kibior::push, nominal case, multiple indices x data, no id_col", {
  remove_all_indices()
  for(d in names(ds)){
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
    res <- kc$count(d)[[d]]
    expect_equal(res, nrow(ds[[d]]))
    res <- kc$get_fields(d)[[d]]
    expect_setequal(res, c("kid", names(ds[[d]])))
  }
  expect_equal(length(kc$list()), length(ds))
})

test_that("kibior::push, nominal case, data with index already created, check mode", {
  remove_all_indices()
  res <- kc$create(single_index_name)[[single_index_name]]
  expect_true(res$acknowledged)
  # check
  expect_error(kc$push(starwars, single_index_name, mode = "check"))
})

test_that("kibior::push, nominal case, data with index already created, recreate mode", {
  remove_all_indices()
  res <- kc$create(single_index_name)[[single_index_name]]
  expect_true(res$acknowledged)
  # recreate
  res <- kc$push(starwars, single_index_name, mode = "recreate")
  expect_equal(res, single_index_name)
  expect_equal(kc$count(single_index_name)[[single_index_name]], nrow(starwars))
  expect_setequal(kc$get_fields(single_index_name)[[single_index_name]], c("kid", names(starwars)))
})

test_that("kibior::push, nominal case, data with index already created, update mode", {
  remove_all_indices()
  res <- kc$create(single_index_name)[[single_index_name]]
  expect_true(res$acknowledged)
  # update
  res <- kc$push(starwars, single_index_name, mode = "update", id_col = "name")
  expect_equal(res, single_index_name)
  expect_equal(kc$count(single_index_name)[[single_index_name]], nrow(starwars))
  # x2 because no id_col, es will determine random ids
  expect_setequal(kc$get_fields(single_index_name)[[single_index_name]], names(starwars))
})


test_that("kibior::push, nominal case, data with index not created before, check mode", {
  remove_all_indices()
  # 
  res <- kc$push(starwars, single_index_name, mode = "check")
  expect_equal(res, single_index_name)
  expect_equal(kc$count(single_index_name)[[single_index_name]], nrow(starwars))
  expect_setequal(kc$get_fields(single_index_name)[[single_index_name]], c("kid", names(starwars)))
})

test_that("kibior::push, nominal case, data with index not created before, recreate mode", {
  remove_all_indices()
  # recreate
  res <- kc$push(starwars, single_index_name, mode = "recreate")
  expect_equal(res, single_index_name)
  expect_equal(kc$count(single_index_name)[[single_index_name]], nrow(starwars))
  expect_setequal(kc$get_fields(single_index_name)[[single_index_name]], c("kid", names(starwars)))
})

test_that("kibior::push, nominal case, data with index not created before, update mode", {
  remove_all_indices()
  # update
  expect_error(kc$push(starwars, single_index_name, mode = "update"))
})


test_that("kibior::push, wrong id_col names", {
  remove_all_indices()
  # does not exists 
  expect_error(kc$push(starwars, single_index_name, id_col = "slkdfjslkdfjskldjf"))
  # not unique
  expect_error(kc$push(starwars, single_index_name, id_col = "homeworld"))
  # multiple not allowed
  expect_error(kc$push(starwars, single_index_name, id_col = c("homeworld", "name")))
})

test_that("kibior::push, nominal id_col, no id_col defined", {
  remove_all_indices()
  # default, let ES do
  res <- kc$push(starwars, single_index_name, id_col = NULL)
  expect_equal(res, single_index_name)
  expect_equal(kc$count(single_index_name)[[single_index_name]], nrow(starwars))
  expect_setequal(c("kid", names(starwars)), kc$get_fields(single_index_name)[[single_index_name]])
  # Metadata "_id" ES field is a sequence
  res <- kc$pull(single_index_name, keep_metadata = TRUE)
  res_id <- res[[single_index_name]] %>% 
    dplyr::select("_id") %>% 
    as.list() %>% 
    .[["_id"]]
  res_source_id <- res[[single_index_name]] %>% 
    dplyr::select("_source.kid") %>% 
    as.list() %>% 
    .[["_source.kid"]]
  expect_setequal(res_id, res_source_id)
})

test_that("kibior::push, nominal id_col, good id_col defined", {
  remove_all_indices()
  res <- kc$push(starwars, single_index_name, id_col = "name") # unique col
  expect_equal(res, single_index_name)
  expect_equal(kc$count(single_index_name)[[single_index_name]], nrow(starwars))
  # no "kid" col added
  expect_setequal(names(starwars), kc$get_fields(single_index_name)[[single_index_name]])
  # Metadata "_id" ES field is what we defined
  res <- kc$pull(single_index_name, keep_metadata = TRUE)
  res_id <- res[[single_index_name]] %>% 
    dplyr::select("_id") %>% 
    as.list() %>% 
    .[["_id"]]
  res_source_id <- res[[single_index_name]] %>% 
    dplyr::select("_source.name") %>% 
    as.list() %>% 
    .[["_source.name"]]
  expect_setequal(res_id, res_source_id)
})

# end push






# start pull ----

test_that("kibior::pull, wrong args", {
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
  # no args
  expect_error(kc$pull())
  # index name
  for(i in a){
    expect_error(kc$pull(index_name = i))
  }
  # bulk 
  for(i in a){
    expect_error(kc$pull(single_index_name, bulk_size = NA))
  }
  # max size
  for(i in a){
    if(!is.null(i)){ # if max_size is not null, else it returns everything
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
  # fields filters 
  for(i in a){
    # if fields is not null or string, else it returns everything
    if(!is.null(i) && !is.character(i)){ 
      expect_error(kc$pull(single_index_name, fields = i))
    }
  }
  # fields filters with metadata
  for(i in a){
    # if fields is not null or string, else it returns everything
    if(!is.null(i) && !is.character(i)){ 
      expect_error(kc$pull(single_index_name, keep_metadata = TRUE, fields = i))
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
    expect_setequal(names(r), c("kid", kc$get_fields(d)[[d]]))
    expect_equal(nrow(r), kc$count(d)[[d]])
  }
})

test_that("kibior::pull, nominal simple case, get two indices", {
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
  r <- kc$pull(c("starwars", "diamonds"))
  expect_length(r, 2)
  expect_setequal(c("starwars", "diamonds"), names(r))
  # test names
  expect_equal(c("kid", names(starwars)), names(r$starwars))
  expect_equal(c("kid", names(ggplot2::diamonds)), names(r$diamonds))
  # test dimensions
  expect_equal(nrow(starwars), nrow(r$starwars))
  expect_equal(nrow(ggplot2::diamonds), nrow(r$diamonds))
})

test_that("kibior::pull, nominal simple case, get indices via pattern", {
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
  r <- kc$pull("s*")
  expect_length(r, 2)
  expect_setequal(c("starwars", "storms"), names(r))
  # test names
  expect_equal(c("kid", names(starwars)), names(r$starwars))
  expect_equal(c("kid", names(storms)), names(r$storms))
  # test dimensions
  expect_equal(nrow(starwars), nrow(r$starwars))
  expect_equal(nrow(storms), nrow(r$storms))
})


test_that("kibior::pull, wrong index names", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  #
  expect_error(kc$pull("aaaa"))
  expect_error(kc$pull("bbbb"))
  expect_error(kc$pull("cccc"))
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
  for(b in c(10, 100, 1000, 10000)){
    for(d in names(ds)){
      r <- kc$pull(d, bulk_size = b)[[d]]
      expect_setequal(names(r), c("kid", kc$get_fields(d)[[d]]))
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
  for(b in c(10, 100, 1000, 10000)){
    r <- kc$pull(c("starwars", "diamonds"), bulk_size = b)
    expect_length(r, 2)
    # dimension
    expect_setequal(names(r$starwars), c("kid", names(starwars)))
    expect_setequal(names(r$diamonds), c("kid", names(ggplot2::diamonds)))
    expect_equal(nrow(r$starwars), nrow(starwars))
    expect_equal(nrow(r$diamonds), nrow(ggplot2::diamonds))
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
  for(b in c(10, 100, 1000, 10000)){
    # get starwars and storms datasets
    r <- kc$pull("s*", bulk_size = b)
    expect_length(r, 2)
    # dimension
    expect_setequal(names(r$starwars), c("kid", names(starwars)))
    expect_setequal(names(r$storms), c("kid", names(storms)))
    expect_equal(nrow(r$starwars), nrow(starwars))
    expect_equal(nrow(r$storms), nrow(storms))
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
  for(s in c(10, 50, 100, 500, 1000, 5000, 10000, 50000, 10000000)){
    for(d in names(ds)){
      r <- kc$pull(d, max_size = s)[[d]]
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

test_that("kibior::pull, multiple indices, nominal expected max_size asked", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # arbitrary sizes
  for(s in c(10, 50, 100, 500, 1000, 5000, 10000, 50000, 10000000)){
    r <- kc$pull(c("starwars", "storms"), max_size = s)
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

test_that("kibior::pull, indices via pattern, nominal expected max_size asked", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # arbitrary sizes
  for(s in c(10, 50, 100, 500, 1000, 5000, 10000, 50000, 10000000)){
    r <- kc$pull("s*", max_size = s)
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
  r <- kc$pull(c("starwars", "diamonds"), keep_metadata = TRUE)
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




test_that("kibior::pull, nominal, single index, fields NULL is complete", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  for(d in names(ds)){
    r <- kc$pull(d, fields = NULL)[[d]]
    expect_setequal(names(r), c("kid", names(ds[[d]])) )
  }
})

test_that("kibior::pull, nominal, multiple indices, fields NULL is complete", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # 
  r <- kc$pull(c("starwars", "diamonds"), fields = NULL)
  expect_length(r, 2)
  for(i in names(r)){
    expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])) )
  }
})

test_that("kibior::pull, nominal, indices via pattern, fields NULL is complete", {
  # push data
  remove_all_indices()
  for(d in names(ds)){
    # by default, field 'kid' is the field used as id
    res <- kc$push(ds[[d]], d)
    expect_equal(res, d)
  }
  expect_setequal(kc$list(), names(ds))
  # 
  r <- kc$pull("s*", fields = NULL)
  expect_length(r, 2)
  for(i in names(r)){
    expect_setequal(names(r[[i]]), c("kid", names(ds[[i]])) )
  }
})



test_that("kibior::pull, nominal, one index, select some fields, without metadata", {
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
    r <- kc$pull(d, fields = selected_fields[[d]])[[d]]
    expect_setequal(names(r), selected_fields[[d]])
  }
})

test_that("kibior::pull, nominal, one index, select some fields, with metadata", {
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
    r <- kc$pull(d, fields = selected_fields[[d]], keep_metadata = TRUE)[[d]]
    # compare with "_source.<field>"
    expect_true(all(selected_fields_with_source[[d]] %in% names(r)))
  }
})



test_that("kibior::pull, nominal, all indices, select one field only present in two datasets, without metadata", {
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
  r <- kc$pull("*", fields = "name", keep_metadata = FALSE)
  expect_length(r, 3)
  expect_true("name" %in% names(r$starwars))
  expect_true(!("_index" %in% names(r$starwars)))
  expect_true("name" %in% names(r$storms))
  expect_true(!("_index" %in% names(r$storms)))
  expect_true(!("name" %in% names(r$diamonds)))
  expect_true(!("_index" %in% names(r$diamonds)))

})

test_that("kibior::pull, nominal, all indices, select one field only present in two datasets, with metadata", {
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
  r <- kc$pull("*", fields = "name", keep_metadata = TRUE)
  expect_length(r, 3)
  expect_true("_source.name" %in% names(r$starwars))
  expect_true("_index" %in% names(r$starwars))
  expect_true("_source.name" %in% names(r$storms))
  expect_true("_index" %in% names(r$storms))
  expect_true(!("_source.name" %in% names(r$diamonds)))
  expect_true("_index" %in% names(r$diamonds))
})

# end pull







# start move ----




# # # reindex fonction (local)
# # curl -X POST "localhost:9200/_reindex?pretty" --user elastic:changeme -H 'Content-Type: application/json' -d'
# # {
# #   "source": {
# #     "index": "smpdb_proteins"
# #   },
# #   "dest": {
# #     "index": "smpdb_proteins"
# #   }
# # }'

# # # reindex fonction (remote)
# # curl -X POST "localhost:9200/_reindex?pretty" --user elastic:changeme -H 'Content-Type: application/json' -d'
# # {
# #   "source": {
# #     "remote": {
# #       "host": "http://192.168.3.70:9200"
# #     },
# #     "index": "pub_smpdb_proteins"
# #   },
# #   "dest": {
# #     "index": "smpdb_proteins"
# #   }
# # }'


test_that("kibior::move, nominal case", {
  
})

# end move





# start copy ----

test_that("kibior::copy, nominal case", {
  
})

# end copy

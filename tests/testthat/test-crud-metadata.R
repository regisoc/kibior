

context("CRUD metadata")



### ------------------------------------------ READ


# start get_metadata ----

test_that("kibior::get_metadata, null arg, no index", {
  remove_all_indices()
  expect_null(kc$get_metadata())
  expect_null(kc$get_metadata(c()))
})

test_that("kibior::get_metadata, null empty index", {
  remove_all_indices()
  expect_null(kc$get_metadata())
  res <- kc$create(single_index_name)[[1]]
  m <- kc$get_metadata(res$index)[[res$index]]
  expect_length(m, 3)
  expect_setequal((names(m)), all_features)
})

test_that("kibior::get_metadata, wrong types args", {
  kc$create(single_index_name, force = TRUE)
  expect_error(kc$get_metadata(index_name = list()))
  expect_error(kc$get_metadata(index_name = NA))
  expect_error(kc$get_metadata(index_name = c("aaa", "bbb")))
  expect_error(kc$get_metadata(index_name = list("aaa", "bbb")))
  expect_error(kc$get_metadata(index_name = list(aa="aaa", bb="bbb")))
})

test_that("kibior::get_metadata, nominal case, one index, all features present", {
  res <- kc$create(index_name = single_index_name, force = TRUE)[[1]]
  m <- kc$get_metadata(res$index)[[res$index]]
  expect_length(names(m), 3)
  expect_setequal(names(m), all_features)
})

test_that("kibior::get_metadata, nominal case, multiple indices, all features present", {
  res <- kc$create(multiple_indice_names, force = TRUE)
  m <- kc$get_metadata(names(res))
  for(i in names(m)){
    expect_length(names(m[[i]]), 3)
    expect_setequal(names(m[[i]]), all_features)
  }
})


# end get_metadata



# start get_mappings ----

test_that("kibior::get_mappings, null arg, no index", {
  remove_all_indices()
  expect_null(kc$get_mappings())
})

test_that("kibior::get_mappings, arg, no index", {
  remove_all_indices()
  expect_error(kc$get_mappings(index_name = single_index_name))
  expect_error(kc$get_mappings(index_name = multiple_indice_names))
})

test_that("kibior::get_mappings, wrong types args", {
  kc$create(single_index_name, force = TRUE)
  expect_error(kc$get_mappings(index_name = list()))
  expect_error(kc$get_mappings(index_name = NA))
  expect_error(kc$get_mappings(index_name = list("aaa", "bbb")))
  expect_error(kc$get_mappings(index_name = list(aa="aaa", bb="bbb")))
})

test_that("kibior::get_mappings, arg, single index", {
  remove_all_indices()
  res <- kc$create(single_index_name)[[1]]
  expect_true(res$acknowledged)
  #
  m <- kc$get_mappings(index_name = single_index_name)
  expect_equal(names(m), single_index_name)
  expect_error(kc$get_mappings(index_name = multiple_indice_names))
})

test_that("kibior::get_mappings, nominal case, single index, no arg, index empty", {
  remove_all_indices()
  res <- kc$create(single_index_name)[[1]]
  expect_true(res$acknowledged)
  m <- kc$get_mappings()
  expect_equal(names(m), single_index_name)
  expect_true(is_list(m[[single_index_name]]) && length(m[[single_index_name]]) == 0)
})

test_that("kibior::get_mappings, nominal case, multiple indices, no arg", {
  remove_all_indices()
  res <- kc$create(multiple_indice_names)
  for(i in names(res)){
    expect_true(res[[i]]$acknowledged)
  }
  m <- kc$get_mappings()
  expect_setequal(names(m), multiple_indice_names)
})

test_that("kibior::get_mappings, nominal case, single index, index with data", {
  remove_all_indices()
  expected_fields <- c(names(dplyr::starwars), "kid")
  res <- kc$push(dplyr::starwars, single_index_name)
  expect_equal(res, single_index_name)
  # test index name
  m <- kc$get_mappings()
  expect_equal(names(m), single_index_name)
  m2 <- kc$get_mappings(single_index_name)
  expect_equal(names(m2), single_index_name)
  expect_length(names(m2), 1)
  # test fields
  fields <- m[[single_index_name]]$properties %>% names()
  expect_setequal(fields, expected_fields)
  fields <- m2[[single_index_name]]$properties %>% names()
  expect_setequal(fields, expected_fields)
})

test_that("kibior::get_mappings, nominal case, multiple indices, indices with data", {
    remove_all_indices()
    expected_fields <- c(names(dplyr::starwars), "kid")
    for(i in multiple_indice_names){
        res <- kc$push(dplyr::starwars, i)
        expect_equal(res, i)
    }
    #
    m <- kc$get_mappings()
    expect_setequal(names(m), multiple_indice_names)
    for(i in multiple_indice_names){
        # test index name
        expect_true(i %in% names(m))
        m2 <- kc$get_mappings(i)
        expect_equal(names(m2), i)
        expect_length(names(m2), 1)
        # test fields
        fields <- m[[i]]$properties %>% names()
        expect_setequal(fields, expected_fields)
        fields <- m2[[i]]$properties %>% names()
        expect_setequal(fields, expected_fields)
    }
})

# end get_mappings



# start get_settings ----

test_that("kibior::get_settings, null arg, no index", {
  remove_all_indices()
  expect_null(kc$get_settings())
})

test_that("kibior::get_settings, arg, no index", {
  remove_all_indices()
  expect_error(kc$get_settings(index_name = single_index_name))
  expect_error(kc$get_settings(index_name = multiple_indice_names))
})

test_that("kibior::get_settings, wrong types args", {
  kc$create(single_index_name, force = TRUE)
  expect_error(kc$get_settings(index_name = list()))
  expect_error(kc$get_settings(index_name = NA))
  expect_error(kc$get_settings(index_name = list("aaa", "bbb")))
  expect_error(kc$get_settings(index_name = list(aa="aaa", bb="bbb")))
})

test_that("kibior::get_settings, arg, single index", {
  remove_all_indices()
  res <- kc$create(single_index_name)[[1]]
  expect_true(res$acknowledged)
  #
  s <- kc$get_settings(single_index_name)
  expect_equal(names(s), single_index_name)
  expect_error(kc$get_settings(multiple_indice_names))
  expect_equal(s[[single_index_name]]$index$provided_name, single_index_name)
})

test_that("kibior::get_settings, nominal case, single index, no arg, index empty", {
  remove_all_indices()
  res <- kc$create(single_index_name)[[1]]
  expect_true(res$acknowledged)
  s <- kc$get_settings()
  expect_equal(names(s), single_index_name)
  expect_equal(s[[single_index_name]]$index$provided_name, single_index_name)
})

test_that("kibior::get_settings, nominal case, multiple indices, no arg", {
  remove_all_indices()
  res <- kc$create(multiple_indice_names)
  for(i in names(res)){
    expect_true(res[[i]]$acknowledged)
  }
  s <- kc$get_settings()
  expect_setequal(names(s), multiple_indice_names)
  for(i in names(s)){
    expect_equal(s[[i]]$index$provided_name, i)
  }
})

test_that("kibior::get_settings, nominal case, single index, index with data", {
  remove_all_indices()
  res <- kc$push(dplyr::starwars, single_index_name)
  expect_equal(res, single_index_name)
  s <- kc$get_settings()
  expect_equal(names(s), single_index_name)
  # get_settings(name) == get_settings()
  m2 <- kc$get_settings(single_index_name)
  d <- setdiff(s, m2) # empty list
  expect_length(d, 0)  
  expect_true(is_list(d))
})

test_that("kibior::get_settings, nominal case, multiple indices, indices with data", {
  remove_all_indices()
  for(i in multiple_indice_names){
    res <- kc$push(dplyr::starwars, i)
    expect_equal(res, i)
  }
  s <- kc$get_settings()
  expect_setequal(names(s), multiple_indice_names)
  for(i in multiple_indice_names){
    # get_settings(name) == get_settings()
    m2 <- kc$get_settings(i)
    d <- setdiff(s, m2) # empty list
    expect_length(d, length(s) - 1)  
    expect_true(is_list(d))
  }
})


# end get_settings



# start get_aliases ----



# end get_aliases

testthat::setup({
    # remove indices if they exist
    remove_all_indices()
})

testthat::teardown({
    # remove indices if they exist
    remove_all_indices()
})


# start count ----

test_that("kibior::count, no index", {
  remove_all_indices()
  expect_error(kc$count())
  expect_error(kc$count("nope"))
})

test_that("kibior::count, wrong type", {
  remove_all_indices()
  expect_error(kc$count(type = "nope"))
  expect_error(kc$count("nope", type = "nopeagain"))
  # with data
  remove_all_indices()
  res <- kc$push(dplyr::starwars, single_index_name)
  expect_equal(res, single_index_name)
  expect_error(kc$count(single_index_name, type = "allwrong"))
  expect_error(kc$count(single_index_name, type = NULL))
  expect_error(kc$count(single_index_name, type = NA))
  expect_error(kc$count(single_index_name, type = c("wesh", "hm")))
  expect_error(kc$count(single_index_name, type = list("wesh", "hm")))
})

test_that("kibior::count, nominal case, single empty index", {
  remove_all_indices()
  res <- kc$create(single_index_name)[[1]]
  expect_true(res$acknowledged)
  expect_equal(0, kc$count(single_index_name, type = "observations")[[single_index_name]])
  expect_equal(0, kc$count(single_index_name)[[single_index_name]]) # default type = "observations"
  expect_equal(0, kc$count(single_index_name, type = "variables")[[single_index_name]])
  # test type
  t <- kc$count(single_index_name)[[single_index_name]]
  expect_equal("integer", typeof(t))
})

test_that("kibior::count, nominal case, multiple empty indices", {
  remove_all_indices()
  for(i in multiple_indice_names){
    res <- kc$create(i)[[1]]
    expect_true(res$acknowledged)
    expect_equal(0, kc$count(i, type = "observations")[[i]])
    expect_equal(0, kc$count(i)[[i]]) # default type = "observations"
    expect_equal(0, kc$count(i, type = "variables")[[i]])
    # test type
    t <- kc$count(i)[[i]]
    expect_equal("integer", typeof(t))
  }
})

test_that("kibior::count, nominal case, single index", {
  remove_all_indices()
  res <- kc$push(dplyr::starwars, single_index_name)
  expect_equal(res, single_index_name)
  expect_equal(nrow(dplyr::starwars), kc$count(single_index_name, type = "observations")[[single_index_name]])
  expect_equal(nrow(dplyr::starwars), kc$count(single_index_name)[[single_index_name]]) # default type = "observations"
  expect_equal(ncol(dplyr::starwars) + 1, kc$count(single_index_name, type = "variables")[[single_index_name]])
})

test_that("kibior::count, nominal case, multiple indices", {
  remove_all_indices()
  for(i in multiple_indice_names){
    res <- kc$push(dplyr::starwars, i)
    expect_equal(res, i)
    expect_equal(nrow(dplyr::starwars), kc$count(i, type = "observations")[[i]])
    expect_equal(nrow(dplyr::starwars), kc$count(i)[[i]]) # default type = "observations"
    expect_equal(ncol(dplyr::starwars) + 1, kc$count(i, type = "variables")[[i]])
  }
})

# end count


# start dim ----

test_that("kibior::dim, no index", {
  remove_all_indices()
  expect_error(kc$dim())
  expect_error(kc$dim("asdasdasd"))
})

test_that("kibior::dim, nominal case, single empty index", {
  remove_all_indices()
  res <- kc$create(single_index_name)[[1]]
  expect_true(res$acknowledged)
  expect_setequal(c(0L, 0L), kc$dim(single_index_name)[[single_index_name]]) 
})

test_that("kibior::dim, nominal case, multiple empty indices", {
  remove_all_indices()
  res <- kc$create(multiple_indice_names)
  expect_setequal(names(res), multiple_indice_names)
  # one by one
  for(i in multiple_indice_names){
    expect_setequal(c(0L, 0L), kc$dim(i)[[i]])
  }
  # all at once
  r <- kc$dim(multiple_indice_names)
  expect_setequal(names(r), multiple_indice_names)
  for(i in multiple_indice_names){
    expect_setequal(c(0L, 0L), r[[i]])
  }
})

test_that("kibior::dim, nominal case, single index", {
  remove_all_indices()
  res <- kc$push(dplyr::starwars, single_index_name)
  expect_equal(res, single_index_name)
  #
  nr <- nrow(dplyr::starwars)
  nc <- ncol(dplyr::starwars) + 1 # kid
  r <- kc$dim(res)[[res]]
  expect_setequal(c(nr, nc), r) 
  # test type
  expect_equal("integer", typeof(r))
})

test_that("kibior::dim, nominal case, multiple indices", {
  remove_all_indices()
  for (i in multiple_indice_names){
    res <- kc$push(dplyr::starwars, i)
    expect_equal(res, i)
  }
  #
  nr <- nrow(dplyr::starwars)
  nc <- ncol(dplyr::starwars) + 1 # kid
  # one by one
  for(i in multiple_indice_names){
    r <- kc$dim(i)[[i]]
    expect_setequal(c(nr, nc), r)
    # test type
    expect_equal("integer", typeof(r))
  }
  # all at once
  r <- kc$dim(multiple_indice_names)
  expect_setequal(names(r), multiple_indice_names)
  for(i in multiple_indice_names){
    expect_setequal(c(nr, nc), r[[i]])
  }
})

# end dim




context("CRUD index")



testthat::setup({
    # remove indices if they exist
    remove_all_indices()
    
})

testthat::teardown({
    # remove indices if they exist
    remove_all_indices()
})




# start create ----

test_that("kibior::create, nominal case single creation", {
  if(kc$has(single_index_name)){
    kc$delete(single_index_name)
  }
  res <- kc$create(single_index_name)[[1]]
  expect_true(res$acknowledged)
  expect_true(res$shards_acknowledged)
  expect_equal(res$index, single_index_name)
})

test_that("kibior::create, nominal case multiple creations", {
  if(kc$has(multiple_indice_names)){
    kc$delete(multiple_indice_names)
  }
  res <- kc$create(multiple_indice_names)
  expect_setequal(names(res), multiple_indice_names)
  expect_equal(length(res), length(multiple_indice_names))
  for(i in multiple_indice_names){
    expect_true(res[[i]]$acknowledged)
    expect_true(res[[i]]$shards_acknowledged)
  }
})

test_that("kibior::create, multiple creations with same name", {
  if(kc$has(single_index_name)){
    kc$delete(single_index_name)
  }
  m <- c(single_index_name, single_index_name)
  expect_error(kc$create(m))
})

test_that("kibior::create, pattern fail", {
  if(kc$has(single_index_name)){
    kc$delete(single_index_name)
  }
  expect_error(kc$create(index_name = "asdf*"))
  expect_error(kc$create("asdf*"))
})

test_that("kibior::create, null arg", {
  if(kc$has(single_index_name)){
    kc$delete(single_index_name)
  }
  expect_error(kc$create(index_name = NULL))
  expect_error(kc$create(NULL))
})

test_that("kibior::create, wrong types args", {
  if(kc$has(single_index_name)){
    kc$delete(single_index_name)
  }
  expect_error(kc$create(123)) # no type coercion
  expect_error(kc$create(list()))
  expect_error(kc$create(c()))
  expect_error(kc$create(NA))
  expect_error(kc$create(list("aaa", "bbb")))
  expect_error(kc$create(list(aa="aaa", bb="bbb")))
})

test_that("kibior::create, try to create already register index name", {
  if(kc$has(single_index_name)){
    kc$delete(single_index_name)
  }
  res <- kc$create(single_index_name)[[1]]
  expect_true(res$acknowledged)
  expect_true(res$shards_acknowledged)
  expect_equal(res$index, single_index_name)
  # twice, without deleting
  expect_error(kc$create(single_index_name))
})

# end create




# start recreate ----

test_that("kibior::recreate, nominal case", {
  if(kc$has(single_index_name)){
    kc$delete(single_index_name)
  }
  res <- kc$create(single_index_name)[[1]]
  expect_true(res$acknowledged)
  expect_true(res$shards_acknowledged)
  expect_equal(res$index, single_index_name)
  # recreate
  res <- kc$create(single_index_name, force = TRUE)[[1]]
  expect_true(res$acknowledged)
  expect_true(res$shards_acknowledged)
  expect_equal(res$index, single_index_name)
})

test_that("kibior::recreate, null args", {
  expect_error(kc$create(index_name = NULL, force = TRUE))
  expect_error(kc$create(NULL, force = TRUE))
})

# end recreate




# start has ----

test_that("kibior::has, nominal case", {
  res <- kc$create(single_index_name, force = TRUE)[[1]]
  expect_equal(res$index, single_index_name)
  current_name <- res$index
  #
  expect_true(kc$has(current_name))
  res <- kc$delete(current_name)
  expect_false(kc$has(current_name))
})

test_that("kibior::has, null arg", {
  expect_error(kc$has(NULL))
})

test_that("kibior::has, wrong types args", {
  expect_error(kc$has(list()))
  expect_error(kc$has(c())) # == NULL
  expect_error(kc$has(NA))
  expect_error(kc$has(list("aaa", "bbb")))
  expect_error(kc$has(list(aa="aaa", bb="bbb")))
})

test_that("kibior::has, multiple test vector", {
  res <- kc$create(multiple_indice_names, force = TRUE)
  expect_equal(names(res), multiple_indice_names)
  #
  expect_true(kc$has(multiple_indice_names))
  kc$delete(multiple_indice_names[[1]]) # take one and delete it
  expect_false(kc$has(multiple_indice_names))
})

# end has



# start delete ----

test_that("kibior::delete, null arg", {
  expect_error(kc$delete(NULL))
})

test_that("kibior::delete, wrong types args", {
  expect_error(kc$delete(list()))
  expect_error(kc$delete(c())) # == NULL
  expect_error(kc$delete(NA))
  expect_error(kc$delete(list("aaa", "bbb")))
  expect_error(kc$delete(list(aa="aaa", bb="bbb")))
})

test_that("kibior::delete, nominal case single index", {
  if(kc$has(single_index_name)){
    kc$delete(single_index_name)
  }
  res <- kc$create(single_index_name)[[1]]
  expect_true(res$acknowledged)
  expect_true(res$shards_acknowledged)
  expect_equal(res$index, single_index_name)
  expect_true(kc$has(res$index))
  # del
  res2 <- kc$delete(res$index)[[1]]
  expect_true(res2$acknowledged)
  expect_false(kc$has(res$index))
})

test_that("kibior::delete, nominal case multiple indices", {
  res <- kc$create(multiple_indice_names, force = TRUE)
  expect_true(kc$has(names(res)))
  # del all indices
  res2 <- kc$delete(names(res))[[1]]
  expect_true(res2$acknowledged)
  expect_false(kc$has(names(res)))
  for(i in multiple_indice_names){
    expect_error(kc$delete(i))
  }
  expect_error(kc$delete(multiple_indice_names))
})

# end delete




# start list ----

test_that("kibior::list, null arg, no index", {
  remove_all_indices()
  expect_null(kc$list())
})

test_that("kibior::list, nominal case, one index", {
  remove_all_indices()
  res <- kc$create(single_index_name)
  expect_true(kc$has(names(res)))
  expect_equal(names(res), single_index_name)
  res2 <- kc$list()
  expect_equal(length(res2), 1)
  expect_false(is.null(res2[1]))  
  expect_equal(res2, names(res))
})

test_that("kibior::list, nominal case, some indices", {
  remove_all_indices()
  res <- kc$create(multiple_indice_names)
  expect_true(kc$has(names(res)))
  res2 <- kc$list()
  expect_equal(length(res2), length(multiple_indice_names))
  expect_setequal(res2, names(res))
})

# end list


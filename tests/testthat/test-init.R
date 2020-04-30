
context("Initialize")

testthat::setup({
    # remove indices if they exist
    remove_all_indices()
    kc_new <- NULL
    # assign var to global env
    assign("kc_new", kc_new, envir = .GlobalEnv)
})

testthat::teardown({
    # remove indices if they exist
    remove_all_indices()
    # remove var from global env 
    rm(kc_new, envir = .GlobalEnv)
})


# start initialize namespace ----

test_that("kibior::initialize, nominal case", {
  kc_new <- Kibior$new(es_endpoint, es_port, es_username, es_password)
  # 
  expect_equal(kc_new$host, es_endpoint)
  expect_equal(kc_new$port, es_port)
  expect_equal(kc_new$user, es_username)
  expect_equal(kc_new$pwd, es_password)
  expect_equal(kc_new$verbose, FALSE)
  expect_equal(kc_new$quiet_progress, FALSE)
  expect_equal(kc_new$quiet_results, FALSE)
  # 
  expect_true(kc_new$cluster_status %in% c("green", "yellow"))
})

test_that("kibior::initialize, no host", {
  expect_error(Kibior$new())
  expect_error(Kibior$new(""))
})

test_that("kibior::initialize, wrong host", {
  expect_error(Kibior$new("nope"))
})

test_that("kibior::initialize, host param, w/wo credentials", {
    # only works when username is not null
    if(!purrr::is_null(es_username)){
        expect_error(Kibior$new(host = es_endpoint, port = es_port, verbose = TRUE))
    } else {
        # no user/pwd given, cannot assert ES is secure or not.
        # if not secure, see previous tests
        # if it is, this test + the next one are taken into account
        expect_true(TRUE)
    }
})

test_that("kibior::initialize, host param and credentials", {
    # only works when username is not null
    if(!purrr::is_null(es_username)){
        # try wrong user/pwd couple
        expect_error(Kibior$new(host = es_endpoint, port = es_port, user = "nope", pwd = "nope"))
    } else {
        # no user/pwd given, cannot assert ES is secure or not.
        # if not secure, see previous tests
        # if it is, this test + the last one are taken into account
        expect_true(TRUE)
    }
})

test_that("kibior::initialize, wrong host message error", {
  expect_error(Kibior$new(verbose = TRUE), info = "Failed to connect to localhost port 9200: Connection refused")
})

test_that("kibior::initialize, wrong host arg type", {
  expect_error(Kibior$new(host = list()))
})

test_that("kibior::initialize, wrong port arg type", {
  expect_error(Kibior$new(port = list()))
})

test_that("kibior::initialize, wrong user arg type", {
  expect_error(Kibior$new(user = list()))
})

test_that("kibior::initialize, wrong pwd arg type", {
  expect_error(Kibior$new(pwd = list()))
})

test_that("kibior::initialize, wrong verbose arg type", {
  expect_error(Kibior$new(verbose = list()))
})


# end initialize namespace 



# start print namespace ----

test_that("kibior::print, nominal case", {
  kc_new <- Kibior$new(es_endpoint)
  expect_output(print(kc_new), "KibioR client: 
  - host: elasticsearch 
  - port: 9200 
  - verbose: no 
  - print result: yes 
  - print progressbar: yes ")
})

test_that("kibior::print, nominal case, args change", {
  kc_new <- Kibior$new(host = es_endpoint, user = es_username, pwd = es_password, verbose = TRUE)
  msg <- paste0("KibioR client: 
  - host: ", es_endpoint, " 
  - port: ", es_port, " ")
  if(!purrr::is_null(es_username)){
    msg <- paste0(msg, "
  - username: ", es_username, " 
  - password: ", es_password, " ")
  }
  msg <- paste0(msg, "
  - verbose: yes 
  - print result: yes 
  - print progressbar: yes ")
  expect_output(print(kc_new), msg)
})

# end print namespace 

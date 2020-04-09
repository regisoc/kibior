
context("Initialize")

# start initialize namespace ----

test_that("kibior::initialize, nominal case", {
  kc <- Kibior$new(host = "elasticsearch")
  expect_equal(kc$host, "elasticsearch")
  expect_equal(kc$port, 9200)
  expect_equal(kc$user, NULL)
  expect_equal(kc$pwd, NULL)
  expect_equal(kc$verbose, FALSE)
  # 
  expect_equal(kc$head_search_size, 5)
  expect_equal(kc$valid_joins, c("inner", "full", "left", "right", "semi", "anti"))
  expect_equal(kc$cluster_name, "docker-cluster")
  expect_true(kc$cluster_status %in% c("green", "yellow"))
})

test_that("kibior::initialize, host param, no credentials", {
  expect_error(Kibior$new(host = "elasticsearch", port = 9203, verbose = TRUE))
})

test_that("kibior::initialize, host param and credentials", {
  expect_error(Kibior$new(host = "elasticsearch", port = 9203, user = "nope", pwd = "qwerty", verbose = TRUE))
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
  kc <- Kibior$new("elasticsearch")
  expect_output(print(kc), "KibioR client: 
  - host: elasticsearch 
  - port: 9200 
  - verbose: FALSE ")
})

test_that("kibior::print, nominal case, args change", {
  kc <- Kibior$new(host = "elasticsearch", user = "elastic", pwd = "changeme", verbose = TRUE)
  expect_output(print(kc), "KibioR client: 
  - host: elasticsearch 
  - port: 9200 
  - username: elastic 
  - password: changeme 
  - verbose: TRUE ")
})

# end print namespace 

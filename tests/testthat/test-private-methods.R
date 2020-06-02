

context("Private methods - utils")



# helper class to test private members
# https://stackoverflow.com/a/44642256
PrivateMethodsTester <- R6Class(
  inherit = Kibior,
  public = list(
    test_define_mappings = function(x){
      private$define_mappings(x)
    }
  )
)
instantiate <- function(){
  do.call(PrivateMethodsTester$new, es_args)
}


# start define_mappings ----

test_that("kibior::define_mappings, wrong args", {
  remove_all_indices()
  private_define_mappings <- instantiate()$test_define_mappings
  #
  expect_error(private_define_mappings())
  expect_error(private_define_mappings(c()))
  expect_error(private_define_mappings("aaa"))
  expect_error(private_define_mappings(c("aaa", "bbb")))
  expect_error(private_define_mappings(1))
  expect_error(private_define_mappings(c(1, 2)))
  expect_error(private_define_mappings(list()))
})

test_that("kibior::define_mappings, nominal case", {
  remove_all_indices()
  res <- kc$push(starwars, single_index_name)
  expect_equal(res, single_index_name)
  private_define_mappings <- instantiate()$test_define_mappings
  #
  m <- private_define_mappings(starwars)$properties
  p <- kc$mappings(single_index_name)[[single_index_name]][[single_index_name]]$properties
  expect_setequal(c(names(m), "kid"), names(p))
  for(pro in names(m)){
    expect_equal(p[[pro]]$type, m[[pro]]$type)
  }
})

# end define_mappings


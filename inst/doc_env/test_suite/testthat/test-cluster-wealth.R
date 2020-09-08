

context("Cluster wealth")

# testthat::setup({
#     # remove indices if they exist
#     remove_all_indices()
# })

# testthat::teardown({
#     # remove indices if they exist
#     remove_all_indices()
# })


# start stats ----

test_that("kibior::stats, nominal use, no arg", {
    s <- kc$stats()
    expect_equal(s$status, kc$cluster_status)
    expect_equal(s$cluster_name, kc$cluster_name)
  
})

test_that("kibior::stats, version check", {
    remove_all_indices()
    expect_true(paste0(kc$version, collapse = ".") %in% kc$stats()$nodes$versions)
})

test_that("kibior::stats, nominal use, constant", {
    remove_all_indices()
    expect_equal(kc$stats()$indices$count, 0)
    res <- kc$create(single_index_name)
    res %>% unlist(use.names = FALSE) %>% expect_true()
    expect_equal(kc$stats()$indices$count, length(single_index_name))
    remove_all_indices()
    expect_equal(kc$stats()$indices$count, 0)
    res <- kc$create(multiple_indice_names)
    res %>% unlist(use.names = FALSE) %>% all() %>% expect_true()
    expect_equal(kc$stats()$indices$count, length(multiple_indice_names))
})

test_that("kibior::stats, list vs", {
    remove_all_indices()
    expect_null(kc$list())
    expect_true(kc$stats()$indices$count == 0)

    res <- kc$create(single_index_name)
    res %>% unlist(use.names = FALSE) %>% expect_true()
    expect_equal(length(kc$list()), kc$stats()$indices$count)

})

# end stats



# start ping ----

test_that("kibior::ping, nominal use", {
    p <- kc$ping()
    expect_equal(p$version$number, paste0(kc$version, collapse = "."))
    expect_equal(p$cluster_name, kc$cluster_name)
    expect_equal(p$tagline, "You Know, for Search")
})



# end ping



# TEST

"test all" does not work with travis
```r
test_check("kibior")
```

Test during more than 10min are automatically killed and check fails
one solution is to split up with filter
https://github.com/travis-ci/travis-ci/issues/3849

```r
test_check("kibior", filter = "crud")
test_check("kibior", filter = "cluster-wealth|init|join|data-manipulation")
test_check("kibior", filter = "pull-1")
test_check("kibior", filter = "pull-2")
test_check("kibior", filter = "pull-3")
test_check("kibior", filter = "push")
test_check("kibior", filter = "move-data|private-methods")
test_check("kibior", filter = "search")
```
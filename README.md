

![](https://github.com/regisoc/kibior/blob/master/inst/logo/kibior.png)

# kibior: easy scientific data handling, searching and sharing with Elasticsearch

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Build Status](https://travis-ci.com/regisoc/kibior.svg?branch=master)](https://travis-ci.com/regisoc/kibior)

## What

**kibior** is a R package dedicated to ease the pain of data handling, searching and sharing.  Its main features allow **pushing, pulling, sharing and searching** tabular data.


## Where

**kibior** is using Elasticsearch as database and search engine. 


## Who

**kibior** mainly targets bioinformaticians, data scientists and anyone having a dataset to search and/or share.


## How

### Install

```r
# Get from Github
devtools::install_github("regisoc/kibior")
```

### Run

```r
library(kibior)

# Get a specific instance
kc <- Kibior$new("server_or_address", port)

# Or try something bigger...
kibio <- Kibior$get_kibio_instance()
kibio$list()

```

## Learn 

Here is an extract of some of the features proposed by `KibioR`. 
See `Introduction` vignette for more advanced usage.

```r
# == DATASET MANIPULATION ==

# Push data (R memory -> Elasticsearch)
dplyr::starwars %>% kc$push("sw")
dplyr::storms %>% kc$push("st")

# Pull data with columns selection (Elasticsearch -> R memory)
kc$pull("sw", query = "homeworld:tatooine", 
              columns = c("name", "homeworld", "height", "mass", "species"))

# Copy dataset (Elasticsearch internal operation)
kc$copy("sw", "sw_copy")

# Delete datasets
kc$delete("sw_copy")



# == METADATA ==

# Search for index names starting with "s"
kc$match("s*")

# List available datasets
kc$list()

# Get columns of all datasets starting with "s"
kc$columns("s*")



# == SEARCH ==

# Search in a single dataset (sw = starwars)

## ...everywhere
kc$search("sw", query = "naboo")

## ...in a specific column
kc$search("sw", query = "homeworld:naboo")

## ...multiple values
kc$search("sw", query = "homeworld:(naboo || tatooine)")

## ...multiple columns
kc$search("sw", query = "homeworld:naboo && height:>180")

## ...only some columns returned (with column name pattern)
kc$search("sw", columns = c("name", "height", "homeworld", "*_color"))


# Search in multiple datasets (s* = sw,st = starwars,storms)

## ...everywhere
kc$search("s*", query = "naboo")

## ...in a specific column
kc$search("s*", query = "homeworld:naboo")

## ...multiple values
kc$search("s*", query = "homeworld:(naboo || tatooine)")

## ...multiple columns
kc$search("s*", query = "homeworld:naboo && height:>180")



# == DATA SUMMARY & STATS ==

# Get unique values of a column
kc$keys("sw", "homeworld")

# Count number of lines in dataset
kc$count("st")

# Count number of lines with query (name of the storm is Anita)
kc$count("st", query = "name:anita")

# Generic stats on columns
kc$stats("sw", c("height", "mass"))

# Specific descriptive stats with query
kc$avg("sw", c("height", "mass"), query = "homeworld:naboo")



# == JOINS ==

# Inner join between a Elasticsearch-based dataset with query and a in-memory R dataset 
kc$inner_join("sw", dplyr::starwars, 
              left_query = "hair_color:black",
              left_columns = c("name", "mass", "height"),
              by = "name")
```


## Cite 

Coming soon.
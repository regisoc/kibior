

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

```r
library(kibior)

# Get a specific instance
kc <- Kibior$new("server_or_address", port)

# Push
dplyr::starwars %>% kc$push("sw")

# Search
kc$search("sw", query = "homeworld:naboo")

# Pull
kc$pull("sw", query = "homeworld:tatooine", 
              columns = c("name", "homeworld", "height", "mass", "species"))

# Or try something bigger...
kibio <- Kibior$get_kibio_instance()
kibio$list()
```

## Learn 

See the "Introduction" vignette for basic and advanced usage.

## Cite 

TODO
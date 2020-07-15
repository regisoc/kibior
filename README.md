

![](https://github.com/regisoc/kibior/blob/master/inst/logo/kibior.png)

# kibior: easy scientific data handling, searching and sharing with Elasticsearch

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

## What

**kibior** is a R package dedicated to ease the pain of data manipulation. 
Its main features allow **pushing, pulling, sharing and searching** tabular data.


## Where

**kibior** is using Elasticsearch as database and search engine. 


## Who

**kibior** mainly targets data scientists


## How

```r
library(kibior)

# Get an instance
kc <- Kibior$new("server_or_address")

# Push
dplyr::starwars %>% kc$push("sw")

# Search
kc$search("sw", query = "my_query")

# Pull
kc$pull("sw")
```

## 
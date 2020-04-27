# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stringr str_split
#' @importFrom purrr is_null is_list is_character is_logical
#' @importFrom jsonlite fromJSON
#' @importFrom rio export import
#' @importFrom dplyr select filter as_tibble
#' @importFrom magrittr %>%
#' @importFrom data.table rbindlist
#' @importFrom elastic mapping_create index_forcemerge index_optimize index_flush index_clear_cache index_open index_close connect index_recreate index_create index_delete cluster_stats index_get docs_bulk_update docs_bulk_index reindex Search docs_mget scroll scroll_clear
#'
#' @export
#'
#' @keywords data integration dataset
#'
#' @name kibior
#' 
#' @title KibioR, an Kibio and Elasticsearch data manipulation package.
#' 
#' @description KibioR is a lightweight package for data manipulation 
#'  with Elasticsearch. Its main features allow easy data import, export,
#'  download, upload, searching and sharing to any Elasticsearch-based open 
#'  architecture, scaling to billions of data and TB capability.  
#'
#' @details A client to send, retrieve, search, join data in Elasticsearch.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @author Régis Ongaro-Carcy,
#'  \email{regis.ongaro-carcy2@crchudequebec.ulaval.ca}
#'
#' @references Kibio.science: \url{http://kibio.science}, \cr
#'  Elasticsearch documentation: 
#'  \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/index.html}
#'
#' @seealso \code{\link{kibior}}
#'
#' @section Constructor: 
#'  Kibior$new(host = "localhost", port = 9200, user = NULL, pwd = NULL, verbose = FALSE)
#'
#' @section Constructor Arguments:
#'  \tabular{llll}{
#'  \strong{Argument} \tab \strong{Type} \tab \strong{Details} \tab \strong{Default} \cr
#'  \code{host} \tab character \tab address or name of Elasticsearch server \tab "localhost" \cr
#'  \code{port} \tab numeric \tab port of Elasticsearch server \tab 9200 \cr
#'  \code{user} \tab character \tab if required by the server, the username for authentication \tab NULL \cr
#'  \code{pwd} \tab character \tab if required by the server, the password for authentication \tab NULL \cr
#'  \code{verbose} \tab logical \tab verbose mode \tab FALSE \cr
#'  }
#'

Kibior <- R6Class(
    c("Kibior", "KibiorOperators"),
    lock_objects = FALSE,
    lock_class = FALSE,

    # ============================================================================
    # PRIVATE
    # ============================================================================

    private = list(

        # --------------------------------------------------------------------------
        # attributes
        # --------------------------------------------------------------------------

        # Private .host Elasticsearch connection host
        .host = NULL,

        # Private .port Elasticsearch connection port
        .port = NULL,

        # Private .user Elasticsearch auth user
        .user = NULL,

        # Private .pwd Elasticsearch auth password
        .pwd = NULL,

        # Private .connection Elstic connection, computed automatically when host or port are 
        #   changed
        .connection = NULL,

        # Private .es_version Elasticsearch version, computed automatically when the connection is 
        #   set
        .es_version = NULL,

        # Private .head_search_size Size of search when head mode is activated, `$search(head = 
        #   TRUE)`
        .head_search_size = 5,

        # Private .es_wait Time in second to wait when an update command is sent to ES
        .es_wait = 2,

        # Private .es_primaries Default allocated number of primary shards when a new ES index is 
        #'  created
        .es_primary_shards = 1,

        # Private .es_replica_shards Default allocated number of replica shards when a new ES index 
        #   is created
        .es_replica_shards = 1,

        # Private .default_id_col Default name given to unique field when pushing data.
        .default_id_col = "kid",


        # --------------------------------------------------------------------------
        # constants
        # --------------------------------------------------------------------------

        # Private .VALID_JOINS List of valid joins, used to test
        .VALID_JOINS = c("inner", "full", "left", "right", "semi", "anti"),

        # Private .VALID_COUNT_TYPES List of count valid types, used to test
        .VALID_COUNT_TYPES = c("observations", "variables"),

        # Private .VALID_FEATURES List of Elasticsearch valid features in metadata, used to test
        .VALID_FEATURES = c("settings", "mappings", "aliases"),

        # Private .VALID_PUSH_MODES List of valid push data modes, used to test
        .VALID_PUSH_MODES = c("check", "recreate", "update"),

        # Private .VALID_IMPORT_MODES List of valid import mode
        .VALID_IMPORT_MODES = c("local", "remote", "both"),

        # Private .RESERVED_QUERY_STRING_CHAR reserved characters for query string syntax
        .RESERVED_QUERY_STRING_CHAR = stringr::str_split("+ - = && || > < ! ( ) { } [ ] ^ \" ~ * ? : \\ /", " ")[[1]],

        # Private .AUTO_IMPORT_EXTENSION_MAPPING List of file extension to try automatic import
        .AUTO_IMPORT_EXTENSION_MAPPING = list(
            # tabular
            "csv"   = "tabular",
            "tsv"   = "tabular",
            "txt"   = "tabular",
            "tab"   = "tabular",
            # features
            "wig"   = "features",
            "bigwig"= "features",
            "gtf"   = "features",
            "gff"   = "features",
            "gff3"  = "features",
            "bed"   = "features",
            # "bb" = stop("Not supported. See https://support.bioconductor.org/p/116736/"),
            # alignment
            "bam"   = "alignments",
            # sequence
            "fasta" = "sequences",
            "fa"    = "sequences",
            "fna"   = "sequences",
            "ffn"   = "sequences",
            "faa"   = "sequences",
            "frn"   = "sequences",
            # reads
            "fastq" = "reads", 
            "fq"    = "reads"
            # "sam" = alignments,
            # # TODO: test
            # # variant
            # "vcf" = "variants",
            # "gvf" = "variants",
            # "vep" = "variants",
            # # mass_spectrometry
            # # MSnbase
            # # https://bioconductor.org/packages/release/bioc/vignettes/MSnbase/inst/doc/v01-MSnbase-demo.html#2_data_structure_and_content
            # "xml" = "mass_spectrometry",
            # "mzxml" = "mass_spectrometry",
            # "mzdata" = "mass_spectrometry",
            # "mzml" = "mass_spectrometry",
            # # TODO: check that
            # "mgf" = "mass_spectrometry",
            # "asc" = "mass_spectrometry",
            # "pkl" = "mass_spectrometry",
            # "dta" = "mass_spectrometry",
            # "pks" = "mass_spectrometry"
        ),


        # --------------------------------------------------------------------------
        # methods - inherits
        # --------------------------------------------------------------------------

        finalize = function(){
            "Destructor: finalize this object, wipe state"

            private$.connection <- NULL
            private$.es_version <- NULL
            private$.host <- NULL
            private$.port <- NULL
            private$.user <- NULL
            private$.pwd <- NULL
        },


        # --------------------------------------------------------------------------
        # methods - abstract & utils
        # --------------------------------------------------------------------------

        join = function(join_type = NULL, left_index = NULL, left_fields = NULL, left_query = NULL, left_bulk_size = 1000, left_max_size = NULL, right_index = NULL, right_fields = NULL, right_query = NULL, right_bulk_size = 1000, right_max_size = NULL, join_fields = NULL, keep_metadata = FALSE) {
            "[Abstract method] Execute a join between two datasets using `dplyr` joins."
            "The datasets can be in-memory (variable name) or the name of an currently stored Elasticsearch index."
            "This should not be call directly. "
            "Use one of the (`$inner_join`, `$full_join`, `$left_join`, `$right_join`, `$anti_join` or `$semi_join`) public methods instead."
            ""
            "@param join_type the join type, defined by `private$.VALID_JOINS`. (default: NULL)"
            "@param left_index the left index name or dataset. (default: NULL)"
            "@param left_fields the left index fields to select. (default: NULL)"
            "@param left_query the left index query. (default: NULL)"
            "@param left_bulk_size the left index bulk size when downloading from Elasticsearch. (default: 1000)"
            "@param left_max_size the left index max size (hard threshold). (default: NULL)"
            "@param right_index the right index name or dataset. (default: NULL)"
            "@param right_fields the right index fields to select. (default: NULL)"
            "@param right_query the right index query. (default: NULL)"
            "@param right_bulk_size the right index bulk size when downloading from Elasticsearch. (default: 1000)"
            "@param right_max_size the right index max size (hard threshold). (default: NULL)"
            "@param join_fields the field names used to join the two datasets. (default: NULL)"
            "@param keep_metadata Keep Elasticsearch metadata? (default: FALSE)"
            "@return the result of the called join"

            # TODO: add a "from_instance" param to join between instances
            
            # join type
            if(!purrr::is_character(join_type)) stop(private$err_param_type_character("join_type"))
            if(!(join_type %in% private$.VALID_JOINS)) stop(private$err_not_in_vector("Join type", private$.VALID_JOINS))
            # side args check (left and right)
            check_side_args <- function(side){
                #
                name_arg <- function(stype) paste0(side, "_", stype)
                call_arg <- function(stype) { name_arg(stype) %>% parse(text = .) %>% eval() }
                # names
                n_index <- name_arg("index")
                n_query <- name_arg("query")
                n_fields <- name_arg("fields")
                n_bulk_size <- name_arg("bulk_size")
                n_max_size <- name_arg("max_size")
                # values
                v_index <- call_arg("index")
                v_query <- call_arg("query")
                v_fields <- call_arg("fields")
                v_bulk_size <- call_arg("bulk_size")
                v_max_size <- call_arg("max_size")
                # index
                if(purrr::is_character(v_index)) {
                    # data from ES index
                    if(private$is_search_pattern(v_index)) stop(private$err_search_pattern_forbidden(n_index))
                    if(!purrr::is_null(v_query)){
                        if(!purrr::is_character(v_query)) stop(private$err_param_type_character(n_query, can_be_null = TRUE))
                        if(length(v_query) > 1) stop(private$err_one_value(n_query))
                    } 
                    if(!purrr::is_null(v_fields) && !purrr::is_character(v_fields)) {
                        stop(private$err_param_type_character(n_fields, can_be_null = TRUE))
                    }
                } else {
                    # data from in-memory 
                    if(purrr::is_null(v_index)) stop(private$err_null_forbidden(n_index))
                    if(!purrr::is_null(v_query) && self$verbose) message("`", n_query, "` will be ignored.")
                }
                # bulk size
                if(!is.numeric(v_bulk_size)) stop(private$err_param_type_numeric(n_bulk_size))
                if(length(v_bulk_size) > 1) stop(private$err_one_value(n_bulk_size))
                if(v_bulk_size < 1) stop(private$err_param_positive(n_bulk_size, can_be_null = FALSE))
                # max size
                if(!purrr::is_null(v_max_size)) {
                    if(!is.numeric(v_max_size)) stop(private$err_param_type_numeric(n_max_size))
                    if(length(v_max_size) > 1) stop(private$err_one_value(n_max_size))
                    if(v_max_size < 1) stop(private$err_param_positive(n_max_size, can_be_null = FALSE))
                }
            }
            # check left side
            check_side_args("left")
            # check right side
            check_side_args("right")

            # join
            if(purrr::is_character(join_fields) && private$is_search_pattern(join_fields)) {
                stop(private$err_search_pattern_forbidden("join_fields"))
            }
            # metadata
            if(is.na(keep_metadata)) stop(private$err_logical_na("keep_metadata"))
            #
            get_data <- function(index, fields, query, bulk_size, max_size){
                "Function: get data from ES index or memory"
                index_data <- NULL
                if(purrr::is_character(index)){
                    # if char, it is the index name needed to be pulled out
                    message("from index '", index, "'")
                    index_data <- self$pull(index_name = index,
                                            fields = fields,
                                            query = query,
                                            bulk_size = bulk_size,
                                            max_size = max_size,
                                            keep_metadata = keep_metadata)[[1]]
                } else {
                    # if data.frame derivated, just load it as tibble
                    message("from memory")
                    index_data <- index %>%
                        dplyr::select( if(purrr::is_null(fields)) everything() else fields ) %>%
                        (function(x){ if(!purrr::is_null(max_size)) head(x, max_size) else x }) %>%
                        dplyr::as_tibble(validate = TRUE)
                }
                index_data
            }
            # get left
            message("Getting left ", appendLF = FALSE)
            left <- get_data(index = left_index, 
                             fields = left_fields, 
                             query = left_query, 
                             bulk_size = left_bulk_size, 
                             max_size = left_max_size)
            # get right
            message("Getting right ", appendLF = FALSE)
            right <- get_data(index = right_index, 
                              fields = right_fields, 
                              query = right_query, 
                              bulk_size = right_bulk_size, 
                              max_size = right_max_size)
            # abstract call to dplyr joins
            fname <- list(paste0(join_type, "_join"), "dplyr")
            join_function <- do.call(what = "getFromNamespace", args = fname)
            join_function(left, right, by = join_fields)
        },


        round = function(nb, nb_decimal = 1){
            "Round to a number to a given decimal number"
            ""
            "@param nb the number to round."
            "@param nb_decimal the number of decimal to get."
            "@return a rounded number"
            if(!is.numeric(nb)) stop(private$err_param_type_numeric("nb"))
            if(!is.numeric(nb_decimal)) stop(private$err_param_type_numeric("nb_decimal"))
            #
            round(nb, nb_decimal) %>%
                format(nsmall = nb_decimal) %>%
                trimws()
        },


        humanize_mstime = function(time){
            "Format a millisecond time to a easily readable string"
            ""
            "@param time the millisecond time"
            "@return a list composed of `$time` and `$unit`"

            # take a time number (in ms) and returns a more readable version with unit
            if(!is.numeric(time)) stop(private$err_param_type_numeric("time"))
            if(time < 0) stop(private$err_param_positive("time"))
            #
            # time in milliseconds
            res <- list(unit = "ms", time = time)
            # if under 1 sec, keep it in ms
            if(res$time > 1000){
                # update to sec
                res$time <- res$time / 1000
                res$unit <- "s"
                if(res$time > 60){
                    # update to min
                    res$time <- res$time / 60
                    res$unit <- "m"
                    if(res$time > 60){
                        # update to hour
                        res$time <- res$time / 60
                        res$unit <- "h"
                        if(res$time > 24){
                            # update to day
                            res$time <- res$time / 24
                            res$unit <- "d"
                        }
                    }
                }
            }
            res$time <- private$round(res$time, nb_decimal = 3)
            res
        },


        is_list_empty = function(my_list){
            "Shortcut to test if a variable is a list and is empty"
            ""
            "@examples"
            "'aaa' %>% is_list_empty()            # ERROR"
            "c('aaa', 'bbb') %>% is_list_empty()  # ERROR"
            "list('aaa') %>% is_list_empty()      # [1] FALSE"
            "list() %>% is_list_empty()           # [1] TRUE"
            ""
            "@param my_list an object to test"
            "return ERROR if my_list is not a list, TRUE if my_list is empty, else FALSE"

            if(!purrr::is_list(my_list)) stop(private$err_param_type_list("my_list"))
            (length(my_list) == 0)
        },


        vector_to_str = function(vect){
            "Transform a vector to a listed string"
            vect %>% 
                paste0(collapse = "', '") %>% 
                paste0("'", ., "'")
        },

        # --------------------------------------------------------------------------
        # methods - Elastic utils
        # --------------------------------------------------------------------------

        connect = function(){
            "Connect to Elasticsearch API with the given attributes by updating the `$connection`."
            "Called automatically when changing `$host` or `$port`."
            "Should not be called otherwise."
            "Also update the Elasticsearch version available through `$version` when connected."

            private$.connection <- elastic::connect(host = self$host, 
                                                    port = self$port, 
                                                    user = self$user, 
                                                    pwd = self$pwd)
            # test connection
            p <- self$ping()
            if(purrr::is_null(p)) stop("Connection to '", self$host, ":", self$port, "' cannot be established.")
            # if connection is good
            msg <- paste0("Connected to '", p$cluster_name, "' at '", self$host, ":", self$port, "'")
            if(self$verbose) msg <- paste0(msg, "[Elasticsearch version: ", p$version$number, "]")
            message(msg)
            # Catch ES version
            private$.es_version <- stringr::str_split(p$version$number, "\\.")[[1]] %>% as.list()
            names(private$.es_version) <- c("major", "minor", "patch")
        },


        get_metadata_type = function(metadata_type = NULL, index_name = NULL){
            if(!purrr::is_character(metadata_type)) stop(private$err_param_type_character("metadata_type"))
            if(length(metadata_type) > 1) stop(private$err_one_value("metadata_type"))
            if(!is.null(index_name) && !purrr::is_character(index_name)) stop(private$err_param_type_character("index_name", can_be_null = TRUE))
            #
            self$get_metadata(index_name) %>%
                lapply(function(x){ x[[metadata_type]] }) %>%
                (function(x){ if(private$is_list_empty(x)) NULL else x})
        },


        define_mappings = function(data){
            "Define the Elasticsearch mapping of a dataset (list subclass)."
            ""
            "@examples"
            "private$define_mappings(starwars) # private method, cannot be called from outside"
            ""
            "@param data a dataset"
            "@return the mapping of the dataset"

            if(!purrr::is_list(data)) stop("Need a dataset type: `tibble`, `data.frame` or `list`.")
            if(purrr::is_null(data) || length(data) == 0) stop(private$err_empty_data("data"))
            # types
            text_type <- list(
                type = "text",
                fields = list(
                    keyword = list(
                        type = "keyword",
                        ignore_above = 256
                    )
                )
            )
            numeric_type <- list(type = "float")
            # map
            map_types <- function(x){
                xclass <- class(x)
                if(length(xclass) > 1 && "factor" %in% xclass){
                    xclass <- "factor"
                }
                switch(xclass,
                    "factor"    = { text_type },
                    "character" = { text_type },
                    "numeric"   = { numeric_type },
                    "integer"   = { numeric_type },
                    "double"    = { numeric_type },
                    "list"      = { map_types(x[[1]]) },
                    stop(private$ERR_WTF, "Unknown type when creating Elasticsearch mapping, found: ", x, " [", xclass, "]")
                )
            }
            # return mapping body
            lapply(data, map_types) %>%
                list(properties = .)
        },


        create_mappings = function(index_name = NULL, data = NULL){
            "Add a mapping on an index based on given data."
            ""
            "@examples"
            "kc <- Kibior$new(verbose = TRUE)"
            "kc$create('aaa')"
            "kc$create_mapping('aaa', starwars)"
            ""
            "@param index_name a vector of index names (default: NULL)."
            "@param data a dataset (default: NULL)."
            "@return a list of indices, each containing their number of observations and variables."

            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(length(index_name) > 1) stop(private$err_one_value("index_name"))
            if(purrr::is_null(data) || length(data) == 0) stop(private$err_empty_data("data"))
            #
            if(self$verbose) message("Applying mapping to '", index_name, "'")
            mapping <- private$define_mappings(data)
            res <- NULL
            if(self$version$major >= 7){
                res <- elastic::mapping_create(conn = self$connection,
                                            index = index_name,
                                            body = mapping)
            } else {
                res <- tryCatch(
                    expr = {
                        # do not insert mapping type
                        elastic::mapping_create(conn = self$connection,
                                                    index = index_name,
                                                    body = mapping)
                    },
                    error = function(e){
                        # old ES version, try another time with mapping types
                        elastic::mapping_create(conn = self$connection,
                                                    index = index_name,
                                                    type = index_name,
                                                    include_type_name = TRUE,
                                                    body = mapping)
                    }
                )
            }
            Sys.sleep(self$elastic_wait)
            res
        },


        force_merge = function(index_name = NULL, max_num_segments = NULL, only_expunge_deletes = FALSE, flush = TRUE){
            "TODO desc"

            elastic::index_forcemerge(self$connection, 
                                      index = index_name, 
                                      max_num_segments = max_num_segments, 
                                      only_expunge_deletes = only_expunge_deletes, 
                                      flush = flush)
        },


        optimize = function(index_name = NULL, max_num_segments = NULL, only_expunge_deletes = FALSE, flush = TRUE, wait_for_merge = TRUE){
            "TODO desc"

            if(self$version$major < 5){
                elastic::index_optimize(self$connection, 
                                        index = index_name, 
                                        max_num_segments = max_num_segments, 
                                        only_expunge_deletes = FALSE, 
                                        flush = flush, 
                                        wait_for_merge = wait_for_merge)
            } else {
                if(self$verbose) message("Elasticsearch version >= 5, skipping optimize.")
            }
        },


        is_search_pattern = function(pattern){
            "Tell if a given string vector contains an Elasticsearch pattern string"
            ""
            "@param s the string to test"
            "@return TRUE if the string is a pattern, else FALSE"

            if(!purrr::is_character(pattern)) stop(private$err_param_type_character("pattern"))
            grepl("*", pattern, fixed = TRUE) %>% any()
        },


        # --------------------------------------------------------------------------
        # Format error messages
        # --------------------------------------------------------------------------

        err_null_forbidden = function(arg_name){
            "Argument cannot be null"
            paste0("`", arg_name, "` must not be NULL.")
        },

        err_active_is_read_only = function(param_name){
            "For Active bindings - argument is read-only"
            paste0("`", param_name, "` is read-only.")
        },

        err_param_type = function(param_name, expected_type, can_be_null = FALSE){
            "Wrong param type"
            s <- if(can_be_null) " or NULL" else ""
            paste0("`", param_name, "` must be ", expected_type, s, ".")
        },

        err_param_type_numeric = function(param_name, can_be_null = FALSE){
            "Wrong param type, expected numeric"
            private$err_param_type(param_name, "numeric", can_be_null = can_be_null)
        },

        err_param_type_logical = function(param_name, can_be_null = FALSE){
            "Wrong param type, expected logical"
            private$err_param_type(param_name, "logical", can_be_null = can_be_null)
        },

        err_param_type_character = function(param_name, can_be_null = FALSE){
            "Wrong param type, expected character"
            private$err_param_type(param_name, "character", can_be_null = can_be_null)
        },

        err_param_type_list = function(param_name, can_be_null = FALSE){
            "Wrong param type, expected list"
            private$err_param_type(param_name, "list", can_be_null = can_be_null)
        },

        err_not_kibior_instance = function(param_name, can_be_null = FALSE){
            "Wrong type"
            private$err_param_type(param_name, "a Kibior instance.", can_be_null = can_be_null)
        },

        err_param_positive = function(param_name, zero_valid = FALSE){
            "Wrong param type"
            s <- if(zero_valid) "" else " and non-NULL"
            paste0("`", param_name, "` must be positive", s, ".")
        },

        err_one_value = function(param_name){
            "Wrong number of value in the variable, expected only one"
            paste0("`", param_name, "` must have a length of 1.")
        },

        err_logical_na = function(param_name){
            "Wrong logical NA"
            paste0("`", param_name, "` must TRUE or FALSE, not NA.")
        },

        err_search_pattern_forbidden = function(param_name){
            "Not search pattern here"
            paste0("`", param_name, "` must not contain search pattern.")
        },

        err_index_unknown = function(index_name){
            "Index unknown"
            private$vector_to_str(index_name) %>%
                paste0("One or more indices in [", ., "] are unknown.")
        },

        err_index_already_exists = function(index_name){
            "Index already there"
            paste0("Index '", index_name, "' already exists.")
        },

        err_not_in_vector = function(s, vect){
            "Value not in vector"
            private$vector_to_str(vect) %>%
                paste0(s, " must be one of [", ., "].")
        },

        err_empty_data = function(param_name){
            "Dataset is empty"
            paste0("Data in `", param_name, "` is empty.") 
        },

        err_field_unknown = function(index_name, field_name){
            "Field is unknown or not present"
            paste0("Field `", field_name, "` is not present in index `", index_name, "`.") 
        },
       
        # Sad error is sad.
        ERR_WTF = "Well. This is sad."

    ),


    # ============================================================================
    # ACTIVE BINDING
    # ============================================================================

    active = list(

        #' @field host Access and change the Elasticsearch host
        host = function(value) {
            "Access and change the Elasticsearch host"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$host"
            "kc$host <- 'elasticsearch' # 'elasticsearch' needs to be online, known and accessible"
            "kc$host"
            ""
            "@param value new host"
            "@return A string representing the host used in connection"

            if(missing(value)) {
                private$.host
            } else {
                if(!purrr::is_character(value)) stop(private$err_param_type_character("$host"), call. = FALSE)
                # change host and reconnect
                private$.host <- value
                private$connect()
            }
        },

        #' @field port Access and change the Elasticsearch port
        port = function(value) {
            "Access and change the Elasticsearch port"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$port"
            "kc$port <- 15000 # the port 15000 needs to be online and accessible"
            "kc$port"
            ""
            "@param value new port"
            "@return A number representing the port used in connection"

            if(missing(value)) {
                private$.port
            } else {
                if(!is.numeric(value)) stop(private$err_param_type_numeric("$port"), call. = FALSE)
                # change port and reconnect
                private$.port <- as.integer(value)
                private$connect()
            }
        },

        #' @field endpoint Access the Elasticsearch main endpoint
        endpoint = function(value) {
            "Access the Elasticsearch endpoint address."
            "Not mutable, initialize another client"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$endpoint"
            "kc$endpoint <- 'foobar' # Error, read only"
            ""
            "@param value not used"
            "@return A string representing the Elasticsearch endpoint."

            if(missing(value)) {
                paste0("http://", self$host, ":", self$port)
            } else {
                stop(private$err_active_is_read_only("$endpoint"), call. = FALSE)
            }
        },

        #' @field user Access the Elasticsearch user.
        user = function(value) {
            "Access the Elasticsearch user."
            "Not mutable, initialize another client"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$user"
            "kc$user <- 'foobar' # Error, read only"
            ""
            "@param value new user, not used"
            "@return A string representing the user used in connection"

            if(missing(value)) {
                private$.user
            } else {
                stop(private$err_active_is_read_only("$user"), call. = FALSE)
            }
        },

        #' @field pwd Access the Elasticsearch password.
        pwd = function(value) {
            "Access the Elasticsearch password."
            "Not mutable, initialize another client"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$pwd"
            "kc$pwd <- 'foobar' # Error, read only"
            ""
            "@param value new pwd, not used"
            "@return A string representing the password used in connection"

            if(missing(value)) {
                private$.pwd
            } else {
                stop(private$err_active_is_read_only("$pwd"), call. = FALSE)
            }
        },

        #' @field connection Access the Elasticsearch connection object.
        connection = function(value) {
            "Access the Elasticsearch connection object. "
            "Not mutable directly, change hsot or port to modify"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$connection"
            "kc$connection <- 'whatever' # Error, read only"
            ""
            "@param value new connection, not used"
            "@return A `elastic::connect()` object"

            if(missing(value)) {
                private$.connection
            } else {
                stop(private$err_active_is_read_only("$connection"), call. = FALSE)
            }
        },

        #' @field head_search_size Access and change the head size default value.
        head_search_size = function(value){
            "Access and change the head size default value."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$head_search_size"
            "kc$head_search_size <- 10"
            "kc$head_search_size"
            ""
            "@param value new head size"
            "@return A number representing the number of records the `$search()` method will get as sample"

            if(missing(value)) {
                private$.head_search_size
            } else {
                if(!is.numeric(value)) stop(private$err_param_type_numeric("$head_search_size"), call. = FALSE)
                private$.head_search_size <- value
            }
        },

        #' @field cluster_name Access the cluster name if and only if already connected.
        cluster_name = function(value){
            "Access the cluster name if and only if already connected."
            "Shortcut to `$stats()$cluster_name`"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$cluster_name"
            "kc$cluster_name <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A string representing the cluster name"

            if(missing(value)) {
                self$stats()$cluster_name
            } else {
                stop(private$err_active_is_read_only("$cluster_name"), call. = FALSE)
            }
        },

        #' @field cluster_status Access the cluster status if and only if already connected.
        cluster_status = function(value){
            "Access the cluster status if and only if already connected."
            "Shortcut to `$stats()$status`"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$cluster_status"
            "kc$cluster_status <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A string representing the cluster status ('green', 'yellow', or 'red')"

            if(missing(value)) {
                self$stats()$status
            } else {
                stop(private$err_active_is_read_only("$cluster_status"), call. = FALSE)
            }
        },

        #' @field nb_documents Access the current cluster total number of documents if and only if 
        #'  already connected.
        nb_documents = function(value){
            "Access the current cluster total number of documents if and only if already connected."
            "Shortcut to `$stats()$indices$docs$count`"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$nb_documents"
            "kc$nb_documents <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A string representing the total number of recorded documents"

            if(missing(value)) {
                self$stats()$indices$docs$count
            } else {
                stop(private$err_active_is_read_only("$nb_documents"), call. = FALSE)
            }
        },

        #' @field version Access the Elasticsearch version if and only if already connected.
        version = function(value){
            "Access the Elasticsearch version if and only if already connected."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$version"
            "kc$version <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A list with a semantic versioning (see `$major`, `$minor`, and `$patch`) of Elasticsearch"

            if(missing(value)) {
                private$.es_version
            } else {
                stop(private$err_active_is_read_only("$version"), call. = FALSE)
            }
        },

        #' @field elastic_wait Access and change the Elasticsearch wait time for update commands if 
        #'  and only if already connected.
        elastic_wait = function(value){
            "Access and change the Elasticsearch wait time for update commands if and only if already connected."
            "It is sometimes needed when Elasticsearch server configuration is not optimized for data ingestion and need extra delay."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$elastic_wait"
            "kc$elastic_wait <- 'whatever' # Error, read only"
            ""
            "@param value the new time to wait in second"
            "@return A list with a semantic versioning (see `$major`, `$minor`, and `$patch`) of Elasticsearch"

            if(missing(value)) {
                private$.es_wait
            } else {
                if(!is.numeric(value)) stop("`$elastic_wait` must be numeric", call. = FALSE)
                if(value <= 0) stop(private$err_param_positive("$elastic_wait", zero_valid = FALSE), call. = FALSE)
                private$.es_wait <- value
            }
        },

        #' @field valid_joins Access the valid joins available in Kibior.
        valid_joins = function(value){
            "Access the valid joins available in Kibior."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$valid_joins"
            "kc$valid_joins <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A vector representing possible valid joins used internally by Kibior"

            if(missing(value)) {
                private$.VALID_JOINS
            } else {
                stop(private$err_active_is_read_only("$valid_joins"), call. = FALSE)
            }
        },

        #' @field valid_count_types Access the valid count types available (mainly observations = 
        #'  rows, variables = columns)
        valid_count_types = function(value){
            "Access the valid count types available (mainly observations = rows, variables = columns)"
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$valid_count_types"
            "kc$valid_count_types <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A vector representing possible valid count types used internally by Kibior"

            if(missing(value)) {
                private$.VALID_COUNT_TYPES
            } else {
                stop(private$err_active_is_read_only("$valid_count_types"), call. = FALSE)
            }
        },

        #' @field valid_elastic_metadata_types Access the valid Elasticsearch metadata types 
        #'  available.
        valid_elastic_metadata_types = function(value){
            "Access the valid Elasticsearch metadata types available."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$valid_elastic_metadata_types"
            "kc$valid_elastic_metadata_types <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A vector representing possible valid push modes"

            if(missing(value)) {
                private$.VALID_FEATURES
            } else {
                stop(private$err_active_is_read_only("$valid_elastic_metadata_types"), call. = FALSE)
            }
        },

        #' @field valid_push_modes Access the valid push modes available.
        valid_push_modes = function(value){
            "Access the valid push modes available."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$valid_push_modes"
            "kc$valid_push_modes <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A vector representing possible valid Elasticsearch metadata types"

            if(missing(value)) {
                private$.VALID_PUSH_MODES
            } else {
                stop(private$err_active_is_read_only("$valid_push_modes"), call. = FALSE)
            }
        },


        #' @field valid_import_modes Access the valid import modes available.
        valid_import_modes = function(value){
            "Access the valid import modes available."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$valid_import_modes"
            "kc$valid_import_modes <- 'whatever' # Error, read only"
            ""
            "@param value not used"
            "@return A vector representing possible valid import modes"

            if(missing(value)) {
                private$.VALID_IMPORT_MODES
            } else {
                stop(private$err_active_is_read_only("$valid_import_modes"), call. = FALSE)
            }
        },


        #' @field shard_number Access and modify the number of allocated primary shards when 
        #'  creating an Elasticsearch index.
        shard_number = function(value){
            "Access and modify the number of allocated primary shards when creating an Elasticsearch index."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$shard_number"
            "kc$shard_number <- 2"
            ""
            "@param value number of primary shards to create"
            "@return The number of primary shards defined for each index at their creation"

            if(missing(value)) {
                private$.es_primary_shards
            } else {
                if(!is.numeric(value)) stop(private$err_param_type_numeric("$shard_number"), call. = FALSE)
                if(value <= 0) stop(private$err_param_positive("$shard_number", zero_valid = FALSE), call. = FALSE)
                private$.es_primary_shards <- value
            }
        },

        #' @field shard_replicas_number Access and modify the number of allocated replicas in an 
        #'  Elasticsearch index.
        shard_replicas_number = function(value){
            "Access and modify the number of allocated replicas in an Elasticsearch index."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$shard_replicas_number"
            "kc$shard_replicas_number <- 2"
            ""
            "@param value number of replica shards to create"
            "@return The number of replicas defined for each index "

            if(missing(value)) {
                private$.es_replica_shards
            } else {
                if(!is.numeric(value)) stop(private$err_param_type_numeric("$shard_replicas_number"), call. = FALSE)
                if(value < 0) stop(private$err_param_positive("$shard_replicas_number", zero_valid = TRUE), call. = FALSE)
                private$.es_replica_shards <- value
            }
        },

        #' @field default_id_col Access and modify the default ID column/field created when pushing 
        #'  data to Elasticsearch.
        default_id_col = function(value){
            "Access and modify the default ID field created when pushing data to Elasticsearch."
            ""
            "@examples"
            "kc <- Kibior()"
            "kc$default_id_col"
            "kc$default_id_col <- 'kibior_unique_id'"
            ""
            "@param value new name of the unique IDs field"
            "@return the name of the current unique IDs field"

            if(missing(value)) {
                private$.default_id_col
            } else {
                if(!purrr::is_character(value)) stop(private$err_param_type_character("$default_id_col"), call. = FALSE)
                if(length(value) > 1) stop(private$err_one_value("$default_id_col"), call. = FALSE)
                private$.default_id_col <- value
            }
        }

    ),


    # ============================================================================
    # PUBLIC
    # ============================================================================

    public = list(

        # --------------------------------------------------------------------------
        # attributes
        # --------------------------------------------------------------------------

        #' @field verbose verbose mode, prints out more informations during execution
        verbose = FALSE,

        #' @field quiet_progress progressbar quiet mode, remove progress bar, useful for cleaner 
        #'  tests
        quiet_progress = FALSE,

        #' @field quiet_results results quiet mode, make results of methods invisible
        quiet_results = FALSE,


        # --------------------------------------------------------------------------
        # methods - Class inherits
        # --------------------------------------------------------------------------


        #' @details
        #' Initialize a new object, automatically called when calling `Kibior$new()`
        #'
        #' @param host The target host to connect to Elasticsearch REST API (default: "localhost").
        #' @param port The target port (default: 9200).
        #' @param user If the server needs authentication, your username (default: NULL).
        #' @param pwd If the server needs authentication, your password (default: NULL).
        #' @param verbose The verbose mode (default: FALSE).
        #'
        #' @return a new instance/object of Kibior
        #'
        #' @examples
        #' # default initiatlization, connect to "localhost:9200"
        #' kc <- Kibior$new()
        #' # connect to "192.168.2.145:9200"
        #' kc <- Kibior$new("192.168.2.145")
        #' # connect to "es:15005", verbose mode activated
        #' kc <- Kibior$new(host = "elasticsearch", port = 15005, verbose = TRUE)
        #' # connect to "192.168.2.145:9450" with credentials "foo:bar"
        #' kc <- Kibior$new(host = "192.168.2.145", port = 9450, 
        #'  user = "foo", pwd = "bar")
        #' # connect to "elasticsearch:9200"
        #' kc <- Kibior$new("elasticsearch")
        #'
        #' # preparing all examples (do not mind this)
        #' kc_one <- Kibior$new("elasticsearch", verbose = TRUE)
        #' kc_two <- Kibior$new("elasticsearch2", verbose = TRUE)
        #' if(kc$has("aaa")) kc$delete("aaa")
        #' if(kc$has("bbb")) kc$delete("bbb")
        #' if(kc$has("ccc")) kc$delete("ccc")
        #' if(kc$has("ddd")) kc$delete("ddd")
        #' if(kc$has("sw")) kc$delete("sw")
        #' if(kc$has("sw_naboo")) kc$delete("sw_naboo")
        #' if(kc$has("sw_tatooine")) kc$delete("sw_tatooine")
        #' if(kc$has("sw_alderaan")) kc$delete("sw_alderaan")
        #' if(kc$has("sw_from_file")) kc$delete("sw_from_file")
        #' if(kc$has("storms")) kc$delete("storms")
        #' if(kc_one$has("sw")) kc_one$delete("sw")
        #' if(kc_one$has("sw_new")) kc_one$delete("sw_new")
        #' if(kc_two$has("sw")) kc_two$delete("sw")
        #' if(kc_two$has("sw_new")) kc_two$delete("sw_new")
        #'
        initialize = function(host = "localhost", port = 9200, user = NULL, pwd = NULL, verbose = getOption("verbose")){
            if(purrr::is_null(host)) host <- "localhost"
            if(!is.numeric(port)) stop(private$err_param_type_numeric("port"))
            if(!purrr::is_character(host)) stop(private$err_param_type_character("host"))
            if(!purrr::is_null(user)){
                if(!purrr::is_character(user)) stop(private$err_param_type_character("user"))
                if(length(user) != 1) stop(private$err_one_value("user"))
            }
            if(!purrr::is_null(pwd)){
                if(!purrr::is_character(pwd)) stop(private$err_param_type_character("pwd"))
                if(length(pwd) != 1) stop(private$err_one_value("pwd"))
            }
            if(!purrr::is_logical(verbose)) stop(private$err_param_type_logical("verbose"))
            if(!(verbose %in% c(FALSE, TRUE))) stop(private$err_logical_na("verbose"))
            # cast
            port <- as.integer(port)
            # build attr
            self$verbose <- verbose
            private$.host <- host
            private$.port <- port
            private$.user <- user
            private$.pwd <- pwd
            # try to connect
            private$connect()
        },

        #' @details
        #' Print simple informations of the current object.
        #'
        #' @examples
        #' kc <- Kibior$new("elasticsearch")
        #' print(kc)
        print = function(){
            f <- function(x) if(x) "yes" else "no"
            cat("KibioR client: \n")
            cat("  - host:", private$.host, "\n")
            cat("  - port:", private$.port, "\n")
            if(!purrr::is_null(private$.user)){
                cat("  - username:", private$.user, "\n")
                cat("  - password:", private$.pwd, "\n")
            }
            cat("  - verbose:", f(self$verbose), "\n")
            cat("  - print result:", f(!self$quiet_results), "\n")
            cat("  - print progressbar:", f(!self$quiet_progress), "\n")
        },

        # TODO test
        #'
        #' @details
        #' Tells if another instance of Kibior has the same `host:port` couple.
        #'
        #' @param other Another instance/object of Kibior (default: NULL).
        #'
        #' @return TRUE if hosts and ports are identical, else FALSE
        #'
        #' @examples
        #' kc_one <- Kibior$new("elasticsearch", verbose = TRUE)
        #' kc_two <- Kibior$new("elasticsearch2", verbose = TRUE)
        #' kc_one$eq(kc_two)
        #' kc_two$eq(kc_one)
        #' 
        eq = function(other = NULL){
            if(!Kibior$is_instance(other)) stop(private$err_not_kibior_instance("other"))
            r <- (self$host == other$host && self$port == other$port)
            if(self$quiet_results) invisible(r) else r
        },

        # TODO test
        #'
        #' @details
        #' Tells if another instance of Kibior has a different `host:port` couple.
        #'
        #' @param other Another instance/object of Kibior (default: NULL).
        #'
        #' @return TRUE if hosts and ports are differents, else FALSE
        #'
        #' @examples
        #' kc_one <- Kibior$new("elasticsearch", verbose = TRUE)
        #' kc_two <- Kibior$new("elasticsearch2", verbose = TRUE)
        #' kc_one$ne(kc_two)
        #' kc_two$ne(kc_one)
        #' 
        ne = function(other = NULL){
            r <- !self$eq(other = other)
            if(self$quiet_results) invisible(r) else r
        },


        # --------------------------------------------------------------------------
        # methods - CRUD index
        # --------------------------------------------------------------------------


        #' @details
        #' Create one or several indices in Elasticsearch.
        #'
        #' @family crud-index
        #'
        #' @param index_name a vector of index names to create (default: NULL).
        #' @param force Erase already existing identical index names? (default: FALSE).
        #'
        #' @return a list containing results of creation per index
        #'
        #' @examples
        #' kc$create("aaa")
        #' kc$create(c("bbb", "ccc"))
        #' 
        create = function(index_name = NULL, force = FALSE){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(private$is_search_pattern(index_name)) stop(private$err_search_pattern_forbidden("index_name"))
            if(!purrr::is_logical(force)) stop(private$err_param_type_logical("force"))
            if(is.na(force)) stop(private$err_logical_na("force"))
            #
            res <- list()
            f <- if(force) elastic::index_recreate else elastic::index_create
            for(i in index_name){
                if(self$verbose) message("Creating index '", i, "'")
                # build with settings but mapping is defined with data to better match fields type
                body <- list(
                    "settings" = list(
                        "index" = list(
                            "number_of_shards" = private$.es_primary_shards,
                            "number_of_replicas" = private$.es_replica_shards
                        )
                    )
                )
                res[[i]] <- f(self$connection, index = i, body = body, verbose = FALSE)
                # control
                if(res[[i]]$acknowledged){
                    private$force_merge()
                    private$optimize(i)
                }
            }
            Sys.sleep(self$elastic_wait)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' List indices in Elasticsearch.
        #'
        #' @family crud-index
        #'
        #' @examples
        #' kc$list()
        #' kc$list(get_specials = TRUE)
        #'
        #' @param get_special a boolean to get special indices (default: FALSE).
        #'
        #' @return a list of index names, NULL if no index found
        #'
        list = function(get_specials = FALSE){
            r <- names(self$get_mappings())
            if(!purrr::is_null(r) && !get_specials){
                # remove special indices
                r <- r[!startsWith(r, ".")]
            }
            if(self$quiet_results) invisible(r) else r
        },

        #' @details
        #' Does Elasticsearch has one or several indices?
        #'
        #' @family crud-index
        #'
        #' @examples
        #' kc$has("aaa")
        #' kc$has(c("bbb", "ccc"))
        #' kc$has(c("bbb", "ddd"))
        #'
        #' @param index_name a vector of index names to check (default: NULL).
        #'
        #' @return TRUE if all given index names are present in Elasticsearch, else FALSE
        #'
        has = function(index_name = NULL){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(private$is_search_pattern(index_name)) stop(private$err_search_pattern_forbidden("index_name"))
            # result
            res <- FALSE
            r <- self$list()
            if(!purrr::is_null(r)){
                res <- vapply(index_name,
                            FUN = function(x){ x %in% r },
                            FUN.VALUE = logical(1)) %>% all()
            }
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Delete one or several indices in Elasticsearch.
        #'
        #' @family crud-index
        #'
        #' @param index_name a vector of index names to delete (default: NULL).
        #'
        #' @return a list containing results of deletion per index
        #'
        #' @examples
        #' kc$delete("aaa")
        #' kc$delete(c("bbb", "ccc"))
        #' 
        delete = function(index_name = NULL){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(index_name == "_all" || index_name == "*") stop("Cannot delete all index at once.")
            if("_all" %in% index_name) stop("Cannot delete all indices at once.")
            res <- list()
            for(i in index_name){
                if(self$verbose) message("Deleting index '", i, "'")
                res[[i]] <- elastic::index_delete(self$connection, index = i, verbose = FALSE)
                if(res[[i]]$acknowledged){
                    private$force_merge()
                    private$optimize(i)
                }
            }
            Sys.sleep(self$elastic_wait)
            if(self$quiet_results) invisible(res) else res
        },


        # --------------------------------------------------------------------------
        # methods - cluster wealth
        # --------------------------------------------------------------------------


        #' @details
        #' Get stats about Elasticsearch cluster
        #'
        #' @family cluster-wealth
        #'
        #' @examples
        #' kc$stats()
        #'
        #' @return a list of statistics about the cluster
        #'
        stats = function(){
            suppressMessages({ 
                r <- elastic::cluster_stats(self$connection) 
            })
            if(self$quiet_results) invisible(r) else r
        },

        #' @details
        #' Ping cluster connection
        #'
        #' @family cluster-wealth
        #'
        #' @examples
        #' kc$ping()
        #'
        #' @return the ping result with some basic infos
        #'
        ping = function(){
            r <- self$connection$ping()
            if(self$quiet_results) invisible(r) else r
        },


        # --------------------------------------------------------------------------
        # methods - CRUD metadata
        # --------------------------------------------------------------------------

        #' @details
        #' Get metadata of indices
        #'
        #' @family crud-metadata
        #'
        #' @examples
        #' # push test data, without magrittr
        #' kc$push(dplyr::starwars, "sw")
        #' kc$push(dplyr::filter(dplyr::starwars, homeworld == "Naboo"), "sw_naboo")
        #' kc$push(dplyr::filter(dplyr::starwars, homeworld == "Tatooine"), "sw_tatooine")
        #' kc$push(dplyr::filter(dplyr::starwars, homeworld == "Alderaan"), "sw_alderaan")
        #' # get_metadata
        #' kc$get_metadata()
        #' kc$get_metadata("sw")
        #' kc$get_metadata(c("sw", "sw_naboo"))
        #'
        #' @param index_name a vector of index names to get metadata (default: NULL).
        #'
        #' @return the list of indices, each containing the 3 features (mappings,settings, aliases) 
        #'
        get_metadata = function(index_name = NULL){
            if(purrr::is_null(index_name)) index_name <- "_all"
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name", can_be_null = TRUE))
            res <- NULL
            m <- tryCatch(
                {
                    r <- suppressWarnings(elastic::index_get(self$connection, 
                                    index = index_name,
                                    include_type_name = FALSE, 
                                    verbose = self$verbose))
                }, 
                warning = function(w){},    # do nothing, just capture the warning
                finally = { r }             # return the result, warnings or not
            )
            if(length(m) > 0){
                res <- m
            }
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Get mappings of indices
        #'
        #' @family crud-metadata
        #'
        #' @examples
        #' kc$get_mappings()
        #' kc$get_mappings("sw")
        #' kc$get_mappings(c("sw", "sw_naboo"))
        #'
        #' @param index_name a vector of index names to get mappings (default: NULL).
        #'
        #' @return the list of indices, containing their mapping
        #'
        get_mappings = function(index_name = NULL){
            res <- private$get_metadata_type(metadata_type = "mappings", 
                                        index_name = index_name)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Get settings of indices
        #'
        #' @family crud-metadata
        #'
        #' @examples
        #' kc$get_settings()
        #' kc$get_settings("sw")
        #' kc$get_settings(c("sw", "sw_tatooine"))
        #'
        #' @param index_name a vector of index names to get settings (default: NULL).
        #'
        #' @return the list of indices, containing their settings
        #'
        get_settings = function(index_name = NULL){
            res <- private$get_metadata_type(metadata_type = "settings", 
                                            index_name = index_name)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Get aliases of indices
        #'
        #' @family crud-metadata
        #'
        #' @examples
        #' kc$get_aliases()
        #' kc$get_aliases("sw")
        #' kc$get_aliases(c("sw", "sw_alderaan"))
        #'
        #' @param index_name a vector of index names to get aliases (default: NULL).
        #'
        #' @return the list of indices, containing their aliases
        #'
        get_aliases = function(index_name = NULL){ 
            res <- private$get_metadata_type(metadata_type = "aliases", 
                                            index_name = index_name)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Count observations or variables in Elasticsearch data
        #'
        #' @family crud-metadata
        #'
        #' @examples
        #' # Number of observations (nb of records) in "sw"
        #' kc$count("sw")
        #' # Number of observations in indices "sw_naboo" and "sw_tatooine"
        #' kc$count(c("sw_naboo", "sw_tatooine"))
        #' # Number of variables (nb of columns) in index "sw_naboo"
        #' kc$count("sw_naboo", type = "variables")
        #'
        #' @param index_name a vector of index names to get aliases (default: NULL).
        #' @param type a string representing the type to count: "observations" or "variables" 
        #'  (default: "observations").
        #' @param query a string as a query string syntax (default: NULL).
        #'
        #' @return the list of indices, containing their number of observations or variables. 
        #'  Use `$dim()` for both
        #'
        count = function(index_name = NULL, type = "observations", query = NULL){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(!self$has(index_name)) stop(private$err_index_unknown(index_name))
            if(length(type) > 1) stop(private$err_one_value("type"))
            if(!(type %in% private$.VALID_COUNT_TYPES)) stop(private$err_not_in_vector("Count type", private$.VALID_COUNT_TYPES))
            if(!purrr::is_null(query)){
                if(!purrr::is_character(query)) stop(private$err_param_type_character("query"))
                if(length(query) > 1) stop(private$err_one_value("query"))
            }
            # 
            res <- list()
            for(i in index_name){
                res[[i]] <- switch(type,
                    "observations" = {
                        elastic::count(self$connection, index = i, q = query)
                    },
                    "variables" = {
                        self$fields(i)[[i]] %>% length()
                    },
                    stop(private$ERR_WTF, " Found type: ", type)
                )
            }
            if(length(res) == 0){
                res <- NULL
            }
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Shortcut to `$count()` to match the classical `dim()` function pattern `[line col]`
        #'
        #' @family crud-metadata
        #'
        #' @examples
        #' # Couple [<nb obs> <nb var>] in "sw"
        #' kc$dim("sw")
        #' # Couple [<nb obs> <nb var>] in indices "sw_naboo" and "sw_alderaan"
        #' kc$dim(c("sw_naboo", "sw_alderaan"))
        #'
        #' @param index_name a vector of index names to get aliases (default: NULL).
        #'
        #' @return the list of indices, containing their number of observations and variables.
        #'
        dim = function(index_name = NULL){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(!self$has(index_name)) stop(private$err_index_unknown("index_name"))
            #
            p <- as.list(index_name)
            names(p) <- index_name
            # 
            res <- p %>% 
                purrr::imap(function(x, y){ 
                    c(
                        self$count(x, type = "observations")[[x]], 
                        self$count(x, type = "variables")[[x]]
                    )
                })
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Get fields (columns) of indices.
        #'
        #' @family crud-metadata
        #'
        #' @examples
        #' kc$fields("sw")          # direct search
        #' kc$fields("sw_*")        # pattern search
        #'
        #' @param index_name a vector of index names, can be a pattern (default: NULL).
        #'
        #' @return a list of indices, each containing their fields.
        #'
        # TODO test
        fields = function(index_name = NULL){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(!private$is_search_pattern(index_name) && !self$has(index_name)) stop(private$err_index_unknown("index_name"))
            #
            res <- list()
            tmp <- self$get_mappings(index_name = index_name)
            get_property_names <- function(node, index_name){
                # try index name
                r <- node[[index_name]]$properties %>% names()
                # if main node is empty, maybe in "_doc"
                if(purrr::is_null(r)){ 
                    r <- node[["_doc"]]$properties %>% names()
                }
                r
            }
            for(i in names(tmp)){
                if(self$version$major > 6){
                    res[[i]] <- get_property_names(tmp, i)
                } else {
                    res[[i]] <- get_property_names(tmp[[i]], i)
                }
            }
            if(length(res) == 0){
                res <- NULL
            }
            if(self$quiet_results) invisible(res) else res
        },


        # --------------------------------------------------------------------------
        # methods - Data manipulation
        # --------------------------------------------------------------------------


        #' @details
        #' Get distinct elements of a specific field.
        #'
        #' @family data-manipulation
        #'
        #' @examples
        #' kc$distinct("sw", "name")
        #' kc$distinct("sw", "eye_color")
        #'
        #' @param index_name an index name (default: NULL).
        #' @param field_name a field name of this index (default: NULL).
        #'
        #' @return a vector of distinct values from this field
        #'
        # TODO test
        distinct = function(index_name = NULL, field_name = NULL){
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(length(index_name) > 1) stop(private$err_one_value("index_name"))
            if(private$is_search_pattern(index_name)) stop(private$err_search_pattern_forbidden("index_name"))
            if(!purrr::is_character(field_name)) stop(private$err_param_type_character("field_name"))
            if(length(field_name) > 1) stop(private$err_one_value("field_name"))
            if(!(field_name %in% self$fields(index_name)[[index_name]])) stop(private$err_field_unknown(index_name, field_name))

            # TODO
            # need to be changed to composite aggr.
            # https://www.elastic.co/guide/en/elasticsearch/reference/6.8/search-aggregations-bucket-composite-aggregation.html

            # request
            body = list(
                "size" = 0,
                "aggs" = list(
                    "kaggs" = list(
                        "terms" = list(
                            "field" = paste0(field_name, ".keyword")
                        )
                    )
                )
            )
            r <- elastic::Search(self$connection, 
                                index_name = index_name, 
                                size = 0, 
                                body = body)
                r$aggregations$kaggs$buckets %>% 
                lapply(function(x){ x$key }) %>% 
                unlist()
        },


        #' @details
        #' Transformation function for collapsing the BAM list of lists format 
        #'  into a single list as per the Rsamtools vignette
        #'
        #' @family data-manipulation
        #'
        #' @examples
        #' bam_param <- ScanBamParam(what = c("pos", "qwidth"))
        #' bam_data <- Rsamtools::scanBam(ff, param = bam_param)
        #' kc$bam_to_tibble(bam_data)
        #'
        #' @param bam_data data from a BAM file (default: NULL).
        #'
        #' @return a tibble of BAM data
        #'
        # TODO test
        bam_to_tibble = function(bam_data = NULL){
            .unlist <- function(x){
                # do.call(c, ...) coerces factor to integer, which is undesired
                x1 <- x[[1L]]
                if (is.factor(x1)){
                    structure(unlist(x), class = "factor", levels = levels(x1))
                } else {
                    do.call(c, x)
                }
            }
            # store names of BAM fields
            bam_field <- names(bam_data[[1]])
            # go through each BAM field and unlist and store as data frame
            bam_df <- lapply(bam_field, function(y){ 
                    .unlist(lapply(bam_data, "[[", y))
                }) %>% 
                do.call(data.frame, .)
            names(bam_df) <- bam_field
            dplyr::as_tibble(bam_df, .name_repair = "unique")
        },


        # --------------------------------------------------------------------------
        # methods - Move data
        # --------------------------------------------------------------------------


        #' @details
        #' Export data to a file.
        #' Some data formats are not installed by default.
        #' Use `rio::install_formats()` to be able to parse them.
        #'
        #' @family move-data
        #'
        #' @examples
        #' f = tempfile(fileext=".csv")
        #' # export and overwrite last file with the same data from Elasticsearch
        #' kc$export(data = "sw", filepath = f)
        #' # export from in-memory data to a file
        #' kc$export(data = dplyr::starwars, filepath = f, force = TRUE)
        #'
        #' @param data an index name or in-memory data to be extracted to a file (default: NULL).
        #' @param filepath the filepath to use as export, must contain the file extention (default: 
        #'  NULL).
        #' @param force overwrite the file? (default: FALSE).
        #'
        #' @return the filepath if correctly exported, else an error
        #'
        export = function(data = NULL, filepath = NULL, force = FALSE){
            if(purrr::is_null(data)) stop(private$err_empty_data("data"))
            if(purrr::is_null(filepath)) stop(private$err_null_forbidden("filepath"))
            if(!purrr::is_logical(force)) stop(private$err_param_type_logical("force"))
            if(is.na(force)) stop(private$err_logical_na("force"))
            if(purrr::is_character(data)){
                if(private$is_search_pattern(data)) stop(private$err_search_pattern_forbidden("data"))
                if(!self$has(data)) stop(private$err_index_unknown("data"))
            }
            if(!force && file.exists(filepath)) stop("File already exists. Use `force = TRUE` to overwrite")
            # 
            # data can be a in-memory dataset, or a name of an index in ES
            dataset <- if(purrr::is_character(data)) self$pull(index_name = data)[[data]] else data
            # write to file
            res <- rio::export(x = dataset, file = filepath)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Import method for tabular data.
        #' Works mainly with CSV, TSV, TAB, TXT and ZIPped formats.
        #'
        #' @family move-data
        #'
        #' @examples
        #' f <- tempfile(fileext = ".csv")
        #' rio::export(ggplot2::diamonds, f)
        #' # import to in-memory variable
        #' kc$import_tabular(filepath = f)
        #' # import raw data
        #' kc$import_tabular(filepath = f, as_tibble = FALSE)
        #'
        #' @param filepath the filepath to use as import, must contain the file extention (default: 
        #'  NULL).
        #' @param to_tibble returns the result as tibble? If FALSE, the raw default rio::import() 
        #'  format will be used (default: TRUE).
        #'
        #' @return data contained in the file as a tibble, or NULL.
        #'
        import_tabular = function(filepath = NULL, to_tibble = TRUE){
            # check pkg
            Kibior$.install_packages("rio")
            # do import
            rio::import(file = filepath) %>% 
                (function(table){
                    if(to_tibble){
                        table <- dplyr::as_tibble(table, .name_repair = "unique")
                    }
                    table
                })
        },

        #' @details
        #' Import method for features data.
        #' Works with BED, GTF, GFFx, and GZIPped formats.
        #'
        #' @family move-data
        #'
        #' @examples
        #' f_bed <- tempfile(fileext = ".bed")
        #' f_gff <- tempfile(fileext = ".gff3.gz")
        #' download.file("https://s3.amazonaws.com/bedtools-tutorials/web/cpg.bed", f_bed)
        #' download.file("ftp://ftp.ensembl.org/pub/release-99/gff3/homo_sapiens/Homo_sapiens.GRCh38.99.chromosome.Y.gff3.gz", f_gff)
        #' # import to in-memory variable
        #' kc$import_features(filepath = f_bed)
        #' kc$import_features(filepath = f_gff)
        #' # import raw data
        #' kc$import_features(filepath = f_bed, as_tibble = FALSE)
        #' kc$import_features(filepath = f_gff, as_tibble = FALSE)
        #'
        #' @param filepath the filepath to use as import, must contain the file extention (default: 
        #'  NULL).
        #' @param to_tibble returns the result as tibble? If FALSE, the raw default 
        #'  rtracklayer::import() format will be used (default: TRUE).
        #'
        #' @return data contained in the file as a tibble, or NULL.
        #'
        import_features = function(filepath = NULL, to_tibble = TRUE){
            Kibior$.install_packages("rtracklayer")
            rtracklayer::import(filepath) %>% 
                (function(features){
                    if(to_tibble){
                        table <- dplyr::as_tibble(table, .name_repair = "unique")
                    }
                    table
                })
        },

        #' @details
        #' Import method for alignments data.
        #' Works with BAM format.
        #'
        #' @family move-data
        #'
        #' @examples
        #' f <- tempfile(fileext = ".bam")
        #' download.file("http://hgdownload.cse.ucsc.edu/goldenPath/hg19/encodeDCC/wgEncodeUwRepliSeq/wgEncodeUwRepliSeqK562G1AlnRep1.bam", f)
        #' # import to in-memory variable
        #' kc$import_alignments(filepath = f)
        #' # import raw data
        #' kc$import_alignments(filepath = f, as_tibble = FALSE)
        #'
        #' @param filepath the filepath to use as import, should contain the file extention (default: 
        #'  NULL).
        #' @param to_tibble returns the result as tibble? If FALSE, the raw default 
        #'  Rsamtools::scanBam() format will be used (default: TRUE).
        #'
        #' @return data contained in the file as a tibble, or NULL.
        #'
        import_alignments = function(filepath = NULL, to_tibble = TRUE){
            # check pkg
            Kibior$.install_packages("Rsamtools")
            # do the job
            Rsamtools::scanBam(filepath) %>%
                (function(bam){
                    if(to_tibble) self$bam_to_tibble(bam) else bam
                })
        },

        #' @details
        #' Import method for sequences data.
        #' Works with FASTA and FASTQ formats.
        #'
        #' @family move-data
        #'
        #' @examples
        #' f <- tempfile(fileext = ".bam")
        #' download.file("http://hgdownload.cse.ucsc.edu/goldenPath/hg19/encodeDCC/wgEncodeUwRepliSeq/wgEncodeUwRepliSeqK562G1AlnRep1.bam", f)
        #' # import to in-memory variable
        #' kc$import_alignments(filepath = f)
        #' # import raw data
        #' kc$import_alignments(filepath = f, as_tibble = FALSE)
        #'
        #' @param filepath the filepath to use as import, should contain the file extention (default: 
        #'  NULL).
        #' @param to_tibble returns the result as tibble? If FALSE, the raw default 
        #'  Rsamtools::scanBam() format will be used (default: TRUE).
        #'
        #' @return data contained in the file as a tibble, or NULL.
        #'
        import_sequences = function(filepath = NULL, to_tibble = TRUE, type = "fasta", fasta_type = "auto"){
            if(!(type %in% c("fasta", "fastq"))) stop("Need type 'fasta' or 'fastq'.")
            # check pkg
            Kibior$.install_packages("Biostrings")
            # run fasta
            if(type == "fasta"){
                # methods
                string_set_to_df <- function(ss){
                    data.frame(
                        width = Biostrings::width(ss), 
                        seq = as.character(ss), 
                        names = names(ss)
                    )
                }
                dna_method <- Biostrings::readDNAStringSet
                rna_method <- Biostrings::readRNAStringSet
                aa_method <- Biostrings::readAAStringSet
                # file exists?
                if(!file.exists(filepath)){
                    f <- tempfile()
                    download.file(filepath, f)
                } else {
                    f <- filepath
                }
                # select mode
                r <- switch(fasta_type,
                    "dna"   = { dna_method(f) },
                    "rna"   = { rna_method(f) },
                    "aa"    = { aa_method(f) },
                    "auto"  = function(){
                        if(self$verbose) message("  Auto-mode [on]")
                        tryCatch({
                                # try dna
                                if(self$verbose) message("  - Try [DNA] parsing...")
                                dna_method(f)
                            }, 
                            warning = function(e){
                                tryCatch({
                                        # try rna
                                        if(self$verbose) message("  - Try [RNA] parsing...")
                                        rna_method(f)
                                    },
                                    warning = function(ee){
                                        tryCatch({
                                                # try prot
                                                if(self$verbose) message("  - Try [AA] parsing...")
                                                aa_method(f)
                                            },
                                            warning = function(eee){
                                                stop("Cannot apply 'Biostrings' to filepath.")
                                            }
                                        )
                                    }
                                )
                            }
                        )
                    },
                    stop("Unknown fasta option '", fasta_type, "'.")
                )
                # tibble
                if(to_tibble){
                    r <- r %>%
                        string_set_to_df() %>% 
                        dplyr::as_tibble(.name_repair = "unique")
                }
                r

            # } else {
            #     # run fastq
            #     Kibior$.install_packages("ShortRead")
            #     f <- tempfile()
            #     download.file(filepath, destfile = f)
            #     r <- ShortRead::readFastq(f)
            #     unlink(f)
            #     if(to_tibble){
            #         if(self$verbose) message("Argument `to_tibble` not taken into accoutn fo fastq data.")
            #     }
            #     r
            }
        },

        # TODO: import_variants
        # Kibior$import_variants <- function(filepath){
        #     # TODO return tibble if possible
        #     Kibior$.install_packages("VariantAnnotation")
        #     s <- VariantAnnotation::readVcf(filepath)
        #     s %>% 
        #         VariantAnnotation::info() %>% 
        #         as.data.frame(stringsAsFactors = FALSE) %>% 
        #         dplyr::as_tibble()
        # }

        # TODO: import_ms
        # Kibior$import_ms <- function(filepath){
        #     # TODO return tibble if possible
        #     Kibior$.install_packages("MSnbase")
        #     # read{MS/Mgf}Data
        #     # MSnbase::read{MS/Mgf}Data(filepath)
        # }


        guess_import = function(filepath = NULL, to_tibble = TRUE){
            guess_import_method <- function(f, rm_compression_extension = FALSE){
                "f can be an url or a fs path."
                "rm_compression_extension = TRUE will try to remove the first layer"
                "of extension (e.g. whatever.csv.gz -> whatever.csv) before trying"
                "to open the original file to identify the non-compressed file extension."

                # try to resolve relative path
                f <- tryCatch(
                    expr = { tools::file_path_as_absolute(f) },
                    error = function(e){ f }
                )
                f %>%
                    (function(x){
                        if(rm_compression_extension) tools::file_path_sans_ext(x) else x
                    }) %>%
                    tools::file_ext() %>%
                    (function(x){ 
                        if(!(x %in% names(private$.AUTO_IMPORT_EXTENSION_MAPPING))) stop("Unknown extension: '", x, "'.")
                        x
                    }) %>%
                    private$.AUTO_IMPORT_EXTENSION_MAPPING[[.]] %>%
                    (function(x){
                        if(self$verbose){
                            message("Try loading [", x, "] data...")
                            message("  file: ", f)
                        }
                        x
                    }) %>%
                    (function(type){
                        args <- list(
                            filepath = f, 
                            to_tibble = to_tibble
                        )
                        m <- switch(type,
                            "tabular"   = { self$import_tabular },
                            "features"  = { self$import_features },
                            "alignments"= { self$import_alignments },
                            "sequences" = { self$import_sequences },
                            "reads"     = { 
                                args[["type"]] = "fastq"
                                self$import_sequences
                            },
                            stop(private$ERR_WTF)
                        )
                        do.call(what = m, args = args)
                    })
            }
            # try calling the right method
            tryCatch(
                expr = {
                    guess_import_method(filepath)
                }, 
                error = function(e){
                    # try removing potential compression extension else raise an error
                    tryCatch(
                        expr = {
                            guess_import_method(filepath, rm_compression_extension = TRUE)   
                        },
                        error = function(ee){
                            if(self$verbose) message(ee$message)
                            stop("Cannot auto-import. Try with specific methods.")
                        }
                    )
                }
            )
        },

        #' @details
        #' Generic import method, will try to guess importation method.
        #' Will also try to uncompress data if they are.
        #' This method will call other import_* methods when trying.
        #' Some data formats are not installed by default.
        #' Use `rio::install_formats()` to be able to parse them.
        #'
        #' @family move-data
        #'
        #' @examples
        #' # import to in-memory variable (data)
        #' data <- kc$import(filepath = f)
        #' # import to Elasticsearch index ("sw_from_file") if not exists
        #' data <- kc$import(filepath = f, import_mode = "remote", push_index = "sw_from_file")
        #' # import to index by recreating it, then pull indexed data
        #' data <- kc$import(filepath = f, import_mode = "remote", push_index = "sw_from_file", 
        #'  push_mode = "recreate")
        #'
        #' @param filepath the filepath to use as import, must contain the file extention (default: 
        #'  NULL).
        #' @param import_mode can be "local" to get file data, "remote" to immediatly push them to 
        #'  Elasticsearch, "both" to push and pull. (default: "local").
        #' @param push_index the name of the index where to push data (default: NULL).
        #' @param push_mode the push mode (default: "check").
        #' @param id_col the column name of unique IDs (default: NULL).
        #' @param to_tibble returns the result as tibble? (default: TRUE).
        #'
        #' @return data contained in the file as a tibble, or NULL.
        #'
        import = function(filepath = NULL, import_type = "auto", import_mode = "local", push_index = NULL, push_mode = "check", id_col = NULL, to_tibble = TRUE){
            if(!purrr::is_character(filepath)) stop(private$err_param_type_character("filepath"))
            if(length(filepath) > 1) stop(private$err_one_value("filepath"))
            if(!(import_mode %in% private$.VALID_IMPORT_MODES)) stop(private$err_not_in_vector("import_mode", private$.VALID_IMPORT_MODES))
            if(!purrr::is_character(import_mode)) stop(private$err_param_type_character("import_mode"))
            if(length(import_mode) > 1) stop(private$err_one_value("import_mode"))
            if(!purrr::is_null(push_index) && !purrr::is_character(push_index)) stop(private$err_param_type_character("push_index", can_be_null = TRUE))
            if(!(push_mode %in% private$.VALID_PUSH_MODES)) stop(private$err_not_in_vector("push_mode", private$.VALID_PUSH_MODES))
            if(!purrr::is_null(id_col)){
                if(!purrr::is_character(id_col)) stop(private$err_param_type_character("id_col"))
                if(length(id_col) > 1) stop(private$err_one_value("id_col"))
            }
            if(!purrr::is_logical(to_tibble)) stop(private$err_param_type_logical("to_tibble"))
            if(is.na(to_tibble)) stop(private$err_logical_na("to_tibble"))
            # perform import function
            m <- switch(import_type,
                "auto"      = { self$guess_import },
                "tabular"   = { self$import_tabular },
                "features"  = { self$import_features },
                "alignments"= { self$import_alignments },
                "sequences" = { self$import_sequences },
                stop(private$ERR_WTF)
            )
            data <- m(filepath = filepath, to_tibble = to_tibble)
            # check/push
            no_data <- { dim(data) == c(0, 0) } %>% all()
            if(no_data && self$verbose) message("No data found.")
            res <- NULL
            # import mode
            if(import_mode == "local"){
                # local
                res <- data
            } else {
                # remote (NULL) or both
                r <- kc$push(data = data, index_name = push_index, mode = push_mode, id_col = id_col)
                if(r != push_index) stop("Something went wrong when pushing data.")
                if(import_mode == "both") {
                    res <- kc$pull(push_index)[[push_index]]
                }
            }
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Push data from in-memory to Elasticsearch.
        #' Everything is done by bulk.
        #'
        #' @family move-data
        #'
        #' @examples
        #' # erase the last push data by recreating the index and re-pushing data
        #' kc$push(dplyr::starwars, index_name = "sw", mode = "recreate")
        #' # characters names are unique, can be used as ID
        #' kc$push(dplyr::starwars, index_name = "sw", mode = "recreate", id_col = "name")
        #' # a bit more complicated: update some data of the dataset "starwars"
        #' # 38 records on 87 filtered
        #' some_new_data <- dplyr::filter(dplyr::starwars, height > 180)
        #' # make them all "gender <- female"
        #' some_new_data["gender"] <- "female"
        #' # update that apply, based on cahracter names to match the right record
        #' kc$push(some_new_data, "sw", mode = "update", id_col = "name")
        #' # view result by querying
        #' kc$pull("sw", query = "height:>180", fields = c("name", "gender"))
        #'
        #' @param data the data to push (default: NULL).
        #' @param index_name the index name to use in Elasticsearch (default: NULL).
        #' @param bulk_size the number of record to send to Elasticsearch in a row (default: 1000).
        #' @param mode the push mode, could be "check", "recreate" or "update" (default: "check").
        #' @param id_col an column anme to use as ID, must be composed of unique elements (default: 
        #'  NULL).
        #'
        #' @return the index_name given if the push ended well, else an error.
        #'
        push = function(data = NULL, index_name = NULL, bulk_size = 1000, mode = "check", id_col = NULL){
            if(purrr::is_null(data)) stop(private$err_null_forbidden("data"))
            if(nrow(data) == 0) stop(private$err_null_forbidden("data"))
            # data names to lowercase
            names(data) <- tolower(names(data))
            if(self$verbose) message("All field names are now to lowercase")
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(length(index_name) > 1) stop(private$err_one_value("index_name"))
            if(!is.numeric(bulk_size)) stop(private$err_param_type_numeric("bulk_size"))
            if(length(bulk_size) > 1) stop(private$err_one_value("bulk_size"))
            if(bulk_size < 1) stop(private$err_param_positive("bulk_size",))
            if(!(mode %in% private$.VALID_PUSH_MODES)) stop(private$err_not_in_vector("mode", private$.VALID_PUSH_MODES))
            if(length(mode) > 1) stop(private$err_one_value("mode"))
            if(mode == "check" && self$has(index_name)) stop(private$err_index_already_exists(index_name))
            if(mode == "update"){
                if(purrr::is_null(id_col)) stop("Update mode needs a unique IDs column name.")
                if(!self$has(index_name)) stop(private$err_index_unknown(index_name))
            }
            # handle id_col
            ids <- NULL
            if(!purrr::is_null(id_col)){
                if(!purrr::is_character(id_col)) stop(private$err_param_type_character("id_col"))
                if(length(id_col) > 1) stop(private$err_one_value("id_col"))
                id_col <- tolower(id_col)
                if(!(id_col %in% names(data))) stop("IDs column name not found.")
                # extracting ids from data
                ids <- dplyr::pull(data, id_col)
                if(length(unique(ids)) != length(ids)) stop("Column used as ID does not have unique elements.")
            } else {
                private$err_msg <- paste0("Kibior tried to add a unique column IDs '", self$default_id_col, "', but the column name already exists. ", 
                    "Try to define a column with unique IDs, change the column name or define another default name with '$default_id_col'.")
                if(self$default_id_col %in% names(data)) stop(private$err_msg)
                # force add a column with unique id (single sequence 1:nrow)
                ids <- seq_len(nrow(data))
                data <- within(data, assign(self$default_id_col, ids))
                if(self$verbose) message("Unique '", self$default_id_col, "' column added to enforce uniqueness of each record.")
            }
            # transform col: field names cannot contain dots, transform and warn user
            has_dot <- grepl(".", names(data), fixed = TRUE)
            if(TRUE %in% has_dot){
                # replace names
                old_names <- names(data)[has_dot == TRUE] %>% paste0(collapse = ", ")
                names(data) <- gsub("\\.", "_", names(data))
                new_names <- names(data)[has_dot == TRUE] %>% paste0(collapse = ', ')
                # warn
                if(self$verbose){
                    message("Fields [", old_names, "] contain forbidden dots.")
                    message("They have been replaced by underscores. New names: [", new_names, "]")
                }
            }
            # -----------------------------------------
            # process: create index and mapping
            if(mode != "update"){
                res <- self$create(index_name = index_name, force = (mode == "recreate"))
                if(!res[[index_name]]$acknowledged) stop("Index not created.")
                # define mapping based on data type
                mapping_res <- private$create_mappings(index_name = index_name, data = data)
                if(purrr::is_null(mapping_res) || !mapping_res$acknowledged) stop("Cannot apply mapping to '", index_name, "'.")
            }
            # prepare
            if(self$verbose) message("Sending data to '", index_name, "'.")
            args <- list(conn = self$connection,
                        x = data,
                        index = index_name,
                        chunk_size = bulk_size,
                        raw = FALSE,
                        doc_ids = ids,
                        quiet = self$quiet_progress,
                        query = list(refresh = "wait_for"))
            bulk_method <- NULL
            if(mode != "update"){
                # if id_col not defined by the user, Elasticsearch will give ids automatically
                args[["es_ids"]] = FALSE
                bulk_method <- elastic::docs_bulk_index
            } else {
                bulk_method <- elastic::docs_bulk_update
            }
            # Time the bulk send
            clock_start <- proc.time()
            bulks_res <- do.call(bulk_method, args)
            elapsed <- proc.time() - clock_start
            # verbose
            if(self$verbose){
                es_took <- lapply(bulks_res, function(x){ x[["took"]] }) %>%
                    unlist(use.names = FALSE) %>%
                    sum() %>%
                    as.double() %>%
                    private$humanize_mstime()
                # user info
                user_took <- { elapsed[["elapsed"]] * 1000 } %>% 
                    private$humanize_mstime()
                message("\nData received in ", es_took$time, es_took$unit, 
                    ", user waited ", user_took$time, user_took$unit)
            }
            # get some time so Elasticsearch can make them available
            Sys.sleep(self$elastic_wait)
            if(self$quiet_results) invisible(index_name) else index_name
        },

        #' @details
        #' Pull data from Elasticsearch.
        #' Everything is done by bulk.
        #' This method is essentially a wrapper around `$search()` with parameter `head = FALSE`
        #'
        #' @family move-data, search
        #'
        #' @examples
        #' # push some data sample
        #' kc$push(dplyr::storms, "storms")
        #' # get the whole "sw" index
        #' kc$pull("sw")
        #' # get the whole "sw" index with all metadata
        #' kc$pull("sw", keep_metadata = TRUE)
        #' # get only "name" and "status" fields of indices starting with "s"
        #' # fields not found will be ignored
        #' kc$pull("s*", fields = c("name", "status"))
        #' # limit the size of the result to 3
        #' kc$pull("storms", max_size = 3)
        #' # use Elasticsearch query syntax to select and filter on all indices, for all data
        #' # Here, we want to search for all records taht match the conditions:
        #' # field "height" is strictly more than 180 AND field homeworld is "Tatooine" OR "Naboo"
        #' r <- kc$pull("sw", query = "height:>180 && homeworld:(Tatooine || Naboo)")
        #' # it can be used in conjunction with `fields` to select only columns that matter
        #' r <- kc$pull("sw", query = "height:>180 && homeworld:(Tatooine || Naboo)", fields = 
        #'  c("name", "hair_color", "homeworld"))
        #'
        #' @param index_name the index name to use in Elasticsearch (default: NULL).
        #' @param bulk_size the number of record to send to Elasticsearch in a row (default: 1000).
        #' @param max_size the number of record Elasticsearch will send (default: NULL (all data)).
        #' @param scroll_timer the time the scroll API will let the request alive to scroll on the 
        #'  result (default: "1m" (1 minute)).
        #' @param keep_metadata does Elasticsearch needs to sent metadata? Data fields will be 
        #'  prefixed by "_source." (default: FALSE).
        #' @param fields a vector of fields to select (default: NULL (all fields)).
        #' @param query a string formatted to Elasticsearch query syntax, see links for the syntax 
        #'  details (default: NULL)
        #'
        #' # Simple syntax details:
        #'
        #' @seealso \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#time-units} 
        #'  for time-units and \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html#query-string-syntax}
        #'  for the Elasticsearch query string syntax.
        #'
        #' @return a list of datasets corresponding to the pull request, else an error. Keys of the 
        #'  list are index names matching the request, value are the associated tibbles
        #'
        pull = function(index_name = NULL, bulk_size = 1000, max_size = NULL, scroll_timer = "1m", keep_metadata = FALSE, fields = NULL, query = NULL) {
            args <- list(
                index_name = index_name, 
                keep_metadata = keep_metadata, 
                fields = fields, 
                query = query, 
                bulk_size = bulk_size, 
                max_size = max_size, 
                scroll_timer = scroll_timer, 
                head = FALSE
            )
            if(self$quiet_results) invisible(do.call(self$search, args)) else do.call(self$search, args)
        },

        # TODO test
        #'
        #' @details
        #' Move data from one index to another.
        #' It needs to be configured in the `config/elasticsearch.yml` file to actually work.
        #'
        #' @family move-data
        #'
        #' @examples
        #' kc_one <- Kibior$new("elasticsearch", verbose = TRUE)
        #' kc_two <- Kibior$new("elasticsearch2", verbose = TRUE)
        #' kc_one$push(dplyr::starwars, "sw", mode = "recreate")
        #' # move data from an index to another (change name, same instance)
        #' r <- kc_one$move(from_index = "sw", to_index = "sw_new")
        #' # copy data from an index to another (same instance)
        #' r <- kc_one$move(from_index = "sw_new", to_index = "sw", copy = TRUE)
        #' kc_one$pull(c("sw","sw_new"))
        #' # copy data from an instance to another
        #' r <- kc_two$move(from_instance = kc_one, from_index = "sw_new", to_index = "sw", copy = TRUE)
        #' # list instances
        #' kc_one$list() 
        #' kc_two$list() 
        #' kc_two$pull("sw")
        #'
        #' @param from_instance If not NULL, the Kibior object of another instance. if NULL 
        #'  (default), this instance will be used. (default: NULL).
        #' @param from_index The source index name (default: NULL).
        #' @param to_index The destination index name (default: NULL).
        #' @param force Does the destination index need to be erase? (default: FALSE)
        #' @param copy Does the destination have to be a copy of the source? FALSE (default) will 
        #'  delete source index, TRUE will keep it. (default: FALSE).
        #'
        #' @seealso: \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-reindex.html}
        #'  Elasticsearch reindex feature for more information.
        #'
        #' @return the reindex result
        #'
        move = function(from_instance = NULL, from_index = NULL, to_index = NULL, force = FALSE, copy = FALSE){
            if(!purrr::is_null(from_instance) && !Kibior$is_instance(from_instance)) stop("Need a Kibior instance type or NULL.")
            is_local <- purrr::is_null(from_instance)
            # select source instance
            source_instance <- if(is_local) self else from_instance
            if(!purrr::is_character(from_index)) stop(private$err_param_type_character("from_index"))
            if(length(from_index) > 1) stop(private$err_one_value("from_index"))
            if(!purrr::is_character(to_index)) stop(private$err_param_type_character("to_index"))
            if(length(to_index) > 1) stop(private$err_one_value("to_index"))
            if(!purrr::is_logical(force)) stop(private$err_param_type_logical("force"))
            if(is.na(force)) stop("force")
            if(is_local && from_index == to_index) stop("Source and destination indices are the same.")
            if(!source_instance$has(from_index)) stop(private$err_index_unknown(from_index))
            if(!force && self$has(to_index)) stop(private$err_index_already_exists(to_index))
            if(!purrr::is_logical(copy)) stop(private$err_param_type_logical("copy"))
            if(is.na(copy)) stop(private$err_logical_na("copy"))
            # msg
            action <- if(copy) "Copying" else "Moving" 
            source_str <- paste0("'" , source_instance$host, ":", source_instance$port, "/", from_index, "'")
            dest_str <- paste0("'", self$host, ":", self$port, "/", to_index, "'")
            message(action, " " , source_str, " to ", dest_str)
            # TODO: add "_source" = fields to select only some fields
            # TODO: add "query" = query to execute a query before moving, need to parse query string to struct
            # building args - source subpart
            source_param <- list(index = from_index)
            if(!is_local){
                source_param[["remote"]] <- list(host = from_instance$endpoint)
                if(!purrr::is_null(from_instance$username)){
                    source_param[["remote"]][["username"]] <- from_instance$username
                }
                if(!purrr::is_null(from_instance$password)){
                    source_param[["remote"]][["password"]] <- from_instance$password   
                }
            }
            # compose args - source + dest
            reindex_args <- list(
                conn = self$connection,
                body = list(
                    source = source_param,
                    dest = list(index = to_index)
                )
            )
            # function to manage ES errors
            handle_error <- function(e){
                message("Some errors occur during data transfer.")
                invisible(self$delete(to_index))
                stop(e)
            }
            # reindex
            self$create(to_index, force = force)
            res <- tryCatch({
                    do.call(elastic::reindex, reindex_args)
                }, error = function(e){
                    handle_error(e)
                }
            )
            # if there are some errors during reindex
            if(length(res$failure) != 0) handle_error(res$failure)
            # removing source index if action is "moving"
            if(!copy) source_instance$delete(from_index)
            # msg
            if(self$verbose){
                t <- private$humanize_mstime(res$took)
                message("Documents transfered: ", res$total, ", took: ", t$time, t$unit)
            }
            Sys.sleep(self$elastic_wait)
            if(self$quiet_results) invisible(res) else res
        },

        # TODO test
        #'
        #' @details
        #' Copy data from one index to another.
        #' It needs to be configured in the `config/elasticsearch.yml` file to actually work.
        #' This method is a wrapper around `$move(copy = TRUE)`.
        #'
        #' @family move-data
        #'
        #' @examples
        #' # copy data from one index to another (same instance)
        #' r <- kc_one$copy(from_index = "sw", to_index = "sw2")
        #' kc_one$pull(c("sw", "sw2"))
        #' # copy data from an instance to another
        #' r <- kc_two$copy(from_instance = kc_one, from_index = "sw2", to_index = "sw2", force = TRUE)
        #' # list instances
        #' kc_one$list() 
        #' kc_two$list() 
        #' kc_two$pull(c("sw2"))
        #'
        #'
        #' @param from_instance If not NULL, the Kibior object of another instance. if NULL 
        #'  (default), this instance will be used. (default: NULL).
        #' @param from_index The source index name (default: NULL).
        #' @param to_index The destination index name (default: NULL).
        #' @param force Does the destination index need to be erase? (default: FALSE)
        #'
        #' @seealso: \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/docs-reindex.html}
        #'  Elasticsearch reindex feature for more information.
        #'
        #' @return the reindex result
        #'
        copy = function(from_instance = NULL, from_index = NULL, to_index = NULL, force = FALSE){
            args <- list(
                from_instance = from_instance, 
                from_index = from_index, 
                to_index = to_index, 
                force = force, 
                copy = TRUE
            )
            if(self$quiet_results) invisible(do.call(self$move, args)) else do.call(self$move, args)
        },


        # --------------------------------------------------------------------------
        # methods - Search
        # --------------------------------------------------------------------------

        
        # b <- list(
        #     "aggs" = list(
        #         "agg_name" = list(
        #             "terms" = list(
        #                 "field" = "name.keyword"
        #             )
        #         )
        #     )
        # )
        # elastic::Search(kc$connection, size=0, asdf=TRUE, body = b) %>% 
        #     .[["aggregations"]] %>% 
        #     .[["agg_name"]] %>% 
        #     .[["buckets"]] %>% 
        #     dplyr::as_tibble()


        #' @details
        #' Search data from Elasticsearch.
        #' The goal of this method is to discover quickly what data are interesting, thus 
        #'  `head = TRUE` by default.
        #' If you want to get all data, use `head = FALSE` or `$pull()`.
        #' Everything is done by bulk.
        #'
        #' @family search
        #'
        #' @examples
        #' # search "sw" index, head mode on
        #' kc$search("sw")
        #' # search "sw" index with all metadata, head mode on
        #' kc$search("sw", keep_metadata = TRUE)
        #' # get only "name" field of the head of indices starting with "s"
        #' # if an index does not have the "name" field, it will be empty
        #' kc$search("s*", fields = "name")
        #' # limit the size of the result to 50 to the whole index
        #' kc$search("storms", max_size = 50, head = FALSE)
        #' # use Elasticsearch query syntax to select and filter on all indices, for all data
        #' # Here, we want to search for all records taht match the conditions:
        #' # field "height" is strictly more than 180 AND field homeworld is "Tatooine" OR "Naboo"
        #' kc$search("*", query = "height:>180 && homeworld:(Tatooine || Naboo)")
        #' # it can be used in conjunction with `fields` to select only columns that matter
        #' kc$search("*", query = "height:>180 && homeworld:(Tatooine || Naboo)", fields = 
        #'  c("name", "hair_color", "homeworld"))
        #'
        #' @param index_name the index name to use in Elasticsearch (default: NULL).
        #' @param bulk_size the number of record to send to Elasticsearch in a row (default: 1000).
        #' @param max_size the number of record Elasticsearch will send (default: NULL (all data)).
        #' @param scroll_timer the time the scroll API will let the request alive to scroll on the 
        #'  result (default: "1m" (1 minute)).
        #' @param keep_metadata does Elasticsearch needs to sent metadata? Data fields will be 
        #'  prefixed by "_source." (default: FALSE).
        #' @param fields a vector of fields to select (default: NULL (all fields)).
        #' @param head a boolean limiting the search result and time (default: TRUE)
        #' @param query a string formatted to Elasticsearch query syntax, see links for the syntax 
        #'  details (default: NULL)
        #'
        #' @seealso \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#time-units} 
        #'  for time-units and \url{https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-string-query.html#query-string-syntax}
        #'  for the Elasticsearch query string syntax.
        #'
        #' @return a list of datasets corresponding to the pull request, else an error. Keys of the 
        #'  list are index names matching the request, value are the associated tibbles
        #'
        search = function(index_name = "_all", keep_metadata = FALSE, fields = NULL,  bulk_size = 500, max_size = NULL, scroll_timer = "3m", head = TRUE, query = NULL){
            if(purrr::is_null(index_name)) index_name <- "_all"
            if(!purrr::is_character(index_name)) stop(private$err_param_type_character("index_name"))
            if(!is.numeric(bulk_size)) stop(private$err_param_type_numeric("bulk_size"))
            if(length(bulk_size) > 1) stop(private$err_one_value("bulk_size"))
            if(bulk_size < 1) stop(private$err_param_positive("bulk_size", can_be_null = FALSE))
            bulk_size <- as.integer(bulk_size)
            if(!purrr::is_null(max_size)){
                if(!is.numeric(max_size)) stop(private$err_param_type_numeric("max_size"))
                if(length(max_size) > 1) stop(private$err_one_value("max_size"))
                if(max_size < 1) stop(private$err_param_positive("max_size", can_be_null = FALSE))
                if(bulk_size > max_size) stop("'bulk_size' > 'max_size' (", bulk_size, ">", max_size, ")")
                max_size <- as.integer(max_size)
            }
            if(!purrr::is_character(scroll_timer)) stop(private$err_param_type_character("scroll_timer"))
            if(length(scroll_timer) > 1) stop(private$err_one_value("scroll_timer"))
            if(!purrr::is_logical(keep_metadata)) stop(private$err_param_type_logical("keep_metadata"))
            if(is.na(keep_metadata)) stop(private$err_logical_na("keep_metadata"))
            if(!purrr::is_null(fields) && !purrr::is_character(fields)) stop(private$err_param_type_character("fields"))
            if(!purrr::is_null(query)){
                if(!purrr::is_character(query)) stop(private$err_param_type_character("query"))
                if(length(query) > 1) stop(private$err_one_value("query"))
            }
            if(!purrr::is_logical(head)) stop(private$err_param_type_logical("head"))
            if(is.na(head)) stop(private$err_logical_na("head"))
            if(("_all" %in% index_name) && !head) message("Retriving ALL data... you should target some indices instead.")
            # define when to stop the search with head mode
            if(self$verbose) {
                message("Head mode: ", if(head) "on" else "off" , "")
                if(head) message("Head size: ", self$head_search_size)
            }
            # terminate_after <- if(head) self$head_search_size else NULL
            # return result
            final_df <- list()
            # init
            selected_fields <- if(purrr::is_null(fields)) NULL else paste0(fields, collapse = ",")
            end_search <- FALSE
            # # escape string for ES
            # # https://stackoverflow.com/a/14838753
            # escape_elastic_reserved_characters <- function(s){
            #     stringr::str_replace_all(s, "(\\W)", "\\\\\\1")
            # }

            # get involved indices in search
            get_involved_indices <- function(req_indices){
                # useless if all indices are named
                if(!private$is_search_pattern(req_indices)){
                    index_names_list <- req_indices
                } else {
                    # get indices
                    index_names_list <- elastic::search_shards(
                            conn = self$connection,
                            index = req_indices
                        ) %>% 
                        .[["indices"]] %>% 
                        names()
                }
                res <- list()
                for(i in index_names_list){
                    res[[i]] <- list()
                }
                res
            }
            # total according to ES version
            get_total_records <- function(hits_total){
                if(self$version$major > 6) hits_total$value else hits_total
            }
            # function: get clean bulk
            get_clean_bulk <- function(bulk){
                "Clean bulks when they arrive"
                # stable
                workable_hits <- list()
                for(i in seq(nrow(bulk))) {
                    # get each line (df) as list
                    t <- bulk[i,] %>% as.list()
                    # extract _source if needed, else flatten the results
                    # Must be only one level, error else
                    # TODO: handle x levels: recusive = T -> prb with rbindlist
                    if(keep_metadata){
                        workable_hits[[i]] <- unlist(t, recursive = FALSE)
                    } else {
                        workable_hits[[i]] <- t[["_source"]]
                    }
                }
                # combine to raw_hits
                workable_hits %>%
                    data.table::rbindlist(use.names = TRUE, fill = FALSE) %>%
                    dplyr::as_tibble(validate = TRUE)
            }
            # function: change column type
            change_column_type <- function(raw){
                "Take a tibble with listed columns and tries to 
                take the inner type of the listed type
                "
                data <- raw
                for(i in names(raw)){
                    n <- data[[i]][[1]]
                    if(length(n) == 1){
                        f <- switch(typeof(n),
                            "character" = { as.character },
                            "integer" = { as.integer },
                            "double" = { as.double },
                            "logical" = { as.logical },
                            stop(private$ERR_WTF, "Unknown type '", n, "'.")
                        )
                        data[[i]] <- f(data[[i]])
                    }
                }
                data
            }
            # extract ids
            extract_ids <- function(hits){
                hits %>% 
                    lapply(function(h){ 
                        h[["_id"]] 
                    }) %>% 
                    unlist(use.names = FALSE)
            }

            # involved_indices
            final_df <- get_involved_indices(index_name)
            if(self$verbose){
                final_df %>% 
                    names() %>% 
                    paste0(collapse = ", ") %>% 
                    paste0("Indices involved: ", .) %>%
                    message()
            }
            # various infos per index
            run_infos <- final_df
            for(i in names(run_infos)){
                run_infos[[i]][["end_reached"]] <- FALSE
                run_infos[[i]][["total_hits"]] <- NA
                run_infos[[i]][["threshold"]] <- NA
            }

            # per index, first search config
            for(current_index in names(run_infos)){

                # first search, init scroll and manage error
                search_res <- tryCatch({
                        elastic::Search(
                            self$connection, 
                            index = current_index, 
                            size = bulk_size, 
                            source = "__",  # nullify _source
                            time_scroll = scroll_timer, 
                            q = query
                        )
                    },
                    error = function(e){
                        # pattern when limit of request size is reached
                        r <- e$message %>% 
                            trimws() %>%
                            stringr::str_match("400 - An HTTP line is larger than ([0-9]+) bytes.") %>%
                            .[1,2]
                        if(!is.na(r)){
                            msg <- paste0("Server '", self$cluster_name, "' (", self$host, ")")
                            msg <- paste0(msg, " cannot take requests that big (max ", r," bytes).")
                            msg <- paste0(msg, " Try cutting down your query into several pieces.")
                        } else {
                            msg <- e$message
                        }
                        stop(msg)
                    }
                )

                if(self$verbose){
                    message("- [", current_index, "]")
                }
                # check total hits
                run_infos[[current_index]][["total_hits"]] <- get_total_records(search_res$hits$total)
                if(self$verbose){
                    message("   Total hits: ", run_infos[[current_index]][["total_hits"]])
                }
                # no results
                if(length(search_res$hits$hits) == 0){
                    run_infos[[current_index]][["end_reached"]] <- FALSE
                }
                
                # max threshold
                tmp_threshold <- if(purrr::is_null(max_size)) run_infos[[current_index]][["total_hits"]] else max_size
                if(head && tmp_threshold > self$head_search_size){
                    tmp_threshold <- self$head_search_size
                } 
                run_infos[[current_index]][["threshold"]] <- tmp_threshold
                if(self$verbose){
                    message("   Threshold: ", run_infos[[current_index]][["threshold"]])
                }
                
                # scroll id
                run_infos[[current_index]][["scroll_id"]] <- search_res[["_scroll_id"]]

                # timer 
                run_infos[[current_index]][["timer"]] <- 0

                # last hits
                run_infos[[current_index]][["hits"]] <- extract_ids(search_res$hits$hits)
            }


            # base args for inc loops
            base_args = list(
                conn = self$connection, 
                source = TRUE, 
                raw = TRUE, 
                verbose = FALSE, 
                callopts=list(verbose=FALSE)
            )

            # verbose total hits info
            if(self$verbose){
                cumul_total_hits <- run_infos %>% 
                    lapply(function(x){ 
                        x[["total_hits"]]
                    }) %>% 
                    unlist(use.names = FALSE) %>%
                    sum()
                message("Total hits: ", cumul_total_hits)
                if(!head && !purrr::is_null(max_size)){
                    message("Max size asked: ", max_size)
                }
            }

            # progress bar init
            if(!head && !self$quiet_progress){
                cumul_threshold <- run_infos %>% 
                    lapply(function(x){ 
                        if(x[["threshold"]] < x[["total_hits"]]) x[["threshold"]] else x[["total_hits"]]
                    }) %>% 
                    unlist(use.names = FALSE) %>%
                    sum()
                # init pbar
                pb_sum <- 0
                pb <- txtProgressBar(min = 0, max = cumul_threshold, initial = 0, style = 3)
            }

            # start timers
            for(current_index in names(final_df)){
                run_infos[[current_index]][["timer"]] <- proc.time()
            }

            # loop until nothing is returned or threshold is reached
            all_end_search <- FALSE
            while(!all_end_search){

                # each index
                for(current_index in names(final_df)){

                    # if end not reached for this index, explore
                    if(!run_infos[[current_index]][["end_reached"]]){

                        # search list of ids
                        if(length(run_infos[[current_index]][["hits"]]) == 1) {

                            # complete args
                            args <- c(base_args, list(
                                index = current_index,
                                id = run_infos[[current_index]][["hits"]],
                                source_includes = selected_fields
                            ))

                            # get data from ES (simple get)
                            raw <- do.call(elastic::docs_get, args) %>%
                                jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
                                (function(x){ 
                                    if(keep_metadata) unlist(x, recursive = FALSE) else x[["_source"]] 
                                }) %>%
                                rbind() %>% 
                                as.data.frame() %>% 
                                dplyr::as_tibble() %>%
                                change_column_type()
                            
                            # combne results in one df
                            final_df[[ current_index ]] <- raw
                            # end
                            run_infos[[current_index]][["end_reached"]] <- TRUE
                            #
                            nb_hits <- 1

                        } else {
                            
                            # ids to req
                            if(head && length(run_infos[[current_index]][["hits"]]) > self$head_search_size){
                                req_ids <- head(run_infos[[current_index]][["hits"]], self$head_search_size) 
                            } else {
                                req_ids <- run_infos[[current_index]][["hits"]]
                            }

                            # complete args
                            args <- c(base_args, list(
                                index = current_index,
                                ids = req_ids,
                                "_source_includes" = selected_fields
                            ))

                            # get data from ES
                            res <- do.call(elastic::docs_mget, args) %>% 
                                jsonlite::fromJSON(simplifyDataFrame = TRUE)
                            
                            # if the result is not empty
                            nb_hits <- nrow(res$docs)
                            if(nb_hits > 0){
                                
                                # tidy data
                                raw <- get_clean_bulk(res$docs)

                                # combne results in one df
                                if(length(final_df[[ current_index ]]) == 0){
                                    final_df[[ current_index ]] <- raw
                                } else {
                                    final_df[[ current_index ]] <- rbind(raw, final_df[[ current_index ]])
                                }
                            }

                        }
                        
                        # update pbar
                        if(!head && !self$quiet_progress){
                            pb_sum <- pb_sum + nb_hits
                            setTxtProgressBar(pb, pb_sum)
                        }

                        # test end
                        if(nrow(final_df[[current_index]]) >= run_infos[[current_index]][["threshold"]]){
                            run_infos[[current_index]][["end_reached"]] <- TRUE

                        } else {
                            # continue search scroll
                            search_res <- elastic::scroll(self$connection, 
                                                        run_infos[[current_index]][["scroll_id"]], 
                                                        time_scroll = scroll_timer)

                            tmp_ids <- extract_ids(search_res$hits$hits)
                            run_infos[[current_index]][["hits"]] <- tmp_ids
                            run_infos[[current_index]][["end_reached"]] <- (length(tmp_ids) == 0)
                        }

                    }

                    # if end reached for this index
                    if(run_infos[[current_index]][["end_reached"]]){
                        # Stop the clock
                        run_infos[[current_index]][["timer"]] <- proc.time() - run_infos[[current_index]][["timer"]]
                        
                    }
                }

                # test if all ends are set
                all_end_search <- run_infos %>% 
                    lapply(function(x){ x[["end_reached"]] }) %>%
                    unlist(use.names = FALSE)  %>%
                    all()
            }

            # verbose time
            if(self$verbose){
                message()
                for(current_index in names(final_df)){
                    # close scroll
                    if(!elastic::scroll_clear(self$connection, run_infos[[current_index]][["scroll_id"]])){
                        message("[", current_index, "] implicite scroll closing pending.")
                    }
                    #
                    user_took <- { 
                            run_infos[[current_index]][["timer"]][["elapsed"]] * 1000 
                        } %>% 
                        private$humanize_mstime()
                    message("[", current_index, "] execution time: ", user_took$time, user_took$unit)

                }
            }

            # close progress bar
            if(!head && !self$quiet_progress) close(pb)
            # return 
            if(self$quiet_results) invisible(final_df) else final_df
        },


        # --------------------------------------------------------------------------
        # methods - Joins
        # --------------------------------------------------------------------------

        #' @details
        #' Execute a inner join between two datasets using `dplyr` joins.
        #' The datasets can be in-memory (variable name) or the name of an currently stored 
        #'  Elasticsearch index. Joins cannot be done on column of type "list" ("join_fields" 
        #'  argument).
        #'
        #' @param ... see `join()` params.
        #'
        #' @family joins
        #'
        #' @examples
        #' # some data for joins examples
        #' kc$push(ggplot2::diamonds, "diamonds")
        #' # prepare join datasets, only big the biggest diamonds are selected (9)
        #' sup_carat <- dplyr::filter(ggplot2::diamonds, carat > 3.5)
        #' r <- kc$push(sup_carat, "diamonds_superior")
        #' # execute a inner_join with one index and one in-memory dataset
        #' kc$inner_join(left_index = ggplot2::diamonds, right_index = "diamonds_superior")
        #' # execute a inner_join with one index queried, and one in-memory dataset
        #' kc$inner_join(left_index = ggplot2::diamonds, right_index = "diamonds", right_query 
        #'  = "carat:>3.5")
        #'
        #' @return a tibble
        #'
        inner_join = function(...) {
            res <- private$join(join_type = "inner", ...)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Execute a full join between two datasets using `dplyr` joins.
        #' The datasets can be in-memory (variable name) or the name of an currently stored 
        #'  Elasticsearch index. Joins cannot be done on column of type "list" ("join_fields" 
        #'  argument).
        #'
        #' @param ... see `join()` params.
        #'
        #' @family joins
        #'
        #' @examples
        #' # prepare join datasets, fair cuts 
        #' fair_cut <- dplyr::filter(ggplot2::diamonds, cut == "Fair")  # 1605 lines
        #' sup_carat <- kc$pull("diamonds_superior")$diamonds_superior
        #' # execute a full_join with one index and one in-memory dataset
        #' kc$full_join(left_index = fair_cut, right_index = "diamonds_superior")
        #' # execute a full_join with one index queried, and one in-memory dataset
        #' kc$full_join(left_index = sup_carat, right_index = "diamonds", right_query 
        #'  = "cut:fair")
        #'
        #' @return a tibble
        #'
        full_join = function(...) {
            res <- private$join(join_type = "full", ...)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Execute a left join between two datasets using `dplyr` joins.
        #' The datasets can be in-memory (variable name) or the name of an currently stored 
        #'  Elasticsearch index. Joins cannot be done on column of type "list" ("join_fields" 
        #'  argument).
        #'
        #' @param ... see `join()` params.
        #'
        #' @family joins
        #'
        #' @examples
        #' # prepare join datasets, fair cuts 
        #' fair_cut <- dplyr::filter(ggplot2::diamonds, cut == "Fair")  # 1605 lines
        #' sup_carat <- kc$pull("diamonds_superior")$diamonds_superior
        #' # execute a left_join with one index and one in-memory dataset
        #' kc$left_join(left_index = fair_cut, right_index = "diamonds_superior")
        #' # execute a left_join with one index queried, and one in-memory dataset
        #' kc$left_join(left_index = sup_carat, right_index = "diamonds", right_query 
        #'  = "cut:fair")
        #'
        #' @return a tibble
        #'
        left_join = function(...) {
            res <- private$join(join_type = "left", ...)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Execute a right join between two datasets using `dplyr` joins.
        #' The datasets can be in-memory (variable name) or the name of an currently stored 
        #'  Elasticsearch index. Joins cannot be done on column of type "list" ("join_fields" 
        #'  argument).
        #'
        #' @param ... see `join()` params.
        #'
        #' @family joins
        #'
        #' @examples
        #' # prepare join datasets, fair cuts 
        #' fair_cut <- dplyr::filter(ggplot2::diamonds, cut == "Fair")  # 1605 lines
        #' sup_carat <- kc$pull("diamonds_superior")$diamonds_superior
        #' # execute a right_join with one index and one in-memory dataset
        #' kc$right_join(left_index = fair_cut, right_index = "sup_carat")
        #' # execute a right_join with one index queried, and one in-memory dataset
        #' kc$right_join(left_index = sup_carat, right_index = "diamonds", right_query 
        #'  = "cut:fair")
        #'
        #' @return a tibble
        #'
        right_join = function(...) {
            res <- private$join(join_type = "right", ...)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Execute a semi join between two datasets using `dplyr` joins.
        #' The datasets can be in-memory (variable name) or the name of an currently stored 
        #'  Elasticsearch index. Joins cannot be done on column of type "list" ("join_fields" 
        #'  argument).
        #'
        #' @param ... see `join()` params.
        #'
        #' @family joins
        #'
        #' @examples
        #' # prepare join datasets, fair cuts 
        #' fair_cut <- dplyr::filter(ggplot2::diamonds, cut == "Fair")  # 1605 lines
        #' sup_carat <- kc$pull("diamonds_superior")$diamonds_superior
        #' # execute a semi_join with one index and one in-memory dataset
        #' kc$semi_join(left_index = fair_cut, right_index = "diamonds_superior")
        #' # execute a semi_join with one index queried, and one in-memory dataset
        #' kc$semi_join(left_index = sup_carat, right_index = "diamonds", right_query 
        #'  = "cut:fair")
        #'
        #' @return a tibble
        #'
        semi_join = function(...) {
            res <- private$join(join_type = "semi", ...)
            if(self$quiet_results) invisible(res) else res
        },

        #' @details
        #' Execute a anti join between two datasets using `dplyr` joins.
        #' The datasets can be in-memory (variable name) or the name of an currently stored 
        #'  Elasticsearch index. Joins cannot be done on column of type "list" ("join_fields" 
        #'  argument).
        #'
        #' @param ... see `join()` params.
        #'
        #' @family joins
        #'
        #' @examples
        #' # prepare join datasets, fair cuts 
        #' fair_cut <- dplyr::filter(ggplot2::diamonds, cut == "Fair")  # 1605 lines
        #' sup_carat <- kc$pull("diamonds_superior")$diamonds_superior
        #' # execute a anti_join with one index and one in-memory dataset
        #' kc$anti_join(left_index = fair_cut, right_index = "diamonds_superior")
        #' # execute a anti_join with one index queried, and one in-memory dataset
        #' kc$anti_join(left_index = sup_carat, right_index = "diamonds", right_query 
        #'  = "cut:fair")
        #'
        #' @return a tibble
        #'
        anti_join = function(...) {
            res <- private$join(join_type = "anti", ...)
            if(self$quiet_results) invisible(res) else res
        }

    )
)


# -------------------------------
# Kibior static methods
#


#' 
#' @title [static] Kibior is instance
#' @name [static] Kibior is instance
#' 
#' @details
#' Tests if a given object is a Kibior instance.
#' Basically compute symmetric difference between two sets of class.
#'
#' @param other an object
#'
#' @family comparison
#'
#' @return TRUE if the given object is an instance of Kibior, else FALSE
#' 
Kibior$is_instance <- function(other){
    x <- c(Kibior$classname, "R6")
    y <- class(other)
    sd <- dplyr::setdiff(dplyr::union(x, y), dplyr::intersect(x, y))
    # sd <- private$symmetric_difference(class(self), class(other))
    (purrr::is_character(sd) && length(sd) == 0)
}

#' 
#' @title [static] Tests if packages are installed
#' @name [static] Tests if packages are installed
#' 
#' @details
#' Given a vector of string, returns a list of packages that are not installed 
#' in the current env
#'
#' @param pkg_names a vector of some package names
#'
#' @family install
#'
#' @return a vector of not installed packages
#' 
Kibior$.pkg_not_installed <- function(pkg_names){
    if(!purrr::is_character(pkg_names)) stop("Need package names.")
    pkg_names[!(pkg_names %in% installed.packages()[,"Package"])]
}

#' 
#' @title [static] Install packages
#' @name [static] Install packages
#' 
#' @details
#' Check packages installation in the current env from a given list
#'
#' @param pkg_names a vector of some package names
#' @param install a logical thtat defines if packages should be installed. (default: TRUE)
#'
#' @family install
#'
#' @return NULL
#' 
Kibior$.install_packages <- function(pkg_names, install = TRUE){
    if(length(Kibior$.pkg_not_installed(pkg_names)) > 0){
        tryCatch(
            expr = {
                # BiocManager
                if(!requireNamespace("BiocManager", quietly = TRUE))
                    install.packages("BiocManager")
                # new package
                if(install) BiocManager::install(pkg_names)
            }, error = function(e){
                if(getOption("verbose")){
                    message(e)
                } else {
                    message("See global verbose variable to see details (options(verbose = TRUE)).")
                }
                paste0(pkg_names, collapse = "', '") %>%
                    message("Errors occur during installation of '", pkg_names, "'.")
                message("Check system dependencies needed on Kibior vignettes.")
            }
        )
    } else {
        if(getOption("verbose")){
            paste0(pkg_names, collapse = "', '") %>% 
                message("Packages '", pkg_names, "' already installed.")
        }
    }
    invisible(NULL)
}


# -------------------------------
# Kibior operators
#

# allow to attach in the current env the "==" and "!=" operators for "KibiorOperators" derived 
#   classes

#' 
#' @title Kibior equals operator
#' @name Kibior equals operator
#' 
#' @details
#' Call kibioR `$eq()` for a comparison of the two instances.
#'
#' @param x the first Kibior instance
#' @param y the second Kibior instance
#'
#' @family comparison
#'
#' @return TRUE if the two instances are equals, else FALSE
#' 
`==.KibiorOperators` = function(x, y){ x$eq(y) }

#' 
#' @title Kibior not-equals operator
#' @name Kibior not-equals operator
#' 
#' @details
#' Call kibioR `$ne()` for a comparison of the two instances.
#'
#' @param x the first Kibior instance
#' @param y the second Kibior instance
#'
#' @family comparison
#'
#' @return TRUE if the two instances are differents, else FALSE
#' 
`!=.KibiorOperators` = function(x, y){ x$ne(y) }
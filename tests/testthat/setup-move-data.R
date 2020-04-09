
message("Setup - move data - start")

# remove indices if they exist
remove_all_indices()

# change names to lower and with underscores
change_names <- function(dataset){
    # to lower
    names(dataset) <- tolower(names(dataset))
    # dots to underscores 
    names(dataset) <- gsub("\\.", "_", names(dataset))
    dataset
}

# some datasets
ds <- list(
    "starwars" = change_names(dplyr::starwars),   # small, 87 records
    "storms" = change_names(dplyr::storms),       # medium, 10k records
    "diamonds" = change_names(ggplot2::diamonds)  # large, 53k records
)

# modifications on parts of datasets

# starwars
## 38 records modified to force them into female gender
s <- dplyr::starwars %>% 
    dplyr::filter(height > 180)
s["gender"] <- "female"

# storms
## 44 records modified to a new category that does not exist
st <- dplyr::storms %>% 
    dplyr::filter(pressure < 980 & status == "tropical storm")
st["category"] <- 18

# diamonds
## 74 records modified to a new color that does not exist
d <- ggplot2::diamonds %>% filter(clarity == "VS1" & depth > 65)
d["color"] <- "W"

# only the updated records here, not all
ds_modified <- list(
    "starwars" = change_names(s),
    "storms" = change_names(st),
    "diamonds" = change_names(d)
)

# temporary file
temp_filepath <- "./wesh.csv"

remove_temp_files <- function(){
    unlink(temp_filepath)
}

count_nb_lines <- function(filepath){
    f <- file(filepath, open="rb")
    nlines <- 0L
    while(length(chunk <- readBin(f, "raw", 65536)) > 0) {
        nlines <- nlines + sum(chunk == as.raw(10L))
    }
    close(f)
    nlines
}

message("Setup - move data - stop")

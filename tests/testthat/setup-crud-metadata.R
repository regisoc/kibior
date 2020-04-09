
message("Setup - CRUD metadata - start")

# remove indices if they exist
remove_all_indices()

all_features <-  c("aliases", "mappings", "settings")

message("Setup - CRUD metadata - stop")

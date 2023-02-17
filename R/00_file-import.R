# import script for initial data

library(EcotaxaTools)

# Only works on local format
raw_data <- ecopart_import('~/BATS_data/ae1913exportAll', trim_to_zoo = T) #there is a missing zoo cast for bv55c14


# |- Trim to relevant casts ----------------

# re-order lists to match meta file
raw_data$par_files <- raw_data$par_files[order(names(raw_data$par_files), raw_data$meta$profileid)]
raw_data$zoo_files <- raw_data$zoo_files[order(names(raw_data$zoo_files), raw_data$meta$profileid)]

keep_casts <- grep('bv55', raw_data$meta$ctd_origfilename)

raw_data$par_files <- raw_data$par_files[keep_casts]
raw_data$zoo_files <- raw_data$zoo_files[keep_casts]
raw_data$meta <- raw_data$meta[keep_casts,]

# confirm class structure
raw_data <- as_ecopart_obj(raw_data)

# |- Trim to only keep Rhizaria ------------------

rhiz_only <- raw_data |> 
  mod_zoo(func = names_keep, keep_names = 'Rhizaria', keep_children = T)

# |- Save rhizaria object
saveRDS(rhiz_only, './data/00_rhizaria.RDS')

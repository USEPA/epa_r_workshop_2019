library(progress)
library(crandb)
all_packages <- available.packages()[,1]
package_maintainers <- vector("character", length(all_packages))
pb <- progress_bar$new(total = length(all_packages))
for(i in seq_along(all_packages)){
  pb$tick()
  package_maintainers[i] <- crandb::package(all_packages[i])$Maintainer
  Sys.sleep(1/length(all_packages))
}
idx <- grepl("epa.gov",package_maintainers)
all_packages[idx]

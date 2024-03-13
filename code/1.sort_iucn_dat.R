#' Combine data downloaded from IUCN Red List 
#' 
#' Download data from the Red List as follows:
#' 1. Login to the IUCN Red List (https://www.iucnredlist.org/) and click on your name to see your account details
#' 2. From your account page, click on 'Edit profile' from the options on the left
#' 3. Scroll down to the bottom of the 'Edit profile' page
#' 4. Check whichever datasets you want to download - I checked everything
#' 5. Leave the 'Edit profile' page and do an advanced search, modifying the search filters accordingly - I searched for LC species, and then everything but LC
#' 6. Once your search filters are correct, click on 'Download' in the top-right corner, and then select 'Search Results' from the drop-down menu
#' 
#' If you did multiple searches you will have multiple files for each dataset 
#' downloaded. The code below can be adapted to combine files, giving just one
#' file per dataset. 

# Combine LC and non LC data
combine_dat <- function(file1, file2){
    file1 <- read.csv(file1)
    file2 <- read.csv(file2)
    return(rbind(file1, file2))
}

dirs <- c("data/iucn_info_LC", "data/iucn_info_nonLC")
files <- list.files(dirs[1], pattern = "*.csv")

all <-
    lapply(files[1:length(files)], function(x){
        combine_dat(file.path(dirs[1], x),
                    file.path(dirs[2], x))
    })
names(all) <- files

# Write each element to csv
dir.create("data/iucn_info")
lapply(1:length(all), function(i){
    write.csv(x = all[[i]], 
              file = file.path("data/iucn_info", files[i]), 
              row.names = FALSE)
})

#' Sort status change data from IUCN Red List 
#' 
#' Status change tables are available in pdf format here (Table 7): https://www.iucnredlist.org/resources/summary-statistics
#' The code below will combine and sort through status change tables, arranging 
#' them into a single, standardised dataframe

# Setup -------------------------------------------------------------------

# Package names
packages <- c("pdftools","dplyr","tidyr","purrr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Sort status data ---------------------------------------------------------------

# Function to identify data (not descriptive text or column headers)
dattest <- function(x){
    string <- unlist(strsplit(x, "_"))
    ifelse(length(string) >= 3, return(TRUE), return(FALSE)) 
}

get_status_data <- function(filepath){
    
    # Get year from filename
    year <-
        basename(filepath) %>% 
        strsplit("_") %>% 
        unlist() %>% 
        .[1] %>% 
        gsub("[RL]", "", .)
    
    iucndat <-
        # Read pdf text
        pdf_text(filepath) %>% 
        # Split by escape
        strsplit(split = "\n") %>%
        # Replace whitespace with underscore
        map(function(x){
            gsub(pattern = "[ ]{2,}", replacement = "_", x = x) 
        }) %>% 
        unlist() %>% 
        map(function(y){
            result <-
                # Remove leading underscore
                gsub(pattern = "^_", replacement = "", x = y) %>% 
                # Remove line end \r
                gsub(pattern = "\r", replacement = "", x = .) %>%
                # Trim whitespace
                trimws(.)
            return(result)
        }) %>% 
        unlist() %>% 
        # Reduce to rows that contain data
        .[which(sapply(., dattest))] %>% 
        # Remove misc column headings
        .[grep("Scientific name", ., invert = TRUE)] %>% 
        .[grep("Category", ., invert = TRUE)] %>% 
        # Split by underscore
        map(function(z) unlist(strsplit(z, "_")))
    
    # Define maximum number of variables
    nvars <- max(unlist(lapply(iucndat, length)))
    # Identify species missing 'common name'
    nocommon_i <- which(unlist(lapply(iucndat, function(i) length(i) < nvars)))
    # Seperate from original list
    nocommon <- iucndat[nocommon_i]
    iucndat <- iucndat[-nocommon_i]
    
    # Insert NA as 2nd element of vector
    nocommon <- lapply(nocommon, function(i) c(i[1], NA, i[2:length(i)]))
    
    iucndat <- 
        # Bind two lists together
        c(iucndat, nocommon) %>% 
        # Coerce to dataframe %>% 
        do.call("rbind", .) %>% 
        as.data.frame(stringsAsFactors = FALSE)
    
    # If year is 2008, add variables: reason for change and version
    if (year == "2008") iucndat <- mutate(iucndat, reason = "G", ver = "2008")
    
    # Define column names
    colnames(iucndat) <- c("scientific", "common", 
                           "cat_old", "cat_new",
                           "reason", "ver")
    # Add year
    iucndat$year <- year
    
    return(iucndat)
}

# Identify all pdf files in dir
files <- list.files("data/status", pattern = ".pdf", full.names = TRUE)

alldat <- 
    # Read in all data
    lapply(files, get_status_data) %>% 
    # Bind
    bind_rows() %>% 
    # Recategorise
    mutate(cat_old  = recode(cat_old,
                             # NR occurs after taxonomy change
                             "NR" = "DD",
                             # Merge possibly extinct into critically endangered
                             "CR (PE)" = "CR",
                             "CR(PE)" = "CR",
                             "CR(PEW)" = "CR",
                             # NE is a typo of EN
                             "NE" = "EN",
                             # Old category E -> EN
                             "E" = "EN",
                             # Old category Lower Risk/Near Threatened -> Near Threatened
                             "LR/nt" = "NT",
                             # Old category Lower Risk/Conservation Depdendent -> Near Threatened
                             "LR/cd" = "NT",
                             # Old category Lower Risk/Least Concern -> Least Concern
                             "LR/lc" = "LC",
                             # VY is a typo of VU
                             "VY" = "VU"),
           cat_new  = recode(cat_new,
                             # NR occurs after taxonomy change
                             "NR" = "DD",
                             # Merge possibly extinct into critically endangered
                             "CR (PE)" = "CR",
                             "CR(PE)" = "CR",
                             "CR(PEW)" = "CR",
                             # NE is a typo of EN
                             "NE" = "EN",
                             # Old category E -> EN
                             "E" = "EN",
                             # Old category Lower Risk/Near Threatened -> Near Threatened
                             "LR/nt" = "NT",
                             # Old category Lower Risk/Conservation Depdendent -> Near Threatened
                             "LR/cd" = "NT",
                             # Old category Lower Risk/Least Concern -> Least Concern
                             "LR/lc" = "LC",
                             # VY is a typo of VU
                             "VY" = "VU")) %>% 
    # Relevel
    mutate(cat_old_cat = cat_old,
           cat_new_cat = cat_new,
           cat_old = as.numeric(paste(factor(cat_old, 
                                             levels = c("LC", "NT", 
                                                        "VU", "EN", 
                                                        "CR", "EW", "EX", 
                                                        "DD"),
                                             labels = c(1:6, 6, NA)))),
           cat_new = as.numeric(paste(factor(cat_new, 
                                             levels = c("LC", "NT", 
                                                        "VU", "EN", 
                                                        "CR", "EW", "EX", 
                                                        "DD"),
                                             labels = c(1:6, 6, NA)))),
           # If positive species has improved in status from previous year
           cat_change = cat_new - cat_old,
           cat_change_bi = 
               case_when(cat_change > 0 ~ "decline",
                         cat_change < 0 ~ "improve")) 

# Check scientific and common names
check_names <- function(x){
    string <- unlist(strsplit(x, " "))
    scientific <- paste(string[1:2], collapse = " ")
    if (length(string) > 2) {
        common <- paste(string[3:length(string)], collapse = " ")
    }else common <- NA
    return(tibble(scientific, common))
}

badnames <- 
    map(alldat$scientific, check_names) %>% 
    bind_rows() %>% 
    mutate(i = row.names(.)) %>% 
    filter(!(is.na(common)))

alldat[badnames$i, "scientific"] <- badnames$scientific
alldat[badnames$i, "common"] <- badnames$common

# # Remove species that purport to be a genuine change but where there is no change
# # [either a mistake, or minor change from CR to CR(PE) or EW to EX]
# filter(!(reason == "G" & cat_change == 0))

# Add taxon ID ------------------------------------------------------------

# Get taxon ID for all names and synonyms
synonyms <-
    read.csv("data/iucn_info/synonyms.csv", stringsAsFactors = FALSE) %>% 
    mutate(genusName = trimws(genusName),
           speciesName = trimws(speciesName),
           synonym = paste(genusName, speciesName, sep = " ")) %>% 
    dplyr::select(-c(name, genusName, speciesName, speciesAuthor, 
                     infraType, infrarankAuthor))
spp_names <- 
    synonyms %>% 
    dplyr::select(-synonym) %>% 
    filter(!(duplicated(scientificName))) 
synonyms <-
    synonyms %>% 
    dplyr::select(-scientificName) %>% 
    rename(scientificName = synonym)
spp_names <- rbind(spp_names, synonyms)


#### This section is not necessary for the demo - uncomment if using on an
#### IUCN Red List dataset that you have downloaded yourself

# # Get additional names for species without synonyms
# missing_names <-
#     read.csv("data/iucn_info/taxonomy.csv", stringsAsFactors = FALSE) %>% 
#     dplyr::select(internalTaxonId, scientificName) %>% 
#     filter(!(scientificName %in% spp_names$synonym))
# spp_names <- 
#     rbind(spp_names, missing_names) %>% 
#     arrange(internalTaxonId)
# 
# # Manually check (outside of R) species still missing
# missing_names <- alldat$scientific[!(alldat$scientific %in% spp_names$scientificName)]
# write.csv(missing_names, "data/missing_names.csv", row.names = FALSE)
# missing_names <- read.csv("data/missing_names_sorted.csv")
# synonyms <-
#     missing_names %>%
#     select(synonym, internalTaxonId) %>%
#     rename(scientificName = synonym)
# missing_names <-
#     rbind(dplyr::select(missing_names, scientificName, internalTaxonId),
#           synonyms)
# spp_names <-
#     rbind(spp_names, missing_names) %>%
#     arrange(internalTaxonId) %>%
#     filter(scientificName %in% alldat$scientific,
#            !(duplicated(scientificName)))

# Merge with status change data
alldat <- 
    merge(x = alldat, y = spp_names, 
          by.x = "scientific", by.y = "scientificName", 
          all.x = TRUE, all.y = FALSE)

# Add taxonomic data ------------------------------------------------------

taxonomy <- 
    read.csv("data/iucn_info/taxonomy.csv", stringsAsFactors = FALSE) %>% 
    select(-c("genusName", "speciesName",
              "infraType", "infraName", "infraAuthority", "subpopulationName",
              "authority", "taxonomicNotes"))

# Change to lower case
taxonomy[, c("kingdomName", "phylumName", "orderName", "className", "familyName")] <-
    apply(taxonomy[, c("kingdomName", "phylumName", "orderName", "className", "familyName")],
          MARGIN = 2, tolower)

# Merge with status change data
alldat <- 
    left_join(x = alldat, y = taxonomy, by = "internalTaxonId") %>% 
    filter(!(is.na(internalTaxonId)))
# Check duplicates --------------------------------------------------------

duplicates <- alldat$internalTaxonId[duplicated(alldat$internalTaxonId)]
duplicates <-
    alldat %>% 
    filter(internalTaxonId %in% duplicates) %>% 
    arrange(internalTaxonId, year)

# Look at cases where a genunine change precedes a non-genuine change, 
# with opposing direction of status change
gen_b4_nongen <- function(reason, cat_change){
    # Are there any genuine changes?
    if ("G" %in% reason) {
        # Indices of genuine change
        g_i <- which(reason == "G")
        # Indices of non-genuine chane
        n_i <- which(reason == "N")
        # Do any of the genuine changes precede non-genuine change?
        all_i <- expand.grid(g_i = g_i, n_i = n_i)
        if (any(all_i[, "g_i"] < all_i[, "n_i"])) {
            # Subset to cases where genuine change precedes non-genuine change
            sub_i <- all_i[which(all_i[, "g_i"] < all_i[, "n_i"]),]
            
            # See if any of the instances of genuine changes precede a nongenuine
            # change in the opposing direction
            if (!(any((cat_change[sub_i[,"g_i"]] < 0) == 
                      (cat_change[sub_i[,"n_i"]] < 0)))) {
                return(TRUE)
            } else return(FALSE)
        } else{
            return(FALSE)
        }
    } else return(NA)
}
duplicates_g_n <- 
    group_by(duplicates, internalTaxonId, scientificName) %>% 
    summarise(g_n = gen_b4_nongen(reason = reason, cat_change = cat_change)) %>% 
    filter(isTRUE(g_n))

# Manually investigate (outside of R)
write.csv(duplicates_g_n, "data/duplicates_g_n.csv", row.names = FALSE)

# Remove these from analyses
alldat <- alldat[!(alldat$internalTaxonId %in% duplicates_g_n$internalTaxonId),]
duplicates <- duplicates[!(duplicates$internalTaxonId %in% duplicates_g_n$internalTaxonId),]

# # Hylexetastes uniformis and Hylexetastes brigidai are synonyms of Hylexetastes perrotii
# # -> remove
# alldat <- filter(alldat, scientific != "Hylexetastes uniformis")

# 24 species genuinely changed status twice
#   - most progressive declines
#   - some progressive improvements (Guadalupe Fur Seal, Greater Stick-nest Rat, Przewalski's Horse, Chatham Petrel)
#   - some decline & then improve (St Helena Plover, Floreana Mockingbird)
#   - none improve & then decline (unless the non-genuine improvement of Pteropus niger was actually genuine)

# Write
saveRDS(alldat, "data/status_change.Rds")

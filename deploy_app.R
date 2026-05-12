all_files <- list.files(
  ".",
  all.files = TRUE,
  recursive = TRUE,
  no.. = TRUE
)

exclude <- grepl(
  paste(
    c(
      "^\\.git/",
      "^\\.Rproj\\.user/",
      "^rsconnect/",
      "^cache/",
      "^ndfd_region8/",
      "^r8_forests/",
      "^\\.DS_Store$",
      "\\.grib2$",
      "\\.bin$",
      "\\.tif$",
      "\\.zip$"
    ),
    collapse = "|"
  ),
  all_files
)

deploy_files <- all_files[!exclude]

# keep simplified forest object
deploy_files <- union(
  deploy_files,
  "r8_forests_simplified.rds"
)

deploy_files[grepl("cache|ndfd_region8|grib2|bin|r8_forests", deploy_files)]

rsconnect::deployApp(
  appDir = ".",
  appFiles = deploy_files
)
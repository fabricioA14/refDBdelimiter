
# Function to fetch data from GBIF for a specific continent, year, and observation type
fetch_data <- function(continent, year = NULL, observation_type) {
  start <- 0  # Initialize start as an integer
  continent_data <- list()
  repeat {
    result <- occ_search(
      continent = continent, 
      hasCoordinate = TRUE, 
      taxonKey = taxon_key, 
      basisOfRecord = observation_type, 
      fields = fields, 
      limit = limit, 
      start = start, 
      year = year
    )
    if (is.null(result$data) || nrow(result$data) == 0) break
    
    # Ensure all required columns are present
    missing_cols <- setdiff(fields, names(result$data))
    for (col in missing_cols) {
      result$data[[col]] <- NA
    }
    
    continent_data <- append(continent_data, list(result$data))
    start <- start + limit  # Increment start correctly
  }
  
  if (length(continent_data) > 0) {
    do.call(rbind, continent_data)
  } else {
    data.frame()  # Return an empty data frame if no data was fetched
  }
}

# Function to capitalize the first letter of each string in a vector
capitalize <- function(x) {
  sapply(x, function(y) paste0(toupper(substring(y, 1, 1)), substring(y, 2)), USE.NAMES = FALSE)
}

# Function to make unique names
make_unique_names <- function(names) {
  make.unique(names, sep = " ")
}

# Function to extract the data and create a data frame
refDB_ExtractData <- function(data) {
  # Extract and modify the taxa vector
  taxa <- unlist(lapply(data, function(x) x$taxa))
  taxa <- sub(",.*", "", taxa)
  
  ids <- unlist(lapply(data, function(x) rep(x$layer$properties$`_leaflet_id`, length(x$taxa))))
  feature_types <- unlist(lapply(data, function(x) rep(x$layer$properties$feature_type, length(x$taxa))))
  geometries <- unlist(lapply(data, function(x) {
    coords <- x$layer$geometry$coordinates[[1]]
    replicate(length(x$taxa), st_polygon(list(matrix(unlist(coords), ncol = 2, byrow = TRUE))), simplify = FALSE)
  }), recursive = FALSE)
  
  df <- data.frame(
    leaflet_id = ids,
    feature_type = feature_types,
    stringsAsFactors = FALSE
  )
  
  df <- st_sf(df, geometry = st_sfc(geometries))
  rownames(df) <- make_unique_names(capitalize(as.character(taxa)))
  
  # Set the CRS for the sf object
  st_crs(df) <- 4326  # CRS WGS 84
  
  return(df)
}

# Function to remove trailing numbers from names
remove_trailing_numbers <- function(name) {
  gsub(" [0-9]+$", "", name)
}


# Função para salvar dados de ocorrência nos formatos especificados
refDB_SaveOccurrenceData <- function(data, base_filename, formats = c("shp", "geojson", "gpkg", "kml", "csv")) {
  # Definir o diretório de saída
  current_directory <- getwd()
  output_directory <- file.path(current_directory, "SHP")
  
  # Definir caminhos de saída com base nos formatos desejados
  output_paths <- list(
    shp = file.path(output_directory, paste0(base_filename, ".shp")),
    geojson = paste0(base_filename, ".geojson"),
    gpkg = paste0(base_filename, ".gpkg"),
    kml = paste0(base_filename, ".kml"),
    csv = paste0(base_filename, ".csv")
  )
  
  # Salvar nos formatos especificados
  if ("shp" %in% formats) {
    if (!dir.exists(output_directory)) {
      dir.create(output_directory)
    }
    # Converter gbifID para character para evitar problemas de largura de campo
    data$gbifID <- as.character(data$gbifID)
    st_write(data, output_paths$shp, delete_layer = TRUE)
  }
  if ("geojson" %in% formats) {
    st_write(data, output_paths$geojson, delete_layer = TRUE)
  }
  if ("gpkg" %in% formats) {
    st_write(data, output_paths$gpkg, delete_layer = TRUE)
  }
  if ("kml" %in% formats) {
    st_write(data, output_paths$kml, delete_layer = TRUE)
  }
  if ("csv" %in% formats) {
    output_csv <- st_drop_geometry(data)
    write.table(output_csv, output_paths$csv, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  }
  
  cat("Data saved in formats:", paste(formats, collapse = ", "), "\n")
}

# Function to delete intermediate files
delete_intermediate_files <- function(files) {
  sapply(files, function(file) {
    if (file.exists(file)) {
      file.remove(file)
    }
  })
}

# Function to create interactive stacked bar plot
refDB_CreateStackedBar <- function(data, taxonomic_level, title, legend_title) {
  # Capitalize the first letter of the legend title
  legend_title <- str_to_title(legend_title)
  
  # Replace empty strings and NA in taxonomic_level with "Empty"
  data[[taxonomic_level]][data[[taxonomic_level]] == ""] <- "Empty"
  data[[taxonomic_level]][is.na(data[[taxonomic_level]])] <- "Empty"
  
  # Replace empty strings and NA in year with "Unknown"
  data$year[data$year == ""] <- "Unknown"
  data$year[is.na(data$year)] <- "Unknown"
  
  # Order the taxonomic levels, keeping "Empty" at the end
  levels_ordered <- c("Empty", rev(sort(unique(data[[taxonomic_level]][data[[taxonomic_level]] != "Empty"]))))
  data[[taxonomic_level]] <- factor(data[[taxonomic_level]], levels = levels_ordered)
  
  # Order the years, keeping "Unknown" at the start
  years_ordered <- c("Unknown", sort(unique(data$year[data$year != "Unknown"])))
  data$year <- factor(data$year, levels = years_ordered)
  
  # Define colors, assigning "grey60" to "Empty"
  colors <- c(RColorBrewer::brewer.pal(n = length(levels_ordered) - 1, name = "Set3"))
  
  data %>%
    group_by(year, !!sym(taxonomic_level)) %>%
    summarise(count = n(), .groups = 'drop') %>%
    plot_ly(x = ~year, y = ~count, type = 'bar', color = as.formula(paste0("~", taxonomic_level)), colors = colors) %>%
    layout(
      title = list(text = title, font = list(family = 'Comfortaa', size = 20)),
      barmode = 'stack',
      xaxis = list(title = 'Year', titlefont = list(family = 'Comfortaa', size = 18), tickfont = list(family = 'Comfortaa')),
      yaxis = list(title = 'Count', titlefont = list(family = 'Comfortaa', size = 18), tickfont = list(family = 'Comfortaa')),
      legend = list(
        title = list(
          text = paste0("   ", legend_title),
          font = list(family = 'Comfortaa', size = 14)
        ),
        font = list(family = 'Comfortaa', size = 12),
        orientation = "v"
      ),
      margin = list(t = 80)  # Add top margin for the title
    )
}

refDB_FormatNcbiDatabase <- function(raw_database, database_cleaned, min_sequence_length, pattern = "UNVERIFIED") {
  
  # Filter sequences based on the given pattern
  filter_command <- paste0(
    "wsl awk",
    " -v pattern='" , pattern, "'",
    " -v output_file='" , database_cleaned, "'",
    " '/^>/ { if ($0 !~ pattern) { if (header) { print header; print sequence } header = $0; sequence = \"\" } next } { sequence = sequence $0 } END { if (header) { print header; print sequence } }' ",
    raw_database,
    ">",
    database_cleaned
  )
  system(filter_command)
  
  # Remove sequences below the minimum length
  command <- paste0('wsl temp_database_cleaned=$(mktemp)', 
                    ' && awk -v min_length=', min_sequence_length, 
                    ' -v RS=">" -v ORS="" \'{',
                    ' if (NR > 1) {',
                    '     header = ">" substr($0, 1, index($0, "\\n"));',
                    '     sequence = substr($0, index($0, "\\n")+1);',
                    '     gsub(/\\n/, "", sequence);',
                    '     if (length(sequence) >= min_length) {',
                    '         print header;',
                    '         print sequence;',
                    '         print "\\n";',
                    '     }',
                    ' }',
                    '}\' "', database_cleaned, '" > $temp_database_cleaned && ',
                    'mv $temp_database_cleaned "', database_cleaned, '"')
  system(command)
  
  # Remove special characters
  system(paste0("wsl tr -d '[]\"' < ", database_cleaned, " > temp_database_cleaned && mv temp_database_cleaned ", database_cleaned))
  
  # Select specific taxonomy labels
  system(paste0("wsl awk '/^>/ {
      if ($3 == \"aff\" || $3 == \"aff.\" || $3 == \"cf\" || $3 == \"cf.\" || $3 == \"cv\" || $3 == \"cv.\" || $3 == \"f\" || $3 == \"f.\" || $3 == \"ined\" || $3 == \"ined.\" || $3 == \"p\" || $3 == \"p.\" || $3 == \"sect\" || $3 == \"sect.\" || $3 == \"lat\" || $3 == \"lat.\" || $3 == \"str\" || $3 == \"str.\" || $3 == \"nov\" || $3 == \"nov.\" || $3 == \"syn\" || $3 == \"syn.\" || $3 == \"var\" || $3 == \"var.\") {
          sub(/^>[^ ]* /, \">\")
          print $1 \" \" $2 \" \" $3 \" \"
      }  else {
          sub(/^>[^ ]* /, \">\")
          print $1 \" \" $2 \" \"
      }
  } 
  !/^>/ {print}' ", database_cleaned ," > temp_database_cleaned && mv temp_database_cleaned ", database_cleaned))
  
  system(paste0(
    "wsl awk '",
    "/^>/ { ",
    "gsub(/ aff\\./, \"\"); gsub(/ cf\\./, \"\"); gsub(/ cv\\./, \"\"); gsub(/ f\\./, \"\"); gsub(/ ined\\./, \"\"); gsub(/ p\\./, \"\"); gsub(/ sect\\./, \"\"); gsub(/ lat\\./, \"\"); gsub(/ str\\./, \"\"); gsub(/ nov\\./, \"\"); gsub(/ syn\\./, \"\"); gsub(/ var\\./, \"\"); gsub(/ sp\\./, \"\"); ",
    "gsub(/ aff /, \"\"); gsub(/ cf /, \"\"); gsub(/ cv /, \"\"); gsub(/ f /, \"\"); gsub(/ ined /, \"\"); gsub(/ p /, \"\"); gsub(/ sect /, \"\"); gsub(/ lat /, \"\"); gsub(/ str /, \"\"); gsub(/ nov /, \"\"); gsub(/ syn /, \"\"); gsub(/ var /, \"\"); gsub(/ sp /, \"\"); ",
    "} { print }' ", database_cleaned, " > temp_database_cleaned && mv temp_database_cleaned ", database_cleaned
  ))
  
  system(paste0("sed -i '/^>/ s/[[:space:]]/_/g' ", database_cleaned))
  
  system(paste0("sed -i '/^>.*_$/s/_$//' ", database_cleaned))
  
}

refDB_SubsetNcbiGbif <- function(gbif_database, cleaned_ncbi_database, ncbi_database_based_on_gbif, condition) {
  
  file.create(ncbi_database_based_on_gbif, showWarnings = FALSE)
  fileConn <- file(ncbi_database_based_on_gbif, open = "wt")
  close(fileConn)
  
  if (condition) {
    # If condition is TRUE:
    system(paste0("
      wsl names=$(<", gbif_database ,")

      # Function to split names into chunks
      split_names_into_chunks() {
          local chunk_size=$1
          local num_chunks=$(( (${#names} + $chunk_size - 1) / $chunk_size ))
          local chunks=()
          for (( i = 0; i < $num_chunks; i++ )); do
              local start=$(( $i * $chunk_size ))
              chunks+=( \"$(echo \"${names:$start:$chunk_size}\")\" )
          done
          echo \"${chunks[@]}\"
      }

      # Function to replace spaces with underscores except the last one
      replace_spaces_with_underscores() {
          sed 's/ /_/g'
      }

      # Function to search and append sequences with exact matching based on the first part before the underscore
      search_and_append() {
          local chunk=\"$1\"
          awk -v pattern=\"$chunk\" '
              BEGIN {
                  split(pattern, pat, \"|\")
                  for (p in pat) {
                      counts[pat[p]] = 0
                  }
              }
              /^>/ {
                  header = substr($0, 2)  # Remove the initial >
                  split(header, parts, \"_\")
                  for (p in pat) {
                      if (parts[1] == pat[p]) {
                          counts[parts[1]]++
                          print \">\" counts[parts[1]] \"_\" header
                          getline
                          print
                          break
                      }
                  }
              }' ", cleaned_ncbi_database ," | replace_spaces_with_underscores >> ", ncbi_database_based_on_gbif ,"
      }

      # Split names into chunks of 1000 names each
      chunk_size=1000
      IFS='|' read -ra chunks <<< \"$(split_names_into_chunks $chunk_size)\"

      # Search and append sequences for each chunk
      for chunk in \"${chunks[@]}\"
      do
          search_and_append \"$chunk\"
      done
    "))
    
  } else {
    # if FALSE:
    system(paste0("
      wsl names=$(<", gbif_database ,")

      # Function to split names into chunks
      split_names_into_chunks() {
          local chunk_size=$1
          local num_chunks=$(( (${#names} + $chunk_size - 1) / $chunk_size ))
          local chunks=()
          for (( i = 0; i < $num_chunks; i++ )); do
              local start=$(( $i * $chunk_size ))
              chunks+=( \"$(echo \"${names:$start:$chunk_size}\")\" )
          done
          echo \"${chunks[@]}\"
      }

      # Function to search and append sequences
      search_and_append() {
          local chunk=\"$1\"
          sed -n -E '/^>('\"$chunk\"')$/ {:a;N;/^>/!ba;s/^>[[:space:]]*/>/p}' ", cleaned_ncbi_database ," | awk '/^>/ {sub(/>/, \">\" ++c \"_\")} 1' >> ", ncbi_database_based_on_gbif ,"
      }

      # Replace spaces with underscores in names
      modified_names=$(echo \"$names\" | sed 's/[[:space:]]/_/g')

      # Split modified names into chunks of 1000 names each
      chunk_size=1000
      IFS='|' read -ra chunks <<< \"$(split_names_into_chunks $chunk_size)\"

      # Search and append sequences for each chunk
      for chunk in \"${chunks[@]}\"
      do
          c=0 # Reset counter for each chunk
          search_and_append \"$chunk\"
      done
    "))
  }
  
  # Add the sed command to remove trailing underscores
  system(paste0("sed -i '/^>.*_$/s/_$//' ", ncbi_database_based_on_gbif))
}

refDB_ncbiToMakeblastdb <- function(cleaned_ncbi_database, ncbi_database_based_on_gbif) {
  
  file.create(ncbi_database_based_on_gbif, showWarnings = FALSE)
  fileConn <- file(ncbi_database_based_on_gbif, open = "wt")
  close(fileConn)
  
  system(paste0("
    wsl 

    # Function to replace spaces with underscores
    replace_spaces_with_underscores() {
        sed 's/ /_/g'
    }

    # Function to append unique numbered sequences based on the entire header string
    append_unique_numbered_sequences() {
        awk '
            /^>/ {
                header = substr($0, 2)  # Remove the initial >
                if (!(header in seen)) {
                    seen[header] = 1
                } else {
                    seen[header]++
                }
                print \">\" seen[header] \"_\" header
                getline
                print
            }' ", cleaned_ncbi_database ," | replace_spaces_with_underscores >> ", ncbi_database_based_on_gbif ,"
    }

    # Append unique numbered sequences
    append_unique_numbered_sequences
  "))
  
  # Add the sed command to remove trailing underscores
  system(paste0("sed -i '/^>.*_$/s/_$//' ", ncbi_database_based_on_gbif))
}

refDB_CreateBlastDB <- function(database, parse_seqids = T, database_type = "nucl", title = "local_database", out = NULL, hash_index = FALSE, mask_data = NULL,  mask_id = NULL, mask_desc = NULL, gi_mask = FALSE,
                                gi_mask_name = NULL, max_file_sz = NULL, logfile = NULL, taxid = NULL, taxid_map = NULL) {
  system(paste0("wsl makeblastdb -in " , database, " ", if (parse_seqids) paste0("-parse_seqids"), " -title ", title, " -dbtype ", database_type, " -out ", out, if (hash_index) "-hash_index ",
                if (!is.null(mask_data) && mask_data != "") paste0("-mask_data ", mask_data, " "), if (!is.null(mask_id) && mask_id != "") paste0("-mask_id ", mask_id, " "),
                if (!is.null(mask_desc) && mask_desc != "") paste0("-mask_desc ", mask_desc, " "), if (gi_mask) "-gi_mask ", if (!is.null(gi_mask_name) && gi_mask_name != "") paste0("-gi_mask_name ", gi_mask_name, " "),
                if (!is.null(max_file_sz) && max_file_sz != "") paste0("-max_file_sz ", max_file_sz, " "), if (!is.null(logfile) && logfile != "") paste0("-logfile ", logfile, " "),
                if (!is.null(taxid) && taxid != "") paste0("-taxid ", taxid, " "), if (!is.null(taxid_map) && taxid_map != "") paste0("-taxid_map ", taxid_map, " ")))
}

refDB_Blast <- function(Directory, Database_File, otu_table = "otu_table.txt", query = "otus.fasta", task = "megablast", out = "blast.txt", 
                        max_target_seqs = 50, perc_identity = 95, qcov_hsp_perc = 95, num_threads = 6, 
                        Specie_Threshold = 99, Genus_Threshold = 97, Family_Threshold = 95, 
                        penalty = NULL, reward = NULL, evalue = NULL, word_size = NULL, gapopen = NULL, 
                        gapextend = NULL, max_hsps = NULL, xdrop_ungap = NULL, xdrop_gap = NULL, 
                        xdrop_gap_final = NULL, searchsp = NULL, sum_stats = NULL, no_greedy = NULL, 
                        min_raw_gapped_score = NULL, template_type = NULL, template_length = NULL, 
                        dust = NULL, filtering_db = NULL, window_masker_taxid = NULL, window_masker_db = NULL, 
                        soft_masking = NULL, ungapped = NULL, culling_limit = NULL, 
                        best_hit_overhang = NULL, best_hit_score_edge = NULL, subject_besthit = NULL, 
                        window_size = NULL, off_diagonal_range = NULL, use_index = NULL, index_name = NULL, 
                        lcase_masking = NULL, query_loc = NULL, strand = NULL, parse_deflines = NULL, 
                        outfmt = "6", show_gis = NULL, num_descriptions = NULL, num_alignments = NULL, 
                        line_length = NULL, html = NULL, sorthits = NULL, sorthsps = NULL, 
                        mt_mode = NULL, remote = NULL) {
  
  linux_path <- gsub("C:/", "/mnt/c/", Directory)
  
  # Optional Parameters
  optional_params <- ""
  add_param <- function(param, value) {
    if (!is.null(value)) {
      return(paste0(" -", param, " ", value))
    }
    return("")
  }
  
  optional_params <- paste0(optional_params, 
                            add_param("penalty", penalty), 
                            add_param("reward", reward), 
                            add_param("evalue", evalue),
                            add_param("word_size", word_size),
                            add_param("gapopen", gapopen),
                            add_param("gapextend", gapextend),
                            add_param("max_hsps", max_hsps),
                            add_param("xdrop_ungap", xdrop_ungap),
                            add_param("xdrop_gap", xdrop_gap),
                            add_param("xdrop_gap_final", xdrop_gap_final),
                            add_param("searchsp", searchsp),
                            add_param("sum_stats", sum_stats),
                            add_param("no_greedy", no_greedy),
                            add_param("min_raw_gapped_score", min_raw_gapped_score),
                            add_param("template_type", template_type),
                            add_param("template_length", template_length),
                            add_param("dust", dust),
                            add_param("filtering_db", filtering_db),
                            add_param("window_masker_taxid", window_masker_taxid),
                            add_param("window_masker_db", window_masker_db),
                            add_param("soft_masking", soft_masking),
                            add_param("ungapped", ungapped),
                            add_param("culling_limit", culling_limit),
                            add_param("best_hit_overhang", best_hit_overhang),
                            add_param("best_hit_score_edge", best_hit_score_edge),
                            add_param("subject_besthit", subject_besthit),
                            add_param("window_size", window_size),
                            add_param("off_diagonal_range", off_diagonal_range),
                            add_param("use_index", use_index),
                            add_param("index_name", index_name),
                            add_param("lcase_masking", lcase_masking),
                            add_param("query_loc", query_loc),
                            add_param("strand", strand),
                            add_param("parse_deflines", parse_deflines),
                            add_param("outfmt", outfmt),
                            add_param("show_gis", show_gis),
                            add_param("num_descriptions", num_descriptions),
                            add_param("num_alignments", num_alignments),
                            add_param("line_length", line_length),
                            add_param("html", html),
                            add_param("sorthits", sorthits),
                            add_param("sorthsps", sorthsps),
                            add_param("mt_mode", mt_mode),
                            add_param("remote", remote)
  )
  
  # blastn
  system(paste0("wsl blastn -query ", paste0(linux_path, query), " -task ", task, " -db ", paste0(linux_path, Database_File), 
                " -out ", out, " -max_target_seqs ", max_target_seqs, 
                " -perc_identity ", perc_identity, 
                " -qcov_hsp_perc ", qcov_hsp_perc, 
                " -num_threads ", num_threads, 
                optional_params))
  
  system(paste0("wsl tr -d '#' < ", otu_table, " > temp_raw_database && mv temp_raw_database ", otu_table))
  
  csv1 <- read.table(paste0(Directory, out), sep = "", header = FALSE)
  
  Samples <- read.table(paste0(Directory, otu_table), sep = "\t", header = TRUE)
  
  colnames(Samples)[1] <- "qseqid"
  
  ########## Files = csv + ID taxa ##########
  colnames(csv1) <- c("qseqid", "seqid", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore")
  
  # Separate only the max value of match per ZOTU
  cl.max_ <- csv1 %>% group_by(qseqid) %>% top_n(1, pident)
  
  # Organizing the species names
  cl.max_$seqid <- gsub("\\d+_", "", cl.max_$seqid)
  cl.max_$seqid <- gsub("_", " ", cl.max_$seqid)
  cl.max_$seqid <- sub("\\b(sp\\.?\\s+).*", "\\1", cl.max_$seqid)
  
  # Split each cell into "characters" separated by blank spaces
  split_seqid <- strsplit(cl.max_$seqid, "\\s+")
  
  # Exclude characters after the second "character" of each cell
  cl.max_$seqid <- sapply(split_seqid, function(chars) {
    paste(chars[1:2], collapse = " ")
  })
  
  # Separate only the min value of match per OTU - Evalue 
  cl.max_ <- cl.max_ %>% group_by(qseqid) %>% top_n(-1, evalue)
  
  # Separate only the min value of match per OTU - Bitscore 
  cl.max_ <- cl.max_ %>% group_by(qseqid) %>% top_n(-1, bitscore)
  
  cl.max_ <- cl.max_[!duplicated(cl.max_[1]), ]
  
  ids <- paste(cl.max_$qseqid, cl.max_$seqid, sep = " ")
  
  # data source: NCBI
  db_download_ncbi()
  src <- src_ncbi()
  
  taxa_to_id <- name2taxid(sapply(strsplit(ids, " "), function(x) paste(x[2:3], collapse = " ")))
  
  taxa_names <- classification(taxa_to_id, db = "ncbi")
  
  # Function to check if an object is a DataFrame
  is_dataframe <- function(obj) {
    inherits(obj, "data.frame")
  }
  
  # Iterate through each sublist
  for (i in seq_along(taxa_names)) {
    # If it's not a DataFrame, create a new DataFrame with NA values
    if (!is_dataframe(taxa_names[[i]])) {
      taxa_names[[i]] <- data.frame(name = NA, rank = NA, id = NA)
    }
  }
  
  # Replace names based on the vector
  names(taxa_names) <- ids
  
  taxa_names_df <- do.call(rbind, lapply(names(taxa_names), function(name) {
    df <- taxa_names[[name]]
    df$Taxa <- name
    return(df)
  }))
  
  sum(grepl("phylum", taxa_names_df$rank, ignore.case = TRUE))
  
  taxa_names_df <- taxa_names_df[, c("Taxa", names(taxa_names_df)[1:(ncol(taxa_names_df) - 1)])] 
  taxa_names_df <- taxa_names_df[, -ncol(taxa_names_df)]
  
  taxa_names_df <- taxa_names_df %>%
    filter(rank %in% c("phylum", "class", "order", "family", "genus", "species", NA)) %>%
    group_by(Taxa) %>%
    distinct()
  
  taxa_remaining <- taxa_names_df$Taxa[is.na(taxa_names_df$rank)]
  
  # Check if taxa_remaining is not empty
  if (length(taxa_remaining) > 0) {
    # If taxa_remaining is not empty, run this block
    taxa_with_na <- sapply(strsplit(taxa_remaining, " "), "[", 2)
    taxa_with_na <- data.frame(name = taxa_with_na)
    row.names(taxa_with_na) <- taxa_remaining
    
    # Add the ID column to taxa_with_na based on matching names
    taxa_with_na_rank <- name2taxid(taxa_with_na$name, out_type = "summary")
    taxa_with_na_with_id <- left_join(taxa_with_na, taxa_with_na_rank, by = "name")
    row.names(taxa_with_na_with_id) <- taxa_remaining
    
    # Match the values in taxa with the values in df$name
    remaining_taxa <- classification(taxa_with_na_with_id$id, db = "ncbi")
    names(remaining_taxa) <- taxa_remaining
    
    # Iterate through each sublist
    for (i in seq_along(remaining_taxa)) {
      # If it's not a DataFrame, create a new DataFrame with NA values
      if (!is_dataframe(remaining_taxa[[i]])) {
        remaining_taxa[[i]] <- data.frame(name = NA, rank = NA, id = NA)
      }
    }
    
    taxa_names_df_rem <- do.call(rbind, lapply(names(remaining_taxa), function(name) {
      df <- remaining_taxa[[name]]
      df$Taxa <- name
      return(df)
    }))
    
    taxa_names_df_rem <- taxa_names_df_rem[, c("Taxa", names(taxa_names_df_rem)[1:(ncol(taxa_names_df_rem) - 1)])]
    taxa_names_df_rem <- taxa_names_df_rem[, -ncol(taxa_names_df_rem)]
    
    taxa_names_df_rem <- taxa_names_df_rem %>%
      filter(rank %in% c("phylum", "class", "order", "family", "genus", "species", NA)) %>%
      group_by(Taxa) %>%
      distinct()
    
    taxa_names_df <- taxa_names_df[complete.cases(taxa_names_df), ]
    
    # Combine the dataframes using rbind
    all_taxa <- data.frame(rbind(taxa_names_df, taxa_names_df_rem))
    
    identified_otus <- all_taxa %>% 
      pivot_wider(names_from = rank, values_from = name)
    
  } else {
    # If taxa_remaining is empty, run this block
    taxa_names_df <- taxa_names_df[complete.cases(taxa_names_df), ]
    
    identified_otus <- taxa_names_df %>%
      pivot_wider(names_from = rank, values_from = name)
  }
  
  # Split the content based on the first blank space
  split_content <- str_split(identified_otus$Taxa, " ", n = 2)
  
  # Extract the first part (characters before the first blank space)
  first_part <- sapply(split_content, function(x) x[1])
  
  # Extract the second part (characters after the first blank space)
  second_part <- sapply(split_content, function(x) ifelse(length(x) > 1, x[2], ""))
  
  # Create new columns in the dataframe for the split content
  identified_otus <- cbind(qseqid = first_part, seqid = second_part, identified_otus[2:ncol(identified_otus)])
  
  colnames(identified_otus) <- c("qseqid", "seqid", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  
  cl.max <- cbind(identified_otus[, 1:8], cl.max_[, 3])
  
  ########## File = Samples ##########
  
  Samples <- Samples[order(match(Samples$qseqid, cl.max$qseqid)), ]
  
  Final_Match_samples <- plyr::match_df(Samples, cl.max, on = "qseqid")
  
  Blast <- cbind(cl.max, Final_Match_samples[, 2:length(colnames(Final_Match_samples))])
  
  Blast$Species <- ifelse(Blast$pident >= Genus_Threshold & Blast$pident < Specie_Threshold, "" ,Blast$Species)
  Blast$Species <- ifelse(Blast$pident >= Family_Threshold & Blast$pident < Genus_Threshold, "" ,Blast$Species)
  Blast$Genus <- ifelse(Blast$pident >= Family_Threshold & Blast$pident < Genus_Threshold, "" ,Blast$Genus)
  
  write.table(Blast, file = paste0(Directory, "taxonomic_assignment.txt"), sep = "\t", row.names = F, col.names = T)
  
  # Find the Zotus not identified with perc_identity = 97
  remaining_sequences <- subset(Samples$qseqid, !(Samples$qseqid %in% unique(csv1$qseqid)))
  
  if (length(remaining_sequences) != 0) { 
    # Create a file with a list of Zotus not identified to first threshold of perc_identity
    write.table(remaining_sequences, file = paste0(Directory, "remaining_sequences.txt"), sep = " ", row.names = F, col.names = F, quote = F)
    print("End of Run")
  } else {
    print("End of Run")
  }
}

refDBdelimiter <- function(run_in_browser = FALSE) {

# List of packages to ensure are installed and loaded
pack <- c('tibble', 'rgbif', 'sf', 'concaveman', 'ggplot2', 'rnaturalearth', 'rnaturalearthdata', 'leaflet',
          'mapedit', 'leaflet.extras2', 'dplyr', 'RColorBrewer', 'leaflet.extras', 'shiny', 'htmlwidgets',
          'tidyr', 'retry', 'openxlsx', 'httr', 'jsonlite', 'bdc', 'tools', 'countrycode', 'data.table', 'stringr',
          'plotly', 'shinyFiles', 'shinyjs', 'taxize', 'taxizedb', 'base64enc')

# Check for packages that are not installed
vars <- pack[!(pack %in% installed.packages()[, "Package"])]

# Install any packages that are not already installed
if (length(vars) != 0) {
  install.packages(vars, dependencies = TRUE)
}

# Load all the packages
sapply(pack, require, character.only = TRUE)

rm(vars, pack)
gc()

scrollable_legend_css <- "
.info.legend {
  max-height: calc(93vh - 93px); /* Adjust the height as needed */
  overflow-y: auto;
}
"

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Comfortaa:wght@700&display=swap"),
    tags$style(HTML("
      body, .container-fluid {
        background-color: white;
        color: black;
        font-family: 'Comfortaa', sans-serif;
        font-weight: bold;
      }
      .well {
        background-color: white;
        border-color: black;
      }
      .nav-tabs > li > a {
        color: black;
        font-family: 'Comfortaa', sans-serif;
        font-weight: bold;
      }
      .nav-tabs > li > a:hover, .nav-tabs > li > a:focus, .nav-tabs > .active > a, .nav-tabs > .active > a:hover, .nav-tabs > .active > a:focus {
        background-color: white;
        color: black;
        font-family: 'Comfortaa', sans-serif;
        font-weight: bold;
      }
      h3, p {
        font-family: 'Comfortaa', sans-serif;
        font-weight: bold;
      }
      .title-panel {
        position: relative;
        text-align: center;
        width: 100%;
      }
      .sidebar {
        background-color: white;
        border-color: black;
        height: 90vh;
        overflow-y: auto;
      }
      .btn-file, .btn-primary {
        background-color: #337ab7;
        color: white;
        border-color: #2e6da4;
      }
      .btn-file.selected, #run.selected, #run_tax.selected, #run_space.selected, #run_time.selected, #run_map.selected, #run_edit_map.selected, #save_data.selected {
        background-color: #337ab7;
        color: white;
        border-color: #2e6da4;
      }
      .shiny-input-container {
        margin-top: 15px;
      }
      .output-tab {
        position: relative;
      }
      .output-tab::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-size: cover;
        background-repeat: no-repeat;
        background-position: center;
        opacity: 0.15;
        z-index: -1;
      }
    ")),
    tags$style(type = "text/css", "#map {height: calc(90vh - 20px) !important;}"),
    tags$style(type = "text/css", "#edit_map {height: calc(90vh - 20px) !important;}"),
    tags$style(HTML(scrollable_legend_css))
  ),
  
  div(class = "title-panel", 
      tags$div(
        tags$img(src = "data:image/png;base64,", height = "80px", style = "opacity: 0.12; display: inline-block; vertical-align: middle;", id = "image_base64")
      ),
      tags$div(
        "refDBdelimiter",
        style = "position: absolute; top: 20px; left: 50%; transform: translateX(-50%); font-size: 30px; font-weight: bold; color: black; z-index: 1000; background-color: rgba(255, 255, 255, 0.5); padding: 5px; border-radius: 5px;"
      )
  ),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Pre-Treatment Process",
                           textInput("input_file", "Input File Path:", value = ""),
                           selectInput("nrows_select", "Number of rows to read:", 
                                       choices = c("All" = "All", "Specific number" = "Specific"), 
                                       selected = "All"),
                           conditionalPanel(
                             condition = "input.nrows_select == 'Specific'",
                             numericInput("nrows", "Specify number of rows:", value = 1000, min = 1)
                           ),
                           selectInput("additional_fields", "Additional Columns to import:",
                                       choices = c("datasetKey", "occurrenceID", "infraspecificEpithet", "taxonRank", 
                                                   "verbatimScientificName", "verbatimScientificNameAuthorship", "occurrenceStatus", 
                                                   "individualCount", "publishingOrgKey", "coordinateUncertaintyInMeters", 
                                                   "coordinatePrecision", "elevation", "elevationAccuracy", "depth", "depthAccuracy", 
                                                   "eventDate", "day", "month", "taxonKey", "speciesKey", "institutionCode", 
                                                   "collectionCode", "catalogNumber", "recordNumber", "identifiedBy", "dateIdentified", 
                                                   "license", "rightsHolder", "recordedBy", "typeStatus", "establishmentMeans", 
                                                   "lastInterpreted", "mediaType", "issue"),
                                       multiple = TRUE),
                           selectInput("taxonomic_level_pre", "Select Taxonomic Level for Visualization:", 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "family"),
                           textInput("save_path_pre", "Save Path for Pre-Treatment:", "1_bdc_PreProcess_cleaned"),
                           sliderInput("dist", "Distance for Inconsistent Coordinates (decimal degrees):",
                                       min = 0.01, max = 1.0, value = 0.1, step = 0.01),
                           checkboxInput("save_outputs", "Save Coordinates Output", FALSE),
                           checkboxGroupInput("formats", "Select Output Formats:",
                                              choices = list("shp" = "shp",
                                                             "geojson" = "geojson",
                                                             "gpkg" = "gpkg",
                                                             "kml" = "kml",
                                                             "csv" = "csv"),
                                              selected = c("")),
                           actionButton("run", "Run Pre-Treatment", class = "btn-primary")
                  ),
                  tabPanel("Taxonomy Process",
                           checkboxInput("replace_synonyms", "Replace synonyms by accepted names", TRUE),
                           checkboxInput("suggest_names", "Suggest names for misspelled names", TRUE),
                           sliderInput("suggestion_distance", "Distance between the searched and suggested names:", min = 0, max = 1, value = 0.9, step = 0.01),
                           textInput("db", "Taxonomic Database:", value = "gbif"),
                           textInput("rank_name", "Taxonomic Rank Name:", value = "Chordata"),
                           #textInput("rank", "Taxonomic Rank:", value = "Phylum"),
                           selectInput("rank", "Taxonomic Rank:", 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "phylum"),
                           selectInput("taxonomic_level_tax", "Select Taxonomic Level for Visualization:", 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "family"),
                           checkboxInput("parallel", "Use parallel processing?", TRUE),
                           numericInput("ncores", "Number of cores:", value = 4, min = 1),
                           checkboxInput("export_accepted", "Export accepted names?", FALSE),
                           textInput("save_path_tax", "Save Path for Taxonomy:", "2_bdc_taxonomy_cleaned"),
                           checkboxGroupInput("formats_tax", "Select Output Formats:",
                                              choices = list("shp" = "shp",
                                                             "geojson" = "geojson",
                                                             "gpkg" = "gpkg",
                                                             "kml" = "kml",
                                                             "csv" = "csv"),
                                              selected = c("")),
                           actionButton("run_tax", "Run Taxonomy Process", class = "btn-primary")
                  ),
                  tabPanel("Space Process",
                           numericInput("ndec", "Number of decimals to be tested:", value = 3, min = 1),
                           selectInput("clustering_level", "Clustering Based on:",
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "species"),
                           checkboxGroupInput("tests", "Select spatial tests:",
                                              choices = list(
                                                "capitals" = "capitals",
                                                "centroids" = "centroids",
                                                "duplicates" = "duplicates",
                                                "equal" = "equal",
                                                "gbif" = "gbif",
                                                "institutions" = "institutions",
                                                "outliers" = "outliers",
                                                "zeros" = "zeros",
                                                "urban" = "urban"
                                              ),
                                              selected = c("capitals", "centroids", "duplicates", "equal", "gbif", "institutions", "zeros")),
                           numericInput("capitals_rad", "Radius for capitals (meters):", value = 3000, min = 0),
                           numericInput("centroids_rad", "Radius for centroids (meters):", value = 10000, min = 0),
                           selectInput("centroids_detail", "Detail level for centroids:", choices = c("both", "country", "province"), selected = "both"),
                           numericInput("inst_rad", "Radius around biodiversity institutions coordinates (meters):", value = 100, min = 0),
                           numericInput("outliers_mtp", "Multiplicative factor for outliers:", value = 5, min = 0),
                           numericInput("outliers_td", "Minimum distance of a record to all other records of a species to be identified as outlier (Km):", value = 1000, min = 0),
                           numericInput("outliers_size", "Minimum number of records in a dataset to run the taxon-specific outlier test:", value = 10, min = 0),
                           numericInput("range_rad", "Range radius:", value = 0, min = 0),
                           numericInput("zeros_rad", "Radius for zeros (decimal degrees):", value = 0.5),
                           selectInput("taxonomic_level_space", "Select Taxonomic Level for Visualization:", 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "family"),
                           textInput("save_path_space", "Save Path for Space:", "3_bdc_space_cleaned"),
                           checkboxGroupInput("formats_space", "Select Output Formats:",
                                              choices = list("shp" = "shp",
                                                             "geojson" = "geojson",
                                                             "gpkg" = "gpkg",
                                                             "kml" = "kml",
                                                             "csv" = "csv"),
                                              selected = c("")),
                           actionButton("run_space", "Run Space Process", class = "btn-primary")
                  ),
                  tabPanel("Time Process",
                           numericInput("year_threshold", "Year threshold:", value = 1950, min = 0),
                           selectInput("taxonomic_level_time", "Select Taxonomic Level for Visualization:", 
                                       choices = list("Phylum" = "phylum",
                                                      "Class" = "class",
                                                      "Order" = "order",
                                                      "Family" = "family",
                                                      "Genus" = "genus",
                                                      "Species" = "species"),
                                       selected = "family"),
                           textInput("save_path_time", "Save Path for Time:", "4_bdc_time_cleaned"),
                           checkboxGroupInput("formats_time", "Select Output Formats:",
                                              choices = list("shp" = "shp",
                                                             "geojson" = "geojson",
                                                             "gpkg" = "gpkg",
                                                             "kml" = "kml",
                                                             "csv" = "csv"),
                                              selected = c("")),
                           actionButton("run_time", "Run Time Process", class = "btn-primary")
                  ),
                  tabPanel("Edit Map",
                           checkboxInput("database_condition", "Are you building a Metabarcoding Database?", TRUE), # Adicionado aqui
                           textInput("shp_path", "Enter Shapefile Path", value = ""),
                           actionButton("load_shp", "Load Shapefile", class = "btn-primary"),
                           actionButton("run_edit_map", "Generate Edit Map", class = "btn-primary"),
                           leafletOutput("edit_map"),
                           verbatimTextOutput("searchedValuesOutput_edit")
                  ),
                  tabPanel("Save Data",
                           checkboxInput("condition", "Genus Flexibility:", TRUE),
                           radioButtons("save_option", "Save Data:",
                                        choices = c("selected_occurrence_data", "excluded_occurrence_data", "both")),
                           textInput("save_path_selected", "Save Path for Selected Data:", "selected_data"),
                           textInput("save_path_excluded", "Save Path for Excluded Data:", "excluded_data"),
                           checkboxGroupInput("formats_edit", "Select Output Formats:",
                                              choices = list("shp" = "shp",
                                                             "geojson" = "geojson",
                                                             "gpkg" = "gpkg",
                                                             "kml" = "kml",
                                                             "csv" = "csv"),
                                              selected = c("")),
                           actionButton("save_data", "Save Data", class = "btn-primary")
                  ),
                  tabPanel("Make Database",
                           checkboxInput("condition", "Genus Flexibility:", TRUE),
                           textInput("raw_database", "Raw Database:", value = "ncbiChordata.fasta"),
                           textInput("gbif_database", "GBIF Database:", value = ""),
                           textInput("final_output_database", "Output Database:", value = "Chordata_Ncbi_Gbif.fasta"),
                           checkboxInput("pattern_unverified", "Exclude UNVERIFIED Sequences", value = TRUE),
                           numericInput("min_sequence_length", "Minimum Sequence Length:", value = 100, min = 1),
                           checkboxInput("parse_seqids", "Parse SeqIDs:", TRUE),
                           selectInput("database_type", "Database Type:", choices = c("nucl", "prot"), selected = "nucl"),
                           textInput("title", "Title:", value = "local_database"),
                           textInput("out", "Database Name (out)", value = ""),
                           checkboxInput("hash_index", "Hash Index", FALSE),
                           textInput("mask_data", "Mask Data", value = ""),
                           textInput("mask_id", "Mask ID", value = ""),
                           textInput("mask_desc", "Mask Description", value = ""),
                           checkboxInput("gi_mask", "GI Mask", FALSE),
                           textInput("gi_mask_name", "GI Mask Name", value = ""),
                           textInput("max_file_sz", "Max File Size", value = ""),
                           textInput("logfile", "Log File", value = ""),
                           textInput("taxid", "TaxID", value = ""),
                           textInput("taxid_map", "TaxID Map File", value = ""),
                           actionButton("run_make_database", "Run Make Database", class = "btn-primary")
                  ),
                  tabPanel("Taxonomic Assignment",
                           textInput("directory", "Directory:", value = ""),
                           textInput("database_file", "Database File:", value = ""),
                           textInput("query", "Query File:", value = "otus.fasta"),  
                           textInput("task", "Task:", value = "megablast"),          
                           textInput("out", "Output File:", value = "blast.txt"),    
                           numericInput("max_target_seqs", "Max Target Seqs:", value = 50, min = 1),
                           sliderInput("perc_identity", "Percentage Identity:", min = 0, max = 100, value = 95, step = 0.5),
                           sliderInput("qcov_hsp_perc", "Query Coverage HSP Percentage:", min = 0, max = 100, value = 95, step = 0.5),
                           sliderInput("specie_threshold", "Specie Threshold:", min = 0, max = 100, value = 99, step = 0.5),
                           sliderInput("genus_threshold", "Genus Threshold:", min = 0, max = 100, value = 97, step = 0.5),
                           sliderInput("family_threshold", "Family Threshold:", min = 0, max = 100, value = 95, step = 0.5),
                           numericInput("num_threads", "Number of Threads:", value = 6, min = 1),
                           numericInput("penalty", "Penalty:", value = NA, min = -100, max = 0, step = 1), 
                           numericInput("reward", "Reward:", value = NA, min = 0, max = 100, step = 1),   
                           numericInput("evalue", "E-value:", value = NA, min = 0),                      
                           numericInput("word_size", "Word Size:", value = NA, min = 1),                 
                           numericInput("gapopen", "Gap Open Penalty:", value = NA, min = 0),             
                           numericInput("gapextend", "Gap Extend Penalty:", value = NA, min = 0),         
                           numericInput("max_hsps", "Max HSPs:", value = NA, min = 1),                    
                           numericInput("xdrop_ungap", "Xdrop Ungap:", value = NA, min = 0),             
                           numericInput("xdrop_gap", "Xdrop Gap:", value = NA, min = 0),                  
                           numericInput("xdrop_gap_final", "Xdrop Gap Final:", value = NA, min = 0),      
                           numericInput("searchsp", "Search Space:", value = NA, min = 0),                
                           numericInput("sum_stats", "Sum Stats:", value = NA),                          
                           numericInput("no_greedy", "No Greedy:", value = NA),                          
                           numericInput("min_raw_gapped_score", "Min Raw Gapped Score:", value = NA, min = 0), 
                           textInput("template_type", "Template Type:", value = ""),                      
                           numericInput("template_length", "Template Length:", value = NA, min = 0),      
                           textInput("dust", "DUST Options:", value = ""),                               
                           textInput("filtering_db", "Filtering DB:", value = ""),                        
                           textInput("window_masker_taxid", "Window Masker Taxid:", value = ""),          
                           textInput("window_masker_db", "Window Masker DB:", value = ""),                
                           numericInput("soft_masking", "Soft Masking:", value = NA),                     
                           numericInput("ungapped", "Ungapped:", value = NA),                             
                           numericInput("culling_limit", "Culling Limit:", value = NA, min = 1),          
                           numericInput("best_hit_overhang", "Best Hit Overhang:", value = NA, min = 0),  
                           numericInput("best_hit_score_edge", "Best Hit Score Edge:", value = NA, min = 0), 
                           numericInput("subject_besthit", "Subject Besthit:", value = NA),               
                           numericInput("window_size", "Window Size:", value = NA, min = 1),              
                           numericInput("off_diagonal_range", "Off Diagonal Range:", value = NA, min = 0),
                           numericInput("use_index", "Use Index:", value = NA),                           
                           textInput("index_name", "Index Name:", value = ""),                            
                           numericInput("lcase_masking", "Lcase Masking:", value = NA),                   
                           textInput("query_loc", "Query Loc:", value = ""),                              
                           textInput("strand", "Strand:", value = ""),                                   
                           numericInput("parse_deflines", "Parse Deflines:", value = NA),                
                           numericInput("outfmt", "Output Format:", value = 6),                           
                           numericInput("show_gis", "Show GIS:", value = NA),                            
                           numericInput("num_descriptions", "Num Descriptions:", value = NA, min = 1),    
                           numericInput("num_alignments", "Num Alignments:", value = NA, min = 1),       
                           numericInput("line_length", "Line Length:", value = NA, min = 1),              
                           numericInput("html", "HTML:", value = NA),                                   
                           textInput("sorthits", "Sort Hits:", value = ""),                               
                           textInput("sorthsps", "Sort HSPs:", value = ""),                              
                           numericInput("mt_mode", "MT Mode:", value = NA, min = 0),                   
                           numericInput("remote", "Remote:", value = NA),                                 
                           actionButton("run_blast", "Run BLAST", class = "btn-primary")                  
                  )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Pre-Treatment Output",
                 div(class = "output-tab",
                     plotlyOutput("pre_input_plot"),
                     plotlyOutput("pre_output_plot")
                 )
        ),
        tabPanel("Taxonomy Output",
                 div(class = "output-tab",
                     plotlyOutput("tax_input_plot"),
                     plotlyOutput("tax_output_plot")
                 )
        ),
        tabPanel("Space Output",
                 div(class = "output-tab",
                     plotlyOutput("space_input_plot"),
                     plotlyOutput("space_output_plot")
                 )
        ),
        tabPanel("Time Output",
                 div(class = "output-tab",
                     plotlyOutput("time_input_plot"),
                     plotlyOutput("time_output_plot")
                 )
        ),
        tabPanel("Edit Map Output",
                 div(class = "output-tab",
                     leafletOutput("edit_map")
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Reactive variables to store processed data
  pre_filtered_data <- reactiveVal(NULL)
  taxonomy_cleaned <- reactiveVal(NULL)
  space_cleaned <- reactiveVal(NULL)
  time_cleaned <- reactiveVal(NULL)
  selected_occurrence_data <- reactiveVal(NULL)
  excluded_occurrence_data <- reactiveVal(NULL)
  drawn_features <- reactiveVal(NULL)
  polygon_data <- reactiveVal(NULL)
  
  observe({
    #img_path <- "C:/Users/fabricio/Desktop/fabricioA14/refDBdelimiter/www/refdb.png"
    img_path <- system.file("www", "refdb.png", package = package_name)
    if (file.exists(img_path)) {
      img_base64 <- base64enc::dataURI(file = img_path, mime = "image/png")
      runjs(sprintf('document.getElementById("image_base64").src = "%s";', img_base64))
    } else {
      showNotification("Image not found", type = "error")
    }
  })
  
  # Pre-Treatment Process
  observeEvent(input$run, {
    if (input$input_file == "")
      return(NULL)
    
    runjs("$('#run').addClass('selected');")
    
    nrows <- if (input$nrows_select == "All") Inf else as.numeric(input$nrows)
    default_fields <- c("gbifID", "kingdom", "phylum", "class", "order", "family", "genus", "species", 
                        "scientificName", "countryCode", "stateProvince", "locality", "decimalLongitude", 
                        "decimalLatitude", "basisOfRecord", "year")
    additional_fields <- input$additional_fields
    fields <- c(default_fields, additional_fields)
    
    data <- fread(input$input_file, select = fields, nrows = nrows)
    
    dataPreProcess <- bdc_scientificName_empty(data, "scientificName") %>%
      bdc_coordinates_empty(lat = "decimalLatitude", lon = "decimalLongitude") %>%
      bdc_coordinates_outOfRange(lat = "decimalLatitude", lon = "decimalLongitude") %>%
      bdc_basisOfRecords_notStandard(basisOfRecord = "basisOfRecord", names_to_keep = "all")
    
    unique_country_codes <- unique(dataPreProcess$countryCode)
    country_names <- countrycode(unique_country_codes, origin = "iso2c", destination = "country.name")
    nome_map <- setNames(country_names, unique_country_codes)
    
    dataPreProcess <- dataPreProcess %>%
      mutate(country = nome_map[countryCode]) %>%
      select(gbifID, scientificName, countryCode, country, everything()) %>%
      bdc_country_from_coordinates(lat = "decimalLatitude", lon = "decimalLongitude", country = "country")
    
    cntr <- dataPreProcess$country %>%
      unique() %>%
      na.omit() %>%
      c()
    
    dataPreProcess <- bdc_coordinates_country_inconsistent(
      data = dataPreProcess,
      country = "country",
      country_name = cntr,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      dist = input$dist
    )
    
    xyFromLocality <- bdc_coordinates_from_locality(
      data = dataPreProcess,
      locality = "locality",
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      save_outputs = input$save_outputs
    )
    
    xyFromLocality[, c("country", "stateProvince", "locality")]
    
    dataPreProcess <- bdc_summary_col(data = dataPreProcess)
    
    pre_filtered <- dataPreProcess %>%
      dplyr::filter(.summary == TRUE) %>%
      bdc_filter_out_flags(data = ., col_to_remove = "all")
    
    pre_filtered_data(pre_filtered)  # Update reactive variable
    
    refDB_SaveOccurrenceData(pre_filtered, input$save_path_pre, formats = input$formats)
    
    output$pre_input_plot <- renderPlotly({
      refDB_CreateStackedBar(data, input$taxonomic_level_pre, "Raw Dataset by Year and Taxonomic Level", input$taxonomic_level_pre)
    })
    
    output$pre_output_plot <- renderPlotly({
      refDB_CreateStackedBar(pre_filtered, input$taxonomic_level_pre, "Pre-Treated Dataset by Year and Taxonomic Level", input$taxonomic_level_pre)
    })
  })
  
  rm(data, dataPreProcess, xyFromLocality)
  gc()
  
  # Taxonomy Process
  observeEvent(input$run_tax, {
    pre_filtered <- pre_filtered_data()
    if (is.null(pre_filtered))
      return(NULL)
    
    runjs("$('#run_tax').addClass('selected');")
    
    rank_lower <- tolower(input$rank)
    
    parse_names <- bdc_clean_names(
      sci_names = unique(pre_filtered$scientificName),
      save_outputs = FALSE
    )
    
    check_taxonomy <- dplyr::left_join(pre_filtered, parse_names, by = "scientificName")
    
    spl <- unique(check_taxonomy$names_clean) %>% sort()
    
    query_names <- bdc_query_names_taxadb(
      sci_name = spl,
      replace_synonyms = input$replace_synonyms,
      suggest_names = input$suggest_names,
      suggestion_distance = input$suggestion_distance,
      db = input$db,
      rank_name = input$rank_name,
      rank = rank_lower,
      parallel = input$parallel,
      ncores = input$ncores,
      export_accepted = input$export_accepted
    )
    
    nome_map <- setNames(query_names$scientificName, query_names$original_search)
    
    check_taxonomy$scientificName_updated <- nome_map[check_taxonomy$names_clean]
    
    check_taxonomy <- check_taxonomy %>%
      mutate(scientificName_updated = if_else(is.na(scientificName_updated), names_clean, scientificName_updated))
    
    taxonomy_cleaned_data <- check_taxonomy %>% select_if(~ !all(is.na(.)))
    
    taxonomy_cleaned(taxonomy_cleaned_data)  # Update reactive variable
    
    refDB_SaveOccurrenceData(taxonomy_cleaned_data, input$save_path_tax, formats = input$formats_tax)
    
    output$tax_input_plot <- renderPlotly({
      refDB_CreateStackedBar(pre_filtered, input$taxonomic_level_tax, "Pre-Treated Dataset by Year and Taxonomic Level", input$taxonomic_level_tax)
    })
    
    output$tax_output_plot <- renderPlotly({
      refDB_CreateStackedBar(taxonomy_cleaned_data, input$taxonomic_level_tax, "Taxonomy-Cleaned Dataset by Year and Taxonomic Level", input$taxonomic_level_tax)
    })
  })
  
  rm(pre_filtered)
  gc()
  
  # Space Process
  observeEvent(input$run_space, {
    taxonomy_cleaned_data <- taxonomy_cleaned()
    if (is.null(taxonomy_cleaned_data))
      return(NULL)
    
    runjs("$('#run_space').addClass('selected');")
    
    check_space <- bdc_coordinates_precision(
      data = taxonomy_cleaned_data,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      ndec = input$ndec
    )
    
    check_space <- CoordinateCleaner::clean_coordinates(
      x = check_space,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      species = input$clustering_level,
      tests = input$tests,
      capitals_rad = input$capitals_rad,
      centroids_rad = input$centroids_rad,
      centroids_detail = input$centroids_detail,
      inst_rad = input$inst_rad,
      outliers_mtp = input$outliers_mtp,
      outliers_td = input$outliers_td,
      outliers_size = input$outliers_size,
      range_rad = input$range_rad,
      zeros_rad = input$zeros_rad,
      value = "spatialvalid"
    ) %>%
      dplyr::tibble()
    
    check_space <- bdc_summary_col(data = check_space)
    
    space_cleaned_data <- check_space %>%
      dplyr::filter(.summary == TRUE) %>%
      bdc_filter_out_flags(data = ., col_to_remove = "all")
    
    space_cleaned_data <- space_cleaned_data %>%
      filter(!is.na(species) & species != "")
    
    space_cleaned(space_cleaned_data)  # Update reactive variable
    
    refDB_SaveOccurrenceData(space_cleaned_data, input$save_path_space, formats = input$formats_space)
    
    output$space_input_plot <- renderPlotly({
      refDB_CreateStackedBar(taxonomy_cleaned_data, input$taxonomic_level_space, "Taxonomy-Cleaned Dataset by Year and Taxonomic Level", input$taxonomic_level_space)
    })
    
    output$space_output_plot <- renderPlotly({
      refDB_CreateStackedBar(space_cleaned_data, input$taxonomic_level_space, "Spatially-Cleaned Dataset by Year and Taxonomic Level", input$taxonomic_level_space)
    })
  })
  
  rm(taxonomy_cleaned_data)
  gc()
  
  # Time Process
  observeEvent(input$run_time, {
    space_cleaned_data <- space_cleaned()
    if (is.null(space_cleaned_data))
      return(NULL)
    
    runjs("$('#run_time').addClass('selected');")
    
    year_threshold <- input$year_threshold
    save_path_time <- input$save_path_time
    formats_time <- input$formats_time
    
    check_time <- bdc_year_outOfRange(
      data = space_cleaned_data,
      eventDate = "year",
      year_threshold = year_threshold
    )
    
    check_time <- bdc_summary_col(data = check_time)
    
    time_cleaned_data <- check_time %>%
      dplyr::filter(.summary == TRUE) %>%
      bdc_filter_out_flags(data = ., col_to_remove = "all") %>%
      dplyr::tibble() 
    
    time_cleaned_data <- time_cleaned_data %>%
      select(-scientificName, -countryCode, -stateProvince, -locality, -basisOfRecord, -names_clean)
    
    
    time_cleaned(time_cleaned_data)  # Update reactive variable
    
    refDB_SaveOccurrenceData(time_cleaned_data, input$save_path_time, formats = input$formats_time)
    
    output$time_input_plot <- renderPlotly({
      refDB_CreateStackedBar(space_cleaned_data, input$taxonomic_level_time, "Spatially-Cleaned Dataset by Year and Taxonomic Level", input$taxonomic_level_time)
    })
    
    output$time_output_plot <- renderPlotly({
      refDB_CreateStackedBar(time_cleaned_data, input$taxonomic_level_time, "Time-Cleaned Dataset by Year and Taxonomic Level", input$taxonomic_level_time)
    })
  })
  
  # Load Shapefile Process
  observeEvent(input$load_shp, {
    req(input$shp_path)
    shapefile_path <- input$shp_path
    if (file.exists(shapefile_path)) {
      polygon <- tryCatch({
        st_read(shapefile_path)
      }, error = function(e) {
        showNotification("Error reading shapefile", type = "error")
        NULL
      })
      if (!is.null(polygon)) {
        polygon_data(st_transform(polygon, 4326))  # Transform the CRS of the polygon to match the points
        showNotification("Shapefile loaded successfully", type = "message")
      }
    } else {
      showNotification("Shapefile not found", type = "error")
    }
  })
  
  # Edit Map Process
  observeEvent(input$run_edit_map, {
    runjs("$('#run_edit_map').addClass('selected');")
    time_cleaned_data <- time_cleaned()
    if (is.null(time_cleaned_data)) {
      showNotification("No data available for mapping. Please run the previous processes first.", type = "error")
      return(NULL)
    }
    
    visualization <- time_cleaned_data %>%
      mutate(identification_level = case_when(
        !is.na(species) & str_trim(species) != "" ~ "species",
        !is.na(genus) & str_trim(genus) != "" ~ "genus",
        !is.na(family) & str_trim(family) != "" ~ "family",
        !is.na(order) & str_trim(order) != "" ~ "order",
        !is.na(class) & str_trim(class) != "" ~ "class",
        !is.na(phylum) & str_trim(phylum) != "" ~ "phylum",
        !is.na(kingdom) & str_trim(kingdom) != "" ~ "kingdom",
        TRUE ~ "unknown"
      ))
    
    visualization <- visualization %>% rename(taxa = scientificName_updated)
    
    # Convert data to sf
    sf_data <- st_as_sf(visualization, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
    # Filter points within the polygon, if a polygon is loaded
    if (!is.null(polygon_data())) {
      sf_data <- sf_data[st_intersects(sf_data, polygon_data(), sparse = FALSE), ]
    }
    
    # Check if any points remain after filtering
    if (nrow(sf_data) == 0) {
      showNotification("No point is inside the loaded polygon.", type = "error")
      return()
    }
    
    # Convert the filtered sf_data back to a data.frame with separate longitude and latitude columns
    visualization <- sf_data %>%
      st_drop_geometry() %>%               # Remove the geometry column
      mutate(
        decimalLongitude = st_coordinates(sf_data)[, 1],  # Extract longitude
        decimalLatitude = st_coordinates(sf_data)[, 2]    # Extract latitude
      )
    
    visualization <- visualization %>%
      distinct(taxa, .keep_all = TRUE)
    
    time_cleaned(visualization)
    
    # Define a color palette for the species
    qual_palette <- colorFactor(palette = brewer.pal(9, "Set1"), domain = visualization$taxa)
    
    # Initialize the map
    map_within_sa_edit <- leaflet(visualization) %>%
      addProviderTiles(providers$Esri.WorldStreetMap)
    
    # Add polygon
    if (!is.null(polygon_data())) {
      map_within_sa_edit <- map_within_sa_edit %>%
        addPolygons(data = polygon_data(), color = "blue", weight = 2, fillOpacity = 0.2)
    }
    
    # Map
    map_within_sa_edit <- map_within_sa_edit %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addCircleMarkers(
        lng = ~decimalLongitude, lat = ~decimalLatitude,
        radius = 3,
        color = ~qual_palette(taxa),
        label = ~taxa,
        popup = ~paste("Taxa:", taxa, "<br>Genus:", genus, "<br>Family:", family, "<br>Order:", order, "<br>Class:", class, "<br>Phylum:", phylum, "<br>Level:", toTitleCase(identification_level)),
        layerId = ~paste0(taxa)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = qual_palette,
        values = ~taxa,
        title = "Species",
        opacity = 1,
        labFormat = function(type, cuts, p) {
          sapply(cuts, function(cut) {
            paste0("<span style='font-style:italic;'>", htmltools::htmlEscape(cut), "</span>")
          })
        }
      ) %>%
      htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      var originalLayers = {};
      var uniqueTaxa = new Set();

      myMap.eachLayer(function(layer) {
        if (layer.options && layer.options.layerId) {
          originalLayers[layer.options.layerId] = layer;
          uniqueTaxa.add(layer.options.layerId);
        }
      });

      var selectedTaxa = Array.from(uniqueTaxa);
      selectedTaxa.forEach(function(taxon) {
        Object.values(originalLayers).forEach(function(layer) {
          if (layer.options.layerId === taxon) {
            myMap.addLayer(layer);
          }
        });
      });

      Shiny.setInputValue('selected_taxa', selectedTaxa, {priority: 'event'});

      var searchControl = L.control({position: 'bottomleft'});

      searchControl.onAdd = function(map) {
        var div = L.DomUtil.create('div', 'info legend');
        div.innerHTML = '<h4>Search Taxa</h4>';
        div.innerHTML += '<input type=\"text\" id=\"search-box\" placeholder=\"Search for species, genus, family, order, class, or phylum...\"><br>';
        div.innerHTML += '<button id=\"search-button\">Search</button>';

        var searchBox = div.querySelector('#search-box');

        searchBox.onfocus = function() {
          myMap.dragging.disable();
          myMap.touchZoom.disable();
          myMap.doubleClickZoom.disable();
          myMap.scrollWheelZoom.disable();
        };

        searchBox.onblur = function() {
          myMap.dragging.enable();
          myMap.touchZoom.enable();
          myMap.doubleClickZoom.enable();
          myMap.scrollWheelZoom.enable();
        };

        div.querySelector('#search-button').onclick = function() {
          var searchValue = searchBox.value;
          var taxaArray = searchValue.split(',').map(function(taxon) {
            return taxon.trim();
          });

          Object.values(originalLayers).forEach(function(layer) {
            myMap.removeLayer(layer);
          });

          if (searchValue === 'all' || searchValue === '') {
            selectedTaxa = Array.from(uniqueTaxa);
            selectedTaxa.forEach(function(taxon) {
              Object.values(originalLayers).forEach(function(layer) {
                if (layer.options.layerId === taxon) {
                  myMap.addLayer(layer);
                }
              });
            });
          } else {
            var foundLayers = new Set();
            taxaArray.forEach(function(taxon) {
              Object.values(originalLayers).forEach(function(layer) {
                var layerGroup = layer.options.layerId;
                var isGenusSearch = taxon.endsWith(' (genus)');
                var isFamilySearch = taxon.endsWith(' (family)');
                var isOrderSearch = taxon.endsWith(' (order)');
                var isClassSearch = taxon.endsWith(' (class)');
                var isPhylumSearch = taxon.endsWith(' (phylum)');
                var taxonWithoutSuffix = taxon.replace(' (genus)', '')
                                              .replace(' (family)', '')
                                              .replace(' (order)', '')
                                              .replace(' (class)', '')
                                              .replace(' (phylum)', '');

                if (isGenusSearch) {
                  if (layer.options.popup.includes('Genus: ' + taxonWithoutSuffix + ' ')) {
                    myMap.addLayer(layer);
                    foundLayers.add(layer.options.layerId);
                  }
                } else if (isFamilySearch) {
                  if (layer.options.popup.includes('Family: ' + taxonWithoutSuffix + ' ')) {
                    myMap.addLayer(layer);
                    foundLayers.add(layer.options.layerId);
                  }
                } else if (isOrderSearch) {
                  if (layer.options.popup.includes('Order: ' + taxonWithoutSuffix + ' ')) {
                    myMap.addLayer(layer);
                    foundLayers.add(layer.options.layerId);
                  }
                } else if (isClassSearch) {
                  if (layer.options.popup.includes('Class: ' + taxonWithoutSuffix + ' ')) {
                    myMap.addLayer(layer);
                    foundLayers.add(layer.options.layerId);
                  }
                } else if (isPhylumSearch) {
                  if (layer.options.popup.includes('Phylum: ' + taxonWithoutSuffix + ' ')) {
                    myMap.addLayer(layer);
                    foundLayers.add(layer.options.layerId);
                  }
                } else {
                  if (layerGroup.includes(taxonWithoutSuffix)) {
                    myMap.addLayer(layer);
                    foundLayers.add(layer.options.layerId);
                  }
                }
              });
            });
            selectedTaxa = Array.from(foundLayers);
          }

          Shiny.setInputValue('selected_taxa', selectedTaxa, {priority: 'event'});
          Shiny.setInputValue('search_box', searchValue, {priority: 'event'});

          requestAnimationFrame(function() {
            searchBox.setSelectionRange(searchBox.value.length, searchBox.value.length);
            searchBox.scrollLeft = searchBox.scrollWidth;
          });
        };

        return div;
      };

      searchControl.addTo(myMap);

      myMap.on('draw:created', function(e) {
        if (selectedTaxa.length > 0) {
          var layer = e.layer;
          var layerType = e.layerType;
          Shiny.setInputValue('drawn_feature', {
            type: layerType,
            layer: layer.toGeoJSON(),
            taxa: selectedTaxa
          });
          
          Shiny.onInputChange('drawnFeaturesGlobal', layer.toGeoJSON());
          drawn_features(layer.toGeoJSON());
        }
      });

      var legendItems = document.querySelectorAll('.leaflet-legend .legend-labels span');
      var maxWidth = 0;

      legendItems.forEach(function(item) {
        var width = item.clientWidth;
        if (width > maxWidth) {
          maxWidth = width;
        }
      });

      var legend = document.querySelector('.leaflet-legend');
      if (legend) {
        legend.style.width = (maxWidth + 50) + 'px';
      }
    }
  ") %>%
      addDrawToolbar(
        targetGroup = 'drawn',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        polylineOptions = FALSE
      )
    
    output$edit_map <- renderLeaflet(map_within_sa_edit)
    
  })
  
  # Save Data Process
  observeEvent(input$save_data, {
    runjs("$('#save_data').addClass('selected');")
    save_option <- input$save_option
    condition <- input$condition
    
    # Insert the logic for the Save Data input here
    time_cleaned_data <- time_cleaned()
    if (is.null(time_cleaned_data)) {
      showNotification("No data available for mapping. Please run the previous processes first.", type = "error")
      return(NULL)
    }
    
    visualization <- time_cleaned_data %>%
      mutate(identification_level = case_when(
        !is.na(species) & str_trim(species) != "" ~ "species",
        !is.na(genus) & str_trim(genus) != "" ~ "genus",
        !is.na(family) & str_trim(family) != "" ~ "family",
        !is.na(order) & str_trim(order) != "" ~ "order",
        !is.na(class) & str_trim(class) != "" ~ "class",
        !is.na(phylum) & str_trim(phylum) != "" ~ "phylum",
        !is.na(kingdom) & str_trim(kingdom) != "" ~ "kingdom",
        TRUE ~ "unknown"
      ))
    
    # Convert data to sf
    sf_data <- st_as_sf(visualization, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
    
    # Check if the updatedFeatures.RData file exists in the current working directory
    if (!file.exists("updatedFeatures.RData")) {
      if (condition) {
        species_chunks <- unique(visualization$taxa[str_detect(visualization$taxa, "\\s")])
        genus_chunks <- unique(str_extract(species_chunks, "^[^\\s]+"))
        collapsed_string <- paste0(paste(genus_chunks, collapse = "|"), "|")
      } else {
        species_chunks <- unique(visualization$taxa[str_detect(visualization$taxa, "\\s")])
        species_chunks <- gsub("\\s", "_", species_chunks)
        collapsed_string <- paste0(paste(species_chunks, collapse = "|"), "|")
      }
      
      # Write collapsed string to file
      writeLines(collapsed_string, "gbif_taxa_dataset.txt")
      # Save all data from the visualization object
      refDB_SaveOccurrenceData(sf_data, input$save_path_selected, formats = input$formats_edit)
      showNotification("All data from the visualization object has been saved.", type = "message")
      return(NULL)
    }
    
    # Use the reactive variable to obtain the drawn data
    load("updatedFeatures.RData")
    if (is.null(updatedFeatures)) {
      showNotification("No drawn features found.", type = "error")
      return(NULL)
    }
    
    selected_sf <- refDB_ExtractData(updatedFeatures)
    
    all_search_results_inside <- list()
    
    for (i in 1:nrow(selected_sf)) {
      name_to_search <- rownames(selected_sf[i, ])
      name_to_search <- remove_trailing_numbers(name_to_search)
      current_polygon <- selected_sf[i, 3]
      search_results <- sf_data[sf_data$taxa == name_to_search, ]
      if (nrow(search_results) > 0) {
        points_sf <- st_as_sf(search_results, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(selected_sf))
        within_polygon <- st_intersects(points_sf, current_polygon, sparse = FALSE)
        search_results_inside <- search_results[within_polygon, ]
      } else {
        search_results_inside <- data.frame()
      }
      all_search_results_inside[[i]] <- search_results_inside
    }
    
    all_search_results_inside <- Filter(function(x) nrow(x) > 0, all_search_results_inside)
    excluded_occurrence_data <- do.call(rbind, all_search_results_inside)
    
    sf_data$wkt <- st_as_text(sf_data$geometry)
    excluded_occurrence_data$wkt <- st_as_text(excluded_occurrence_data$geometry)
    coords <- sf::st_coordinates(excluded_occurrence_data)
    excluded_occurrence_data$decimalLongitude <- coords[, "X"]
    excluded_occurrence_data$decimalLatitude <- coords[, "Y"]
    
    unique_rows <- anti_join(as.data.frame(sf_data), as.data.frame(excluded_occurrence_data), 
                             by = c("gbifID", "species", "genus", "family", "order", "class", "phylum", "kingdom")) %>% select(-geometry)
    
    excluded_occurrence_data <- excluded_occurrence_data %>% select(-wkt) %>% st_drop_geometry()
    selected_occurrence_data <- st_as_sf(unique_rows, wkt = "wkt", crs = st_crs(sf_data))
    coords <- sf::st_coordinates(selected_occurrence_data)
    selected_occurrence_data$decimalLongitude <- coords[, "X"]
    selected_occurrence_data$decimalLatitude <- coords[, "Y"]
    selected_occurrence_data <- selected_occurrence_data %>% st_drop_geometry()
    
    if (condition) {
      species_chunks <- unique(selected_occurrence_data$taxa[str_detect(selected_occurrence_data$taxa, "\\s")])
      genus_chunks <- unique(str_extract(species_chunks, "^[^\\s]+"))
      collapsed_string <- paste0(paste(genus_chunks, collapse = "|"), "|")
    } else {
      species_chunks <- unique(selected_occurrence_data$taxa[str_detect(selected_occurrence_data$taxa, "\\s")])
      species_chunks <- gsub("\\s", "_", species_chunks)
      collapsed_string <- paste0(paste(species_chunks, collapse = "|"), "|")
    }
    
    # Write collapsed string to file
    writeLines(collapsed_string, "gbif_taxa_dataset.txt")
    
    if (save_option == "selected_occurrence_data" || save_option == "both") {
      refDB_SaveOccurrenceData(selected_occurrence_data, input$save_path_selected, formats = input$formats_edit)
    }
    if (save_option == "excluded_occurrence_data" || save_option == "both") {
      refDB_SaveOccurrenceData(excluded_occurrence_data, input$save_path_excluded, formats = input$formats_edit)
    }
  })
  
  # Make Database Process
  observeEvent(input$run_make_database, {
    runjs("$('#run_make_database').addClass('selected');")
    raw_database <- input$raw_database
    gbif_database <- input$gbif_database
    final_output_database <- input$final_output_database
    min_sequence_length <- input$min_sequence_length
    pattern <- if (input$pattern_unverified) "UNVERIFIED" else ""
    parse_seqids <- input$parse_seqids
    database_type <- input$database_type
    title <- input$title
    out <- input$out
    hash_index <- input$hash_index
    mask_data <- input$mask_data
    mask_id <- input$mask_id
    mask_desc <- input$mask_desc
    gi_mask <- input$gi_mask
    gi_mask_name <- input$gi_mask_name
    max_file_sz <- input$max_file_sz 
    logfile <- input$logfile
    taxid <- input$taxid
    taxid_map <- input$taxid_map
    condition <- input$condition
    
    # Define intermediate parameters
    database_cleaned <- "ncbiChordataToGbif.fasta"
    
    # Call the function refDB_FormatNcbiDatabase
    refDB_FormatNcbiDatabase(raw_database, database_cleaned, min_sequence_length, pattern)
    
    # Conditionally call the appropriate function
    if (gbif_database != "") {
      refDB_SubsetNcbiGbif(gbif_database, database_cleaned, final_output_database, condition)
    } else {
      refDB_ncbiToMakeblastdb(database_cleaned, final_output_database)
    }
    
    # Call the function refDB_CreateBlastDB
    refDB_CreateBlastDB(final_output_database, parse_seqids, database_type, title, out, hash_index, mask_data, mask_id, mask_desc, gi_mask, gi_mask_name, max_file_sz, logfile, taxid, taxid_map)
  })
  
  # Adding reactives and observers for "Edit Map"
  searchedValues <- reactiveVal(character(0))
  drawnFeatures <- reactiveVal(list())
  
  observeEvent(input$selected_taxa, {
    newSearch <- input$selected_taxa
    currentSearches <- searchedValues()
    updatedSearches <- unique(c(currentSearches, unlist(newSearch)))
    searchedValues(updatedSearches)
    assign("searchedValuesGlobal", updatedSearches, envir = .GlobalEnv)
  })
  
  observeEvent(input$search_box, {
    searchValue <- input$search_box
    currentSearches <- searchedValues()
    updatedSearches <- unique(c(currentSearches, searchValue))
    searchedValues(updatedSearches)
    assign("searchedValuesGlobal", updatedSearches, envir = .GlobalEnv)
  })
  
  observeEvent(input$drawn_feature, {
    feature <- input$drawn_feature
    taxa <- feature$taxa
    print(paste("New feature drawn for taxa:", taxa))
    
    # Add the taxa and the drawn polygon to the list
    currentFeatures <- drawnFeatures()
    updatedFeatures <- append(currentFeatures, list(feature))
    drawnFeatures(updatedFeatures)
    
    # Update the global object with the drawn taxa
    taxaList <- lapply(updatedFeatures, function(f) f$taxa)
    assign("searchedValuesGlobal", taxaList, envir = .GlobalEnv)
    assign("drawnFeaturesGlobal", updatedFeatures, envir = .GlobalEnv)
    save(updatedFeatures, file = "updatedFeatures.RData")
  })
  
  output$searchedValuesOutput_edit <- renderPrint({
    searchedValues()
    
  rm(searchedValuesGlobal, drawnFeaturesGlobal)
  gc()
  
  })
  
  observeEvent(input$run_blast, {
    Directory <- input$directory
    Database_File <- input$database_file
    query <- input$query
    task <- input$task
    out <- input$out
    max_target_seqs <- input$max_target_seqs
    perc_identity <- input$perc_identity
    qcov_hsp_perc <- input$qcov_hsp_perc
    num_threads <- input$num_threads
    Specie_Threshold <- input$specie_threshold
    Genus_Threshold <- input$genus_threshold
    Family_Threshold <- input$family_threshold
    penalty <- if (is.na(input$penalty)) NULL else input$penalty
    reward <- if (is.na(input$reward)) NULL else input$reward
    evalue <- if (is.na(input$evalue)) NULL else input$evalue
    word_size <- if (is.na(input$word_size)) NULL else input$word_size
    gapopen <- if (is.na(input$gapopen)) NULL else input$gapopen
    gapextend <- if (is.na(input$gapextend)) NULL else input$gapextend
    max_hsps <- if (is.na(input$max_hsps)) NULL else input$max_hsps
    xdrop_ungap <- if (is.na(input$xdrop_ungap)) NULL else input$xdrop_ungap
    xdrop_gap <- if (is.na(input$xdrop_gap)) NULL else input$xdrop_gap
    xdrop_gap_final <- if (is.na(input$xdrop_gap_final)) NULL else input$xdrop_gap_final
    searchsp <- if (is.na(input$searchsp)) NULL else input$searchsp
    sum_stats <- if (is.na(input$sum_stats)) NULL else input$sum_stats
    no_greedy <- if (is.na(input$no_greedy)) NULL else input$no_greedy
    min_raw_gapped_score <- if (is.na(input$min_raw_gapped_score)) NULL else input$min_raw_gapped_score
    template_type <- if (input$template_type == "") NULL else input$template_type
    template_length <- if (is.na(input$template_length)) NULL else input$template_length
    dust <- if (input$dust == "") NULL else input$dust
    filtering_db <- if (input$filtering_db == "") NULL else input$filtering_db
    window_masker_taxid <- if (input$window_masker_taxid == "") NULL else input$window_masker_taxid
    window_masker_db <- if (input$window_masker_db == "") NULL else input$window_masker_db
    soft_masking <- if (is.na(input$soft_masking)) NULL else input$soft_masking
    ungapped <- if (is.na(input$ungapped)) NULL else input$ungapped
    culling_limit <- if (is.na(input$culling_limit)) NULL else input$culling_limit
    best_hit_overhang <- if (is.na(input$best_hit_overhang)) NULL else input$best_hit_overhang
    best_hit_score_edge <- if (is.na(input$best_hit_score_edge)) NULL else input$best_hit_score_edge
    subject_besthit <- if (is.na(input$subject_besthit)) NULL else input$subject_besthit
    window_size <- if (is.na(input$window_size)) NULL else input$window_size
    off_diagonal_range <- if (is.na(input$off_diagonal_range)) NULL else input$off_diagonal_range
    use_index <- if (is.na(input$use_index)) NULL else input$use_index
    index_name <- if (input$index_name == "") NULL else input$index_name
    lcase_masking <- if (is.na(input$lcase_masking)) NULL else input$lcase_masking
    query_loc <- if (input$query_loc == "") NULL else input$query_loc
    strand <- if (input$strand == "") NULL else input$strand
    parse_deflines <- if (is.na(input$parse_deflines)) NULL else input$parse_deflines
    outfmt <- input$outfmt
    show_gis <- if (is.na(input$show_gis)) NULL else input$show_gis
    num_descriptions <- if (is.na(input$num_descriptions)) NULL else input$num_descriptions
    num_alignments <- if (is.na(input$num_alignments)) NULL else input$num_alignments
    line_length <- if (is.na(input$line_length)) NULL else input$line_length
    html <- if (is.na(input$html)) NULL else input$html
    sorthits <- if (input$sorthits == "") NULL else input$sorthits
    sorthsps <- if (input$sorthsps == "") NULL else input$sorthsps
    mt_mode <- if (is.na(input$mt_mode)) NULL else input$mt_mode
    remote <- if (is.na(input$remote)) NULL else input$remote
    
    refDB_Blast(Directory, Database_File, query, task, out, max_target_seqs, perc_identity, qcov_hsp_perc, num_threads, 
                Specie_Threshold, Genus_Threshold, Family_Threshold, penalty, reward, evalue, word_size, gapopen, 
                gapextend, max_hsps, xdrop_ungap, xdrop_gap, xdrop_gap_final, searchsp, sum_stats, no_greedy, 
                min_raw_gapped_score, template_type, template_length, dust, filtering_db, window_masker_taxid, 
                window_masker_db, soft_masking, ungapped, culling_limit, best_hit_overhang, best_hit_score_edge, 
                subject_besthit, window_size, off_diagonal_range, use_index, index_name, lcase_masking, query_loc, 
                strand, parse_deflines, outfmt, show_gis, num_descriptions, num_alignments, line_length, html, 
                sorthits, sorthsps, mt_mode, remote)
  })
}

if (run_in_browser) {
  shinyApp(ui, server, options = list(launch.browser = TRUE))
} else {
  shinyApp(ui, server)
}

}
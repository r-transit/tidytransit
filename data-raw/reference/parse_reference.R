library(dplyr)

# https://stackoverflow.com/questions/48087762/markdown-table-to-data-frame-in-r
read_markdown_table = function(lines) {
  lines <- lines[!grepl('^[[:blank:]+-=:_|]*$', lines)]
  lines <- gsub('(^\\s*?\\|)|(\\|\\s*?$)', '', lines)
  readr::read_delim(paste(lines, collapse = '\n'), delim = '|', 
                    trim_ws = TRUE, show_col_types = FALSE)
}

# parse all field definitions
parse_fields = function(reference.md) {
  field_reference = list()
  
  ref_lines = readr::read_lines(reference.md)
  ref_lines[length(ref_lines)+1] <- "" # ensure empty last row
  
  table_index = stringr::str_starts(ref_lines, "\\| ") # lines with table markdown
  
  i = which(ref_lines == "## Field Definitions")
  while(i <= length(ref_lines)) {
    .line = ref_lines[i]
    if(stringr::str_starts(.line, "### ")) {
      .current_file <- stringr::str_replace_all(.line, "### ", "")
    }
    if(stringr::str_starts(.line, "File: ")) {
      .file_presence <- stringr::str_replace_all(.line, "File: ", "")
    }
    if(stringr::str_starts(.line, "Primary key ")) {
      .primary_key <- stringr::str_replace_all(.line, "Primary key \\(", "")
      .primary_key <- stringr::str_replace_all(.primary_key, "\\)", "")
    }
    
    # parse fields table
    if(stringr::str_starts(.line, "\\|[ ]+Field Name \\| Type")) {
      j = min(which(!table_index & seq_along(ref_lines) > i))-1
      ref_table = read_markdown_table(ref_lines[i:j])
      
      stopifnot(!is.null(.current_file), !is.null(.file_presence))
      if(is.null(.primary_key)) stopifnot(stringr::str_ends(.current_file, "geojson"))

      # print problems if available 
      if(nrow(readr::problems(ref_table)) > 0) {
        cat(.current_file, "\n")
        print(readr::problems(ref_table)[,1:4])
      }

      # cleanup attributes
      attributes(ref_table)$presence <- .file_presence
      attributes(ref_table)$primary_key <- .primary_key
      attributes(ref_table)$spec <- NULL # remove col_type info
      attributes(ref_table)$problems <- NULL # remove col_type info
      
      # assign to return list
      field_reference[[.current_file]] <- ref_table
      
      # clear values
      .current_file <- .file_presence <- .primary_key <- NULL
    }
    i <- i+1
  }
  return(field_reference)
}

# Parse current reference
ref = parse_fields("https://raw.githubusercontent.com/google/transit/master/gtfs/spec/en/reference.md")

# table containing all fields 
fields = bind_rows(ref, .id = "file") |> 
  rename(Field_Name = `Field Name`) |> 
  mutate(Field_Name = gsub("`", "", Field_Name)) |> 
  mutate(Presence = gsub("**", "", Presence, fixed = TRUE)) |> 
  select(-Description)

# Link gtfs types to R types ####
fields$tidytransit_type <- NA

# Enum
fields$tidytransit_type[fields$Type == "Enum"] <- "integer_character"

# ID: character
fields$tidytransit_type[startsWith(fields$Type, "Foreign ID")] <- "character"
fields$tidytransit_type[startsWith(fields$Type, "ID referencing")] <- "character"
fields$tidytransit_type[fields$Type %in% c("ID", "Foreign ID", "Unique ID")] <- "character"

# Text/Strings
fields$tidytransit_type[fields$Type %in% c("Text", "String")] <- "character"
fields$tidytransit_type[fields$Type %in% c("URL", "Language code", "Currency code", "Email",
                                 "Phone number", "Timezone", "Color",
                                 "Text or URL or Email or Phone number")] <- "character"

# Date and Time
fields$tidytransit_type[fields$Type == "Date"] <- "tidytransit_Date"
fields$tidytransit_type[fields$Type == "Time"] <- "tidytransit_Time"

# Numerics
fields$tidytransit_type[fields$Type %in% c("Latitude", "Longitude", "Non-negative float", 
                                 "Positive float", "Float", "Currency amount")] <- "numeric"
fields$tidytransit_type[fields$Type %in% c("Non-negative integer", "Non-zero integer", 
                                 "Positive integer", "Non-null integer", "Integer")] <- "integer"

# Geojson
fields$tidytransit_type[fields$Type == "Array"] <- "geojson_array"
fields$tidytransit_type[fields$Type == "Object"] <- "geojson_object"

fields$Field_Name[fields$file == "locations.geojson"] <- gsub("&nbsp;", "", fields$Field_Name[fields$file == "locations.geojson"])
fields$Field_Name[fields$file == "locations.geojson"] <- gsub("\\\\", "", fields$Field_Name[fields$file == "locations.geojson"])
fields$Field_Name[fields$file == "locations.geojson"] <- gsub("-", "", fields$Field_Name[fields$file == "locations.geojson"])

stopifnot(all(!is.na(fields$tidytransit_type)))

# Presence ####
fields$tidytransit_presence <- NA
fields$tidytransit_presence[fields$Presence %in% c("Required")] <- "required"
fields$tidytransit_presence[fields$Presence %in% c("Conditionally Required", "Conditionally Forbidden")] <- "conditional"
fields$tidytransit_presence[fields$Presence %in% c("Optional", "Recommended")] <- "optional"

stopifnot(all(!is.na(fields$tidytransit_presence)))

write.csv(fields, "tidytransit_field_conversion_types.csv", row.names = FALSE)
save(fields, file = "../../data/tidytransit_field_conversion_types.rda")

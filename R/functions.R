library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)

# This returns all the tiers from the data
get_tiers <- function(data) {
  tiers <- names(data)[which(names(data) != "name")]
  return(tiers)
}

# Gets the distinct items in the set
get_distinct <- function(data) {
  vals <- c()
  tiers <- get_tiers(data)
  for (tier in tiers) {
    for (i in length(data[tier][[1]])){
      new_vals <- data[tier][[1]][[i]]
      vals <- c(vals, new_vals)
    }
  }
  vals %<>% unique() %>% sort()
  return (vals)
}

# Encodes the items numerically based on their tier
# The default is to give the lowest 0 and increment by 1 for each tier
encode_item <- function(item, row, tiers, weights) {
  if (is.na(weights[1])) {
    weights <- seq((length(tiers) - 1), 0)
  }
  for (i in (1:length(tiers))) {
    if (item %in% row[[tiers[i]]][[1]]){
      enc <- weights[i]
      break
    }
  }
  return (enc)
}

# Adds a column named for each distinct item
# The value of the column is the encoded value
add_enc_cols <- function(row, tiers, items, weights) {
  for (item in items) {
    col_name <- item %>% tolower() %>% str_replace(" ", "_")
    row[, col_name] <- encode_item(item, row, tiers, weights)
  }
  return(row)
}

# Transforms the dataframe and subsets it to the transformed columns
trans_df <- function(data, weights=NA) {
  tiers <- get_tiers(data)
  items <- get_distinct(data)
  df <- add_enc_cols(snackcakes_json[1, ], tiers, items, weights)
  for (j in seq(2, nrow(snackcakes_json))) {
    newrow <- add_enc_cols(snackcakes_json[j, ], tiers, items, weights)
    df %<>% bind_rows(newrow)
  }
  return(df %>% select(-tiers))
}
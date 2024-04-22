# helper functions

get_barplot_df <- function(df, col, categories) {
  tmp <- df[, 
            c(.N, 
              sum(get(col), na.rm = TRUE),
              .N - sum(get(col), na.rm = TRUE),
              (sum(get(col), na.rm = TRUE)/.N)*100), 
            view]
  setnames(tmp, "V1", "value")
  
  tmp[, category := rep(categories, length(unique(tmp$view)))]
  tmp[, fill_colour_category := paste(view, category, sep = " | ")]
  
  return(tmp)
}

get_barplot_labels <- function(df, categories) {
  tmp <- df[category==categories[2], .(view, value)]
  setnames(tmp, "value", paste(strsplit(categories[2], split = " ")[[1]], collapse = ""))
  tmp[, paste(strsplit(categories[4], split = " ")[[1]], collapse = "") := df[category==categories[4], value]]
  tmp[, paste(strsplit(categories[1], split = " ")[[1]], collapse = "") := df[category==categories[1], value]]
}

get_number_in_group_per_view <- function(df, col, group_levels) {
  tmp <- df[, .N, .(get(col), view)]
  
  tmp[, get := as.character(get)]
  tmp[is.na(get), get := group_levels[length(group_levels)]]
  
  for (g in group_levels) {
    for (v in views) {
      if (tmp[get == g & view == v, .N] == 0) {
        tmp <- rbind(tmp,
                     data.table(get = g, view = v, N = 0))
      }
    }
  }
  
  tmp[, get := factor(get, levels = group_levels)]
  tmp[, view := factor(view, levels = views)]
  
  setnames(tmp, "get", col)
  
  return(tmp)
}
# helper functions

get_barplot_df <- function(df, col, categories) {
  tmp <- df[, 
            c(.N, 
              sum(get(col)),
              .N - sum(get(col)),
              (sum(get(col))/.N)*100), 
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

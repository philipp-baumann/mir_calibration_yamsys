plot_spc <- function(spc_tbl, x = NULL, y, by, 
                     xlab = expression(paste("Wavenumber [", cm^-1, "]")),
                     ylab = "Absorbance", slice = TRUE) {
  
  # (0) Slice spectra tibble to remove triplicate spectra (reps)
  # only sample_id level
  if(slice == TRUE) {
    spc_tbl <- dplyr::slice(spc_tbl)
  }
  # (1) Gather spectra into one data.table
  if(y == "spc") {
    # raw spectra are not yet data.tables and extraction is done alternatively
    # via do.call(rbind, list) -> a little bit slower
    dt <- data.table::data.table(do.call(rbind, spc_tbl[, y][[y]]))
  } else {
    dt <- data.table::rbindlist(spc_tbl[, y][[y]])
  }
  # (2) Extract ID variable and append it to the data.table
  id <- spc_tbl[, by][[by]]
  dt <- dt[,id:=id]
  # (3) Convert data.table from wide to long form
  dt_long <- data.table::melt(
    dt, measure=names(dt)[!names(dt) %in% c("id")]
  )
  # Convert variable column from factor to numeric
  dt_long <- dt_long[, variable := as.numeric(as.character(variable))]
  # (4) Plot spectra
  # Define nice breaks for x axis
  brk  <- pretty(as.numeric(names(dt)[!names(dt) %in% c("id")]), n = 10)
  p <- ggplot(dt_long, aes(variable, value)) +  # group = group)) +
    labs(x = xlab, y = ylab) +
    theme_bw() +
    scale_x_reverse(breaks = brk) +
    geom_line(aes(colour = id), alpha = 0.2, size = 0.2) +
    scale_color_manual(values = rep("black", nrow(dt))) +
    # Remove legend
    guides(colour = FALSE)
  p
}
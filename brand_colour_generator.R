# declare colours to choose from
brand_colors <- c(`blue-likert-dark` = "#0571b0",
                  `blue-likert-light` = "#92c5de",
                  `grey` = "grey90",
                  `grey-likert` = "#f7f7f7",
                  `red-likert-dark` = "#ca0020",
                  `red-likert-light` = "#f4a582",
)


# build a function to extract colour based on name
brand_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (brand_colors)
  
  brand_colors[cols]
}

# combine colours into palettes
brand_palettes <- list(
  
  `main` = brand_cols("grey", "blue-likert-dark"),
  
  `likert` = brand_cols("blue-likert-dark", "blue-likert-light", "grey-likert", 
                        "red-likert-light", "red-likert-dark")
)

# crea
brand_pal <- function(palette = "main", reverse = FALSE) {
  pal <- brand_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal)
}

# pick palette, choose if you need discrete levels or a continuous gradient
scale_color_brand <- function(palette = "main", discrete = TRUE, reverse = FALSE) {
  # locates palette by name
  pal <- brand_pal(palette = palette, reverse = reverse)
  
  if (discrete == TRUE) {
    discrete_scale(aesthetics = "color", 
                   scale_name = paste0("brand_", palette), 
                   palette = pal)
  } else {
    scale_color_gradientn(colors = pal(256))
  }
}

scale_colour_brand <- function(palette = "main", discrete = TRUE, reverse = FALSE) {
  pal <- brand_pal(palette = palette, reverse = reverse)
  
  if (discrete == TRUE) {
    discrete_scale(aesthetics = "colour", 
                   scale_name = paste0("brand_", palette), 
                   palette = pal) 
  } else {
    # 256 is the number of levels required by the gradientn call.
    scale_colour_gradientn(colours = pal(256))
  }
}

scale_fill_brand <- function(palette = "main", discrete = TRUE, reverse = FALSE) {
  pal <- brand_pal(palette = palette, reverse = reverse)
  
  if (discrete == TRUE) {
    discrete_scale(aesthetics = "fill", 
                   scale_name = paste0("brand_", palette), 
                   palette = pal)
  } else {
    scale_fill_gradientn(colours = pal(256))
  }
}
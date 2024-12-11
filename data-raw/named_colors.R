## code to prepare `named_colors` dataset goes here

colorname_url <- file.path(
   "https://raw.githubusercontent.com/meodai/color-names",
   "master/src/colornames.csv")
colornames <- data.table::fread(colorname_url, data.table=FALSE)

# create named vector, subset for `good name = "x"`
colornames_sub <- subset(colornames, `good name` %in% "x")

# adjust names
colornames_v <- toupper(colornames_sub$hex)
names(colornames_v) <- gsub("[ ]+", "_",
   tolower(colornames_sub$name))
# make all hex colors uppercase

# covert R colors to hex, removing alpha
hex_colors <- toupper(jamba::unalpha(
   jamba::rgb2col(col2rgb(colors()))))
# add prefix "R"
names(hex_colors) <- paste0("R", colors());
# remove repeated hex values
hex_colors <- hex_colors[!duplicated(hex_colors)];

# add only new entries from hex colors
hex_colors_new <- hex_colors[!hex_colors %in% colornames_v];

# combine color vectors
new_named_colors <- c(colornames_v, hex_colors_new)

# sort saturated colors first, then unsaturated (greyscale)
new_named_colors <- c(
   colorjam::sort_colors(new_named_colors, C > 10, byCols=c("H", "C", "L")),
   colorjam::sort_colors(new_named_colors, C <= 10, byCols=c("H", "C", "L")))

# store for re-use
named_colors <- new_named_colors
usethis::use_data(named_colors, overwrite=TRUE)

## TODO: code to prepare colorjam hexsticker

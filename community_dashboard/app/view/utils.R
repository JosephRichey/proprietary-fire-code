box::use(
  bsicons[bs_icon],
  grDevices[hcl.colors],
  stats[setNames],
)

# ---- palette + icons ---------------------------------------------------------
known_pal <- c(
  "Fire"              = "#E74C3C",
  "EMS"               = "#3498DB",
  "Wildland"          = "#AD8A64",
  "Other"             = "#7F8C8D",
  "HazMat"            = "#2ECC71",
  "Technical Rescue"  = "#8E44AD",
  "Mixed"             = "#FFF",
  "None"              = "#000000"
)

icon_map <- list(
  "Fire"              = bs_icon("fire"),
  "EMS"               = bs_icon("activity"),
  "Wildland"          = bs_icon("tree-fill"),
  "Other"             = bs_icon("question-circle-fill"),
  "HazMat"            = bs_icon("exclamation-octagon-fill"),
  "Technical Rescue"  = bs_icon("tools")
)

# build a deterministic extra palette for unknowns
MakeColorsPlot <- function(categories) {
  cats_sorted <- categories[order(startsWith(categories, "*"), tolower(categories))]
  unknown <- setdiff(cats_sorted, names(known_pal))
  extra   <- setNames(hcl.colors(length(unknown), "Dark 3"), unknown)
  c(known_pal, extra)
}

MakeColorsBoxes <- function(cat) {
  if (cat %in% names(known_pal)) known_pal[[cat]] else "#000000"
}

test_strings <- c("b41f32b59+b34",
                  "f22b43a23")
str_split(test_strings, "b", simplify = TRUE)

## from this blog post:
## https://community.rstudio.com/t/split-character-vector-into-tibble-using-regex-but-remove-nothing/28282/6

strsplit_keep <- function(x,
                          split,
                          type = "remove",
                          perl = FALSE,
                          ...) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", split, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", split, ")"),
                          perl = TRUE,
                          ...)
  } else {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}

vals <- strsplit_keep(test_strings, "[a-zA-Z]", type = "before")
purrr::map_df(vals, ~data.frame(value = .x), .id = "id") %>%  
  pivot_wider(names_from = id,
              values_from = value)
## then pivot_wider so that you can merge it back with the original data set

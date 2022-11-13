




in_dat <- gapminder::gapminder %>% mutate(group = sample(letters[1:3], nrow(.), replace = TRUE))

grouped_dat <- in_dat %>% group_by(country, continent, group)

selected = c("United States","Americas")
rand_id <- random_id()
group_ids <- group_keys(grouped_dat) %>%
  mutate_all(as.character)

list_opts <- group_ids %>%
  gather(key, value) %>%
  distinct() %>%
  split(.$key) %>%
  imap(~ {
    pull(.x, value)
  })



select_options <- list_opts %>%
  imap(~ {
    tags$select(style = "display:inline-block",
      id = paste0(.y, rand_id),
      map(.x, ~ {
        if ((!is.null(selected) & .x %in% selected)) {
          tags$option(value = snakecase::to_lower_camel_case(.), ., `selected` = TRUE)
        }else {
          tags$option(value = snakecase::to_lower_camel_case(.), .)
        }
      })
    )
  })

js_ids <- names(list_opts) %>% paste0(rand_id)

group_glued_str <- names(group_ids) %>% paste("{",.,"}") %>% paste(collapse = "_")

group_opts <- group_ids %>%
  rowwise() %>%
  mutate(key = glue::glue(group_glued_str)) %>%
  pull(key) %>%
  snakecase::to_upper_camel_case()





group_split(grouped_dat)

glue::glue()

# var camalize = function camalize(str) {
#   return str.toLowerCase().replace(/[^a-zA-Z0-9]+(.)/g, function(match, chr)
#   {
#     return chr.toUpperCase();
#   });
# }

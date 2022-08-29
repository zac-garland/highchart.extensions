## code to prepare `hcExamples` dataset goes here
library(rvest)

clipr::read_clip() %>% paste(collapse = "") %>% htmltools::HTML() %>% read_html() -> pg

pg %>%
  html_nodes(".option") %>%
  # .[2] %>%
  map_dfr(~{
    example_href = html_attr(.x %>% html_nodes(".sample > a"),"href")
    # example_href = case_when(length(example_href) == 0 ~ "hello",TRUE ~ example_href)
    print(example_href)

    if(length(example_href)>0){
      tibble(
        node = html_name(.x),
        text = html_text(.x %>% html_nodes("span")),
        desc = html_text(.x %>% html_nodes("p"))
      ) %>%
        mutate(example = list(example_href))

    }else{
      tibble(
        node = html_name(.x),
        text = html_text(.x %>% html_nodes("span")),
        desc = html_text(.x %>% html_nodes("p"))
      )
    }


  }) %>%
  filter(desc != "",!str_detect(desc,"TypeScript|Configuration options|Since")) %>%
  unnest(example) -> hcExamples


usethis::use_data(hcExamples, overwrite = TRUE)

gapminder::gapminder %>%
  # filter(country %in% c("United States", "China", "India")) %>%
  hchart("line", hcaes(year, pop, name = country)) -> init_chart

selected <- c("United States", "Americas")


rand_id_begin <- paste(sample(letters, 3, replace = FALSE),
  sample(letters, 4, replace = FALSE),
  sample(letters, 3, replace = FALSE),
  collapse = ""
) %>% stringr::str_remove_all(" ")

drop_selectors <- c("country", "continent")

list_opts <- map(setNames(drop_selectors, drop_selectors), ~ {
  select_name <- .x
  init_chart$x$hc_opts$series %>%
    map(~ {
      purrr::map(purrr::pluck(.x, "data"), ~ {
        purrr::pluck(.x, select_name)
      }) %>%
        unlist()
    }) %>%
    unlist() %>%
    unique()
})

select_options <- list_opts %>%
  imap(~ {
    tags$select(
      style = "display:inline-block",
      id = paste0(.y, rand_id_begin),
      map(.x, ~ {
        if ((!is.null(selected) & .x %in% selected)) {
          tags$option(value = ., ., `selected` = TRUE)
        } else {
          tags$option(value = ., .)
        }
      })
    )
  })

names(list_opts) %>%
  map_chr(~ {
    glue::glue("var select_{paste0(.x, rand_id_begin)} = document.getElementById('{paste0(.x, rand_id_begin)}');")
  }) %>%
  paste(collapse = "") -> var_declaration

names(list_opts) %>%
  map_chr(~ {
    glue::glue("obj.{.x} === select_{paste0(.x, rand_id_begin)}.value")
  }) %>%
  paste(collapse = " & ") -> filter_declaration

names(list_opts) %>%
  map_chr(~ {
    glue::glue("select_{paste0(.x, rand_id_begin)}.onchange = updateChart;")
  }) %>%
  paste(collapse = "") -> onchg_events




js_fun <- "function(){{
  var this_chart = this;
  const cloneData = (sample) => {{ return JSON.parse(JSON.stringify(sample));}}
  const init_data = cloneData(this_chart.options.series[0].data);
  {var_declaration}

  function updateChart(){{
      new_data = init_data.filter(function(obj){{
        return {filter_declaration}
      }});
      if(new_data.length>0){{
        this_chart.series[0].setData(new_data);
      }}

  }}

  {onchg_events}
  updateChart();

}}"

js_fun %>% writeLines()
# select_options

init_chart %>%
  hc_chart(
    events = list(
      load = JS(glue::glue(js_fun))
    )
  ) %>%
  shiny::tagList(
    select_options, .
  ) %>%
  htmltools::browsable()

# shiny::selectInput("test","test",c("United States", "China", "India"))

# %>%
# htmltools::html_print()
js_fun <- "function(){
  var this_chart = this;
  const cloneData = (sample) => { return JSON.parse(JSON.stringify(sample));}
  const init_data = cloneData(this_chart.options.series[0].data);
  var select_element = document.getElementById('countrySelector');
  var cont_selector = document.getElementById('continentSelector');

  function updateChart(){
      new_data = init_data.filter(function(obj){
        return obj.country===select_element.value &
          obj.continent===cont_selector.value
      });
      if(new_data.length>0){
        this_chart.series[0].setData(new_data);
      }
  }
  select_element.onchange = updateChart;
  cont_selector.onchange = updateChart;
  updateChart();

}"

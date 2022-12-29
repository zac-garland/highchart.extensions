add_multi_drop <- function(hc, selectors, selected = NULL) {
  rand_id_begin <- paste(sample(letters, 3, replace = FALSE),
                         sample(letters, 4, replace = FALSE),
                         sample(letters, 3, replace = FALSE),
                         collapse = ""
  ) %>% stringr::str_remove_all(" ")

  list_opts <- purrr::map(setNames(selectors, selectors), ~ {
    select_name <- .x
    hc$x$hc_opts$series %>%
      purrr::map(~ {
        purrr::map(purrr::pluck(.x, "data"), ~ {
          purrr::pluck(.x, select_name)
        }) %>%
          unlist()
      }) %>%
      unlist() %>%
      unique()
  })

  select_options <- list_opts %>%
    purrr::imap(~ {
      htmltools::tags$select(
        style = "display:inline-block",
        id = paste0(.y, rand_id_begin),
        purrr::map(.x, ~ {
          if ((!is.null(selected) & .x %in% selected)) {
            htmltools::tags$option(value = ., ., `selected` = TRUE)
          } else {
            htmltools::tags$option(value = ., .)
          }
        })
      )
    })

  names(list_opts) %>%
    purrr::map_chr(~ {
      glue::glue("var select_{paste0(.x, rand_id_begin)} = document.getElementById('{paste0(.x, rand_id_begin)}');")
    }) %>%
    paste(collapse = "") -> var_declaration

  names(list_opts) %>%
    purrr::map_chr(~ {
      glue::glue("obj.{.x} == select_{paste0(.x, rand_id_begin)}.value")
    }) %>%
    paste(collapse = " & ") -> filter_declaration

  names(list_opts) %>%
    purrr::map_chr(~ {
      glue::glue("select_{paste0(.x, rand_id_begin)}.onchange = updateChart;")
    }) %>%
    paste(collapse = "") -> onchg_events

  js_fun <- "function(){{
  var this_chart = this;
  const cloneData = (sample) => {{ return JSON.parse(JSON.stringify(sample));}}
  const init_data = [];
  this_chart.options.series.map((series,index)=>{{
    init_data[index] = cloneData(series.data);
  }})

  // init_data = cloneData(this_chart.options.series[0].data);

  {var_declaration}

  function updateChart(){{
      init_data.map((series,index)=>{{
      new_data = series.filter(function(obj){{
        return {filter_declaration}
        }});

      if(new_data.length>0){{
        this_chart.series[index].setData(new_data);
      }}

      }})

    this_chart.reflow();

   }}
  {onchg_events}
  updateChart();
  }}"

  highcharter::hc_chart(hc,
                        events = list(
                          load = highcharter::JS(glue::glue(js_fun))
                        )
  ) %>%
    htmltools::tagList(
      select_options, .
    ) %>%
    htmltools::browsable()
}

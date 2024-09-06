




quick_dfExtract_comparison = function(var){
  list(
    data = list(data_archived, data_new, data_new_not_in_old)
    ,sffx = c("old", "new", "ex")
  ) %>%
    pmap(
      ~{
        var_n = str_glue("n_{.y}")
        var_pct = str_glue("pct_{.y}")
        var_ratio = str_glue("ratio_{.y}")

        .x %>%
          count(across({{var}}), name = var_n) %>%
          ungroup() %>%
          mutate(
            !!var_pct := dgt2(!!as.symbol(var_n)/sum(!!as.symbol(var_n)))
            ,!!var_ratio := dgt2(1/!!as.symbol(var_pct)))
      }
    ) %>%
    reduce(merge, all = T)
}

quick_dfExtract_comparison_sum = function(
    compare_object, var_group, var_agg, strip = F){

  list(
    data = list(compare_object$data_archived, compare_object$data_new, compare_object$data_new_not_in_old)
    ,sffx = c("old", "new", "ex")
  ) %>%
    pmap(
      ~{
        # browser()
        # var_n = str_glue("n_{.y}")
        # var_pct = str_glue("pct_{.y}")
        # var_ratio = str_glue("ratio_{.y}")

        temp_data = .x

        if (strip){
          temp_data = temp_data %>%
            select(accident_id_pro, all_of(var_group), all_of(var_agg)) %>%
            unique()
        }


        col_pct = str_glue("{var_agg}_{.y}_n")
        cols_rat = str_glue("{var_agg}_{.y}_pct")

        temp_data %>%
          group_by(across({{var_group}})) %>%
          summarise(
            across({{var_agg}}, sum, .names = "{var_agg}_{.y}_n")
          ) %>%
          mutate(
            across(col_pct, ~.x/sum(.x), .names = "{gsub('_n', '', col_pct)}_pct")
            ,across(col_pct, ~1/(.x/sum(.x)), .names = "{gsub('_n', '', col_pct)}_ratio")

          ) %>%
          mutate(across(c(ends_with("_pct")), round, 2)) %>%
          mutate(across(c(ends_with("_ratio")), round, 1))

      }
    ) %>%
    reduce(merge, all = T) %>%
    select(all_of(var_group), ends_with("_n"), ends_with("_pct"), ends_with("_ratio"))
}


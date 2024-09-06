


library(gauntlet)

pkgs = c("tibble", "tidyverse", "lubridate", "data.table", "reactable", "magrittr"
         ,"here", "tidymodels", "crosstalk", "plotly", "broom", "speedglm"
         ,"caret", "marginaleffects", "reactable", "cluster", "furrr")

package_load(pkgs)



# practice======================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mtcars %>% 
  select(vs, hp, am, drat)

## make model======================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#basic glm model 
mod <- glm(vs ~ hp + am + drat + gear, data = mtcars, family = binomial)

## bulk predictions=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# These make the same
#note: both methods to supply new data here produce the same result
#----- per data row grid for each prediction
method_var = predictions(
  mod
  ,newdata = mtcars[c(1, 19),]
  ,variables = list(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
  )
  # ,type = "response"
) %>% 
  arrange(am, drat,  hp) %>% 
  data.frame()

method_dg$R = predictions(
  mod
  ,newdata = datagrid(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
    ,grid_type = "counterfactual")
  # ,type = "response"
) %>% 
  dplyr::select(vs, hp, am, drat, everything()) %>% 
  arrange(w) %>%
  data.frame()

## average predictions==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#now averages
#these also equal the same

tmp_index = c(100, 150, 200) 
index_max_levels = tmp_index
tmp = "hp"
tmp_index %>% 
  map_df(~{
    # print(.x)
    
    avg_predictions(
      mod
      ,variables = list(
        hp = .x
        # ,drat = c(3.5, 4.3)
      )
    )
  })

avg_predictions(
  mod
  ,variables = list(
    hp = tmp_index
  ))

plot_predictions(
  mod
  ,by = "hp"
  ,byfun = mean
  ,newdata = datagrid(
    hp = c(100, 150, 200)
    # ,drat = c(3.5, 4.3)
    ,grid_type = "counterfactual")
)


avg_predictions(
  mod
  ,condition = tmp
  ,variables = list(var = shorten_seq(max_level)) %>%
    setNames(tmp)
) 

predictions(
  mod
  ,by = c('hp', 'drat')
  ,byfun = mean
  ,newdata = datagrid(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
    ,grid_type = "counterfactual")
) %>% arrange(drat,  hp)

predictions(
  mod
  ,newdata = datagrid(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
    ,grid_type = "counterfactual")
) %>% group_by(hp, drat) %>% summarise(mean = mean(estimate)) %>% ungroup()

## conditional vs by==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_predictions(
  mod
  ,by = c('hp')
  ,byfun = mean
  ,newdata = datagrid(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
    ,grid_type = "counterfactual")
) 

plot_predictions(
  mod
  ,by = c('drat')
  ,byfun = mean
  ,newdata = datagrid(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
    ,grid_type = "counterfactual"
  )
) 

plot_predictions(
  mod
  ,condition = "drat"
  ,newdata = datagrid(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
    ,grid_type = "counterfactual")
) 

plot_predictions(
  mod
  ,condition = "drat"
  ,newdata = datagrid(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
    ,grid_type = "counterfactual")
) 



# practice======================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## make model======================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mod = trained_models %>% 
  ungroup() %>% 
  .['mod'] %>% 
  .[[1]] %>% 
  .[[1]]

agumented_data = trained_models %>% 
  ungroup() %>% 
  .['agumented_data'] %>% 
  .[[1]] %>% 
  .[[1]]

agumented_data_sm = agumented_data %>% 
  # mutate(count = 1) %>%
  group_by(across(colnames(agumented_data))) %>% 
  summarise(count = n()) %>% 
  ungroup()

agumented_data_smmry = agumented_data %>% 
  dplyr::select(response_var, all_of(index_varImp)) %>% 
  # group_by(response_var) %>% 
  skimr::skim() %>% 
  data.frame()

index_varImp = trained_models_1 %>% 
  ungroup() %>% 
  .['index_varImp_list'] %>% 
  .[[1]] %>% 
  .[[1]] %>% 
  .$index_varImp

agumented_data %>% 
  mutate(across(all_of(index_varImp), ~1)) %>% 
  pull(Speeding) 



## bulk predictions=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# These make the same
#note: both methods to supply new data here produce the same result
#----- per data row grid for each prediction
method_var = predictions(
  mod
  ,variables = list(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
  )
  # ,type = "response"
) %>% arrange(am, drat,  hp)

# method_dg = 
plot_predictions(
  mod
  ,condition = 'Speeding'
) 

plot_predictions(
  mod
  ,by = 'Speeding'
  ,newdata = datagrid(
    # Fail_Due_Careless = c(0, agumented_data_smmry[agumented_data_smmry$skim_variable == "Fail_Due_Careless"
    #                                               , "numeric.p100"])
    Speeding = c(0:agumented_data_smmry[agumented_data_smmry$skim_variable == "Speeding"
                                        , "numeric.p100"])
    # ,Fail_Comply_Order = c(0, agumented_data_smmry[agumented_data_smmry$skim_variable == "Fail_Comply_Order"
    #                                                , "numeric.p100"])
    # Unlicensed_Suspended = c(0, agumented_data_smmry[agumented_data_smmry$skim_variable == "Unlicensed_Suspended"
    #                                                , "numeric.p100"])
    ,grid_type = "counterfactual")
  # ,type = "response"
)

plot_predictions(
  mod
  ,condition = tmp
) %>% ggplotly()

tmp = "Speeding"
tmp = "Unlicensed_Suspended"

max(agumented_data[,..tmp])

tmp_z = agumented_data %>% 
  mutate(count = 1) %>% 
  count_percent_zscore(
    grp_c = tmp, grp_p = c(), col = count, rnd = 3) %>% 
  mutate(count_cum = cumsum(count)) %>% 
  arrange(desc(!!as.symbol(tmp))) %>% 
  mutate(count_cum_inv = cumsum(count)) %>% 
  arrange(!!as.symbol(tmp)) %>% 
  rename(level = tmp)  
  
tmp = c("Speeding", "Unlicensed_Suspended")
avg_predictions(
  mod
  ,by = c(tmp)
  ,variables = list(
    var = c(0:6)
    ,var_1 = c(0:10)
    ) %>% 
    setNames(tmp)
) %>% 
  data.frame() %>% 
  rename(level = tmp) %>% 
  mutate(var = tmp) %>% 
  merge(tmp_z
        ,by = "level", all.x = T)

## average predictions==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#now averages
#these also equal the same
avg_predictions(
  mod
  ,variables = list(Speeding = c(0:6))
)

tictoc::tic()
avg_predictions(
  mod
  ,by = "Unlicensed_Suspended"
  ,newdata = agumented_data_sm
  ,wts = agumented_data_sm$count
  ,variables = list(Unlicensed_Suspended = 0:19)
)
tictoc::toc()
  
predictions(
  mod
  ,by = c('hp', 'drat')
  ,byfun = mean
  ,newdata = datagrid(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
    ,grid_type = "counterfactual")
) %>% arrange(drat,  hp)

predictions(
  mod
  ,newdata = datagrid(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
    ,grid_type = "counterfactual")
) %>% group_by(hp, drat) %>% summarise(mean = mean(estimate)) %>% ungroup()

## average predictions==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_predictions(
  mod
  ,by = c('hp')
  ,byfun = mean
  ,newdata = datagrid(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
    ,grid_type = "counterfactual")
) 

plot_predictions(
  mod
  ,by = c('drat')
  ,byfun = mean
  ,newdata = datagrid(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
    ,grid_type = "counterfactual"
  )
) 

plot_predictions(
  mod
  ,condition = "drat"
  ,newdata = datagrid(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
    ,grid_type = "counterfactual")
) 

plot_predictions(
  mod
  ,condition = "drat"
  ,newdata = datagrid(
    hp = c(100, 150)
    ,drat = c(3.5, 4.3)
    ,grid_type = "counterfactual")
) 






















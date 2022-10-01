
# add functions to execute analysis with ::::::::::::::::::

# simple macro plot ------------------------------

fun_plt_macro_trend1 <- function(arg_df = dfplt, 
                                 arg_pltnm = pltname) {
  dff_yr <- arg_df |>
    group_by(time_yr) |>
    summarise(mean_measure = mean(measure_val), 
              xstart = min(time_dt), 
              xend = max(time_dt))
  p1 <- arg_df |> 
    ggplot(aes(x = time_dt, y = measure_val)) + 
    geom_step(aes(color = as.factor(time_yr))) + 
    geom_segment(data = dff_yr, 
                 aes(x = xstart, y = mean_measure, 
                     xend = xend, yend = mean_measure, 
                     color = as.factor(time_yr)), 
                 linetype = 2) +
    geom_smooth(color = 'black', size = 0.5, se = FALSE) + 
    labs(subtitle = arg_pltnm, 
         # caption = etl_metadata, 
         y = measurement, x = '')
  p2 <- my_gg(p1) + 
    theme(legend.position = 'none')
  return(p2)
}

# tests ?????????????????????????
# fun_plt_macro_trend1()

# macro plot trend but sum the years instead of month grain -----

fun_plt_macro_trend2 <- function(arg_df = dfplt, 
                                 arg_pltnm = pltname, 
                                 arg_months = c(1, 1)) {
  
  caption_txt <- ('Sum of Measurement From Month Number ' %ps% 
                    arg_months[[1]] %ps% 
                    ' to ' %ps% arg_months[[2]])
  
  p1 <- arg_df |> 
    filter(time_mon_num >= arg_months[[1]] & 
             time_mon_num <= arg_months[[2]]) |> 
    group_by(time_yr) |> 
    summarise(measure_sum = sum(measure_val)) |> 
    ggplot(aes(x = time_yr, group = 1, 
               y = measure_sum)) + 
    geom_line(size = 0.9) + 
    geom_point(size = 3) + 
    labs(y = measurement, x = '', 
         subtitle = caption_txt, 
         title = arg_pltnm)
  
  p1 <- my_gg(p1)
  
  return(p1)
}

# tests ?????????????????????????????
# fun_plt_macro_trend2()
# fun_plt_macro_trend2(arg_months = c(1, 6))

# simple yoy macro plot --------------------------

fun_plt_macro_yoy1 <- function(arg_df = dfplt, 
                                 arg_pltnm = pltname) {
  p1 <- arg_df |> 
    ggplot(aes(x = time_mon_num, y = measure_val)) + 
    geom_line(aes(color = as.factor(time_yr)), 
              size = 0.9) + 
    geom_point(aes(color = as.factor(time_yr)), 
               size = 2) + 
    scale_x_continuous(breaks = seq(1, 12, 1)) + 
    scale_color_brewer(palette = 'Set1') + 
    labs(subtitle = arg_pltnm, color = '', 
         y = measurement, x = 'Month')
  p2 <- my_gg(p1)
  return(p2)
}

# tests ????????????????????????????
# fun_plt_macro_yoy1()

# index against us total exhibit -------------

fun_plt_index1 <- function(arg_df = dfplt, 
                           arg_pltnm = pltname, 
                           arg_indexyr = 2015, 
                           arg_measurement = measurement) {
  
  inter_fun <- function(argg_dff) {
    aa <- argg_dff |> 
      filter(time_yr == arg_indexyr) |> 
      group_by(time_yr) |> 
      summarise(mean_val = mean(measure_val))
    index_val <- max(aa[1, 2])
    bb <- argg_dff |>
      mutate(measure_val_var = measure_val / index_val)
    return(bb)
  }
  
  df1 <- dfa |> 
    filter(is_adj == FALSE) |> 
    filter(max_grain_ind == TRUE, 
           dt_desc == measurement, 
           time_yr >= arg_indexyr) 
  
  df1 <- inter_fun(df1)
  df2 <- inter_fun(arg_df)
  
  p1 <- ggplot() + 
    geom_hline(aes(yintercept = 1), linetype = 2) + 
    geom_line(data = df1, 
              aes(x = time_dt, y = measure_val_var), 
              size = 1.1) + 
    geom_line(data = df2, 
              aes(x = time_dt, y = measure_val_var), 
              color = 'red') + 
    labs(caption = 'Indexed against ' %ps% arg_indexyr, 
         y = arg_measurement, 
         x = '', title = arg_pltnm, 
         subtitle = 'Black = US Total Baseline; ' %ps% 
           'Red = Filtered Cell Comparison; ')
  
  p1 <- my_gg(p1) + 
    theme(legend.position = 'none')
  
  return(p1)
}

# tests ????????????????????????????
# fun_plt_index1()



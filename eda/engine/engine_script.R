
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
    geom_step(aes(color = as.factor(time_yr)), 
              size = 1) + 
    geom_segment(data = dff_yr, 
                 aes(x = xstart, y = mean_measure, 
                     xend = xend, yend = mean_measure, 
                     color = as.factor(time_yr)), 
                 linetype = 2, size = 1) +
    geom_smooth(color = 'black', size = 0.8, se = FALSE) + 
    labs(subtitle = arg_pltnm, 
         # caption = etl_metadata, 
         y = measurement, x = '')
  p2 <- my_gg(p1) + 
    theme(legend.position = 'none')
  return(p2)
}

# tests ?????????????????????????
fun_plt_macro_trend1()

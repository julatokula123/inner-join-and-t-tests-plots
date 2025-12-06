
#zrobienie id uczestnika dla wyklad t1
library(dplyr)

wyklad_t1 <- wyklad_t1 |>
  mutate(id_t1 = paste0(Code_1, Code_2, Code_3, Code_4))


#zrobienie id uczestnika dla wyklad t2 


wyklad_t2 <- wyklad_t2 |>
  mutate(id_t2 = paste0(Code_1, Code_2, Code_3, Code_4))


#zrobienie w wyklad t1 w id_t1 malych liter 
wyklad_t1 <- wyklad_t1 |>
  mutate(id_t1 = tolower(id_t1))


#zrobienie w wyklad t2 w id_t2 malych liter 
wyklad_t2 <- wyklad_t2 |>
  mutate(id_t2 = tolower(id_t2))

#łaczenie tabeli 

wyklad_t1 <- wyklad_t1 |> rename(id = id_t1)
wyklad_t2 <- wyklad_t2 |> rename(id = id_t2)

wyklad_t1_2 <- inner_join(wyklad_t1, wyklad_t2, by = "id")


#Test T studenta 

library(dplyr)
library(broom)
library(ggplot2)
library(purrr)
library(tidyr)

# lista par zmiennych
pairs <- list(
  c("Gender_ess_1.x", "Gender_ess_1.y"),
  c("GE_personal.x",  "GE_personal.y"),
  c("Gender_ess_2.x", "Gender_ess_2.y"),
  c("GE_action.x",    "GE_action.y"),
  c("Zero_sum.x",     "Zero_sum.y")
)

# funkcja do t-testu zależnych dla jednej pary
run_ttest <- function(var_x, var_y) {
  t.test(
    wyklad_t1_2[[var_x]],
    wyklad_t1_2[[var_y]],
    paired = TRUE
  ) |>
    broom::tidy() |>
    mutate(
      variable = gsub("\\.x$", "", var_x),  # poprawione
      var_x = var_x,
      var_y = var_y
    )
}

# 1) 5x t tests
results <- map_df(pairs, ~ run_ttest(.x[1], .x[2]))

# 2) TABELA WYNIKOWA
results_table <- results |>
  select(variable, statistic, p.value, parameter, estimate) |>
  mutate(significant = ifelse(p.value < .05, "TAK", "NIE"))

print(results_table)

# 3) WYKRESY boxplot T1 vs T2 dla każdej zmiennej
plot_list <- map(pairs, function(pair) {
  var_x <- pair[1]
  var_y <- pair[2]
  var_name <- gsub("\\.x$", "", var_x)
  
  df <- wyklad_t1_2 |>
    select(all_of(var_x), all_of(var_y)) |>
    pivot_longer(cols = everything(), names_to = "time", values_to = "value") |>
    mutate(time = ifelse(time == var_x, "T1", "T2"))
  
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_boxplot() +
    ggtitle(paste("Różnice T1 vs T2:", var_name)) +
    theme_minimal()
  
  return(p)
})

# wyświetlenie wszystkich wykresów
plot_list


options(scipen = 999)


library(gt)

results_table |>
  gt() |>
  fmt_number(columns = c(estimate, statistic, p.value, conf.low, conf.high), decimals = 4)


#clean table 

library(dplyr)
library(gt)

# zaokrąglenie liczb do 3 miejsc po przecinku
results_table_clean <- results_table |>
  mutate(
    statistic  = round(statistic, 3),
    p.value    = round(p.value, 3),
    estimate   = round(estimate, 3),
    parameter  = round(parameter, 0)
  )

# tabela GT
results_table_clean |>
  gt() |>
  cols_label(
    variable  = "Zmienna",
    statistic = "t",
    parameter = "df",
    estimate  = "Estymata",
    p.value   = "p-value",
    significant = "Istotne?"
  ) |>
  tab_header(
    title = "Wyniki testów t dla prób zależnych wyklad-wyklad"
  )


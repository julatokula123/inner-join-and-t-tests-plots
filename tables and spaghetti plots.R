


library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)  
library(gt)


# pairs variables


pairs <- list(
  c("Gender_ess_1.x", "Gender_ess_1.y"),
  c("GE_personal.x",  "GE_personal.y"),
  c("Gender_ess_2.x", "Gender_ess_2.y"),
  c("GE_action.x",    "GE_action.y"),
  c("Zero_sum.x",     "Zero_sum.y"),
  c("mean_Gender_ess_1", "mean_Gender_ess_2")
)


# t test function

run_ttest <- function(var_x, var_y) {
  t.test(
    wyklad_t1_2[[var_x]],
    wyklad_t1_2[[var_y]],
    paired = TRUE
  ) |>
    broom::tidy() |>
    mutate(
      variable = gsub("\\.x$", "", var_x),
      var_x = var_x,
      var_y = var_y,
      significant = ifelse(p.value < 0.05, "TAK", "NIE")
    )
}

# run tests


results <- map_df(pairs, ~run_ttest(.x[1], .x[2]))


#  tabela wynikowa (ładna)


results_table <- results |>
  select(variable, statistic, p.value, parameter, estimate, significant) |>
  mutate(
    statistic = round(statistic, 3),
    p.value   = round(p.value, 3),
    estimate  = round(estimate, 3),
    parameter = round(parameter, 0)
  )

results_table |>
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
    title = "Wyniki testów t dla prób zależnych – wykład/warsztat"
  )


# tabela śr + SD dla wszystkich zmiennych (T1 i T2)


mean_sd_table <- map_df(pairs, function(pair) {
  var_x <- pair[1]
  var_y <- pair[2]
  var_name <- gsub("\\.x$", "", var_x)
  
  tibble(
    variable = var_name,
    mean_T1 = mean(wyklad_t1_2[[var_x]], na.rm = TRUE),
    sd_T1   = sd(wyklad_t1_2[[var_x]], na.rm = TRUE),
    mean_T2 = mean(wyklad_t1_2[[var_y]], na.rm = TRUE),
    sd_T2   = sd(wyklad_t1_2[[var_y]], na.rm = TRUE)
  )
})

mean_sd_table |>
  mutate(across(where(is.numeric), ~round(., 3))) |>
  gt() |>
  tab_header(title = "Średnie i odchylenia standardowe T1 vs T2")


# spaghetti plots


spaghetti_plots <- map(pairs, function(pair) {
  var_x <- pair[1]
  var_y <- pair[2]
  var_name <- gsub("\\.x$", "", var_x)
  
  df <- wyklad_t1_2 |>
    select(all_of(var_x), all_of(var_y)) |>
    mutate(id = row_number()) |>
    pivot_longer(cols = c(var_x, var_y), names_to = "time", values_to = "value") |>
    mutate(time = ifelse(time == var_x, "T1", "T2"))
  
  ggplot(df, aes(x = time, y = value, group = id)) +
    geom_line(alpha = 0.3) +
    geom_point(size = 2) +
    ggtitle(paste("Zmiana T1 → T2:", var_name)) +
    theme_minimal()
})

spaghetti_plots  #  lista wykresów

#dodatkowa analiza pod WYKLAD T1 i PO WARSZTACIE 
#PRZYGOTOWANIE TABELI POD ANALIZE###########################################
#zrobienie id uczestnika dla po warsztatcie
library(dplyr)

po_warsztacie <- po_warsztacie |>
  mutate(id_war = paste0(Code_1, Code_2, Code_3, Code_4))


#zrobienie w po warsztacie w id war malych liter 
po_warsztacie <- po_warsztacie |>
  mutate(id_war = tolower(id_war))


#łaczenie tabel


po_warsztacie <- po_warsztacie |> rename(id = id_war)

wyklad_war <- inner_join(wyklad_t1, po_warsztacie, by = "id")


#zrobienie nowej zmiennej i polaczenie ze stara tabela

library(dplyr)
library(broom)
library(gt)

# 1) Tworzymy średnie zmienne
wyklad_war <- wyklad_war |>
  mutate(
    mean_Gender_ess_1 = (Gender_ess_1.x + Gender_ess_2.x)/2,
    mean_Gender_ess_2 = (Gender_ess_2.x + Gender_ess_2.y)/2
  )


#KONIEC PRZYGOTOWANIA ############################################

library(dplyr)
library(purrr)
library(broom)
library(gt)

#DODATKOWE ZMIENNE
wyklad_war <- wyklad_war |>
  mutate(
    mean_Gender_ess_1 = (Gender_ess_1.x + Gender_ess_1.y) / 2,
    mean_Gender_ess_2 = (Gender_ess_2.x + Gender_ess_2.y) / 2
  )


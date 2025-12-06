#wykład t1 t2 z dodatkowymi informacjami i wykresami 

#PRZYGOTOWANIE TABEL POD ANALIZE ################################################

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

#łaczenie tabel

wyklad_t1 <- wyklad_t1 |> rename(id = id_t1)
wyklad_t2 <- wyklad_t2 |> rename(id = id_t2)

wyklad_t1_2 <- inner_join(wyklad_t1, wyklad_t2, by = "id")


#KONIEC PRZYGOTOWANIA ########################################################

library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(gt)
library(ggplot2)

#-------------------------------------------------------
# 0) Dodajemy zmienne mean_Gender_ess_1 i mean_Gender_ess_2
#-------------------------------------------------------

wyklad_t1_2 <- wyklad_t1_2 |>
  mutate(
    mean_Gender_ess_1 = (Gender_ess_1.x + Gender_ess_1.y) / 2,
    mean_Gender_ess_2 = (Gender_ess_2.x + Gender_ess_2.y) / 2
  )


library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)  
library(gt)

#-------------------------------------------------------
# 1) Lista par zmiennych (dodajemy mean-y jako szóstą parę)
#-------------------------------------------------------

pairs <- list(
  c("Gender_ess_1.x", "Gender_ess_1.y"),
  c("GE_personal.x",  "GE_personal.y"),
  c("Gender_ess_2.x", "Gender_ess_2.y"),
  c("GE_action.x",    "GE_action.y"),
  c("Zero_sum.x",     "Zero_sum.y"),
  c("mean_Gender_ess_1", "mean_Gender_ess_2")
)

#-------------------------------------------------------
# 2) Funkcja do t-testu zależnego
#-------------------------------------------------------

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

#-------------------------------------------------------
# 3) Uruchomienie testów
#-------------------------------------------------------

results <- map_df(pairs, ~run_ttest(.x[1], .x[2]))

#-------------------------------------------------------
# 4) Tabela wynikowa (ładna)
#-------------------------------------------------------

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

#-------------------------------------------------------
# 5) Tabela śr + SD dla wszystkich zmiennych (T1 i T2)
#-------------------------------------------------------

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

#-------------------------------------------------------
# 6) Wykres spaghetti (połączone punkty T1 → T2)
#-------------------------------------------------------

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

spaghetti_plots  # wyświetla listę wykresów

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


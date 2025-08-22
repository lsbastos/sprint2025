library(tidyverse)
library(INLA)
source("code/sprint_fun.R")


dengue <- read_csv("data/dengue.csv.gz")

selufs <- sort(unique(dengue$uf))

dengue <- dengue %>% 
  filter(uf %in% selufs)

dengue.tbl <- dengue %>% 
  group_by(uf, macroregional_geocode) %>% tally() %>% 
  group_by(uf) %>% 
  mutate( n = n())

# For replicability purposes
set.seed(42)

# Macroregioes ------------------------------------------------------------


# Criando as listas

macros <- unique(dengue$macroregional_geocode)

list.forecast = vector(mode = "list", length = length(macros))
names(list.forecast) = macros


#k = 1
for(k in 1:length(macros)){
  
  data.train.macro.k = dengue %>% 
    filter(
      macroregional_geocode == macros[k]
    ) %>% 
    group_by(Date = date, macroregional_geocode, uf) %>% 
    summarise(
      cases = sum(casos),
      train = TRUE, target = FALSE, .groups = "drop"
    ) 
  
  data.train.macro.k = data.train.macro.k |> 
    bind_rows(
      tibble(
        Date = ymd("2025-10-05") + 7*(0:52),
        macroregional_geocode = data.train.macro.k$macroregional_geocode[1], 
        uf = data.train.macro.k$uf[1],
        cases = NA,
        train = FALSE,
        target = TRUE)
    )
  
  
  # Forescasting target 3
  aux <- forecasting.inla(dados = data.train.macro.k %>% 
                            filter(Date >= "2015-10-11"), 
                          MC =T)
  aux$pred$uf = data.train.macro.k$uf[1]
  aux$pred$macrocode = data.train.macro.k$macroregional_geocode[1]
  
  aux$MC$uf = data.train.macro.k$uf[1]
  aux$MC$macrocode = data.train.macro.k$macroregional_geocode[1]
  
  list.forecast[[k]]$out <- aux
  
  cat(k, data.train.macro.k$uf[1] , 
      data.train.macro.k$macroregional_geocode[1], "\n")
  
}


df.forecast <- list.forecast %>% map(function(x) x$out$MC) %>% bind_rows()

# saveRDS(df.forecast, file = "forecasts/samples/sprint2025_forecast.rds")
# df.forecast = readRDS(file = "forecasts/samples/sprint2025_forecast.rds")



# Forecast

tbl.total.uf.forecast <- df.forecast %>%
  group_by(uf, samples) %>%
  summarise(
    values = sum(values)
  ) %>% group_by(uf) %>%
  summarise(
    pred = median(values),
    lower_95 =  quantile(values, probs = 0.025),
    lower_90 =  quantile(values, probs = 0.05),
    lower_80 = quantile(values, probs = 0.10),
    lower_50 =  quantile(values, probs = 0.25),
    upper_50 =  quantile(values, probs = 0.75),
    upper_80 = quantile(values, probs = 0.9),
    upper_90 =  quantile(values, probs = 0.95),
    upper_95 = quantile(values, probs = 0.975),
  ) %>%
  bind_rows(
    tibble(uf = "BR") %>% bind_cols(df.forecast %>%
                                      group_by(samples) %>%
                                      summarise(
                                        values = sum(values)
                                      ) %>% #group_by(uf) %>%
                                      summarise(
                                        pred = median(values),
                                        lower_95 =  quantile(values, probs = 0.025),
                                        lower_90 =  quantile(values, probs = 0.05),
                                        lower_80 = quantile(values, probs = 0.10),
                                        lower_50 =  quantile(values, probs = 0.25),
                                        upper_50 =  quantile(values, probs = 0.75),
                                        upper_80 = quantile(values, probs = 0.9),
                                        upper_90 =  quantile(values, probs = 0.95),
                                        upper_95 = quantile(values, probs = 0.975),                                      )
    )
  )


tbl.uf.week.forecast <- df.forecast %>% 
  group_by(uf, week, samples) %>% 
  summarise(
    values = sum(values)
  ) %>% group_by(uf, week) %>% 
  summarise(
    pred = median(values),
    lower_95 =  quantile(values, probs = 0.025),
    lower_90 =  quantile(values, probs = 0.05),
    lower_80 = quantile(values, probs = 0.10),
    lower_50 =  quantile(values, probs = 0.25),
    upper_50 =  quantile(values, probs = 0.75),
    upper_80 = quantile(values, probs = 0.9),
    upper_90 =  quantile(values, probs = 0.95),
    upper_95 = quantile(values, probs = 0.975),
  ) %>% 
  bind_rows(
    tibble(uf = "BR") %>%
      bind_cols(df.forecast %>%
                  group_by(week, samples) %>%
                  summarise(
                    values = sum(values)
                  ) %>% group_by(week) %>%
                  summarise(
                    pred = median(values),
                    lower_95 =  quantile(values, probs = 0.025),
                    lower_90 =  quantile(values, probs = 0.05),
                    lower_80 = quantile(values, probs = 0.10),
                    lower_50 =  quantile(values, probs = 0.25),
                    upper_50 =  quantile(values, probs = 0.75),
                    upper_80 = quantile(values, probs = 0.9),
                    upper_90 =  quantile(values, probs = 0.95),
                    upper_95 = quantile(values, probs = 0.975),
                  )
      )
  )

write_csv(tbl.uf.week.forecast, file = "forecasts/tbl.sprint.uf.week.forecast.csv")





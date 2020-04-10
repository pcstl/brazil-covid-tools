library(tidyverse)

### 1. baixar o CSV de casos disponível em https://covid.saude.gov.br/
### 2. abrir o arquivo e renomear o campo "região" no header para "reg" (o R não consegue lidar direito com o ~)
### 3. substituir o nome do arquivo no argumento file abaixo
read.table(file = "filepath", header=TRUE, sep=";", stringsAsFactors = FALSE) %>%
mutate(date = as.Date(data, format = "%d/%m/%Y"),
           state = sigla
           region = reg) %>%
    select(-data, -sigla, -reg, -casosAcumulados, -obitosAcumulados) %>%
    rename(new_cases = casosNovos,
           new_deaths = obitosNovos) %>%
    arrange(date) %>%
    as_tibble() %>% {

brazil_aggregate <- group_by(., date) %>%
    summarize(new_cases = sum(new_cases),
              new_deaths = sum(new_deaths)) %>%
    ungroup() %>%
    mutate(total_cases = cumsum(new_cases),
           total_deaths = cumsum(new_deaths),
           region = as.factor("Brasil")) %>%
    as_tibble()

by_state <-  group_by(., state) %>%
    mutate(total_cases = cumsum(new_cases),
           total_deaths = cumsum(new_deaths)) %>%
    ungroup() %>%
    mutate(region = state) %>%
    select(-state) %>%
    as_tibble()

by_region <- group_by(., region) %>%
    mutate(total_cases = cumsum(new_cases),
           total_deaths = cumsum(new_deaths)) %>%
    ungroup() %>%
    select(-state) %>%
    as_tibble()

brazil_covid_data <<- bind_rows(brazil_aggregate, by_state, by_region) %>%
    mutate(region = as.factor(region))
    
    }

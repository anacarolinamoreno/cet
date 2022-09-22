

library(tidyverse)
library(tidylog)

# Source of the files: http://mobilidadesegura.prefeitura.sp.gov.br/QvAJAXZfc/opendoc.htm?document=Painel_Mobilidade_Segura.qvw&host=QVS%40c65v27i&anonymous=true

sheets <- list.files("raw", full.names =TRUE)


for (i in 1:length(sheets)) {
  df <- read.csv2(sheets[i])

  if (i==1) {
    big_df <- df
  } else {
    big_df <- rbind(big_df, df)
  }

  print(nrow(big_df))
}


# Data Wrangling

clean_df <- big_df %>%
  rename(dia = Dia,
         ano = Ano,
         mes = Mês,
         dia_da_semana = Dia.Semana,
         hora = Hora,
         tipo_de_infracao = Tipo.Infração,
         enquadramento = Enquadramento,
         local = Local,
         quantidade_infracoes = Qtd..Infrações
  ) %>%
  filter(!is.na(ano)) %>%
  mutate(quantidade_infracoes = as.integer(quantidade_infracoes))

# Data analysis

# 1- Grouping sum of fines per year (from January till June)

year <- clean_df %>%
  group_by(ano) %>%
  summarise(total = sum(quantidade_infracoes)) %>%
  pivot_wider(names_from = ano, values_from = total) %>%
  mutate(pct_var = round(( ( `2022` - `2021`) * 100 ) / `2021`,1),
         pct_var_21_22 = str_c(pct_var, "%")) %>%
  select(-pct_var)

write.csv2(year, "data/multas_ano.csv",
           fileEncoding = "Windows-1252", row.names = F)

# 2- Grouping sum of fines per month and year (from January till June)

month <- clean_df %>%
  group_by(ano, mes) %>%
  summarise(total = sum(quantidade_infracoes)) %>%
  pivot_wider(names_from = mes, values_from = total) %>%
  select(ano, jan, fev, mar, abr, mai, jun)

write.csv2(month, "data/multas_mes_ano.csv",
           fileEncoding = "Windows-1252", row.names = F)


# 3- Grouping sum of fines per type of infraction per year

infraction <- clean_df %>%
  group_by(ano, enquadramento) %>%
  summarise(total = sum(quantidade_infracoes)) %>%
  pivot_wider(names_from = ano, values_from = total) %>%
  arrange(desc(`2022`))

write.csv2(infraction, "data/multas_por_enquadramento.csv",
           fileEncoding = "Windows-1252", row.names = F)

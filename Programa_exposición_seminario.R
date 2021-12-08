library(pacman)


p_load(tidyverse, openxlsx, janitor, data.table)


datos <- fread("MEN_ESTADISTICAS_EN_EDUCACION_EN_PREESCOLAR__B_SICA_Y_MEDIA_POR_MUNICIPIO.csv", encoding = "UTF-8")
historico <- fread("MEN_MATRICULA_EN_EDUCACION_EN_PREESCOLAR__B_SICA_Y_MEDIA_2018_2020.csv", encoding = "UTF-8" )
colegios <- fread("MEN_ESTABLECIMIENTOS_EDUCATIVOS_PREESCOLAR_B_SICA_Y_MEDIA.csv", encoding = "UTF-8") 
PIB_TOTAL <- read.xlsx("PIB.xlsx", sheet = "PIB") %>% clean_names %>% as.data.table
PIB_TOTAL <- PIB_TOTAL[, codigo_departamento := as.numeric(as.character(codigo_departamento))]


datos <- datos %>% clean_names %>% as.data.table
historico <- historico %>% clean_names %>% as.data.table
colegios <- colegios %>% clean_names %>% as.data.table
names(datos)

datos <- datos[, codigo_municipio := str_pad(codigo_municipio, width = 5, side = "left", pad = "0")]
datos <- datos[ano == 2020]

historico_2020 <- historico[anno_inf == 2020]
historico_2020[, cod_dane_municipio := str_pad(cod_dane_municipio, width = 5, side = "left", pad = "0")]
summary_matriculados <-  historico_2020[, .N, by = .(cod_dane_departamento, departamento, cod_dane_municipio)]
summary_matriculados <- summary_matriculados %>% setnames("N", "N_matriculados")

colegios[, cod_dane_municipio := str_pad(cod_dane_municipio, width = 5, side = "left", pad = "0")]
colegios_2019 <- colegios[ano == 2019]

summary_colegios <- colegios_2019[, .N, by = .(cod_dane_departamento, departamento, cod_dane_municipio, municipio)]
summary_colegios <- summary_colegios %>% setnames("N", "N_colegios")


datos_1 <- datos %>% merge(summary_matriculados[, -c("departamento")],
      by.y = c("cod_dane_departamento", "cod_dane_municipio"),
      by.x = c("codigo_departamento", "codigo_municipio"), all.x = TRUE) %>% 
  merge(summary_colegios[, -c("departamento", "municipio")],
        by.y = c("cod_dane_departamento", "cod_dane_municipio"),
        by.x = c("codigo_departamento", "codigo_municipio"), all.x = TRUE) %>% 
  merge(PIB_TOTAL[, c("codigo_departamento", "x2020pr")],
        by.x = "codigo_departamento",
        by.y = "codigo_departamento", all.x = TRUE)


datos_1 %>% ggplot(aes(N_colegios , desercion_secundaria, color = departamento)) + geom_point() +
  geom_smooth(method = "lm", se = F)+
xlim(0, 1000)  

datos_1 %>% ggplot(aes(N_matriculados , desercion_secundaria, color = departamento)) + geom_point() +
#  geom_smooth(method = "lm", se = F) +
  xlim(0, 1000) +
  guides(color = FALSE)

datos_1 %>% ggplot() +
  geom_point(aes(repitencia_primaria , desercion_secundaria, color = departamento)) +
  geom_smooth(aes(repitencia_primaria , desercion_secundaria), method = "lm") +
  guides(color = F)
#  geom_smooth(method = "lm", se = F)

datos_1 %>% ggplot() +
  geom_point(aes(tasa_matriculacion_5_16 , desercion_secundaria, color = departamento)) +
  geom_smooth(aes(tasa_matriculacion_5_16 , desercion_secundaria), method = "lm") +
  facet_wrap(~departamento, scales = "free") +
  guides(color = F)


datos_1 %>% ggplot() +
  geom_point(aes(x2020pr , desercion_secundaria, color = departamento)) +
  geom_smooth(aes(tasa_matriculacion_5_16 , desercion_secundaria), method = "lm") +
  facet_wrap(~departamento, scales = "free") +
  guides(color = F)


fit <- glm(desercion_secundaria ~ tasa_matriculacion_5_16 + 
             repitencia_primaria +
             reprobacion_secundaria + x2020pr, data = datos_1dassa)
summary(fit)
plot(residuals(fit))

names(datos_1)
#geom_smooth(method = "lm", se = F)


#read.xlsx("PIB.xlsx", sheet = "PIB_PER_CAPITA") %>% clean_names %>% as.data.table


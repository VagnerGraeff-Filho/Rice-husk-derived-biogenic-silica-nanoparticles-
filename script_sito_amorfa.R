# 1 Bibliotecas -------------------------------------------------------------
library(tidyverse)
library(readxl)
library(drc)
library(ggplot2)
library(scales)
library(patchwork)
library(ggpattern)
library(glmmTMB)
library(car)
library(DHARMa)
library(emmeans)

# 2 Importação e exploração -------------------------------------------------

df_sito <- read_excel("Dados_Sitophilus.xlsx", sheet = "Tab_uniao")
df_sito$Data <- as.Date(df_sito$Data, format = "%d/%m/%Y")


# Incluindo as doses e tratamentos

df_sito <- df_sito %>%
  mutate(
    Dose = case_when(
      Tratamento == "A" ~ 0,
      Tratamento == "B" ~ 0.5,
      Tratamento == "C" ~ 1,
      Tratamento == "D" ~ 1.5,
      Tratamento == "E" ~ 2,
      Tratamento == "F" ~ 2.5,
      Tratamento == "G" ~ 0.5,
      Tratamento == "H" ~ 1,
      Tratamento == "I" ~ 1.5,
      Tratamento == "J" ~ 2,
      Tratamento == "K" ~ 2.5,
      TRUE ~ NA_real_ # Valor padrão (caso apareça algum tratamento não previsto)
    )
  )

df_sito$Dose <- as.numeric(df_sito$Dose)
df_sito$Dosefac <- as.factor(df_sito$Dose)

df_sito <- df_sito %>%
  mutate(
    Material = case_when(
      Tratamento == "A" ~ "Control",
      Tratamento == "B" ~ "BSNPs",
      Tratamento == "C" ~ "BSNPs",
      Tratamento == "D" ~ "BSNPs",
      Tratamento == "E" ~ "BSNPs",
      Tratamento == "F" ~ "BSNPs",
      Tratamento == "G" ~ "DE",
      Tratamento == "H" ~ "DE",
      Tratamento == "I" ~ "DE",
      Tratamento == "J" ~ "DE",
      Tratamento == "K" ~ "DE",
      TRUE ~ NA_character_ # Valor padrão (caso apareça algum tratamento não previsto)
    )
  )


### Correção dos grupos de controle

# Separamos o controle do restante dos dados
df_controle <- subset(df_sito, Material == "Control")
ctrl_silica <- df_controle
ctrl_silica$Material <- "BSNPs" # Use o nome exato que está no seu df_sito

# Criamos uma cópia do controle para a Terra de Diatomáceas
ctrl_terra <- df_controle
ctrl_terra$Material <- "DE" # Use o nome exato que está no seu df_sito

# Juntamos tudo novamente (removendo a nomenclatura antiga "controle")
df_sito <- rbind(
  subset(df_sito, Material != "Control"), 
  ctrl_silica, 
  ctrl_terra
)

df_sito$prop_mortos <- df_sito$Mortos / 15
df_sito$Total <- 15


  ## 2.1 Exploração -------------------------------------------------------

resumo <- df_sito |> 
  group_by(Dose, Material, Data) |> 
  summarise(
    Media_Mortos = mean(Mortos, na.rm = TRUE), # Nome novo para a média
    SD_Mortos = round(sd(Mortos, na.rm = TRUE),2),      # Usa a coluna 'Mortos' original intacta
    .groups = "drop"                           # Boa prática para evitar avisos no console
  )


### Soma acumulada
# Substitua 20 pelo número inicial de insetos por unidade experimental/repetição
total_insetos <-15

df_acumulado <- df_sito |> 
  group_by(Data, Dosefac, Material) |> 
  summarise(
    acum    = round(mean(prop_mortos * 100, na.rm = TRUE), 2),
    acum_SE = round(sd(prop_mortos * 100, na.rm = TRUE) / sqrt(sum(!is.na(prop_mortos))), 2),
    acum_SD = round(sd(prop_mortos * 100, na.rm = TRUE), 2),
    .groups = "drop"
  )



df_agrup <- df_sito |> 
  filter(Data == "2026-09-21") |> 
  group_by(Dose, Material) |> 
  summarise(Mean = mean (Mortos))

ggplot(df_agrup, aes(y = Mean, x = Dose, color = Material ))+
  geom_point()+
  geom_line()

df_agrup <- df_sito |> 
  filter(Data == "2026-09-28") |> 
  group_by(Dose, Material) |> 
  summarise(Mean = mean (Mortos))

ggplot(df_agrup, aes(y = Mean, x = Dose, color = Material ))+
  geom_point()+
  geom_line()

df_agrup <- df_sito |> 
  filter(Data == "2026-10-05") |> 
  group_by(Dose, Material) |> 
  summarise(Mean = mean (Mortos))

ggplot(df_agrup, aes(y = Mean, x = Dose, color = Material ))+
  geom_point()+
  geom_line()

# Para o ultimo dia 05/10
df_sito |> 
  filter(Data == "2026-10-05") |> 
  filter(Material == "Amorphous Silica") |> 
ggplot(aes(y = Mortos, x = Dosefac, color = Material ))+
  geom_boxplot()+
  geom_jitter()+
  theme_classic()



# 3 Modelos LC50 ---------------------------------------------------------------

  ## 3.1 Semana 1 -------------------------

df_sem1 <- subset(df_sito, Data == "2026-09-21")
# Criar a coluna de proporção de mortalidade (varia de 0 a 1)


modelo_drc <- drm(prop_mortos ~ Dose, 
                  curveid = Material,
                  weights = Total, 
                  data = df_sem1, 
                  fct = LL.2(), 
                  type = "binomial")

# Resumo do modelo (Mostrará a inclinação e LC50 de CADA material)
summary(modelo_drc)

# 3. EXTRAÇÃO DAS LC's
# Calculará as LCs para ambos os materiais automaticamente
valores_lc <- ED(modelo_drc, c(10, 50, 90), interval = "delta")
print(valores_lc)

# 4. COMPARAÇÃO ESTATÍSTICA DAS LCs (Opcional, mas recomendado)
# Testa se a LC50 da Sílica é significativamente diferente da LC50 da Terra
EDcomp(modelo_drc, c(50, 50), type = "relative")

# Se a sua menor dose testada for, por exemplo, 10, você pode trocar esse '0.1' por '1'
df_sem1$Dose_plot <- ifelse(df_sem1$Dose == 0, 0.1, df_sito_corrigido$Dose)


  ## 3.1  Gráficos modelos LC50
# 1. Garantir que a Dose seja numérica ANTES do agrupamento
df_sem1$Dose <- as.numeric(as.character(df_sem1$Dose))

library(ggplot2)
library(scales) # Necessário para a formatação de porcentagem

# 2. Agrupamento e cálculo de Média e Desvio Padrão
df_sem1.agrup <- df_sem1 |> 
  group_by(Dose, Material) |> 
  summarise(Mean = mean(prop_mortos, na.rm = TRUE),
            SD = sd(prop_mortos, na.rm = TRUE),
            n = n(),
            SE = SD / sqrt(n),
            .groups = "drop") 

plot.sem1 <- ggplot(df_sem1.agrup, aes(x = Dose, y = Mean, shape = Material, group = Material)) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.05, 
                linewidth = 0.7, 
                position = position_dodge(0.05)) +
  geom_line(linewidth = 0.7, position = position_dodge(0.05),
            linetype = 'dashed') +
  geom_point(size = 3, position = position_dodge(0.05), fill = 'white') +
  
  # Conversão do eixo Y para porcentagem (considerando proporção de 0 a 1)
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  
  labs(x = expression("Dose"~(g~kg^{-1})), 
       y = "Mortality (%)", 
       color = "Material") +
  scale_color_manual(values = c("#0072B2", "#D55E00")) +
  scale_shape_manual(values = c(15, 23)) +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 14, face = 'plain'),
    axis.title = element_text(face = "plain", size = 14),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    legend.position = c(0.8, 0.13), 
    legend.title = element_blank(), 
    legend.background = element_rect(fill = "transparent")
  )


  ## 3.2 Semana 2 -------------------------

df_sem2 <- subset(df_sito, Data == "2026-09-28")
# Criar a coluna de proporção de mortalidade (varia de 0 a 1)


modelo_drc <- drm(prop_mortos ~ Dose, 
                  curveid = Material,
                  weights = Total, 
                  data = df_sem2, 
                  fct = LL.2(), 
                  type = "binomial")

# Resumo do modelo (Mostrará a inclinação e LC50 de CADA material)
summary(modelo_drc)

# 3. EXTRAÇÃO DAS LC's
# Calculará as LCs para ambos os materiais automaticamente
valores_lc <- ED(modelo_drc, c(10, 50, 90), interval = "delta")
print(valores_lc)

# 4. COMPARAÇÃO ESTATÍSTICA DAS LCs (Opcional, mas recomendado)
# Testa se a LC50 da Sílica é significativamente diferente da LC50 da Terra
EDcomp(modelo_drc, c(50, 50), type = "relative")

# Se a sua menor dose testada for, por exemplo, 10, você pode trocar esse '0.1' por '1'
df_sem2$Dose_plot <- ifelse(df_sem2$Dose == 0, 0.1, df_sito_corrigido$Dose)

# 2. Agrupamento e cálculo de Média e Desvio Padrão
df_sem2.agrup <- df_sem2 |> 
  group_by(Dose, Material) |> 
  summarise(Mean = mean(prop_mortos, na.rm = TRUE),
            SD = sd(prop_mortos, na.rm = TRUE),
            n = n(),
            SE = SD / sqrt(n),
            .groups = "drop") 

plot.sem2 <- ggplot(df_sem2.agrup, aes(x = Dose, y = Mean, shape = Material, group = Material)) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.05, 
                linewidth = 0.7, 
                position = position_dodge(0.05)) +
  geom_line(linewidth = 0.7, position = position_dodge(0.05),
            linetype = 'dashed') +
  geom_point(size = 3, position = position_dodge(0.05), fill = 'white') +
  
  # Conversão do eixo Y para porcentagem (considerando proporção de 0 a 1)
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  
  labs(x = expression("Dose"~(g~kg^{-1})), 
       y = "Mortality (%)", 
       color = "Material") +
  # scale_color_manual(values = c("#0072B2", "#D55E00")) +
  scale_shape_manual(values = c(15, 23)) +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 14, face = 'plain'),
    axis.title = element_text(face = "plain", size = 14),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    legend.position = c(0.8, 0.13), 
    legend.title = element_blank(), 
    legend.background = element_rect(fill = "transparent")
  )

  ## 3.3 Semana 3 -------------------------

df_sem3 <- subset(df_sito, Data == "2026-10-05")
# Criar a coluna de proporção de mortalidade (varia de 0 a 1)


modelo_drc <- drm(prop_mortos ~ Dose, 
                  curveid = Material,
                  weights = Total, 
                  data = df_sem3, 
                  fct = LL.2(), 
                  type = "binomial")

# Resumo do modelo (Mostrará a inclinação e LC50 de CADA material)
summary(modelo_drc)

# 3. EXTRAÇÃO DAS LC's
# Calculará as LCs para ambos os materiais automaticamente
valores_lc <- ED(modelo_drc, c(10, 50, 90), interval = "delta")
print(valores_lc)

# 4. COMPARAÇÃO ESTATÍSTICA DAS LCs (Opcional, mas recomendado)
# Testa se a LC50 da Sílica é significativamente diferente da LC50 da Terra
EDcomp(modelo_drc, c(50, 50), type = "relative")

# Se a sua menor dose testada for, por exemplo, 10, você pode trocar esse '0.1' por '1'
df_sem3$Dose_plot <- ifelse(df_sem3$Dose == 0, 0.1, df_sito_corrigido$Dose)

# 2. Agrupamento e cálculo de Média e Desvio Padrão
df_sem3.agrup <- df_sem3 |> 
  group_by(Dose, Material) |> 
  summarise(Mean = mean(prop_mortos, na.rm = TRUE),
            SD = sd(prop_mortos, na.rm = TRUE),
            n = n(),
            SE = SD / sqrt(n),
            .groups = "drop") 

plot.sem3 <- ggplot(df_sem3.agrup, aes(x = Dose, y = Mean, shape = Material, group = Material)) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = 0.05, 
                linewidth = 0.7, 
                position = position_dodge(0.05)) +
  geom_line(linewidth = 0.7, position = position_dodge(0.05),
            linetype = 'dashed') +
  geom_point(size = 3, position = position_dodge(0.05), fill = 'white') +
  
  # Conversão do eixo Y para porcentagem (considerando proporção de 0 a 1)
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  
  labs(x = expression("Dose"~(g~kg^{-1})), 
       y = "Mortality (%)", 
       color = "Material") +
  scale_color_manual(values = c("#0072B2", "#D55E00")) +
  scale_shape_manual(values = c(15, 23)) +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 14, face = 'plain'),
    axis.title = element_text(face = "plain", size = 14),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    legend.position = c(0.8, 0.13), 
    legend.title = element_blank(), 
    legend.background = element_rect(fill = "transparent")
  )

### Plot unido

grafico_painel <- (plot.sem1 + plot.sem2) / plot.sem3 + 
  plot_layout(heights = c(1, 1)) + 
  plot_annotation(
    tag_levels = 'a',
    theme = theme(plot.tag = element_text(size = 14, face = "plain", vjust = -1))
  )

grafico_painel

ggsave("painel_mortalidade.tiff", plot = grafico_painel, 
       width = 24, 
       height = 18,
       units = "cm", 
       dpi = 600, 
       compression = "lzw")

# 4 Modelos LT50 -------------
# PREPARAÇÃO DOS DADOS PARA LT50
df_tempo <- df_sito
# Converter a coluna 'Semana' (1, 2, 3) para 'Dias' (7, 14, 21)
df_tempo$Dias <- df_tempo$Semana * 7

# ABORDAGEM 1: Comparar os materiais em uma dose específica
# (Ideal para provar qual age mais rápido aplicando a mesma quantidade)

# Filtre os dados para uma dose que você sabe que causou boa mortalidade
# (Substitua '2.0' pelo valor real de uma das suas maiores doses)
df_dose_fixa <- subset(df_tempo, Dose == 2.5)

# O eixo X agora é 'Dias'. O curveid separa os materiais.
modelo_lt50_comparativo <- drm(prop_mortos ~ Dias, 
                               curveid = Material, 
                               weights = Total, 
                               data = df_dose_fixa, 
                               fct = LL.2(), 
                               type = "binomial")

summary(modelo_lt50_comparativo)

# Extrair a LT50 (em dias) para a Sílica e para a Terra na dose escolhida
ED(modelo_lt50_comparativo, c(50), interval = "delta")
EDcomp(modelo_lt50_comparativo, c(50, 50), type = "relative")



# 5 Analise trajetórias, animal TA ----------------------------------------

df_ta <- read_excel("AnimalTA_tab_excel.xlsx")


df_ta <- df_ta %>%
  mutate(
    Dose = case_when(
      Group == "A" ~ 0,
      Group == "B" ~ 0.5,
      Group == "C" ~ 1,
      Group == "D" ~ 1.5,
      Group == "E" ~ 2,
      Group == "F" ~ 2.5,
      Group == "G" ~ 0.5,
      Group == "H" ~ 1,
      Group == "I" ~ 1.5,
      Group == "J" ~ 2,
      Group == "K" ~ 2.5,
      TRUE ~ NA_real_ # Valor padrão (caso apareça algum Group não previsto)
    )
  )

df_ta$Dose <- as.numeric(df_ta$Dose)
df_ta$Dosefac <- as.factor(df_ta$Dose)

df_ta <- df_ta %>%
  mutate(
    Material = case_when(
      Group == "A" ~ "Control",
      Group == "A" ~ "Control",
      Group == "B" ~ "BSNPs",
      Group == "C" ~ "BSNPs",
      Group == "D" ~ "BSNPs",
      Group == "E" ~ "BSNPs",
      Group == "F" ~ "BSNPs",
      Group == "G" ~ "DE",
      Group == "H" ~ "DE",
      Group == "I" ~ "DE",
      Group == "J" ~ "DE",
      Group == "K" ~ "DE",
      TRUE ~ NA_character_ # Valor padrão (caso apareça algum Group não previsto)
    )
  )


df_ta$Traveled_Dist <- as.numeric(df_ta$Traveled_Dist) 
df_ta$Average_Speed <- as.numeric(df_ta$Average_Speed) 
df_ta$Prop_time_moving <- as.numeric(df_ta$Prop_time_moving) 

### Correção dos grupos de controle

# Separamos o controle do restante dos dados
df_controle <- subset(df_ta, Material == "Control")
ctrl_silica <- df_controle
ctrl_silica$Material <- "BSNPs" # Use o nome exato que está no seu df_sito

# Criamos uma cópia do controle para a Terra de Diatomáceas
ctrl_terra <- df_controle
ctrl_terra$Material <- "DE" # Use o nome exato que está no seu df_sito

# Juntamos tudo novamente (removendo a nomenclatura antiga "controle")
df_ta <- rbind(
  subset(df_ta, Material != "Control"), 
  ctrl_silica, 
  ctrl_terra
)


str(df_ta)


  # 5.1 Exploração de trajetórias -------------

# Distancia caminhada
ggplot(df_ta, aes (y = Traveled_Dist, x = Group)) +
  geom_boxplot()+
  geom_point()

# Velocidade média
ggplot(df_ta, aes (y = Average_Speed, x = Dosefac, fill = Material)) +
  geom_boxplot()+
  geom_point()+
  theme_classic()

  # 5.2 Gráfico de barras com erros para trajetórias ------------

# 1. Agrupamento e cálculo da Média e Erro Padrão (SE)
df_ta.agrup <- df_ta |> 
  group_by(Dosefac, Material) |> 
  summarise(
    Mean = mean(Average_Speed, na.rm = TRUE),
    SD   = sd(Average_Speed, na.rm = TRUE),
    n    = n(),
    SE   = SD / sqrt(n),
    .groups = "drop"
  )

library(ggplot2)
library(ggpattern)

plot.velo <- ggplot(df_ta.agrup, aes(x = Dosefac, y = Mean, pattern = Material)) +
  geom_col_pattern(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black",            # Contorno externo das barras
    fill = "white",             # Fundo da barra sempre branco
    pattern_fill = "black",     # Cor de preenchimento dos pontos/linhas
    pattern_color = "black",    # Contorno dos elementos da textura
    pattern_density = 0.35,     # Proporção da área coberta pelo padrão
    pattern_spacing = 0.015,    # Distância entre os elementos do padrão
    pattern_size = 0.1,         # Espessura dos traços da textura
    linewidth = 0.5
  ) +
  geom_errorbar(
    aes(ymin = Mean - SE, ymax = Mean + SE),
    position = position_dodge(width = 0.8),
    width = 0.25,
    linewidth = 0.7,
    color = "black"
  ) +
  # Texturas geométricas: pontilhado de bolinhas ('circle') vs quadriculado cruzado ('crosshatch')
  scale_pattern_manual(values = c("stripe", "none")) +
  
  labs(
    x = expression("Dose"~(g~kg^{-1})),
    y = expression("Average speed (cm" ~ s^{-1} * ")")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 14, face = "plain"),
    axis.title = element_text(face = "plain", size = 14),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    legend.position = c(0.85, 0.85),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "white", color = "black")
  )

  # 5.3 Modelos para trajetórias -----------
# 1. Ajuste do modelo via glmmTMB para variável contínua positiva
# A distribuição Gamma(link = "log") é a escolha recomendada para medidas de velocidade
mod_velocidade <- glmmTMB(
  Average_Speed ~ Dosefac * Material, 
  family = Gamma(link = "log"), 
  data = df_ta
)

summary(mod_velocidade)

# 2. Avaliação dos pressupostos e resíduos do modelo
res <- simulateResiduals(mod_velocidade)
plot(res)
testDispersion(res)

# 3. Tabela de Wald (Efeitos principais e interação)
Anova(mod_velocidade, type = "III")

# 'type = "response"' converte a média marginal de volta para cm/s
emm_vel <- emmeans(mod_velocidade, ~ Material | Dosefac, type = "response")

# Contrastes pareados entre BSNPs e DE dentro de cada dose com correção de Bonferroni
comp_vel_materiais <- pairs(emm_vel, adjust = "bonferroni")

# Visualização das médias estimadas e dos contrastes
summary(emm_vel)
summary(comp_vel_materiais)


# 6 Graos danados ---------------------------------------------------------

df_graos <- read_excel("cont_graos_danados.xlsx")


df_graos <- df_graos %>%
  mutate(
    Dose = case_when(
      Group == "A" ~ 0,
      Group == "B" ~ 0.5,
      Group == "C" ~ 1,
      Group == "D" ~ 1.5,
      Group == "E" ~ 2,
      Group == "F" ~ 2.5,
      Group == "G" ~ 0.5,
      Group == "H" ~ 1,
      Group == "I" ~ 1.5,
      Group == "J" ~ 2,
      Group == "K" ~ 2.5,
      TRUE ~ NA_real_ # Valor padrão (caso apareça algum Group não previsto)
    )
  )

df_graos$Dose <- as.numeric(df_graos$Dose)
df_graos$Dosefac <- as.factor(df_graos$Dose)

df_graos <- df_graos %>%
  mutate(
    Material = case_when(
      Group == "A" ~ "Control",
      Group == "B" ~ "BSNPs",
      Group == "C" ~ "BSNPs",
      Group == "D" ~ "BSNPs",
      Group == "E" ~ "BSNPs",
      Group == "F" ~ "BSNPs",
      Group == "G" ~ "DE",
      Group == "H" ~ "DE",
      Group == "I" ~ "DE",
      Group == "J" ~ "DE",
      Group == "K" ~ "DE",
      TRUE ~ NA_character_ # Valor padrão (caso apareça algum Group não previsto)
    )
  )

### Correção dos grupos de controle

# Separamos o controle do restante dos dados
df_controle <- subset(df_graos, Material == "Control")
ctrl_silica <- df_controle
ctrl_silica$Material <- "BSNPs" # Use o nome exato que está no seu df_sito

# Criamos uma cópia do controle para a Terra de Diatomáceas
ctrl_terra <- df_controle
ctrl_terra$Material <- "DE" # Use o nome exato que está no seu df_sito

# Juntamos tudo novamente (removendo a nomenclatura antiga "controle")
df_graos <- rbind(
  subset(df_graos, Material != "Control"), 
  ctrl_silica, 
  ctrl_terra
)

# Graos
# 1. Agrupamento e cálculo da Média e Erro Padrão (SE)
df_graos.agrup <- df_graos |> 
  group_by(Dosefac, Material) |> 
  summarise(
    Mean = mean(Damaged, na.rm = TRUE),
    SD   = sd(Damaged, na.rm = TRUE),
    n    = n(),
    SE   = SD / sqrt(n),
    .groups = "drop"
  )


plot.graos <- ggplot(df_graos.agrup, aes(x = Dosefac, y = Mean, pattern = Material)) +
  geom_col_pattern(
    position = position_dodge(width = 0.8),
    width = 0.7,
    color = "black",            # Contorno externo das barras
    fill = "white",             # Fundo da barra sempre branco
    pattern_fill = "black",     # Cor de preenchimento dos pontos/linhas
    pattern_color = "black",    # Contorno dos elementos da textura
    pattern_density = 0.35,     # Proporção da área coberta pelo padrão
    pattern_spacing = 0.015,    # Distância entre os elementos do padrão
    pattern_size = 0.1,         # Espessura dos traços da textura
    linewidth = 0.5
  ) +
  geom_errorbar(
    aes(ymin = Mean - SE, ymax = Mean + SE),
    position = position_dodge(width = 0.8),
    width = 0.25,
    linewidth = 0.7,
    color = "black"
  ) +
  geom_text(
    data = subset(df_graos.agrup, Material == "DE" & Dosefac != "0"),
    aes(x = Dosefac, y = Mean + SE, label = "*", group = Material),
    position = position_dodge(width = 0.75),
    vjust = -0.25, # Desloca ligeiramente para cima do topo do SE
    hjust = -1.5, # Desloca ligeiramente para cima do topo do SE
    size = 6,     # Tamanho do asterisco
    color = "black"
  )+
  # Texturas geométricas: pontilhado de bolinhas ('circle') vs quadriculado cruzado ('crosshatch')
  scale_pattern_manual(values = c("stripe", "none")) +
  
  labs(
    x = expression("Dose"~(g~kg^{-1})),
    y = expression("Number of damaged grains")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(color = "black"),
    axis.text = element_text(color = "black", size = 14, face = "plain"),
    axis.title = element_text(face = "plain", size = 14),
    axis.line = element_line(color = "black", linewidth = 0.7),
    axis.ticks = element_line(color = "black", linewidth = 0.7),
    legend.position = c(0.85, 0.85),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "white", color = "black")
  )

  ### Plots unidos.
grafico_painel.2 <- plot.velo+plot.graos +
  plot_layout(heights = c(1, 1)) + 
  plot_annotation(
    tag_levels = 'a',
    theme = theme(plot.tag = element_text(size = 14, face = "plain", vjust = -1))
  )

library(cowplot)
grafico_painel.2 <- plot_grid(
  plot.velo, 
  plot.graos,
  labels = c("a", "b"),
  label_size = 14,
  label_fontface = "plain",
  label_y = 1.0,           # Ajuste fino da posição vertical do rótulo
  ncol = 2,                # Lado a lado (use nrow = 2 se quiser um sobre o outro)
  align = "hv",            # Alinha eixos horizontal e verticalmente
  axis = "tb"              # Iguala o alinhamento das caixas dos eixos
)
  


ggsave("barras.tiff", plot = grafico_painel.2, 
       width = 24, 
       height = 12,
       units = "cm", 
       dpi = 600, 
       compression = "lzw")


#6.1 Modelos para grãos danados ---------------

library(glmmTMB)
library(emmeans)
library(car)      # Para Anova tipo III
library(DHARMa)   # Para checagem de resíduos

# 1. Ajuste do modelo via glmmTMB
# Caso haja superdispersão (muito comum em contagens biológicas), usamos nbinom2
mod_graos <- glmmTMB(
  Damaged ~ Dosefac * Material, 
  family = nbinom2(link = "log"), 
  data = df_graos
)

summary(mod_graos)
# 2. Avaliação dos pressupostos e resíduos do modelo
res <- simulateResiduals(mod_graos)
plot(res)
testDispersion(res)

# 3. Tabela de Wald (Efeitos principais e interação)
Anova(mod_graos, type = "III")

# 4. Comparações múltiplas com emmeans
# Estimativa das médias marginais ajustadas (escala de resposta original: 'response')
emm_graos <- emmeans(mod_graos, ~ Material | Dosefac, type = "response")

# Contrastes entre BSNPs e DE dentro de cada nível de Dosefac
comp_materiais <- pairs(emm_graos, adjust = "bonferroni")

# Visualização dos resultados tabulados
summary(emm_graos)
summary(comp_materiais)


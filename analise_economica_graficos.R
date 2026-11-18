library(ggplot2)
library(dplyr)
library(patchwork)


# Gráfico de linhas e barras ----------------------------------------------


# 1. Base de dados derivada do seu estudo
df_economic <- data.frame(
  Dose = c(0.5, 1.0, 1.5, 2.0, 2.5),
  # Mortalidades médias aos 21 dias (%)
  Mort_BSNP = c(26.67, 45.33, 48.00, 50.67, 57.33),
  Mort_DE   = c(68.67, 85.33, 86.67, 90.00, 94.00),
  # Custo em R$/t de milho (Dose em kg/t * Custo unitário)
  # BSNP = R$ 0.2886/kg | DE = R$ 22.00/kg
  Cost_BSNP = c(0.5, 1.0, 1.5, 2.0, 2.5) * 0.2886,
  Cost_DE   = c(0.5, 1.0, 1.5, 2.0, 2.5) * 22.00
)

# Painel A: Mortalidade Biológica
p1 <- ggplot(df_economic, aes(x = Dose)) +
  geom_line(aes(y = Mort_DE, color = "Diatomaceous Earth (DE)"), linewidth = 1.1) +
  geom_point(aes(y = Mort_DE, color = "Diatomaceous Earth (DE)"), size = 3, shape = 15) +
  geom_line(aes(y = Mort_BSNP, color = "Biogenic Silica (BSNPs)"), linewidth = 1.1) +
  geom_point(aes(y = Mort_BSNP, color = "Biogenic Silica (BSNPs)"), size = 3, shape = 18) +
  scale_color_manual(values = c("Diatomaceous Earth (DE)" = "#2c7bb6", 
                                "Biogenic Silica (BSNPs)" = "#d7191c")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  labs(x = expression(paste("Application Dose (g ", kg^-1, " or kg ", t^-1, ")")),
       y = "21-Day Mortality (%)",
       title = "A  Biological Insecticidal Efficacy") +
  theme_classic() +
  theme(legend.position = "top", legend.title = element_blank())

# Painel B: Custo de Aplicação por Tonelada de Grão
p2 <- ggplot(df_economic, aes(x = Dose)) +
  geom_col(aes(y = Cost_DE, fill = "Diatomaceous Earth (DE)"), 
           position = position_nudge(x = -0.06), width = 0.1) +
  geom_col(aes(y = Cost_BSNP, fill = "Biogenic Silica (BSNPs)"), 
           position = position_nudge(x = 0.06), width = 0.1) +
  scale_fill_manual(values = c("Diatomaceous Earth (DE)" = "#74add1", 
                               "Biogenic Silica (BSNPs)" = "#f46d43")) +
  geom_text(aes(y = Cost_DE, label = sprintf("R$ %.1f", Cost_DE)), 
            position = position_nudge(x = -0.06), vjust = -0.5, size = 3) +
  geom_text(aes(y = Cost_BSNP, label = sprintf("R$ %.2f", Cost_BSNP)), 
            position = position_nudge(x = 0.06), vjust = -0.5, size = 3) +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 60, 15)) +
  labs(x = expression(paste("Application Dose (g ", kg^-1, " or kg ", t^-1, ")")),
       y = "Direct Treatment Cost (BRL/t grain)",
       title = "B  Direct Application Cost per Metric Ton") +
  theme_classic() +
  theme(legend.position = "none")

# Combinação lado a lado
final_plot <- p1 / p2
# print(final_plot)
# 2 - Gráfico radar -------------------------------------------------------


# Instalar pacote se necessário: install.packages("fmsb")
library(fmsb)

# 1. Estruturação dos dados normalizados (0 a 100)
# A primeira linha define o valor máximo (100) e a segunda o mínimo (0)
radar_data <- data.frame(
  Cost_Affordability = c(100, 0, 100, 5),    # Custo: quanto maior, mais econômico
  Speed_of_Action    = c(100, 0, 42, 100),   # Velocidade de ação (LT50)
  Acute_Toxicity     = c(100, 0, 14, 100),   # Toxicidade aguda (LC50)
  Mobility_Inhibition= c(100, 0, 100, 100),  # Inibição locomotora
  Sustainability     = c(100, 0, 100, 25)    # Sustentabilidade / Fonte renovável
)

# Nomes das linhas (Max, Min e os dois tratamentos)
rownames(radar_data) <- c("Max", "Min", "BSNPs", "Commercial DE")

# Rótulos legíveis para os eixos do gráfico
vlabels <- c(
  "Economic\nAffordability\n(Lower Cost)",
  "Speed of\nAction (LT50)",
  "Acute\nToxicity (LC50)",
  "Locomotory\nInhibition",
  "Source\nSustainability\n(Circular Bioeconomy)"
)

# 2. Definição de cores e transparência
colors_border <- c(rgb(0.84, 0.10, 0.11, 0.9),   # Vermelho para BSNPs
                   rgb(0.17, 0.48, 0.71, 0.9))   # Azul para DE
colors_in     <- c(rgb(0.84, 0.10, 0.11, 0.3),   # Preenchimento BSNPs
                   rgb(0.17, 0.48, 0.71, 0.3))   # Preenchimento DE

# 3. Plotagem do Gráfico Radar
par(mar = c(2, 2, 3, 2)) # Ajuste de margens

radarchart(
  radar_data,
  axistype = 1,
  # Polígonos
  pcol = colors_border,
  pfcol = colors_in,
  plwd = 2.5,
  plty = 1,
  # Grade de fundo
  cglcol = "grey70",
  cglty = 1,
  cglwd = 0.8,
  axislabcol = "grey40",
  caxislabels = seq(0, 100, 25),
  # Rótulos
  vlabels = vlabels,
  vlcex = 0.85
)

# Título e Legenda
title(main = "Multidimensional Performance: BSNPs vs. Diatomaceous Earth", 
      cex.main = 1.1, font.main = 2)

legend(
  x = "topright",
  legend = c("Biogenic Silica (BSNPs)", "Commercial DE"),
  bty = "n",
  pch = 20,
  col = colors_border,
  text.col = "black",
  cex = 0.9,
  pt.cex = 2
)
# 3 - Heat map ------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(viridis)

# 1. Parâmetros do modelo TEA (Base: 1 t de milho tratado)
dose_BSNP <- 2.5   # kg de BSNP / t de milho
dose_DE   <- 1.0   # kg de DE / t de milho

# Custos fixos de produção da BSNP (exceto o frete variável da cinza)
custo_fixo_BSNP_kg <- 0.0162 + 0.1540 + 0.0400  # R$ 0.2102 / kg

# Parâmetros ANTT (Portaria SUROC nº 3/2026 - Granel Sólido, 5 eixos)
CCD_ANTT <- 6.5381   # R$/km
CC_ANTT  <- 642.10   # R$ fixo por frete
carga_t  <- 13.0     # Capacidade da caçamba em toneladas de cinza bruta
rend_massa <- 0.99   # Rendimento útil do pó

# 2. Construção da grade de sensibilidade ajustada
# Agora o preço da DE começa em R$ 1.00 e vai até R$ 25.00
distancia_seq <- seq(0, 300, by = 2)      # 0 a 300 km
preco_DE_seq  <- seq(0.1, 3.636, by = 0.2) # R$ 1,00 a R$ 25,00 / kg

grid_sensibilidade <- expand.grid(
  Distancia_km = distancia_seq,
  Preco_DE_kg  = preco_DE_seq
) %>%
  mutate(
    # Custo do frete rodoviário por kg de cinza bruta ajustado ao rendimento
    Frete_viagem = (Distancia_km * CCD_ANTT) + CC_ANTT,
    Custo_frete_kg = (Frete_viagem / (carga_t * 1000)) / rend_massa,
    
    # Custo total unitário de produção da BSNP (R$/kg)
    Custo_BSNP_kg = custo_fixo_BSNP_kg + Custo_frete_kg,
    
    # Custo do tratamento de 1 tonelada de milho (R$/t de grão)
    Custo_Trat_BSNP = Custo_BSNP_kg * dose_BSNP,
    Custo_Trat_DE   = Preco_DE_kg * dose_DE,
    
    # Indicadores econômicos
    Economia_Liquida_R = Custo_Trat_DE - Custo_Trat_BSNP,
    Economia_Percentual = ((Custo_Trat_DE - Custo_Trat_BSNP) / Custo_Trat_DE) * 100
  )

# 3. Construção do Gráfico
p_heatmap <- ggplot(grid_sensibilidade, aes(x = Distancia_km, y = Preco_DE_kg)) +
  # Gradiente contínuo
  geom_raster(aes(fill = Economia_Percentual), interpolate = TRUE) +
  
  # Linhas de contorno em níveis chave de economia
  geom_contour(aes(z = Economia_Percentual), 
               breaks = c(0, 25, 50, 75, 90), 
               color = "black", linewidth = 0.8, linetype = "dashed") +
  
  # Rótulos manuais limpos para as isolinhas (sem repetições do stat_contour)
  annotate("text", x = 270, y = 1.09, label = "0% (Parity)", color = "white", size = 2.8, fontface = "bold") +
  annotate("text", x = 270, y = 1.45, label = "25% Savings", color = "black", size = 2.8, fontface = "bold") +
  annotate("text", x = 270, y = 2.15, label = "50% Savings", color = "black", size = 2.8, fontface = "bold") +
  annotate("text", x = 195, y = 3.45, label = "75% Savings", color = "black", size = 2.8, fontface = "bold") +
  annotate("text", x = 270, y = 11.0, label = "90% Savings", color = "black", size = 2.8, fontface = "bold") +
  
  # Ponto do Cenário Base (50 km, DE a R$ 22,00/kg)
  annotate("point", x = 50, y = 2.07, color = "black", fill = "#e41a1c", 
           size = 3.8, shape = 21, stroke = 1.1) +
  annotate("text", x = 56, y = 2.07, 
           label = "Conservative Scenario\n(50 km, BRL 2.07 kg-1; ~65.8% Savings)", 
           hjust = 0, size = 3.2, fontface = "bold", color = "black") +
  
  # Ajuste da paleta de cores (iniciando em 0% para exibir todo o degradê de roxo a amarelo)
  scale_fill_viridis_c(
    option = "turbo",
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    labels = function(x) paste0(x, "%"),
    name = "Direct Cost\nSavings (%)",
    oob = scales::squish,
    alpha  = 1
  ) +
  
  scale_x_continuous(breaks = seq(0, 300, by = 100), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 4, by = 0.5), expand = c(0, 0), limits = c(0.1, 4)) +
  
  labs(
    title = "",
    subtitle = "",
    x = "RHA Transportation Distance from Source (km)",
    y = "Commercial Diatomaceous Earth Market Price (BRL kg-1)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9.5, color = "grey30", margin = margin(b = 10)),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 9, color = "black"),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8.5),
    legend.position = "right"
  )

print(p_heatmap)



ggsave("heatmap.tiff", plot = p_heatmap, 
       width = 18, 
       height = 12,
       units = "cm", 
       dpi = 600, 
       compression = "lzw")

# 4 - capacidade armazenamento --------------------------------------------


# Instalar pacotes se necessário:
# install.packages(c("ggplot2", "dplyr", "scales"))

library(ggplot2)
library(dplyr)
library(scales)

# 1. Definição das capacidades dos silos e parâmetros de custo unitário
capacidades_t <- c(500, 5000, 20000, 50000)

# Custos diretos calculados por tonelada de milho tratada:
# DE: 1.0 kg/t * R$ 22.00/kg = R$ 22.00 / t
# BSNPs: 2.5 kg/t * R$ 0.2886/kg = R$ 0.7215 / t
custo_DE_t   <- 22.00
custo_BSNP_t <- 0.7215

# Estimativa de CAPEX da planta de moagem (R$ 120.000,00)
capex_maquinario <- 120000

# 2. Estruturação do data.frame com os cálculos de despesa anual e delta
df_silos <- data.frame(
  Silo_Capacidade = factor(
    paste0(format(capacidades_t, big.mark = ".", scientific = FALSE), " t"),
    levels = paste0(format(capacidades_t, big.mark = ".", scientific = FALSE), " t")
  ),
  Capacidade_num = capacidades_t,
  Gasto_DE   = capacidades_t * custo_DE_t,
  Gasto_BSNP = capacidades_t * custo_BSNP_t
) %>%
  mutate(
    Delta_Economia = Gasto_DE - Gasto_BSNP,
    # Posição y para posicionar os rótulos de economia
    Y_pos_delta = Gasto_DE + (Gasto_DE * 0.08)
  )

# Conversão para formato longo (tidy) para plotagem em barras lado a lado
df_plot <- df_silos %>%
  tidyr::pivot_longer(
    cols = c("Gasto_DE", "Gasto_BSNP"),
    names_to = "Tratamento",
    values_to = "Custo_Total"
  ) %>%
  mutate(
    Tratamento = factor(
      Tratamento,
      levels = c("Gasto_DE", "Gasto_BSNP"),
      labels = c("Commercial DE (1.0 kg/t)", "Biogenic Silica BSNPs (2.5 kg/t)")
    )
  )

# 3. Construção do Gráfico
p_escala <- ggplot(df_plot, aes(x = Silo_Capacidade, y = Custo_Total, fill = Tratamento)) +
  # Barras comparativas de despesa
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black", linewidth = 0.3) +
  
  # Rótulos de valor no topo de cada coluna
  geom_text(
    aes(
      label = ifelse(
        Custo_Total >= 1000000,
        paste0("R$ ", format(round(Custo_Total / 1000000, 2), nsmall = 2), " M"),
        paste0("R$ ", format(round(Custo_Total / 1000, 1), nsmall = 1), " k")
      )
    ),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3.2,
    fontface = "bold"
  ) +
  
  # Anotação de destaque do Delta de Economia (Savings)
  geom_label(
    data = df_silos,
    aes(
      x = Silo_Capacidade,
      y = Y_pos_delta,
      label = paste0("Δ Savings:\nR$ ", format(round(Delta_Economia, 0), big.mark = "."), " (-96.7%)")
    ),
    inherit.aes = FALSE,
    fill = "#ffffcc",
    color = "#800026",
    size = 3.0,
    fontface = "bold",
    label.padding = unit(0.25, "lines"),
    label.r = unit(0.15, "lines")
  ) +
  
  # Linha tracejada de referência do CAPEX do moinho (R$ 120.000,00)
  geom_hline(yintercept = capex_maquinario, linetype = "dashed", color = "grey30", linewidth = 0.7) +
  annotate(
    "text",
    x = 1.6,
    y = capex_maquinario + 40000,
    label = "Total Milling CAPEX (R$ 120.000)",
    color = "grey20",
    size = 3.2,
    fontface = "italic"
  ) +
  
  # Ajuste de escalas e formatação monetária no eixo Y
  scale_y_continuous(
    labels = function(x) paste0("R$ ", format(x / 1000, big.mark = "."), "k"),
    expand = expansion(mult = c(0, 0.18)),
    limits = c(0, 1350000)
  ) +
  scale_fill_manual(values = c("Commercial DE (1.0 kg/t)" = "#2c7bb6", 
                               "Biogenic Silica BSNPs (2.5 kg/t)" = "#d7191c")) +
  
  labs(
    title = "Annual Grain Protection Expenditure Across Silo Storage Capacities",
    subtitle = "Financial comparison between Commercial DE (R$ 22.00/kg) and RHA-derived BSNPs (R$ 0.29/kg)",
    x = "Storage Facility Bulk Capacity (Metric Tons)",
    y = "Total Treatment Expenditure per Storage Cycle (BRL)",
    fill = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9.5, color = "grey30", margin = margin(b = 15)),
    axis.title = element_text(face = "bold", size = 10),
    axis.text.x = element_text(face = "bold", size = 9.5, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    legend.position = "top",
    legend.text = element_text(size = 9.5),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4)
  )

print(p_escala)

# Salvar com resolução para submissão:
# ggsave("silo_scale_economic_savings.pdf", p_escala, width = 8.0, height = 5.8, dpi = 300)


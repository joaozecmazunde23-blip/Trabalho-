# ==============================================================================
# Trabalho Prático I - Análise de Dados (Análise Exploratória)
# Tema: Análise dos Factores Associados ao Efeito do Novo Tratamento na Perda de Peso
# Entrada : BasedeDados.txt (separador TAB, formato longo: 2 linhas por indivíduo)
# Saídas  : tabelas em ./tabelas/*.csv  e figuras em ./fig/*.pdf
# Pacotes : dplyr, tidyr, readr, ggplot2   (install.packages(c("dplyr","tidyr","readr","ggplot2")))
# ==============================================================================
options(encoding = "UTF-8")   # ficheiro guardado em UTF-8
library(dplyr); library(tidyr); library(readr); library(ggplot2)
dir.create("tabelas", showWarnings = FALSE); dir.create("fig", showWarnings = FALSE)
G <- c("Controlo", "Placebo", "Tratamento")

# ---------------------------------------------------------------- 1. Leitura e consistência
bruto <-read.delim("C:/Users/asus/Downloads/BasedeDados.txt") %>%
  mutate(across(where(is.character), trimws))
cat("Linhas:", nrow(bruto), " | Valores omissos:", sum(is.na(bruto)), "\n")
cat("Medições por indivíduo (deve ser só 2):\n"); print(table(table(bruto$ID)))
cat("Duplicados ID x VISIT:", sum(duplicated(bruto[, c("ID", "VISIT")])), "\n")
# covariáveis constantes dentro de cada indivíduo?
const <- bruto %>% group_by(ID) %>%
  summarise(across(c(TREAT, AGE, LIVINGSTATUS, SMOKESTATUS, GENDER), n_distinct), .groups = "drop")
cat("Indivíduos com covariáveis inconsistentes:", sum(const[, -1] != 1), "\n")

# ---------------------------------------------------------------- 2. Formato largo e variáveis derivadas
d <- bruto %>%
  pivot_wider(id_cols = c(ID, TREAT, AGE, LIVINGSTATUS, SMOKESTATUS, GENDER),
              names_from = VISIT, values_from = WEIGHT) %>%
  mutate(
    PERDA     = PRE - POST,                 # kg perdidos (positivo = perdeu peso)
    PERDA_PCT = 100 * PERDA / PRE,          # % do peso inicial
    RESP      = PERDA_PCT >= 5,             # resposta clínica: perda >= 5% do peso inicial
    TREAT     = factor(recode(TREAT, CONTROL = "Controlo", PLACEBO = "Placebo", TREATMENT = "Tratamento"), levels = G),
    GENERO    = recode(GENDER, FEMALE = "Feminino", MALE = "Masculino"),
    FUMO      = factor(recode(SMOKESTATUS, SMOKER = "Fumador", `QUIT-SMOKER` = "Ex-fumador", `NON-SMOKER` = "Não fumador"),
                       levels = c("Fumador", "Ex-fumador", "Não fumador")),
    VIVE      = factor(recode(LIVINGSTATUS,
                              `ALONE (SINGLE)` = "Sozinho (solteiro)", `ALONE (DIVORCED)` = "Sozinho (divorciado)",
                              `ALONE (WIDOWER)` = "Sozinho (viúvo)", `WITH FRIENDS` = "Com amigos",
                              `WITH PARTNER` = "Com parceiro(a)", `WITH PARTNER AND CHILDREN` = "Com parceiro(a) e filhos"),
                       levels = c("Sozinho (solteiro)", "Sozinho (divorciado)", "Sozinho (viúvo)",
                                  "Com amigos", "Com parceiro(a)", "Com parceiro(a) e filhos")),
    FAIXA     = cut(AGE, breaks = c(19, 34, 49, 65), labels = c("20-34", "35-49", "50-65"))
  ) %>% rename(IDADE = AGE)
write_csv(d, "tabelas/dados_largos.csv")

# ---------------------------------------------------------------- 3. Caracterização da amostra
resumo <- function(x) sprintf("%.1f (%.1f)", mean(x), sd(x))
perfil_cont <- d %>% group_by(TREAT) %>% summarise(Idade = resumo(IDADE), Peso_inicial = resumo(PRE), Peso_6meses = resumo(POST), .groups = "drop")
print(perfil_cont); print(d %>% summarise(Idade = resumo(IDADE), Peso_inicial = resumo(PRE), min_idade = min(IDADE), max_idade = max(IDADE)))
for (v in c("GENERO", "FUMO", "VIVE")) {
  cat("\n---", v, "---\n"); tab <- table(d[[v]], d$TREAT); print(tab); print(round(100 * prop.table(tab, 2), 1))
  write_csv(as.data.frame.matrix(tab) %>% tibble::rownames_to_column(v), paste0("tabelas/perfil_", v, ".csv"))
}
write_csv(perfil_cont, "tabelas/perfil_continuas.csv")

# ---------------------------------------------------------------- 4. Variação do peso: global e por grupo
global <- d %>% summarise(Media_PRE = mean(PRE), Media_POST = mean(POST), Perda_media = mean(PERDA),
                          DP_perda = sd(PERDA), Maior_perda = max(PERDA), Menor_perda = min(PERDA))
print(round(global, 2)); write_csv(global, "tabelas/global.csv")

# ---------------------------------------------------------------- 4b. Estatísticas descritivas do PESO nos dois momentos (inicial e 6 meses)
# Peso inicial (PRE) e peso depois (POST, 6 meses): n, média, DP, mediana, Q1, Q3, mínimo e máximo, global e por grupo.
peso_long <- d %>% pivot_longer(c(PRE, POST), names_to = "momento", values_to = "peso") %>%
  mutate(momento = factor(momento, levels = c("PRE", "POST"), labels = c("Inicial (PRE)", "6 meses (POST)")))
resumo_peso <- function(df) df %>%
  summarise(n = n(), media = mean(peso), dp = sd(peso), mediana = median(peso), q1 = quantile(peso, .25), q3 = quantile(peso, .75),
            minimo = min(peso), maximo = max(peso), .groups = "drop")
peso_global   <- peso_long %>% group_by(momento) %>% resumo_peso()
peso_por_grupo <- peso_long %>% group_by(TREAT, momento) %>% resumo_peso()
cat("\n=== Peso (kg): estatísticas descritivas global ===\n");  print(peso_global %>% mutate(across(where(is.numeric), ~ round(.x, 2))))
cat("\n=== Peso (kg): estatísticas descritivas por grupo ===\n"); print(peso_por_grupo %>% mutate(across(where(is.numeric), ~ round(.x, 2))), n = 20)
write_csv(peso_global, "tabelas/peso_global.csv"); write_csv(peso_por_grupo, "tabelas/peso_por_grupo.csv")
# Peso aos 6 meses (POST) por subgrupo, dentro de cada grupo
peso_post_sub <- bind_rows(lapply(c("GENERO", "FUMO", "VIVE", "FAIXA"), function(v)
  d %>% group_by(TREAT, factor = v, nivel = as.character(.data[[v]])) %>%
    summarise(n = n(), media_PRE = mean(PRE), media_POST = mean(POST), dp_POST = sd(POST), mediana_POST = median(POST),
              min_POST = min(POST), max_POST = max(POST), .groups = "drop")))
write_csv(peso_post_sub, "tabelas/peso_pos_por_subgrupo.csv")

por_grupo <- d %>% group_by(TREAT) %>%
  summarise(n = n(), PRE = mean(PRE), POST = mean(POST), Media = mean(PERDA), DP = sd(PERDA), Mediana = median(PERDA),
            Minimo = min(PERDA), Maximo = max(PERDA), Perda_pct = mean(PERDA_PCT),
            Sem_perda = sum(PERDA <= 0), Resp5 = sum(RESP), Resp5_pct = 100 * mean(RESP), .groups = "drop")
print(por_grupo %>% mutate(across(where(is.numeric), ~ round(.x, 2))))
write_csv(por_grupo, "tabelas/por_grupo.csv")

# ---------------------------------------------------------------- 5. Subgrupos: média da perda, n e % de respondedores
subgrupo <- function(v) {
  d %>% group_by(TREAT, nivel = .data[[v]]) %>%
    summarise(n = n(), media = mean(PERDA), dp = sd(PERDA), mediana = median(PERDA), resp_pct = 100 * mean(RESP), .groups = "drop") %>%
    mutate(factor = v)
}
sub_all <- bind_rows(lapply(c("GENERO", "FUMO", "VIVE", "FAIXA"), function(v) subgrupo(v) %>% mutate(nivel = as.character(nivel))))
print(sub_all %>% mutate(across(where(is.numeric), ~ round(.x, 2))), n = 100)
write_csv(sub_all, "tabelas/subgrupos.csv")

# Composição género x tabagismo no grupo tratamento (possível confundimento)
trat <- d %>% filter(TREAT == "Tratamento")
print(table(trat$GENERO, trat$FUMO))
print(trat %>% group_by(GENERO, FUMO) %>% summarise(n = n(), media = round(mean(PERDA), 2), .groups = "drop"))
# Correlações descritivas (Pearson) com a perda, por grupo
print(d %>% group_by(TREAT) %>% summarise(r_idade = round(cor(IDADE, PERDA), 2), r_peso_inicial = round(cor(PRE, PERDA), 2), .groups = "drop"))

# ---------------------------------------------------------------- 5b. QUEM RESPONDEU AO TRATAMENTO? (perda >= 5% do peso inicial)
# Respondedor = RESP == TRUE. Para cada grupo e subgrupo: total, n e % de respondedores, n e % de não respondedores.
resp_factor <- function(v) {
  d %>% group_by(TREAT, nivel = as.character(.data[[v]])) %>%
    summarise(total = n(), responderam = sum(RESP), nao_responderam = n() - sum(RESP),
              pct_responderam = round(100 * mean(RESP), 1), pct_nao_responderam = round(100 - 100 * mean(RESP), 1),
              .groups = "drop") %>%
    mutate(factor = v, resumo = paste0(responderam, "/", total, " (", format(pct_responderam, nsmall = 1), "%)"))
}
resp_global <- d %>% group_by(TREAT) %>%
  summarise(total = n(), responderam = sum(RESP), nao_responderam = n() - sum(RESP),
            pct_responderam = round(100 * mean(RESP), 1), pct_nao_responderam = round(100 - 100 * mean(RESP), 1), .groups = "drop") %>%
  mutate(factor = "Global", nivel = "Todos", resumo = paste0(responderam, "/", total, " (", format(pct_responderam, nsmall = 1), "%)"))
resposta <- bind_rows(resp_global, lapply(c("GENERO", "FUMO", "VIVE", "FAIXA"), resp_factor)) %>%
  select(factor, nivel, TREAT, total, responderam, pct_responderam, nao_responderam, pct_nao_responderam, resumo)
cat("\n=== Respondedores (perda >= 5%) por grupo e subgrupo ===\n")
print(resposta %>% filter(TREAT == "Tratamento"), n = 50)          # grupo tratamento (o mais relevante)
# Tabela larga: uma coluna por grupo, no formato "n/total (%)"
resposta_larga <- resposta %>% select(factor, nivel, TREAT, resumo) %>% pivot_wider(names_from = TREAT, values_from = resumo)
print(resposta_larga, n = 50)
write_csv(resposta, "tabelas/respondedores_detalhe.csv"); write_csv(resposta_larga, "tabelas/respondedores_tabela.csv")

# ---------------------------------------------------------------- 5c. EFEITO OBSERVADO DO TRATAMENTO POR SUBGRUPO (objectivo c)
# Efeito observado = perda média no Tratamento - perda média no Controlo (e vs Placebo), dentro de cada subgrupo.
# É descritivo: mostra em que subgrupos a diferença entre grupos é maior ou menor, sem testar significância.
efeito_sub <- function(v) {
  d %>% group_by(nivel = as.character(.data[[v]]), TREAT) %>% summarise(m = mean(PERDA), .groups = "drop") %>%
    pivot_wider(names_from = TREAT, values_from = m) %>%
    left_join(trat %>% group_by(nivel = as.character(.data[[v]])) %>% summarise(n_trat = n(), .groups = "drop"), by = "nivel") %>%
    mutate(factor = v)
}
trat <- d %>% filter(TREAT == "Tratamento")
efeito_global <- d %>% group_by(TREAT) %>% summarise(m = mean(PERDA), .groups = "drop") %>% pivot_wider(names_from = TREAT, values_from = m) %>%
  mutate(nivel = "Todos", n_trat = 40, factor = "Global")
efeito <- bind_rows(efeito_global, lapply(c("GENERO", "FUMO", "VIVE", "FAIXA"), efeito_sub)) %>%
  mutate(efeito_vs_controlo = Tratamento - Controlo, efeito_vs_placebo = Tratamento - Placebo) %>%
  select(factor, nivel, Controlo, Placebo, Tratamento, efeito_vs_controlo, efeito_vs_placebo, n_trat) %>%
  mutate(across(where(is.numeric) & !n_trat, ~ round(.x, 2)))
cat("\n=== Efeito observado do tratamento por subgrupo (kg) ===\n"); print(efeito, n = 50)
write_csv(efeito, "tabelas/efeito_por_subgrupo.csv")

# ---------------------------------------------------------------- 6. Figuras
tema <- theme_minimal(base_size = 10) + theme(legend.position = "bottom", legend.title = element_blank())
cores <- c(Controlo = "#8c8c8c", Placebo = "#e0a030", Tratamento = "#1f6fb2")

# Fig 1: peso por visita e perda por grupo
longo <- d %>% pivot_longer(c(PRE, POST), names_to = "Visita", values_to = "Peso") %>%
  mutate(Visita = factor(recode(Visita, PRE = "Inicial", POST = "6 meses"), levels = c("Inicial", "6 meses")))
f1a <- ggplot(longo, aes(TREAT, Peso, fill = Visita)) + geom_boxplot(width = .6, outlier.size = .8) +
  scale_fill_manual(values = c("#bcd3e8", "#1f6fb2")) + labs(x = NULL, y = "Peso (kg)", title = "(a) Peso inicial e aos 6 meses") + tema
f1b <- ggplot(d, aes(TREAT, PERDA, fill = TREAT)) + geom_boxplot(width = .5, outlier.shape = NA, alpha = .8) +
  geom_jitter(width = .15, size = 1, alpha = .5) + geom_hline(yintercept = 0, colour = "red", linetype = "dashed") +
  scale_fill_manual(values = cores) + labs(x = NULL, y = "Perda de peso (kg)", title = "(b) Perda de peso por grupo") + tema + theme(legend.position = "none")
if (requireNamespace("patchwork", quietly = TRUE)) {
  ggsave("fig/fig1_grupos.pdf", patchwork::wrap_plots(f1a, f1b, nrow = 1), width = 10, height = 3.9)
} else { ggsave("fig/fig1a.pdf", f1a, width = 5, height = 3.9); ggsave("fig/fig1b.pdf", f1b, width = 5, height = 3.9) }

# Fig 2: perda por grupo segundo género, tabagismo e condição de vida (uma legenda por painel)
painel <- function(v, titulo) {
  ggplot(d, aes(TREAT, PERDA, fill = .data[[v]])) + geom_boxplot(outlier.size = .6, linewidth = .3) +
    geom_hline(yintercept = 0, colour = "red", linetype = "dashed", linewidth = .3) +
    scale_fill_viridis_d(end = .9) + labs(x = NULL, y = "Perda de peso (kg)", title = titulo) + tema +
    guides(fill = guide_legend(ncol = 1))
}
p2a <- painel("GENERO", "(a) Género"); p2b <- painel("FUMO", "(b) Tabagismo"); p2c <- painel("VIVE", "(c) Condição de vida")
if (requireNamespace("patchwork", quietly = TRUE)) {
  ggsave("fig/fig2_subgrupos.pdf", patchwork::wrap_plots(p2a, p2b, p2c, nrow = 1), width = 12.5, height = 4.6)
} else { ggsave("fig/fig2a.pdf", p2a, width = 4.5, height = 4); ggsave("fig/fig2b.pdf", p2b, width = 4.5, height = 4); ggsave("fig/fig2c.pdf", p2c, width = 5.5, height = 4) }

# Fig 3: perda vs idade e vs peso inicial
lc <- d %>% pivot_longer(c(IDADE, PRE), names_to = "var", values_to = "x") %>%
  mutate(var = recode(var, IDADE = "(a) Idade (anos)", PRE = "(b) Peso inicial (kg)"))
f3 <- ggplot(lc, aes(x, PERDA, colour = TREAT)) + geom_point(size = 1.2, alpha = .7) + geom_smooth(method = "lm", se = FALSE, linewidth = .7) +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed", linewidth = .3) + scale_colour_manual(values = cores) +
  facet_wrap(~ var, scales = "free_x") + labs(x = NULL, y = "Perda de peso (kg)") + tema
ggsave("fig/fig3_continuas.pdf", f3, width = 10, height = 3.8)

# Fig 4: respondedores (>=5%) no grupo tratamento, por factor, com percentagens
# NOTA: os níveis internos do factor não têm acentos (evita o rótulo "NA" em sistemas com codificação diferente de UTF-8)
lr <- trat %>% transmute(RESP, Genero = GENERO, Tabagismo = as.character(FUMO), Condicao = as.character(VIVE)) %>%
  pivot_longer(-RESP, names_to = "fator", values_to = "nivel") %>%
  group_by(fator, nivel) %>% summarise(Responderam = 100 * mean(RESP), n = n(), .groups = "drop") %>%
  mutate(`Não responderam` = 100 - Responderam, etiqueta = paste0(nivel, " (n=", n, ")")) %>%
  pivot_longer(c(Responderam, `Não responderam`), names_to = "res", values_to = "pct") %>%
  mutate(res = factor(res, levels = c("Não responderam", "Responderam")),
         fator = factor(fator, levels = c("Genero", "Tabagismo", "Condicao")))
rotulos <- lr %>% filter(res == "Responderam") %>% mutate(txt = paste0(round(pct), "%"))
nomes_fator <- c(Genero = "Género", Tabagismo = "Tabagismo", Condicao = "Condição de vida")
f4 <- ggplot(lr, aes(pct, etiqueta, fill = res)) + geom_col(width = .7) +
  geom_text(data = rotulos, aes(x = pct, y = etiqueta, label = txt), inherit.aes = FALSE,
            hjust = 1.15, colour = "white", size = 3, fontface = "bold") +
  scale_fill_manual(values = c(Responderam = "#1f6fb2", `Não responderam` = "#d9d9d9")) +
  facet_grid(fator ~ ., scales = "free_y", space = "free_y", labeller = labeller(fator = nomes_fator)) +
  labs(x = "% dos indivíduos do grupo tratamento", y = NULL) + tema
ggsave("fig/fig4_resposta.pdf", f4, width = 7, height = 5.2)
cat("\nConcluído: tabelas em ./tabelas e figuras em ./fig\n")

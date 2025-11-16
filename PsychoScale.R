# Instale os pacotes se ainda não tiver
#install.packages(c("psych", "factoextra", " GPArotation", "irr", "ICC", "MASS", "dplyr", "ggplot2"))

library(psych)
library(factoextra)
library(GPArotation)
library(irr)
library(ICC)
library(ggplot2)
library(MASS)
library(dplyr)

######################################### Geração do Dataset (MANTER)
# O código de geração de dados é mantido e funciona corretamente
set.seed(2025)
n <- 150
Sigma <- matrix(c(
  1, 0.7,0.6,0.5, 0.2,0.1,0.1,0.1, 0.5,0.4,
  0.7, 1, 0.7,0.6, 0.1,0.2,0.1,0.1, 0.5,0.4,
  0.6,0.7, 1, 0.6, 0.1,0.1,0.2,0.1, 0.4,0.3,
  0.5,0.6,0.6, 1, 0.1,0.1,0.1,0.2, 0.4,0.3,
  0.2,0.1,0.1,0.1, 1, 0.7,0.6,0.5, 0.3,0.4,
  0.1,0.2,0.1,0.1, 0.7, 1, 0.7,0.6, 0.3,0.3,
  0.1,0.1,0.2,0.1, 0.6,0.7, 1, 0.6, 0.2,0.2,
  0.1,0.1,0.1,0.2, 0.5,0.6,0.6, 1, 0.2,0.2,
  0.5,0.5,0.4,0.4, 0.3,0.3,0.2,0.2, 1, 0.6,
  0.4,0.4,0.3,0.3, 0.4,0.3,0.2,0.2, 0.6, 1
), nrow = 10, byrow = TRUE)
dados_cont <- mvrnorm(n = n, mu = rep(3, 10), Sigma = Sigma)
itens_likert <- apply(dados_cont, 2, function(x) pmin(pmax(round(x), 1), 5))
dados <- as.data.frame(itens_likert)
colnames(dados) <- paste0("q", 1:10)
escore_total <- rowSums(dados)
saude_geral <- round(30 + 0.4 * escore_total + rnorm(n, 0, 8))
saude_geral <- pmin(pmax(saude_geral, 0), 100)
dados$saude_geral <- saude_geral

n_tr <- 30
set.seed(2025 + 1)
tr_cont_t1 <- mvrnorm(n = n_tr, mu = rep(3, 5), Sigma = Sigma[1:5, 1:5])
tr_cont_t2 <- tr_cont_t1 + rnorm(n_tr * 5, 0, 0.3)
q_t1 <- apply(tr_cont_t1, 2, function(x) pmin(pmax(round(x), 1), 5))
q_t2 <- apply(tr_cont_t2, 2, function(x) pmin(pmax(round(x), 1), 5))
dados_tr <- data.frame(
  id_tr = 1:n_tr,
  q1_t1 = q_t1[,1], q2_t1 = q_t1[,2], q3_t1 = q_t1[,3], q4_t1 = q_t1[,4], q5_t1 = q_t1[,5],
  q1_t2 = q_t2[,1], q2_t2 = q_t2[,2], q3_t2 = q_t2[,3], q4_t2 = q_t2[,4], q5_t2 = q_t2[,5]
)
write.csv(dados, "exemplo_validacao_escala.csv", row.names = FALSE)
write.csv(dados_tr, "exemplo_test_retest.csv", row.names = FALSE)
cat("✅ Arquivos gerados:\n")
cat("- 'exemplo_validacao_escala.csv' (n =", n, "participantes)\n")
cat("- 'exemplo_test_retest.csv' (n =", n_tr, "para reprodutibilidade)\n")

######################################### ANÁLISE CORRIGIDA

dados <- read.csv("exemplo_validacao_escala.csv")
dados_tr <- read.csv("exemplo_test_retest.csv") # ⭐️ CORREÇÃO 3: Ler dados_tr

# Certificar-se de que o pacote 'psych' está carregado
library(psych) 

# Selecionar todos os 10 itens da escala
itens <- dados[, paste0("q", 1:10)]

## 1. Alfa de Cronbach (Consistência Interna)

# ⭐️ SOLUÇÃO MAIS SEGURA: Usar apenas o argumento do data frame
alpha_result <- psych::alpha(itens)

# Imprimir o resultado para confirmar que funcionou
print(alpha_result)

## 2. Análise Fatorial Exploratória (EFA)

# Verificar adequação da amostra (KMO e Bartlett)
kmo_result <- KMO(itens)
bartlett_test <- cortest.bartlett(itens)

cat("\n--- KMO e Teste de Bartlett ---\n")
print(kmo_result)
print(bartlett_test)

# Determinar número de fatores (scree plot + eigenvalues > 1)
cat("\n--- Análise Paralela (Scree Plot) ---\n")
fa.parallel(itens, fa = "fa", n.iter = 100, show.legend = FALSE)

# Rodar análise fatorial (ex: 2 fatores, rotação varimax)
fa_result <- fa(itens, nfactors = 2, rotate = "varimax", fm = "ml")

# Mostrar resultados
cat("\n--- Cargas Fatoriais (2 fatores, ML, Varimax) ---\n")
print(fa_result, cut = 0.3)

# Plotar fatores
fa.plot(fa_result, labels = colnames(itens))

## 3. Validade e Reprodutibilidade

# Validade Convergente
escore_total <- rowSums(itens, na.rm = TRUE)
cor_validade <- cor(escore_total, dados$saude_geral, use = "complete.obs")

cat("\n--- Validade Convergente (correlação com saúde geral) ---\n")
print(cor_validade)

# Reprodutibilidade (Teste-Reteste) com ICC

# Usar os dados corretos do arquivo 'dados_tr'
itens_t1 <- dados_tr[, paste0("q", 1:5, "_t1")]
itens_t2 <- dados_tr[, paste0("q", 1:5, "_t2")]

escore_t1 <- rowSums(itens_t1, na.rm = TRUE)
escore_t2 <- rowSums(itens_t2, na.rm = TRUE)

# ICC simples (com pacote 'irr') - Reprodutibilidade do escore total
icc_simple <- icc(cbind(escore_t1, escore_t2), type = "agreement", unit = "single")

cat("\n--- ICC (Reprodutibilidade Teste-Reteste) ---\n")
print(icc_simple)

######################################### 4. Relatório Final Automático (opcional)

# Ajuste a linha no seu relatório:
kmo_value <- kmo_result$MSA[1] # Acesso mais seguro ao valor principal
# OU kmo_value <- kmo_result$overall 

# Gerar o relatório final corrigido:
cat("\n========================================\n")
cat("RELATÓRIO DE VALIDAÇÃO DA ESCALA\n")
cat("========================================\n")
cat("- Alfa de Cronbach: ", round(alpha_result$total$raw_alpha, 3), "\n")
cat("- KMO (adequação da amostra): ", round(kmo_value, 3), "\n") # LINHA CORRIGIDA
cat("- Número de fatores sugerido: ", fa_result$n.factors, "\n")
cat("- Validade convergente (com saúde geral): ", round(cor_validade, 3), "\n")

if(exists("icc_simple")) {
  cat("- ICC (reprodutibilidade, acordo absoluto): ", round(icc_simple$value, 3), "\n")
}
cat("\n✅ Análise concluída!\n")

# Salvar output em arquivo de texto
sink("relatorio_validacao.txt")
cat("### RESULTADO DO ALPHA DE CRONBACH ###\n")
print(alpha_result)
cat("\n\n### RESULTADO DA ANÁLISE FATORIAL ###\n")
print(fa_result)
cat("\n\n### VALIDADE CONVERGENTE ###\n")
print(cor_validade)
if(exists("icc_simple")) {
  cat("\n\n### ICC TESTE-RETESTE ###\n")
  print(icc_simple)
}
sink()

# Salvar gráficos (opcional)
png("scree_plot.png")
fa.parallel(itens, fa = "fa")
dev.off()
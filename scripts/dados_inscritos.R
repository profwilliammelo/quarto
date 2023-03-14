library(dplyr)
library(ggplot2)

bolsa_selecao <- readxl::read_xlsx("bases/dados_bolsa.xlsx")

bolsa_selecao_editado <-
  bolsa_selecao %>% select(
    nome_estudante,
    data_nascimento_estudante,
    id_personalizada,
    email,
    raca,
    genero,
    bairro_cidade_estado,
    tamanho_familia,
    renda,
    auxilio_brasil_ou_bolsa_familia,
    auxilio_emergencial,
    auxilios_outros,
    escolaridade_estudante,
    escolaridade_mae,
    escolaridade_pai,
    pagamento_faxina,
    rua_pavimentada,
    rua_agua_tratada,
    rua_agua_encanada,
    rua_poco,
    rua_iluminacao,
    rua_rede_esgoto,
    quantidade_geladeiras,
    quantidade_tablets,
    quantidade_celulares,
    quantidade_computadores,
    quantidade_quartos,
    quantidade_tvs,
    quantidade_banheiros,
    quantidade_carros,
    tv_cabo,
    wifi,
    internet_cabo_ou_fibra,
    internet_movel,
    quarto_proprio,
    guarda_livros,
    mesa_estudos,
    garagem,
    microondas,
    aspirador,
    maquina_lavar,
    freezer,
    celular_proprio,
    computador_proprio,
    frequencia_acesso_internet_casa,
    frequencia_acesso_internet_fora_casa
  ) %>%
  mutate(
    auxilio_brasil_ou_bolsa_familia = case_when(auxilio_brasil_ou_bolsa_familia == 0 ~ 1, T ~ 0),
    auxilio_emergencial = case_when(auxilio_emergencial == 0 ~ 1, T ~ 0),
    auxilios_outros = case_when(auxilios_outros == 0 ~ 1, T ~ 0),
    escolaridade_estudante = case_when(
      escolaridade_estudante %in% c(
        "ensino medio completo",
        "ensino superior incompleto",
        "ensino superior completo",
        "pos graduacao incompleta",
        "pos graduacao completa"
      ) ~ 1,
      T ~ 0
    ),
    escolaridade_mae = case_when(
      escolaridade_mae %in% c(
        "ensino medio completo",
        "ensino superior incompleto",
        "ensino superior completo",
        "pos graduacao incompleta",
        "pos graduacao completa"
      ) ~ 1,
      T ~ 0
    ),
    escolaridade_pai = case_when(
      escolaridade_pai %in% c(
        "ensino medio completo",
        "ensino superior incompleto",
        "ensino superior completo",
        "pos graduacao incompleta",
        "pos graduacao completa"
      ) ~ 1,
      T ~ 0
    ),
    renda = case_when(renda %in% c("entre 2 e 3 salarios", "entre 4 e 5 salarios") ~ 1, T ~ 0),
    pagamento_faxina = case_when(pagamento_faxina == "de vez em quando" ~ 1, T ~ 0),
    across(
      .cols = starts_with("quantidade"),
      .fns = ~ case_when(.x %in% c("1", "2", "3 ou mais") ~ 1, T ~ 0)
    ),
    frequencia_acesso_internet_casa = ifelse(frequencia_acesso_internet_casa == "sempre ou quase sempre", 1, 0),
    frequencia_acesso_internet_fora_casa = ifelse(frequencia_acesso_internet_fora_casa == "sempre ou quase sempre", 1, 0)

  )

variaveis_inse <- colnames(
  bolsa_selecao_editado %>% select(
    renda,
    auxilio_brasil_ou_bolsa_familia,
    auxilio_emergencial,
    auxilios_outros,
    escolaridade_estudante,
    escolaridade_mae,
    escolaridade_pai,
    pagamento_faxina,
    pagamento_faxina,
    starts_with("rua"),
    starts_with("quantidade"),
    tv_cabo,
    wifi,
    internet_cabo_ou_fibra,
    internet_movel,
    quarto_proprio,
    guarda_livros,
    mesa_estudos,
    garagem,
    microondas,
    aspirador,
    maquina_lavar,
    freezer,
    celular_proprio,
    computador_proprio,
    frequencia_acesso_internet_casa,
    frequencia_acesso_internet_fora_casa
  )
)


INSE_INSE_per_capta <- bolsa_selecao_editado %>%
  rowwise() %>% #AGRUPAR POR cada linha
  summarise(
    INSE = mean(c_across(all_of(variaveis_inse)),
      na.rm = T
    ),
    INSE_per_capta = INSE/tamanho_familia
  )

bolsa_selecao_editado <- bolsa_selecao_editado %>% bind_cols(INSE_INSE_per_capta)

bolsa_selecao_editado %>%
  ggplot(aes(x = reorder(nome_estudante, INSE_per_capta), y = INSE_per_capta, fill = raca)) +
  geom_col(col = "black") +
  coord_flip() +
  xlab("Estudantes") +
  ylab("Indicador de Nível Socioeconômico por pessoa na família") +
  labs(fill = "Cor") +
  ggthemes::theme_few()

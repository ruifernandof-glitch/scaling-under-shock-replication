# ==============================================================================
# IJOPM-04-2026-0601 - PIPELINE ANALITICO V4
# Scaling under shock: reconfiguracao estrutural das importacoes ucranianas
# relacionadas a drones
# ==============================================================================
#
# ESTE SCRIPT IMPLEMENTA O DESENHO REVISADO DO ARTIGO:
#   1. painel inferencial harmonizado, 2017-2024;
#   2. painel descritivo completo, 2022-2024;
#   3. conjunto amplo de doadores definido apenas ex ante;
#   4. controle sintetico como estrategia contrafactual principal;
#   5. DiD/estudo de evento apenas como analises de sensibilidade;
#   6. heterogeneidade por macrobloco;
#   7. amplitude, concentracao, entrada, saida, nucleo e periferia;
#   8. placebos no espaco/tempo, exclusao da China e limiares alternativos;
#   9. serie temporal interrompida mensal como analise complementar;
#  10. preservacao das analises descritivas e das redes bimodais.
#
# LIMITES SUBSTANTIVOS:
# Os dados medem transacoes internacionais reportadas. Nao medem producao
# domestica, capacidade produtiva, contratos, confianca, colaboracao, lead time,
# velocidade de entrega, desempenho operacional ou continuidade em combate.
#
# SEGURANCA:
# A chave da API nao deve ser gravada neste arquivo. Antes de executar:
# Sys.setenv(COMTRADE_PRIMARY = "SUA_CHAVE")
#
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PACOTES
# ------------------------------------------------------------------------------
required_packages <- c(
  "comtradr", "dplyr", "tidyr", "stringr", "purrr", "readr",
  "ggplot2", "scales", "igraph", "ggraph", "tibble", "janitor",
  "writexl", "fixest", "broom", "modelsummary", "sandwich", "lmtest",
  "MASS", "WDI", "Synth"
)

missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0L) {
  install.packages(missing_packages, dependencies = TRUE)
}
invisible(lapply(required_packages, library, character.only = TRUE))

options(stringsAsFactors = FALSE, scipen = 999)

# ------------------------------------------------------------------------------
# 1. PARAMETROS EDITAVEIS
# ------------------------------------------------------------------------------
ANO_INICIO_INFERENCIAL <- 2017L
ANO_INICIO_DESCRITIVO <- 2022L
ANO_TRATAMENTO <- 2022L
ANO_FIM <- 2024L
ANO_FIM_MENSAL <- 2025L
DATA_FIM_MENSAL <- as.Date("2025-09-01")
DATA_CHOQUE_MENSAL <- as.Date("2022-02-01")

REPORTER_TRATADO <- "UKR"

# Conjunto amplo de doadores. A ponderacao sera estimada separadamente para cada
# desfecho pelo controle sintetico; nenhum subconjunto sera escolhido com base
# nos resultados pos-2022.
REPORTERS_CANDIDATOS <- c(
  "AUT", "BEL", "BGR", "HRV", "CZE", "DNK", "EST", "FIN", "FRA",
  "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "NLD", "POL", "PRT",
  "ROU", "SVK", "SVN", "ESP", "SWE", "NOR", "CHE", "GBR", "CAN",
  "AUS", "NZL", "JPN", "KOR", "TUR", "THA", "MYS"
)

# CONTROLES_FINAIS pode restringir o donor pool por razoes substantivas definidas
# ex ante. Quando NULL, todos os candidatos com painel completo permanecem.
CONTROLES_FINAIS <- NULL
MIN_DONOR_POOL <- 15L
MAX_PRE_RMSPE_MULTIPLIER <- 5
PRETREND_ALPHA <- 0.10

# Informe aqui candidatos que sofreram choque equivalente ou que devam ser
# excluidos por razao substantiva. Nao use resultados pos-2022 para decidir.
CONTROLES_EXCLUIDOS_EX_ANTE <- character(0)

REPORTERS_PAINEL <- unique(c(REPORTER_TRATADO, REPORTERS_CANDIDATOS))
FLOW <- "import"

# Execucao e cache
RUN_DOWNLOAD_ANNUAL <- TRUE
RUN_DOWNLOAD_MONTHLY <- TRUE
RUN_MONTHLY_ITS <- TRUE
RUN_MODELS <- TRUE
RUN_SCM_PLACEBOS <- TRUE
USE_LOCAL_CACHE <- TRUE
MAX_API_ATTEMPTS <- 3L
MIN_MONTHLY_COVERAGE_FOR_ITS <- 0.90

# Compatibilidade e diagnostico do Comtrade. O script usa cache proprio em RDS;
# por isso, o cache interno do comtradr fica desligado. Se all_countries falhar,
# a consulta e refeita com lotes explicitos de parceiros, evitando URLs longas.
MIN_COMTRADR_VERSION <- "1.0.6"
RUN_COMTRADE_DIAGNOSTIC <- TRUE
COMTRADE_PARTNER_STRATEGY <- "auto_then_explicit"
COMTRADE_PARTNER_BATCH_SIZE <- 60L
COMTRADE_REQUESTS_PER_SECOND <- 10 / 60
USE_COMTRADR_CACHE <- FALSE
STOP_ON_PARTIAL_DOWNLOAD <- TRUE

# Depois da auditoria de cobertura, informe aqui anos que tenham retorno parcial
# (por exemplo, 2025) e que nao devam entrar nos modelos inferenciais.
YEARS_EXCLUDED_INFERENCE <- c(2025L)

# Definicoes analiticas
MIN_VALUE_SUPPLIER <- 1000
CORE_SHARE_THRESHOLD <- 0.05
CORE_CUMULATIVE_THRESHOLD <- 0.80
SUPPLIER_THRESHOLDS <- c(0, 1000, 10000, 100000)
N_WILD_BOOT <- 9999L
SEED <- 20260902L
set.seed(SEED)

# Dados macroeconomicos opcionais. Quando TRUE, o script tenta obter dados do
# World Bank/WDI, incluindo series de gasto militar originalmente baseadas no
# SIPRI. Se a consulta falhar, a selecao prossegue apenas com variaveis de trade
# e registra a limitacao.
USE_WDI_COVARIATES <- TRUE

OUTPUT_ROOT <- "resultados_ijopm_v4"
DIR_RAW <- file.path(OUTPUT_ROOT, "01_dados_brutos")
DIR_PROCESSED <- file.path(OUTPUT_ROOT, "02_dados_processados")
DIR_TABLES <- file.path(OUTPUT_ROOT, "03_tabelas")
DIR_FIGURES <- file.path(OUTPUT_ROOT, "04_graficos")
DIR_NETWORKS <- file.path(OUTPUT_ROOT, "05_redes")
DIR_MODELS <- file.path(OUTPUT_ROOT, "06_modelos")
DIR_LOGS <- file.path(OUTPUT_ROOT, "07_logs")

invisible(lapply(
  c(DIR_RAW, DIR_PROCESSED, DIR_TABLES, DIR_FIGURES, DIR_NETWORKS,
    DIR_MODELS, DIR_LOGS),
  dir.create, recursive = TRUE, showWarnings = FALSE
))

# ------------------------------------------------------------------------------
# 2. CODIGOS HS E PAINEIS DE COMPARABILIDADE
# ------------------------------------------------------------------------------
codigo_bloco <- tibble::tribble(
  ~cmd_code, ~macrobloco_id, ~macrobloco, ~subgrupo,
  "8806", "final", "Plataformas finais e partes", "Aeronaves nao tripuladas",
  "8807", "final", "Plataformas finais e partes", "Partes",
  "850760", "energy", "Energia, propulsao e eletronica", "Baterias de ions de litio",
  "850131", "energy", "Energia, propulsao e eletronica", "Motores eletricos DC",
  "853400", "energy", "Energia, propulsao e eletronica", "Circuitos impressos",
  "8542", "energy", "Energia, propulsao e eletronica", "Circuitos integrados",
  "851762", "control", "Comunicacao, navegacao e controle", "Comunicacao de dados",
  "852691", "control", "Comunicacao, navegacao e controle", "Radionavegacao",
  "852580", "control", "Comunicacao, navegacao e controle", "Cameras e sensores",
  "852692", "control", "Comunicacao, navegacao e controle", "Radiocontrole"
)

CODIGOS_COMPLETOS <- codigo_bloco$cmd_code

# 8806 entrou no HS 2022. 8807 nao possui interpretacao especificamente ligada
# a drones estavel antes de 2022. Ambos ficam fora do painel inferencial base.
CODIGOS_HARMONIZADOS <- setdiff(CODIGOS_COMPLETOS, c("8806", "8807"))

# A revisao HS 2022 substituiu 852580 por quatro subcodigos. As consultas usam
# os codigos vigentes em cada periodo; na limpeza, os quatro descendentes sao
# agregados novamente ao codigo analitico 852580. Isso preserva a comparabilidade
# do grupo amplo "cameras e sensores" sem consultar um codigo extinto apos 2021.
CODIGOS_CAMERA_HS2022 <- c("852581", "852582", "852583", "852589")
CODIGOS_ESTAVEIS_HARMONIZADOS <- setdiff(CODIGOS_HARMONIZADOS, "852580")
CODIGOS_CONSULTA_PRE2022 <- c(CODIGOS_ESTAVEIS_HARMONIZADOS, "852580")
CODIGOS_CONSULTA_POS2022_HARMONIZADO <- c(
  CODIGOS_ESTAVEIS_HARMONIZADOS,
  CODIGOS_CAMERA_HS2022
)
CODIGOS_CONSULTA_POS2022_COMPLETO <- c(
  "8806", "8807",
  CODIGOS_CONSULTA_POS2022_HARMONIZADO
)
QUERY_SCHEMA_VERSION <- "hsmap_v2"

map_query_code_to_analytical_code <- function(x) {
  dplyr::if_else(x %in% CODIGOS_CAMERA_HS2022, "852580", x)
}

codigo_harmonizacao_hs <- dplyr::bind_rows(
  tibble::tibble(
    query_code = CODIGOS_CONSULTA_PRE2022,
    analytical_code = map_query_code_to_analytical_code(CODIGOS_CONSULTA_PRE2022),
    first_year = ANO_INICIO_INFERENCIAL,
    last_year = ANO_TRATAMENTO - 1L,
    panel_use = "harmonized_pre2022"
  ),
  tibble::tibble(
    query_code = CODIGOS_CONSULTA_POS2022_HARMONIZADO,
    analytical_code = map_query_code_to_analytical_code(
      CODIGOS_CONSULTA_POS2022_HARMONIZADO
    ),
    first_year = ANO_TRATAMENTO,
    last_year = ANO_FIM,
    panel_use = "harmonized_post2022"
  ),
  tibble::tibble(
    query_code = c("8806", "8807"),
    analytical_code = c("8806", "8807"),
    first_year = ANO_TRATAMENTO,
    last_year = ANO_FIM,
    panel_use = "descriptive_post2022_only"
  )
) |>
  dplyr::distinct()

readr::write_csv(
  codigo_bloco,
  file.path(DIR_PROCESSED, "dicionario_codigos_macroblocos.csv")
)
readr::write_csv(
  codigo_harmonizacao_hs,
  file.path(DIR_PROCESSED, "correspondencia_temporal_codigos_hs.csv")
)

# ------------------------------------------------------------------------------
# 3. FUNCOES GERAIS
# ------------------------------------------------------------------------------
mensagem <- function(...) cat(sprintf(...), "\n")

safe_name <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}

safe_log <- function(x) log1p(pmax(x, 0))

safe_slope <- function(y, x) {
  ok <- stats::complete.cases(y, x)
  if (sum(ok) < 3L || stats::sd(x[ok]) == 0) return(NA_real_)
  unname(stats::coef(stats::lm(y[ok] ~ x[ok]))[[2L]])
}

first_existing_column <- function(data, candidates, label, regex = NULL) {
  found <- candidates[candidates %in% names(data)]
  if (length(found) > 0L) return(found[[1L]])
  
  if (!is.null(regex)) {
    found_regex <- names(data)[stringr::str_detect(names(data), regex)]
    if (length(found_regex) > 0L) return(found_regex[[1L]])
  }
  
  stop(
    sprintf(
      paste0(
        "Coluna nao localizada (%s). Colunas retornadas: %s. ",
        "Salve names(dados_raw) e revise o mapeamento em first_existing_column()."
      ),
      label, paste(names(data), collapse = ", ")
    ),
    call. = FALSE
  )
}

parse_month_date <- function(period, ref_year = NA_integer_, ref_month = NA_integer_) {
  period_chr <- as.character(period)
  digits <- stringr::str_replace_all(period_chr, "[^0-9]", "")
  ym <- dplyr::case_when(
    nchar(digits) >= 6L ~ substr(digits, 1L, 6L),
    !is.na(ref_year) & !is.na(ref_month) ~ sprintf("%04d%02d", ref_year, ref_month),
    TRUE ~ NA_character_
  )
  as.Date(paste0(ym, "01"), format = "%Y%m%d")
}

write_if_nonempty <- function(data, path) {
  if (!is.null(data) && nrow(data) > 0L) readr::write_csv(data, path)
  invisible(data)
}

# ------------------------------------------------------------------------------
# 4. DOWNLOAD ROBUSTO DO UN COMTRADE
# ------------------------------------------------------------------------------
get_api_key <- function() {
  key <- Sys.getenv("COMTRADE_PRIMARY")
  if (!nzchar(key)) {
    stop(
      paste0(
        "A chave nao esta definida. Execute antes do script: ",
        "Sys.setenv(COMTRADE_PRIMARY = 'SUA_CHAVE')"
      ),
      call. = FALSE
    )
  }
  key
}

collapse_condition_messages <- function(x) {
  x <- unique(trimws(as.character(x)))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) NA_character_ else paste(x, collapse = " | ")
}

capture_comtrade_call <- function(expr) {
  warning_messages <- character(0)
  error_message <- NA_character_
  
  value <- withCallingHandlers(
    tryCatch(
      force(expr),
      error = function(e) {
        error_message <<- conditionMessage(e)
        NULL
      }
    ),
    warning = function(w) {
      warning_messages <<- c(warning_messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  list(
    data = value,
    error = error_message,
    warnings = collapse_condition_messages(warning_messages)
  )
}

comtrade_frame_state <- function(x) {
  if (is.null(x)) return("failed")
  if (!is.data.frame(x)) return("unexpected_object")
  if (ncol(x) == 0L || identical(names(x), "count")) return("empty")
  if (nrow(x) >= 100000L) return("truncated")
  
  clean_columns <- janitor::make_clean_names(names(x))
  expected_pattern <- paste(
    c("primary_value", "trade_value", "cif_value", "cmd_code", "commodity_code"),
    collapse = "|"
  )
  if (!any(stringr::str_detect(clean_columns, expected_pattern))) {
    return("unexpected_schema")
  }
  
  if (nrow(x) == 0L) "empty" else "ok"
}

query_date_bounds <- function(year, frequency) {
  if (frequency == "M") {
    list(start = sprintf("%s-01", year), end = sprintf("%s-12", year))
  } else {
    list(start = as.character(year), end = as.character(year))
  }
}

active_partner_codes <- function(year) {
  country_table <- janitor::clean_names(comtradr::country_codes)
  required_columns <- c("iso_3", "entry_year", "exit_year", "group", "partner")
  missing_columns <- setdiff(required_columns, names(country_table))
  
  if (length(missing_columns) > 0L) {
    stop(
      sprintf(
        "A tabela comtradr::country_codes nao possui: %s.",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  country_table |>
    dplyr::mutate(
      partner_flag = as.character(partner) %in% c("TRUE", "1", "true"),
      group_flag = as.character(group) %in% c("TRUE", "1", "true"),
      entry_year_num = suppressWarnings(as.integer(entry_year)),
      exit_year_num = suppressWarnings(as.integer(exit_year))
    ) |>
    dplyr::filter(
      partner_flag,
      !group_flag,
      !is.na(iso_3), nzchar(iso_3),
      is.na(entry_year_num) | entry_year_num <= year,
      is.na(exit_year_num) | exit_year_num >= year
    ) |>
    dplyr::pull(iso_3) |>
    unique() |>
    sort()
}

partner_batches_for_year <- function(year) {
  partners <- active_partner_codes(year)
  if (length(partners) == 0L) {
    stop(sprintf("Nenhum parceiro ativo localizado para %s.", year), call. = FALSE)
  }
  split(partners, ceiling(seq_along(partners) / COMTRADE_PARTNER_BATCH_SIZE))
}

run_comtrade_query <- function(
    reporter,
    year,
    codes,
    frequency,
    partners,
    stage,
    partner_batch,
    max_attempts = MAX_API_ATTEMPTS
) {
  bounds <- query_date_bounds(year, frequency)
  errors_seen <- character(0)
  warnings_seen <- character(0)
  last_state <- "failed"
  attempts_made <- 0L
  
  for (attempt in seq_len(max_attempts)) {
    attempts_made <- attempt
    mensagem(
      paste0(
        "Comtrade: reporter=%s; ano=%s; freq=%s; etapa=%s; ",
        "lote=%s; tentativa=%s/%s"
      ),
      reporter, year, frequency, stage, partner_batch, attempt, max_attempts
    )
    
    captured <- capture_comtrade_call(
      comtradr::ct_get_data(
        type = "goods",
        frequency = frequency,
        commodity_classification = "HS",
        commodity_code = as.character(codes),
        flow_direction = FLOW,
        reporter = reporter,
        partner = partners,
        start_date = bounds$start,
        end_date = bounds$end,
        process = TRUE,
        tidy_cols = TRUE,
        verbose = FALSE,
        primary_token = get_api_key(),
        partner_2 = "World",
        requests_per_second = COMTRADE_REQUESTS_PER_SECOND,
        cache = USE_COMTRADR_CACHE
      )
    )
    
    if (!is.na(captured$error)) errors_seen <- c(errors_seen, captured$error)
    if (!is.na(captured$warnings)) warnings_seen <- c(warnings_seen, captured$warnings)
    last_state <- comtrade_frame_state(captured$data)
    
    if (last_state %in% c("ok", "empty")) {
      returned_data <- if (last_state == "ok") captured$data else tibble::tibble()
      if (last_state == "ok") {
        returned_data$._reporter_hint <- reporter
        returned_data$._year_hint <- year
        returned_data$._frequency_hint <- frequency
      }
      return(list(
        data = returned_data,
        status = tibble::tibble(
          reporter = reporter,
          year = as.integer(year),
          frequency = frequency,
          code_set = paste(codes, collapse = ";"),
          stage = stage,
          partner_batch = as.character(partner_batch),
          status = last_state,
          attempts = attempt,
          n_rows = ifelse(last_state == "ok", nrow(captured$data), 0L),
          n_partners_requested = ifelse(
            identical(partners, "all_countries"),
            NA_integer_,
            length(partners)
          ),
          error_message = collapse_condition_messages(errors_seen),
          warning_message = collapse_condition_messages(warnings_seen),
          run_time_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
        )
      ))
    }
    
    detail <- collapse_condition_messages(c(
      captured$error,
      captured$warnings,
      if (last_state == "unexpected_schema" && is.data.frame(captured$data)) {
        paste0("Colunas inesperadas: ", paste(names(captured$data), collapse = ", "))
      } else if (last_state == "unexpected_object") {
        paste0("Classe retornada: ", paste(class(captured$data), collapse = ", "))
      } else if (last_state == "truncated") {
        "A resposta atingiu 100.000 linhas e pode estar truncada."
      } else {
        NA_character_
      }
    ))
    errors_seen <- c(errors_seen, detail)
    mensagem("  Falha capturada: %s", ifelse(is.na(detail), last_state, detail))
    
    non_retryable <- stringr::str_detect(
      stringr::str_to_lower(ifelse(is.na(detail), "", detail)),
      "invalid|not valid|possible values|unauthori[sz]ed|forbidden|api key|token|401|403"
    )
    if (non_retryable) break
    if (attempt < max_attempts) Sys.sleep(min(2^attempt, 8))
  }
  
  list(
    data = tibble::tibble(),
    status = tibble::tibble(
      reporter = reporter,
      year = as.integer(year),
      frequency = frequency,
      code_set = paste(codes, collapse = ";"),
      stage = stage,
      partner_batch = as.character(partner_batch),
      status = ifelse(last_state == "truncated", "truncated", "failed"),
      attempts = attempts_made,
      n_rows = 0L,
      n_partners_requested = ifelse(
        identical(partners, "all_countries"),
        NA_integer_,
        length(partners)
      ),
      error_message = collapse_condition_messages(errors_seen),
      warning_message = collapse_condition_messages(warnings_seen),
      run_time_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
    )
  )
}

check_comtradr_version <- function() {
  installed_version <- utils::packageVersion("comtradr")
  if (installed_version < numeric_version(MIN_COMTRADR_VERSION)) {
    stop(
      sprintf(
        paste0(
          "comtradr %s instalado; este pipeline requer >= %s. ",
          "Execute install.packages('comtradr') e reinicie o R."
        ),
        installed_version, MIN_COMTRADR_VERSION
      ),
      call. = FALSE
    )
  }
  mensagem("comtradr %s detectado.", installed_version)
  invisible(installed_version)
}

diagnose_comtrade <- function() {
  probe_reporter <- "PRT"
  probe_year <- 2021L
  
  connectivity <- run_comtrade_query(
    reporter = probe_reporter,
    year = probe_year,
    codes = "TOTAL",
    frequency = "A",
    partners = "World",
    stage = "diagnostic_connectivity",
    partner_batch = "world",
    max_attempts = 1L
  )
  
  if (connectivity$status$status[[1L]] == "failed") {
    readr::write_csv(
      connectivity$status,
      file.path(DIR_LOGS, "diagnostico_comtrade.csv")
    )
    stop(
      paste0(
        "O teste minimo do Comtrade falhou antes do download do painel. Erro: ",
        connectivity$status$error_message[[1L]],
        ". Verifique a chave COMTRADE_PRIMARY, a conexao e o limite da API."
      ),
      call. = FALSE
    )
  }
  
  parameter_probe <- run_comtrade_query(
    reporter = probe_reporter,
    year = probe_year,
    codes = CODIGOS_CONSULTA_PRE2022,
    frequency = "A",
    partners = c("CHN", "DEU"),
    stage = "diagnostic_codes",
    partner_batch = "CHN_DEU",
    max_attempts = 1L
  )
  
  if (parameter_probe$status$status[[1L]] == "failed") {
    diagnostic_status <- dplyr::bind_rows(connectivity$status, parameter_probe$status)
    readr::write_csv(
      diagnostic_status,
      file.path(DIR_LOGS, "diagnostico_comtrade.csv")
    )
    stop(
      paste0(
        "A API respondeu ao teste minimo, mas rejeitou os parametros/codigos do estudo. Erro: ",
        parameter_probe$status$error_message[[1L]],
        sprintf(". Consulte %s/07_logs/diagnostico_comtrade.csv.", OUTPUT_ROOT)
      ),
      call. = FALSE
    )
  }
  
  post2022_probe <- run_comtrade_query(
    reporter = probe_reporter,
    year = 2023L,
    codes = CODIGOS_CONSULTA_POS2022_COMPLETO,
    frequency = "A",
    partners = c("CHN", "DEU"),
    stage = "diagnostic_codes_hs2022",
    partner_batch = "CHN_DEU",
    max_attempts = 1L
  )
  
  if (post2022_probe$status$status[[1L]] == "failed") {
    diagnostic_status <- dplyr::bind_rows(
      connectivity$status,
      parameter_probe$status,
      post2022_probe$status
    )
    readr::write_csv(
      diagnostic_status,
      file.path(DIR_LOGS, "diagnostico_comtrade.csv")
    )
    stop(
      paste0(
        "A API aceitou os codigos pre-2022, mas rejeitou os codigos HS 2022. Erro: ",
        post2022_probe$status$error_message[[1L]],
        sprintf(". Consulte %s/07_logs/diagnostico_comtrade.csv.", OUTPUT_ROOT)
      ),
      call. = FALSE
    )
  }
  
  all_countries_probe <- run_comtrade_query(
    reporter = probe_reporter,
    year = probe_year,
    codes = CODIGOS_CONSULTA_PRE2022[[1L]],
    frequency = "A",
    partners = "all_countries",
    stage = "diagnostic_all_countries",
    partner_batch = "auto",
    max_attempts = 1L
  )
  
  diagnostic_status <- dplyr::bind_rows(
    connectivity$status,
    parameter_probe$status,
    post2022_probe$status,
    all_countries_probe$status
  )
  readr::write_csv(
    diagnostic_status,
    file.path(DIR_LOGS, "diagnostico_comtrade.csv")
  )
  
  if (all_countries_probe$status$status[[1L]] %in% c("failed", "truncated")) {
    warning(
      paste0(
        "all_countries falhou no diagnostico; o pipeline usara lotes explicitos ",
        "de parceiros. Veja 07_logs/diagnostico_comtrade.csv."
      ),
      call. = FALSE
    )
    return("explicit")
  }
  
  "auto_then_explicit"
}

download_one_query <- function(reporter, year, codes, frequency = "A") {
  freq_tag <- ifelse(frequency == "M", "mensal", "anual")
  code_tag <- dplyr::case_when(
    setequal(codes, CODIGOS_CONSULTA_POS2022_COMPLETO) ~ "complete_hs2022",
    setequal(codes, CODIGOS_CONSULTA_POS2022_HARMONIZADO) ~ "harmonized_hs2022",
    setequal(codes, CODIGOS_CONSULTA_PRE2022) ~ "harmonized_pre2022",
    TRUE ~ "custom"
  )
  cache_path <- file.path(
    DIR_RAW,
    sprintf(
      "comtrade_%s_%s_%s_%s_%s.rds",
      reporter, year, freq_tag, code_tag, QUERY_SCHEMA_VERSION
    )
  )
  
  if (USE_LOCAL_CACHE && file.exists(cache_path)) {
    cached_data <- readRDS(cache_path)
    return(list(
      data = cached_data,
      status = tibble::tibble(
        reporter = reporter,
        year = as.integer(year),
        frequency = frequency,
        code_set = paste(codes, collapse = ";"),
        stage = "local_cache",
        partner_batch = "cache",
        status = "cache",
        attempts = 0L,
        n_rows = nrow(cached_data),
        n_partners_requested = NA_integer_,
        error_message = NA_character_,
        warning_message = NA_character_,
        run_time_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
      )
    ))
  }
  
  all_status <- list()
  
  if (PARTNER_STRATEGY_ACTIVE == "auto_then_explicit") {
    automatic <- run_comtrade_query(
      reporter = reporter,
      year = year,
      codes = codes,
      frequency = frequency,
      partners = "all_countries",
      stage = "automatic_all_countries",
      partner_batch = "auto"
    )
    all_status[[length(all_status) + 1L]] <- automatic$status
    
    if (automatic$status$status[[1L]] %in% c("ok", "empty")) {
      if (automatic$status$status[[1L]] == "ok") saveRDS(automatic$data, cache_path)
      return(list(
        data = automatic$data,
        status = dplyr::bind_rows(all_status)
      ))
    }
    
    automatic$status$status <- "auto_failed_fallback_started"
    all_status[[length(all_status)]] <- automatic$status
  }
  
  partner_batches <- partner_batches_for_year(year)
  batch_results <- purrr::imap(
    partner_batches,
    function(partners, batch_id) {
      run_comtrade_query(
        reporter = reporter,
        year = year,
        codes = codes,
        frequency = frequency,
        partners = partners,
        stage = "explicit_partner_batches",
        partner_batch = batch_id
      )
    }
  )
  all_status <- c(all_status, purrr::map(batch_results, "status"))
  batch_status <- dplyr::bind_rows(purrr::map(batch_results, "status"))
  batch_data <- dplyr::bind_rows(purrr::map(batch_results, "data"))
  
  if (!any(batch_status$status %in% c("failed", "truncated"))) {
    if (ncol(batch_data) > 0L) saveRDS(batch_data, cache_path)
  } else {
    warning(
      sprintf(
        paste0(
          "Download incompleto para %s-%s (%s). O painel nao sera estimado ",
          "enquanto houver lotes com falha."
        ),
        reporter, year, frequency
      ),
      call. = FALSE
    )
  }
  
  list(data = batch_data, status = dplyr::bind_rows(all_status))
}

download_comtrade_grid <- function(reporters, years, codes, frequency = "A") {
  grid <- tidyr::expand_grid(reporter = reporters, year = years)
  results <- purrr::pmap(
    grid,
    function(reporter, year) download_one_query(reporter, year, codes, frequency)
  )
  
  status <- dplyr::bind_rows(purrr::map(results, "status"))
  status_path <- file.path(
    DIR_LOGS,
    paste0("log_download_", ifelse(frequency == "M", "mensal", "anual"), ".csv")
  )
  if (file.exists(status_path)) {
    previous_status <- suppressMessages(readr::read_csv(status_path, show_col_types = FALSE))
    previous_status <- dplyr::mutate(previous_status, dplyr::across(dplyr::any_of(c("error_message", "warning_message")), as.character))
    current_pairs <- status |>
      dplyr::distinct(reporter, year, frequency)
    previous_status <- previous_status |>
      dplyr::anti_join(
        current_pairs,
        by = c("reporter", "year", "frequency")
      )
    status <- dplyr::bind_rows(status, previous_status)
  }
  write_if_nonempty(status, status_path)
  
  data <- dplyr::bind_rows(purrr::map(results, "data"))
  fatal_status <- status |>
    dplyr::filter(status %in% c("failed", "truncated"))
  
  if (STOP_ON_PARTIAL_DOWNLOAD && nrow(fatal_status) > 0L) {
    first_failures <- fatal_status |>
      dplyr::transmute(
        failure = paste0(
          reporter, "-", year, "/", frequency,
          " [", stage, ", lote ", partner_batch, "]: ",
          dplyr::coalesce(error_message, warning_message, status)
        )
      ) |>
      dplyr::slice_head(n = 5L) |>
      dplyr::pull(failure)
    stop(
      paste0(
        "O download ficou incompleto e foi interrompido para evitar estimativas ",
        "com cobertura parcial. Primeiras falhas: ",
        paste(first_failures, collapse = " || "),
        ". Consulte ", status_path, "."
      ),
      call. = FALSE
    )
  }
  
  if (ncol(data) == 0L) {
    stop(
      paste0(
        "As consultas foram processadas, mas nenhuma linha comercial foi retornada. ",
        "Consulte ", status_path, " e o diagnostico_comtrade.csv."
      ),
      call. = FALSE
    )
  }
  data
}

standardize_comtrade <- function(raw_data) {
  data <- janitor::clean_names(raw_data)
  
  col_value <- first_existing_column(
    data,
    c("primary_value", "cif_value", "trade_value_usd", "trade_value", "value_usd", "value"),
    "valor comercial",
    regex = "^(primary|cif|trade).*value$"
  )
  col_partner_iso <- first_existing_column(
    data,
    c("partner_iso", "partner_code_iso_alpha_3", "partner_iso3"),
    "ISO do parceiro",
    regex = "^partner.*iso"
  )
  col_partner_desc <- first_existing_column(
    data,
    c("partner_desc", "partner_description", "partner"),
    "descricao do parceiro",
    regex = "^partner.*(desc|name)$"
  )
  col_reporter_iso <- first_existing_column(
    data,
    c("reporter_iso", "reporter_code_iso_alpha_3", "reporter_iso3", "reporter_hint"),
    "ISO do reporter",
    regex = "^reporter.*iso"
  )
  col_year <- first_existing_column(
    data,
    c("ref_year", "year", "period_year", "year_hint"),
    "ano",
    regex = "(^|_)year$"
  )
  col_period <- c("period", "ref_period_id", "ref_period")
  col_period <- col_period[col_period %in% names(data)]
  col_period <- if (length(col_period) > 0L) col_period[[1L]] else NA_character_
  col_month <- c("ref_month", "month")
  col_month <- col_month[col_month %in% names(data)]
  col_month <- if (length(col_month) > 0L) col_month[[1L]] else NA_character_
  col_cmd <- first_existing_column(
    data,
    c("cmd_code", "commodity_code"),
    "codigo HS",
    regex = "(cmd|commodity).*code$"
  )
  col_cmd_desc <- first_existing_column(
    data,
    c("cmd_desc", "commodity_desc", "commodity_description"),
    "descricao HS",
    regex = "(cmd|commodity).*(desc|name)$"
  )
  
  period_value <- if (!is.na(col_period)) data[[col_period]] else data[[col_year]]
  month_value <- if (!is.na(col_month)) as.integer(data[[col_month]]) else NA_integer_
  
  standardized <- tibble::tibble(
    reporter_iso = as.character(data[[col_reporter_iso]]),
    ref_year = as.integer(data[[col_year]]),
    ref_month = month_value,
    period = as.character(period_value),
    partner_iso = as.character(data[[col_partner_iso]]),
    partner_desc = as.character(data[[col_partner_desc]]),
    cmd_code_raw = stringr::str_replace_all(
      as.character(data[[col_cmd]]),
      "[^0-9]", ""
    ),
    cmd_desc_raw = as.character(data[[col_cmd_desc]]),
    primary_value = suppressWarnings(as.numeric(data[[col_value]]))
  ) |>
    dplyr::mutate(cmd_code = map_query_code_to_analytical_code(cmd_code_raw))
  standardized |>
    dplyr::filter(
      !is.na(ref_year),
      !is.na(primary_value), primary_value > 0,
      !is.na(reporter_iso), !is.na(partner_iso),
      partner_iso != reporter_iso,
      cmd_code %in% CODIGOS_COMPLETOS,
      !partner_iso %in% c("W00", "WLD", "XZZ", "EUU", "OAS")
    ) |>
    dplyr::filter(
      !stringr::str_detect(
        stringr::str_to_upper(partner_desc),
        "WORLD|AREAS, NES|UNSPECIFIED|FREE ZONES|SPECIAL CATEGORIES"
      )
    ) |>
    dplyr::left_join(codigo_bloco, by = "cmd_code") |>
    dplyr::filter(!is.na(macrobloco_id)) |>
    dplyr::group_by(
      reporter_iso, ref_year, ref_month, period, partner_iso, partner_desc,
      cmd_code, macrobloco_id, macrobloco, subgrupo
    ) |>
    dplyr::summarise(primary_value = sum(primary_value), .groups = "drop") |>
    dplyr::mutate(cmd_desc = subgrupo) |>
    dplyr::select(
      reporter_iso, ref_year, ref_month, period, partner_iso, partner_desc,
      cmd_code, cmd_desc, macrobloco_id, macrobloco, subgrupo, primary_value
    )
}

# ------------------------------------------------------------------------------
# 5. FUNCOES DE METRICAS E DINAMICA DE FORNECEDORES
# ------------------------------------------------------------------------------
calculate_structure_metrics <- function(
    data,
    time_vars = c("reporter_iso", "ref_year"),
    min_value_supplier = MIN_VALUE_SUPPLIER,
    core_threshold = CORE_SHARE_THRESHOLD,
    cumulative_core_threshold = CORE_CUMULATIVE_THRESHOLD
) {
  full_totals <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(time_vars))) |>
    dplyr::summarise(reported_import_value = sum(primary_value), .groups = "drop")
  
  supplier_flows <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(time_vars, "partner_iso")))) |>
    dplyr::summarise(value = sum(primary_value), .groups = "drop") |>
    dplyr::filter(value >= min_value_supplier) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(time_vars))) |>
    dplyr::arrange(dplyr::desc(value), .by_group = TRUE) |>
    dplyr::mutate(
      covered_import_value = sum(value),
      share = dplyr::if_else(covered_import_value > 0, value / covered_import_value, NA_real_),
      rank = dplyr::row_number(),
      cumulative_share = cumsum(share),
      is_core = share >= core_threshold,
      is_core_80 = cumulative_share - share < cumulative_core_threshold,
      is_top5 = rank <= 5L
    ) |>
    dplyr::ungroup()
  
  metrics <- supplier_flows |>
    dplyr::group_by(dplyr::across(dplyr::all_of(time_vars))) |>
    dplyr::summarise(
      covered_import_value = sum(value),
      n_suppliers = dplyr::n_distinct(partner_iso),
      hhi = sum(share^2),
      effective_suppliers = dplyr::if_else(hhi > 0, 1 / hhi, NA_real_),
      entropy = -sum(share * log(share), na.rm = TRUE),
      entropy_effective_suppliers = exp(entropy),
      top1_share = dplyr::first(share),
      top5_share = sum(share[rank <= 5L]),
      n_core = sum(is_core),
      n_peripheral = sum(!is_core),
      core_share = sum(share[is_core]),
      peripheral_share = sum(share[!is_core]),
      n_core_80 = sum(is_core_80),
      n_peripheral_80 = sum(!is_core_80),
      core_80_share = sum(share[is_core_80]),
      peripheral_80_share = sum(share[!is_core_80]),
      .groups = "drop"
    ) |>
    dplyr::left_join(full_totals, by = time_vars) |>
    dplyr::mutate(
      retained_value_share = dplyr::if_else(
        reported_import_value > 0,
        covered_import_value / reported_import_value,
        NA_real_
      )
    )
  
  list(flows = supplier_flows, metrics = metrics)
}

calculate_supplier_dynamics <- function(
    data,
    group_vars = c("reporter_iso"),
    threshold = MIN_VALUE_SUPPLIER
) {
  flows <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, "ref_year", "partner_iso")))) |>
    dplyr::summarise(value = sum(primary_value), .groups = "drop") |>
    dplyr::filter(value >= threshold)
  
  split_keys <- flows |>
    dplyr::distinct(dplyr::across(dplyr::all_of(group_vars)))
  
  purrr::pmap_dfr(split_keys, function(...) {
    key <- list(...)
    names(key) <- group_vars
    g <- flows
    for (v in group_vars) g <- dplyr::filter(g, .data[[v]] == key[[v]])
    
    years <- sort(unique(g$ref_year))
    purrr::map_dfr(seq_along(years), function(idx) {
      y <- years[[idx]]
      current <- unique(g$partner_iso[g$ref_year == y])
      previous <- if (idx > 1L && years[[idx - 1L]] == y - 1L) {
        unique(g$partner_iso[g$ref_year == y - 1L])
      } else {
        NULL
      }
      
      values <- if (is.null(previous)) {
        c(entry = NA_real_, exit = NA_real_, persistent = NA_real_, persistence_rate = NA_real_)
      } else {
        persistent <- length(intersect(current, previous))
        c(
          entry = length(setdiff(current, previous)),
          exit = length(setdiff(previous, current)),
          persistent = persistent,
          persistence_rate = ifelse(length(previous) > 0L, persistent / length(previous), NA_real_)
        )
      }
      
      dplyr::bind_cols(
        tibble::as_tibble(key),
        tibble::tibble(
          ref_year = y,
          supplier_entry = values[["entry"]],
          supplier_exit = values[["exit"]],
          persistent_suppliers = values[["persistent"]],
          persistence_rate = values[["persistence_rate"]]
        )
      )
    })
  })
}

add_macro_composition <- function(metrics_total, data) {
  block_values <- data |>
    dplyr::group_by(reporter_iso, ref_year, macrobloco_id) |>
    dplyr::summarise(block_value = sum(primary_value), .groups = "drop") |>
    dplyr::group_by(reporter_iso, ref_year) |>
    dplyr::mutate(
      total_value_blocks = sum(block_value),
      block_share = block_value / total_value_blocks
    ) |>
    dplyr::ungroup()
  
  block_wide <- block_values |>
    dplyr::select(reporter_iso, ref_year, macrobloco_id, block_share) |>
    tidyr::pivot_wider(
      names_from = macrobloco_id,
      values_from = block_share,
      names_prefix = "share_block_",
      values_fill = 0
    )
  
  macro_hhi <- block_values |>
    dplyr::group_by(reporter_iso, ref_year) |>
    dplyr::summarise(macroblock_hhi = sum(block_share^2), .groups = "drop")
  
  metrics_total |>
    dplyr::left_join(macro_hhi, by = c("reporter_iso", "ref_year")) |>
    dplyr::left_join(block_wide, by = c("reporter_iso", "ref_year"))
}

build_annual_outcomes <- function(data, threshold = MIN_VALUE_SUPPLIER) {
  structure <- calculate_structure_metrics(data, min_value_supplier = threshold)
  dynamics <- calculate_supplier_dynamics(data, threshold = threshold)
  
  metrics <- structure$metrics |>
    dplyr::left_join(dynamics, by = c("reporter_iso", "ref_year")) |>
    add_macro_composition(data = data) |>
    dplyr::mutate(
      log_import_value = safe_log(reported_import_value),
      treated = as.integer(reporter_iso == REPORTER_TRATADO),
      event_time = ref_year - ANO_TRATAMENTO,
      post = as.integer(ref_year >= ANO_TRATAMENTO)
    )
  
  list(flows = structure$flows, metrics = metrics)
}

# ------------------------------------------------------------------------------
# 6. EXTRACAO, LIMPEZA E CONSTRUCAO DOS DOIS PAINEIS
# ------------------------------------------------------------------------------
PARTNER_STRATEGY_ACTIVE <- COMTRADE_PARTNER_STRATEGY
download_requested <- RUN_DOWNLOAD_ANNUAL ||
  (RUN_MONTHLY_ITS && RUN_DOWNLOAD_MONTHLY)

if (download_requested) {
  check_comtradr_version()
  comtradr::set_primary_comtrade_key(get_api_key())
  if (RUN_COMTRADE_DIAGNOSTIC) {
    PARTNER_STRATEGY_ACTIVE <- diagnose_comtrade()
  }
  mensagem("Estrategia ativa de parceiros: %s", PARTNER_STRATEGY_ACTIVE)
}

annual_raw_path <- file.path(DIR_RAW, "comtrade_anual_todos_reporters_raw.rds")

if (RUN_DOWNLOAD_ANNUAL) {
  annual_pre_raw <- download_comtrade_grid(
    reporters = REPORTERS_PAINEL,
    years = ANO_INICIO_INFERENCIAL:(ANO_TRATAMENTO - 1L),
    codes = CODIGOS_CONSULTA_PRE2022,
    frequency = "A"
  )
  annual_post_raw <- download_comtrade_grid(
    reporters = REPORTERS_PAINEL,
    years = ANO_TRATAMENTO:ANO_FIM,
    codes = CODIGOS_CONSULTA_POS2022_COMPLETO,
    frequency = "A"
  )
  annual_raw <- dplyr::bind_rows(annual_pre_raw, annual_post_raw)
  saveRDS(annual_raw, annual_raw_path)
} else {
  if (!file.exists(annual_raw_path)) stop("Arquivo anual bruto nao encontrado.")
  annual_raw <- readRDS(annual_raw_path)
}

annual_clean <- standardize_comtrade(annual_raw)
capture.output(names(janitor::clean_names(annual_raw)), file = file.path(DIR_LOGS, "colunas_comtrade_retornadas.txt"))
saveRDS(annual_clean, file.path(DIR_PROCESSED, "comtrade_anual_limpo.rds"))
readr::write_csv(annual_clean, file.path(DIR_PROCESSED, "comtrade_anual_limpo.csv"))

coverage_annual <- annual_clean |>
  dplyr::group_by(reporter_iso, ref_year) |>
  dplyr::summarise(
    n_observations = dplyr::n(),
    n_codes = dplyr::n_distinct(cmd_code),
    n_harmonized_codes = dplyr::n_distinct(cmd_code[cmd_code %in% CODIGOS_HARMONIZADOS]),
    n_partners = dplyr::n_distinct(partner_iso),
    reported_value = sum(primary_value),
    .groups = "drop"
  ) |>
  tidyr::complete(
    reporter_iso = REPORTERS_PAINEL,
    ref_year = ANO_INICIO_INFERENCIAL:ANO_FIM,
    fill = list(
      n_observations = 0L, n_codes = 0L, n_harmonized_codes = 0L,
      n_partners = 0L, reported_value = NA_real_
    )
  ) |>
  dplyr::mutate(
    expected_harmonized_codes = length(CODIGOS_HARMONIZADOS),
    coverage_flag = dplyr::if_else(n_observations == 0L, "no_return", "observed"),
    code_presence_flag = dplyr::case_when(
      n_observations == 0L ~ "no_return",
      n_harmonized_codes < expected_harmonized_codes ~ "some_codes_zero_or_absent",
      TRUE ~ "all_codes_with_positive_rows"
    )
  )
readr::write_csv(coverage_annual, file.path(DIR_TABLES, "cobertura_anual.csv"))

# A Comtrade normalmente omite combinacoes sem comercio positivo. Por isso, a
# ausencia de uma linha de codigo nao e convertida automaticamente em falha de
# cobertura. A tabela abaixo permite auditar cada celula; falhas reais devem ser
# verificadas em conjunto com o log da consulta.
code_presence <- tidyr::expand_grid(
  reporter_iso = REPORTERS_PAINEL,
  ref_year = ANO_INICIO_INFERENCIAL:ANO_FIM,
  cmd_code = CODIGOS_HARMONIZADOS
) |>
  dplyr::left_join(
    annual_clean |>
      dplyr::filter(cmd_code %in% CODIGOS_HARMONIZADOS) |>
      dplyr::group_by(reporter_iso, ref_year, cmd_code) |>
      dplyr::summarise(value = sum(primary_value), .groups = "drop"),
    by = c("reporter_iso", "ref_year", "cmd_code")
  ) |>
  dplyr::mutate(
    positive_trade_row = !is.na(value),
    value = dplyr::coalesce(value, 0)
  )
readr::write_csv(code_presence, file.path(DIR_TABLES, "presenca_codigos_anuais.csv"))

# Painel A: inferencial harmonizado, todos os reporters, 2017-2024.
panel_harmonized_transactions <- annual_clean |>
  dplyr::filter(
    ref_year >= ANO_INICIO_INFERENCIAL,
    ref_year <= ANO_FIM,
    cmd_code %in% CODIGOS_HARMONIZADOS
  )

# Painel B: descritivo completo da Ucrania, 2022-2024.
ukraine_full_transactions <- annual_clean |>
  dplyr::filter(
    reporter_iso == REPORTER_TRATADO,
    ref_year >= ANO_INICIO_DESCRITIVO,
    ref_year <= ANO_FIM,
    cmd_code %in% CODIGOS_COMPLETOS
  )

saveRDS(panel_harmonized_transactions, file.path(DIR_PROCESSED, "painel_A_harmonizado_transacoes.rds"))
saveRDS(ukraine_full_transactions, file.path(DIR_PROCESSED, "painel_B_completo_ucrania_transacoes.rds"))

panel_harmonized <- build_annual_outcomes(panel_harmonized_transactions)
ukraine_full <- build_annual_outcomes(ukraine_full_transactions)

readr::write_csv(panel_harmonized$metrics, file.path(DIR_PROCESSED, "painel_A_metricas_anuais.csv"))
readr::write_csv(ukraine_full$metrics, file.path(DIR_TABLES, "painel_B_metricas_ucrania.csv"))
readr::write_csv(ukraine_full$flows, file.path(DIR_TABLES, "painel_B_fluxos_fornecedores.csv"))

# Metricas por macrobloco no painel harmonizado e no painel completo.
panel_block_metrics <- calculate_structure_metrics(
  panel_harmonized_transactions,
  time_vars = c("reporter_iso", "ref_year", "macrobloco_id")
)$metrics |>
  dplyr::left_join(
    calculate_supplier_dynamics(
      panel_harmonized_transactions,
      group_vars = c("reporter_iso", "macrobloco_id")
    ),
    by = c("reporter_iso", "macrobloco_id", "ref_year")
  ) |>
  dplyr::mutate(
    log_import_value = safe_log(reported_import_value),
    treated = as.integer(reporter_iso == REPORTER_TRATADO),
    event_time = ref_year - ANO_TRATAMENTO,
    post = as.integer(ref_year >= ANO_TRATAMENTO)
  )

ukraine_full_block_metrics <- calculate_structure_metrics(
  ukraine_full_transactions,
  time_vars = c("reporter_iso", "ref_year", "macrobloco_id", "macrobloco")
)$metrics

readr::write_csv(panel_block_metrics, file.path(DIR_PROCESSED, "painel_A_metricas_macrobloco.csv"))
readr::write_csv(ukraine_full_block_metrics, file.path(DIR_TABLES, "painel_B_metricas_macrobloco.csv"))

# Composicao interna dos codigos no painel completo.
composition_full <- ukraine_full_transactions |>
  dplyr::group_by(ref_year, macrobloco_id, macrobloco, cmd_code, subgrupo) |>
  dplyr::summarise(value = sum(primary_value), .groups = "drop_last") |>
  dplyr::mutate(
    block_value = sum(value),
    within_block_share = value / block_value
  ) |>
  dplyr::ungroup()
readr::write_csv(composition_full, file.path(DIR_TABLES, "composicao_intra_bloco_completa.csv"))

# ------------------------------------------------------------------------------
# 7. COVARIAVEIS PRE-TRATAMENTO E ELEGIBILIDADE DO DONOR POOL
# ------------------------------------------------------------------------------
build_trade_pre_summary <- function(panel_metrics) {
  share_cols <- names(panel_metrics)[stringr::str_detect(names(panel_metrics), "^share_block_")]
  
  basic <- panel_metrics |>
    dplyr::filter(ref_year < ANO_TRATAMENTO) |>
    dplyr::group_by(reporter_iso) |>
    dplyr::summarise(
      n_pre_years = dplyr::n_distinct(ref_year),
      mean_log_import = mean(log_import_value, na.rm = TRUE),
      slope_log_import = safe_slope(log_import_value, ref_year),
      mean_hhi = mean(hhi, na.rm = TRUE),
      slope_hhi = safe_slope(hhi, ref_year),
      mean_suppliers = mean(n_suppliers, na.rm = TRUE),
      slope_suppliers = safe_slope(n_suppliers, ref_year),
      mean_top1 = mean(top1_share, na.rm = TRUE),
      mean_top5 = mean(top5_share, na.rm = TRUE),
      .groups = "drop"
    )
  
  if (length(share_cols) == 0L) return(basic)
  
  shares <- panel_metrics |>
    dplyr::filter(ref_year < ANO_TRATAMENTO) |>
    dplyr::group_by(reporter_iso) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(share_cols), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
      .groups = "drop"
    )
  dplyr::left_join(basic, shares, by = "reporter_iso")
}

get_wdi_pre_covariates <- function(reporters) {
  if (!USE_WDI_COVARIATES) return(NULL)
  if (!requireNamespace("WDI", quietly = TRUE)) {
    warning("Pacote WDI nao instalado; selecao baseada apenas em trade.")
    return(NULL)
  }
  
  indicators <- c(
    gdp = "NY.GDP.MKTP.CD",
    gdp_pc = "NY.GDP.PCAP.CD",
    imports_gdp = "NE.IMP.GNFS.ZS",
    military_usd = "MS.MIL.XPND.CD",
    military_gdp = "MS.MIL.XPND.GD.ZS"
  )
  
  ans <- tryCatch(
    WDI::WDI(
      country = reporters,
      indicator = indicators,
      start = ANO_INICIO_INFERENCIAL,
      end = ANO_TRATAMENTO - 1L,
      extra = FALSE
    ),
    error = function(e) {
      warning(sprintf("Falha na consulta WDI: %s", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(ans) || nrow(ans) == 0L) return(NULL)
  
  janitor::clean_names(ans) |>
    dplyr::rename(reporter_iso = iso3c, ref_year = year) |>
    dplyr::group_by(reporter_iso) |>
    dplyr::summarise(
      mean_log_gdp = mean(log1p(gdp), na.rm = TRUE),
      mean_log_gdp_pc = mean(log1p(gdp_pc), na.rm = TRUE),
      mean_imports_gdp = mean(imports_gdp, na.rm = TRUE),
      mean_log_military = mean(log1p(military_usd), na.rm = TRUE),
      mean_military_gdp = mean(military_gdp, na.rm = TRUE),
      .groups = "drop"
    )
}

PRIMARY_OUTCOMES <- c(
  "log_import_value", "hhi", "n_suppliers", "top5_share"
)

select_donor_pool <- function(pre_summary, panel_metrics, final_controls = CONTROLES_FINAIS) {
  required_years <- ANO_INICIO_INFERENCIAL:ANO_FIM
  
  panel_eligibility <- panel_metrics |>
    dplyr::filter(
      reporter_iso %in% c(REPORTER_TRATADO, REPORTERS_CANDIDATOS),
      ref_year %in% required_years
    ) |>
    dplyr::mutate(
      primary_complete = dplyr::if_all(
        dplyr::all_of(PRIMARY_OUTCOMES),
        ~ is.finite(.x)
      )
    ) |>
    dplyr::group_by(reporter_iso) |>
    dplyr::summarise(
      n_panel_years = dplyr::n_distinct(ref_year),
      complete_primary_panel = all(primary_complete),
      .groups = "drop"
    )
  
  eligibility <- tibble::tibble(
    reporter_iso = c(REPORTER_TRATADO, REPORTERS_CANDIDATOS)
  ) |>
    dplyr::left_join(pre_summary, by = "reporter_iso") |>
    dplyr::left_join(panel_eligibility, by = "reporter_iso") |>
    dplyr::mutate(
      excluded_ex_ante = reporter_iso %in% CONTROLES_EXCLUIDOS_EX_ANTE,
      eligible = dplyr::coalesce(
        n_pre_years >= (ANO_TRATAMENTO - ANO_INICIO_INFERENCIAL) &
          n_panel_years == length(required_years) &
          complete_primary_panel &
          !excluded_ex_ante,
        FALSE
      ),
      exclusion_reason = dplyr::case_when(
        excluded_ex_ante ~ "excluded_ex_ante",
        is.na(n_pre_years) |
          n_pre_years < (ANO_TRATAMENTO - ANO_INICIO_INFERENCIAL) ~ "insufficient_pre_years",
        is.na(n_panel_years) | n_panel_years < length(required_years) ~ "incomplete_year_panel",
        is.na(complete_primary_panel) | !complete_primary_panel ~ "missing_primary_outcome",
        TRUE ~ "eligible"
      )
    )
  
  if (!REPORTER_TRATADO %in% eligibility$reporter_iso[eligibility$eligible]) {
    stop("A Ucrania nao possui painel completo para os desfechos primarios.")
  }
  
  eligible_donors <- eligibility |>
    dplyr::filter(eligible, reporter_iso != REPORTER_TRATADO) |>
    dplyr::pull(reporter_iso)
  
  if (!is.null(final_controls)) {
    missing_requested <- setdiff(final_controls, eligible_donors)
    if (length(missing_requested) > 0L) {
      stop(sprintf(
        "Doadores solicitados sem elegibilidade: %s",
        paste(missing_requested, collapse = ", ")
      ))
    }
    eligible_donors <- final_controls
  }
  
  if (length(eligible_donors) < MIN_DONOR_POOL) {
    stop(sprintf(
      "Apenas %s doadores elegiveis. Sao exigidos pelo menos %s.",
      length(eligible_donors), MIN_DONOR_POOL
    ))
  }
  
  list(selected = sort(eligible_donors), eligibility = eligibility)
}

trade_pre <- build_trade_pre_summary(panel_harmonized$metrics)
wdi_pre <- get_wdi_pre_covariates(REPORTERS_PAINEL)
pre_summary <- if (is.null(wdi_pre)) {
  trade_pre
} else {
  dplyr::left_join(trade_pre, wdi_pre, by = "reporter_iso")
}

control_selection <- select_donor_pool(pre_summary, panel_harmonized$metrics)
SELECTED_CONTROLS <- control_selection$selected
ANALYSIS_REPORTERS <- c(REPORTER_TRATADO, SELECTED_CONTROLS)

readr::write_csv(pre_summary, file.path(DIR_TABLES, "covariaveis_pre_tratamento.csv"))
readr::write_csv(control_selection$eligibility, file.path(DIR_TABLES, "elegibilidade_doadores.csv"))

# Equilibrio nao ponderado. O equilibrio ponderado especifico de cada desfecho
# sera calculado depois da estimacao do controle sintetico.
balance_vars <- pre_summary |>
  dplyr::select(where(is.numeric)) |>
  names()
balance_table <- purrr::map_dfr(balance_vars, function(v) {
  target_value <- pre_summary[[v]][pre_summary$reporter_iso == REPORTER_TRATADO]
  control_values <- pre_summary[[v]][pre_summary$reporter_iso %in% SELECTED_CONTROLS]
  control_sd <- stats::sd(control_values, na.rm = TRUE)
  tibble::tibble(
    variable = v,
    ukraine = ifelse(length(target_value) == 1L, target_value, NA_real_),
    controls_mean = mean(control_values, na.rm = TRUE),
    controls_sd = control_sd,
    standardized_difference = ifelse(
      is.finite(control_sd) && control_sd > 0 && length(target_value) == 1L,
      (target_value - mean(control_values, na.rm = TRUE)) / control_sd,
      NA_real_
    )
  )
})
readr::write_csv(balance_table, file.path(DIR_TABLES, "equilibrio_pre_tratamento.csv"))
readr::write_lines(
  c(
    paste0("Tratado: ", REPORTER_TRATADO),
    paste0("Donor pool elegivel: ", paste(SELECTED_CONTROLS, collapse = ", ")),
    paste0("Controles excluidos ex ante: ", paste(CONTROLES_EXCLUIDOS_EX_ANTE, collapse = ", "))
  ),
  file.path(DIR_LOGS, "controles_selecionados.txt")
)

analysis_panel <- panel_harmonized$metrics |>
  dplyr::filter(reporter_iso %in% ANALYSIS_REPORTERS)

analysis_block_panel <- panel_block_metrics |>
  dplyr::filter(reporter_iso %in% ANALYSIS_REPORTERS)

# Anos comuns evitam comparar um retorno ausente com zero comercial.
common_years <- analysis_panel |>
  dplyr::distinct(reporter_iso, ref_year) |>
  dplyr::count(ref_year, name = "n_reporters") |>
  dplyr::filter(
    n_reporters == length(ANALYSIS_REPORTERS),
    !ref_year %in% YEARS_EXCLUDED_INFERENCE
  ) |>
  dplyr::pull(ref_year)

analysis_panel_common <- analysis_panel |>
  dplyr::filter(ref_year %in% common_years)
analysis_block_panel_common <- analysis_block_panel |>
  dplyr::filter(ref_year %in% common_years)

if (!all((ANO_INICIO_INFERENCIAL:ANO_FIM) %in% common_years)) {
  warning(sprintf(
    "O painel comum nao contem todos os anos. Anos retidos: %s",
    paste(common_years, collapse = ", ")
  ))
}

# ------------------------------------------------------------------------------
# 8. DIAGNOSTICOS GRAFICOS DE PRE-TENDENCIAS
# ------------------------------------------------------------------------------
primary_outcomes <- PRIMARY_OUTCOMES

plot_pretrend <- function(outcome, data = analysis_panel_common) {
  plot_data <- data |>
    dplyr::mutate(group = ifelse(treated == 1L, "Ukraine", "Comparison mean")) |>
    dplyr::group_by(group, ref_year) |>
    dplyr::summarise(value = mean(.data[[outcome]], na.rm = TRUE), .groups = "drop")
  
  ggplot2::ggplot(plot_data, ggplot2::aes(ref_year, value, color = group)) +
    ggplot2::geom_vline(xintercept = ANO_TRATAMENTO - 0.5, linetype = 2, color = "grey50") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.3) +
    ggplot2::scale_x_continuous(breaks = sort(unique(plot_data$ref_year))) +
    ggplot2::labs(x = NULL, y = outcome, color = NULL, title = paste("Pre-trends and post-shock path:", outcome)) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")
}

purrr::walk(primary_outcomes, function(outcome) {
  p <- plot_pretrend(outcome)
  ggplot2::ggsave(
    file.path(DIR_FIGURES, paste0("pretrend_", safe_name(outcome), ".png")),
    p, width = 9, height = 5.5, dpi = 300
  )
})

# ------------------------------------------------------------------------------
# 9. CONTROLE SINTETICO PRINCIPAL E DID COMO SENSIBILIDADE
# ------------------------------------------------------------------------------
# O controle sintetico e estimado separadamente para cada desfecho. A funcao
# exige painel balanceado e usa toda a trajetoria anual de 2017-2021 para
# construir os pesos. O primeiro periodo tratado e 2022.
validate_scm_panel <- function(data, outcome, treated_unit, donors) {
  required_years <- ANO_INICIO_INFERENCIAL:ANO_FIM
  required_units <- c(treated_unit, donors)
  
  d <- data |>
    dplyr::filter(
      reporter_iso %in% required_units,
      ref_year %in% required_years
    ) |>
    dplyr::select(reporter_iso, ref_year, dplyr::all_of(outcome))
  
  duplicate_cells <- d |>
    dplyr::count(reporter_iso, ref_year) |>
    dplyr::filter(n != 1L)
  if (nrow(duplicate_cells) > 0L) {
    stop(sprintf("Celulas duplicadas no painel SCM de %s.", outcome))
  }
  
  expected <- tidyr::expand_grid(
    reporter_iso = required_units,
    ref_year = required_years
  )
  d <- expected |>
    dplyr::left_join(d, by = c("reporter_iso", "ref_year"))
  
  if (any(!is.finite(d[[outcome]]))) {
    bad <- d |>
      dplyr::filter(!is.finite(.data[[outcome]])) |>
      dplyr::transmute(cell = paste(reporter_iso, ref_year, sep = "-")) |>
      dplyr::pull(cell)
    stop(sprintf(
      "Painel SCM incompleto para %s: %s",
      outcome, paste(utils::head(bad, 12L), collapse = ", ")
    ))
  }
  
  unit_map <- tibble::tibble(
    reporter_iso = sort(required_units),
    unit_id = seq_along(sort(required_units))
  )
  
  list(
    data = d |>
      dplyr::left_join(unit_map, by = "reporter_iso") |>
      dplyr::arrange(unit_id, ref_year),
    unit_map = unit_map
  )
}

fit_scm <- function(
    outcome,
    data = analysis_panel_common,
    treated_unit = REPORTER_TRATADO,
    donors = SELECTED_CONTROLS
) {
  checked <- validate_scm_panel(data, outcome, treated_unit, donors)
  d <- checked$data
  unit_map <- checked$unit_map
  pre_years <- ANO_INICIO_INFERENCIAL:(ANO_TRATAMENTO - 1L)
  all_years <- ANO_INICIO_INFERENCIAL:ANO_FIM
  treated_id <- unit_map$unit_id[unit_map$reporter_iso == treated_unit]
  donor_ids <- unit_map$unit_id[unit_map$reporter_iso %in% donors]
  
  special_predictors <- lapply(
    pre_years[-length(pre_years)],
    function(year_i) list(outcome, year_i, "mean")
  )
  
  prepared <- Synth::dataprep(
    foo = as.data.frame(d),
    predictors = outcome,
    predictors.op = "mean",
    special.predictors = special_predictors,
    dependent = outcome,
    unit.variable = "unit_id",
    unit.names.variable = "reporter_iso",
    time.variable = "ref_year",
    treatment.identifier = treated_id,
    controls.identifier = donor_ids,
    time.predictors.prior = pre_years,
    time.optimize.ssr = pre_years,
    time.plot = all_years
  )
  
  fitted <- Synth::synth(prepared, verbose = FALSE)
  observed <- as.numeric(prepared$Y1plot)
  synthetic <- as.numeric(prepared$Y0plot %*% fitted$solution.w)
  
  path <- tibble::tibble(
    ref_year = all_years,
    observed = observed,
    synthetic = synthetic,
    gap = observed - synthetic,
    post = ref_year >= ANO_TRATAMENTO
  )
  
  weight_values <- as.numeric(fitted$solution.w)
  weight_ids <- suppressWarnings(as.integer(rownames(fitted$solution.w)))
  if (length(weight_ids) != length(weight_values) || anyNA(weight_ids)) {
    weight_ids <- donor_ids
  }
  weights <- tibble::tibble(
    unit_id = weight_ids,
    weight = weight_values
  ) |>
    dplyr::left_join(unit_map, by = "unit_id") |>
    dplyr::arrange(dplyr::desc(weight))
  
  pre_rmspe <- sqrt(mean(path$gap[!path$post]^2))
  post_rmspe <- sqrt(mean(path$gap[path$post]^2))
  diagnostics <- tibble::tibble(
    treated_unit = treated_unit,
    outcome = outcome,
    pre_rmspe = pre_rmspe,
    post_rmspe = post_rmspe,
    rmspe_ratio = dplyr::if_else(pre_rmspe > 0, post_rmspe / pre_rmspe, NA_real_),
    mean_post_gap = mean(path$gap[path$post]),
    max_abs_pre_gap = max(abs(path$gap[!path$post])),
    max_donor_weight = max(weights$weight),
    n_positive_weights = sum(weights$weight > 0.001)
  )
  
  list(
    outcome = outcome,
    treated_unit = treated_unit,
    model = fitted,
    dataprep = prepared,
    path = path,
    weights = weights,
    diagnostics = diagnostics
  )
}

fit_scm_safe <- function(...) {
  tryCatch(fit_scm(...), error = function(e) e)
}

SCM_OUTCOMES <- unique(c(
  primary_outcomes,
  "core_share", "n_peripheral"
))

scm_results <- stats::setNames(
  lapply(SCM_OUTCOMES, function(outcome) {
    fit_scm_safe(outcome = outcome)
  }),
  SCM_OUTCOMES
)

scm_status <- purrr::imap_dfr(scm_results, function(result, outcome) {
  tibble::tibble(
    outcome = outcome,
    status = if (inherits(result, "error")) "failed" else "ok",
    message = if (inherits(result, "error")) conditionMessage(result) else NA_character_
  )
})

if (all(scm_status$status == "failed")) {
  stop("Todos os modelos de controle sintetico falharam. Consulte status_scm.csv.")
}

scm_paths <- purrr::imap_dfr(scm_results, function(result, outcome) {
  if (inherits(result, "error")) return(tibble::tibble())
  result$path |>
    dplyr::mutate(outcome = outcome, treated_unit = result$treated_unit, .before = 1)
})
scm_weights <- purrr::imap_dfr(scm_results, function(result, outcome) {
  if (inherits(result, "error")) return(tibble::tibble())
  result$weights |>
    dplyr::mutate(outcome = outcome, .before = 1)
})
scm_diagnostics <- purrr::map_dfr(
  scm_results[!vapply(scm_results, inherits, logical(1), what = "error")],
  "diagnostics"
)

readr::write_csv(scm_status, file.path(DIR_TABLES, "status_scm.csv"))
write_if_nonempty(scm_paths, file.path(DIR_TABLES, "scm_trajetorias.csv"))
write_if_nonempty(scm_weights, file.path(DIR_TABLES, "scm_pesos_doadores.csv"))
write_if_nonempty(scm_diagnostics, file.path(DIR_TABLES, "scm_diagnosticos.csv"))
saveRDS(scm_results, file.path(DIR_MODELS, "modelos_controle_sintetico.rds"))

# Equilibrio ponderado, calculado separadamente para cada desfecho.
synthetic_balance <- purrr::imap_dfr(scm_results, function(result, outcome) {
  if (inherits(result, "error")) return(tibble::tibble())
  numeric_vars <- pre_summary |>
    dplyr::select(where(is.numeric)) |>
    names()
  weights <- result$weights |>
    dplyr::filter(weight > 0) |>
    dplyr::select(reporter_iso, weight)
  
  purrr::map_dfr(numeric_vars, function(variable) {
    target <- pre_summary |>
      dplyr::filter(reporter_iso == REPORTER_TRATADO) |>
      dplyr::pull(dplyr::all_of(variable))
    donor_values <- pre_summary |>
      dplyr::select(reporter_iso, value = dplyr::all_of(variable)) |>
      dplyr::inner_join(weights, by = "reporter_iso") |>
      dplyr::filter(is.finite(value), is.finite(weight))
    if (length(target) != 1L || nrow(donor_values) == 0L) return(tibble::tibble())
    normalized_weight <- donor_values$weight / sum(donor_values$weight)
    tibble::tibble(
      outcome = outcome,
      variable = variable,
      ukraine = target,
      synthetic = sum(donor_values$value * normalized_weight),
      difference = target - sum(donor_values$value * normalized_weight)
    )
  })
})
write_if_nonempty(synthetic_balance, file.path(DIR_TABLES, "scm_equilibrio_ponderado.csv"))

# Figuras de trajetoria e gap.
purrr::iwalk(scm_results, function(result, outcome) {
  if (inherits(result, "error")) return(invisible(NULL))
  long <- result$path |>
    dplyr::select(ref_year, observed, synthetic) |>
    tidyr::pivot_longer(-ref_year, names_to = "series", values_to = "value")
  p <- ggplot2::ggplot(long, ggplot2::aes(ref_year, value, color = series)) +
    ggplot2::geom_vline(xintercept = ANO_TRATAMENTO - 0.5, linetype = 2, color = "grey50") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_x_continuous(breaks = ANO_INICIO_INFERENCIAL:ANO_FIM) +
    ggplot2::labs(
      x = NULL, y = outcome, color = NULL,
      title = paste("Ukraine versus synthetic control:", outcome)
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave(
    file.path(DIR_FIGURES, paste0("scm_", safe_name(outcome), ".png")),
    p, width = 9, height = 5.5, dpi = 300
  )
})

outcomes <- c(
  "log_import_value", "hhi", "effective_suppliers", "entropy_effective_suppliers",
  "top1_share", "top5_share", "n_suppliers", "supplier_entry", "supplier_exit",
  "persistence_rate", "n_core", "n_peripheral", "core_share", "peripheral_share",
  "macroblock_hhi"
)

# Os modelos abaixo sao sensibilidades. Eles nao substituem o controle
# sintetico e somente recebem interpretacao causal quando as pre-tendencias nao
# sao rejeitadas e a matriz de variancia e valida.

fit_event_study <- function(outcome, data = analysis_panel_common) {
  formula <- stats::as.formula(sprintf(
    "%s ~ i(event_time, treated, ref = -1) | reporter_iso + ref_year",
    outcome
  ))
  fixest::feols(formula, data = data, vcov = ~ reporter_iso)
}

fit_did <- function(outcome, data = analysis_panel_common, treatment = "treated", post_var = "post") {
  formula <- stats::as.formula(sprintf(
    "%s ~ %s:%s | reporter_iso + ref_year",
    outcome, treatment, post_var
  ))
  fixest::feols(formula, data = data, vcov = ~ reporter_iso)
}

tidy_fixest_safe <- function(model, outcome, model_type) {
  if (inherits(model, "error") || is.null(model)) {
    return(tibble::tibble(
      outcome = outcome, model = model_type, term = NA_character_,
      estimate = NA_real_, std.error = NA_real_, statistic = NA_real_,
      p.value = NA_real_, conf.low = NA_real_, conf.high = NA_real_
    ))
  }
  broom::tidy(model, conf.int = TRUE) |>
    dplyr::mutate(outcome = outcome, model = model_type, .before = 1)
}

validate_fixest_vcov <- function(model) {
  if (inherits(model, "error") || is.null(model)) return(FALSE)
  v <- tryCatch(stats::vcov(model), error = function(e) NULL)
  if (is.null(v) || any(!is.finite(v)) || nrow(v) == 0L) return(FALSE)
  if (qr(v)$rank < ncol(v)) return(FALSE)
  TRUE
}
if (RUN_MODELS) {
  event_models <- stats::setNames(
    lapply(outcomes, function(outcome) {
      tryCatch(fit_event_study(outcome), error = function(e) e)
    }),
    outcomes
  )
  
  did_models <- stats::setNames(
    lapply(outcomes, function(outcome) {
      tryCatch(fit_did(outcome), error = function(e) e)
    }),
    outcomes
  )
  
  saveRDS(event_models, file.path(DIR_MODELS, "modelos_event_study.rds"))
  saveRDS(did_models, file.path(DIR_MODELS, "modelos_did.rds"))
  
  event_coefficients <- purrr::imap_dfr(
    event_models,
    ~ tidy_fixest_safe(.x, .y, "event_study")
  )
  did_coefficients <- purrr::imap_dfr(
    did_models,
    ~ tidy_fixest_safe(.x, .y, "did")
  )
  
  readr::write_csv(event_coefficients, file.path(DIR_TABLES, "coeficientes_event_study.csv"))
  readr::write_csv(did_coefficients, file.path(DIR_TABLES, "coeficientes_did.csv"))
  
  # Teste conjunto dos coeficientes anteriores a 2022.
  pretrend_tests <- purrr::imap_dfr(event_models, function(model, outcome) {
    if (inherits(model, "error")) {
      return(tibble::tibble(
        outcome = outcome, statistic = NA_real_, p_value = NA_real_,
        model_status = "estimation_failed"
      ))
    }
    if (!validate_fixest_vcov(model)) {
      return(tibble::tibble(
        outcome = outcome, statistic = NA_real_, p_value = NA_real_,
        model_status = "singular_vcov"
      ))
    }
    ans <- tryCatch(
      fixest::wald(model, keep = "event_time::-"),
      error = function(e) NULL
    )
    if (is.null(ans)) {
      tibble::tibble(
        outcome = outcome, statistic = NA_real_, p_value = NA_real_,
        model_status = "wald_failed"
      )
    } else {
      statistic <- if (length(ans$stat) > 0L) as.numeric(unname(ans$stat))[[1L]] else NA_real_
      p_value <- if (length(ans$p) > 0L) as.numeric(unname(ans$p))[[1L]] else NA_real_
      tibble::tibble(
        outcome = outcome,
        statistic = ifelse(is.finite(statistic), statistic, NA_real_),
        p_value = ifelse(is.finite(p_value), p_value, NA_real_),
        model_status = ifelse(
          is.finite(statistic) && is.finite(p_value), "ok", "nonfinite_wald"
        )
      )
    }
  })
  
  did_eligibility <- pretrend_tests |>
    dplyr::transmute(
      outcome,
      pretrend_p = p_value,
      pretrend_status = model_status,
      causal_interpretation_allowed =
        model_status == "ok" & !is.na(p_value) & p_value >= PRETREND_ALPHA
    )
  did_coefficients <- did_coefficients |>
    dplyr::left_join(did_eligibility, by = "outcome")
  event_coefficients <- event_coefficients |>
    dplyr::left_join(did_eligibility, by = "outcome")
  
  # Sobrescreve as versoes preliminares com os indicadores de validade.
  readr::write_csv(event_coefficients, file.path(DIR_TABLES, "coeficientes_event_study.csv"))
  readr::write_csv(did_coefficients, file.path(DIR_TABLES, "coeficientes_did.csv"))
  readr::write_csv(pretrend_tests, file.path(DIR_TABLES, "testes_conjuntos_pre_tendencias.csv"))
  
  # Graficos dos coeficientes dinamicos.
  purrr::iwalk(event_models, function(model, outcome) {
    if (inherits(model, "error")) return(invisible(NULL))
    png_path <- file.path(DIR_FIGURES, paste0("event_study_", safe_name(outcome), ".png"))
    grDevices::png(png_path, width = 1800, height = 1200, res = 180)
    print(fixest::iplot(
      model,
      main = paste("Event study:", outcome),
      xlab = "Anos relativos ao choque de 2022",
      ylab = "Estimativa e IC 95%",
      ref.line = 0
    ))
    grDevices::dev.off()
  })
  
  valid_event <- event_models[!vapply(event_models, inherits, logical(1), what = "error")]
  valid_did <- did_models[!vapply(did_models, inherits, logical(1), what = "error")]
  if (length(valid_event) > 0L) {
    modelsummary::modelsummary(
      valid_event,
      output = file.path(DIR_MODELS, "event_study.html"),
      stars = TRUE, gof_omit = "IC|Log|F|RMSE"
    )
  }
  if (length(valid_did) > 0L) {
    modelsummary::modelsummary(
      valid_did,
      output = file.path(DIR_MODELS, "did.html"),
      stars = TRUE, gof_omit = "IC|Log|F|RMSE"
    )
  }
}

# ------------------------------------------------------------------------------
# 10. HETEROGENEIDADE POR MACROBLOCO E CONTRASTES (H4)
# ------------------------------------------------------------------------------
INFERENTIAL_BLOCKS <- c("energy", "control")
DESCRIPTIVE_BLOCKS <- c("energy", "control", "final")
block_outcomes <- c("log_import_value", "hhi", "n_suppliers", "top5_share")

# Controle sintetico por macrobloco. O bloco final permanece exclusivamente
# descritivo porque 8806/8807 nao sao comparaveis em toda a janela pre-2022.
scm_block_results <- list()
for (block_i in INFERENTIAL_BLOCKS) {
  block_data <- analysis_block_panel_common |>
    dplyr::filter(macrobloco_id == block_i)
  for (outcome_i in block_outcomes) {
    key <- paste(block_i, outcome_i, sep = "__")
    scm_block_results[[key]] <- fit_scm_safe(
      outcome = outcome_i,
      data = block_data
    )
  }
}

scm_block_status <- purrr::imap_dfr(scm_block_results, function(result, key) {
  parts <- strsplit(key, "__", fixed = TRUE)[[1L]]
  tibble::tibble(
    macrobloco_id = parts[[1L]],
    outcome = parts[[2L]],
    status = if (inherits(result, "error")) "failed" else "ok",
    message = if (inherits(result, "error")) conditionMessage(result) else NA_character_
  )
})
scm_block_paths <- purrr::imap_dfr(scm_block_results, function(result, key) {
  if (inherits(result, "error")) return(tibble::tibble())
  parts <- strsplit(key, "__", fixed = TRUE)[[1L]]
  result$path |>
    dplyr::mutate(
      macrobloco_id = parts[[1L]],
      outcome = parts[[2L]],
      .before = 1
    )
})
scm_block_diagnostics <- purrr::imap_dfr(scm_block_results, function(result, key) {
  if (inherits(result, "error")) return(tibble::tibble())
  parts <- strsplit(key, "__", fixed = TRUE)[[1L]]
  result$diagnostics |>
    dplyr::mutate(macrobloco_id = parts[[1L]], .before = 1)
})
readr::write_csv(scm_block_status, file.path(DIR_TABLES, "scm_macroblocos_status.csv"))
write_if_nonempty(scm_block_paths, file.path(DIR_TABLES, "scm_macroblocos_trajetorias.csv"))
write_if_nonempty(scm_block_diagnostics, file.path(DIR_TABLES, "scm_macroblocos_diagnosticos.csv"))
saveRDS(scm_block_results, file.path(DIR_MODELS, "modelos_scm_macroblocos.rds"))

# O estudo de evento empilhado abaixo e mantido apenas como sensibilidade para
# contrastes formais; sua validade continua condicionada as pre-tendencias.
fit_block_event_studies <- function(outcome, data = analysis_block_panel_common) {
  blocks <- sort(unique(data$macrobloco_id))
  models <- stats::setNames(lapply(blocks, function(block) {
    block_data <- dplyr::filter(data, macrobloco_id == block)
    tryCatch(fit_event_study(outcome, block_data), error = function(e) e)
  }), blocks)
  
  coefficients <- purrr::imap_dfr(models, function(model, block) {
    tidy_fixest_safe(model, outcome, paste0("block_", block)) |>
      dplyr::mutate(macrobloco_id = block, .before = 1)
  }) |>
    dplyr::group_by(outcome, term) |>
    dplyr::mutate(p_fdr_bh = stats::p.adjust(p.value, method = "BH")) |>
    dplyr::ungroup()
  
  list(models = models, coefficients = coefficients)
}

block_results <- stats::setNames(lapply(block_outcomes, fit_block_event_studies), block_outcomes)
block_coefficients <- purrr::map_dfr(block_results, "coefficients")
readr::write_csv(block_coefficients, file.path(DIR_TABLES, "event_study_por_macrobloco.csv"))
saveRDS(purrr::map(block_results, "models"), file.path(DIR_MODELS, "modelos_por_macrobloco.rds"))

# Modelo empilhado com efeitos evento x macrobloco para contrastes diretos.
fit_stacked_block_model <- function(outcome, data = analysis_block_panel_common) {
  d <- data
  blocks <- sort(unique(d$macrobloco_id))
  event_values <- sort(setdiff(unique(d$event_time), -1L))
  term_map <- tidyr::expand_grid(macrobloco_id = blocks, event_time = event_values) |>
    dplyr::mutate(
      event_label = ifelse(event_time < 0, paste0("m", abs(event_time)), paste0("p", event_time)),
      variable = paste("ukr", event_label, macrobloco_id, sep = "_")
    )
  
  for (i in seq_len(nrow(term_map))) {
    v <- term_map$variable[[i]]
    b <- term_map$macrobloco_id[[i]]
    k <- term_map$event_time[[i]]
    d[[v]] <- as.integer(d$treated == 1L & d$macrobloco_id == b & d$event_time == k)
  }
  
  rhs <- paste(term_map$variable, collapse = " + ")
  formula <- stats::as.formula(sprintf(
    "%s ~ %s | reporter_iso^macrobloco_id + ref_year^macrobloco_id",
    outcome, rhs
  ))
  model <- fixest::feols(formula, data = d, vcov = ~ reporter_iso)
  list(model = model, term_map = term_map)
}

pairwise_block_contrasts <- function(stacked, outcome) {
  model <- stacked$model
  map <- stacked$term_map |>
    dplyr::filter(event_time >= 0L)
  b <- stats::coef(model)
  v <- stats::vcov(model)
  
  purrr::map_dfr(sort(unique(map$event_time)), function(k) {
    mk <- dplyr::filter(map, event_time == k, variable %in% names(b))
    if (nrow(mk) < 2L) return(tibble::tibble())
    pairs <- utils::combn(seq_len(nrow(mk)), 2L)
    purrr::map_dfr(seq_len(ncol(pairs)), function(j) {
      a <- mk[pairs[1L, j], ]
      c <- mk[pairs[2L, j], ]
      va <- a$variable[[1L]]
      vc <- c$variable[[1L]]
      estimate <- b[[va]] - b[[vc]]
      se <- sqrt(max(0, v[va, va] + v[vc, vc] - 2 * v[va, vc]))
      z <- estimate / se
      tibble::tibble(
        outcome = outcome,
        event_time = k,
        block_1 = a$macrobloco_id[[1L]],
        block_2 = c$macrobloco_id[[1L]],
        estimate_difference = estimate,
        std_error = se,
        z = z,
        p_value = 2 * stats::pnorm(abs(z), lower.tail = FALSE)
      )
    })
  }) |>
    dplyr::group_by(outcome, event_time) |>
    dplyr::mutate(p_fdr_bh = stats::p.adjust(p_value, method = "BH")) |>
    dplyr::ungroup()
}

stacked_results <- purrr::map(block_outcomes, function(outcome) {
  tryCatch(fit_stacked_block_model(outcome), error = function(e) e)
})
names(stacked_results) <- block_outcomes

block_contrasts <- purrr::imap_dfr(stacked_results, function(result, outcome) {
  if (inherits(result, "error")) return(tibble::tibble())
  pairwise_block_contrasts(result, outcome)
})
write_if_nonempty(block_contrasts, file.path(DIR_TABLES, "contrastes_macroblocos_H4.csv"))
saveRDS(stacked_results, file.path(DIR_MODELS, "modelos_empilhados_H4.rds"))

# ------------------------------------------------------------------------------
# 11. TESTES DE ROBUSTEZ E INFERENCIA COM POUCOS CLUSTERS
# ------------------------------------------------------------------------------
# 11.1 Placebos temporais apenas no periodo pre-tratamento.
placebo_years <- seq.int(ANO_INICIO_INFERENCIAL + 2L, ANO_TRATAMENTO - 1L)
time_placebos <- purrr::map_dfr(placebo_years, function(fake_year) {
  d <- analysis_panel_common |>
    dplyr::filter(ref_year < ANO_TRATAMENTO) |>
    dplyr::mutate(fake_post = as.integer(ref_year >= fake_year))
  
  purrr::map_dfr(primary_outcomes, function(outcome) {
    model <- tryCatch(
      fit_did(outcome, d, treatment = "treated", post_var = "fake_post"),
      error = function(e) e
    )
    tidy_fixest_safe(model, outcome, "time_placebo") |>
      dplyr::mutate(fake_year = fake_year, .before = 1)
  })
})
readr::write_csv(time_placebos, file.path(DIR_TABLES, "placebos_temporais.csv"))

# 11.2 Placebos no espaco para o controle sintetico. A Ucrania real e retirada
# do donor pool de cada placebo para nao contaminar o contrafactual pos-2022.
run_scm_placebos <- function(outcome) {
  placebo_data <- analysis_panel_common |>
    dplyr::filter(reporter_iso != REPORTER_TRATADO)
  fitted <- stats::setNames(lapply(SELECTED_CONTROLS, function(fake_treated) {
    fit_scm_safe(
      outcome = outcome,
      data = placebo_data,
      treated_unit = fake_treated,
      donors = setdiff(SELECTED_CONTROLS, fake_treated)
    )
  }), SELECTED_CONTROLS)
  
  status <- purrr::imap_dfr(fitted, function(result, fake_treated) {
    tibble::tibble(
      outcome = outcome,
      fake_treated = fake_treated,
      status = if (inherits(result, "error")) "failed" else "ok",
      message = if (inherits(result, "error")) conditionMessage(result) else NA_character_
    )
  })
  diagnostics <- purrr::imap_dfr(fitted, function(result, fake_treated) {
    if (inherits(result, "error")) return(tibble::tibble())
    result$diagnostics |>
      dplyr::mutate(fake_treated = fake_treated, .before = 1)
  })
  paths <- purrr::imap_dfr(fitted, function(result, fake_treated) {
    if (inherits(result, "error")) return(tibble::tibble())
    result$path |>
      dplyr::mutate(
        outcome = outcome,
        fake_treated = fake_treated,
        .before = 1
      )
  })
  list(status = status, diagnostics = diagnostics, paths = paths)
}

if (RUN_SCM_PLACEBOS) {
  scm_placebo_results <- stats::setNames(
    lapply(SCM_OUTCOMES, run_scm_placebos),
    SCM_OUTCOMES
  )
  scm_placebo_status <- purrr::map_dfr(scm_placebo_results, "status")
  scm_placebo_diagnostics <- purrr::map_dfr(scm_placebo_results, "diagnostics")
  scm_placebo_paths <- purrr::map_dfr(scm_placebo_results, "paths")
} else {
  scm_placebo_status <- tibble::tibble()
  scm_placebo_diagnostics <- tibble::tibble()
  scm_placebo_paths <- tibble::tibble()
}

write_if_nonempty(scm_placebo_status, file.path(DIR_TABLES, "scm_placebos_status.csv"))
write_if_nonempty(scm_placebo_diagnostics, file.path(DIR_TABLES, "scm_placebos_diagnosticos.csv"))
write_if_nonempty(scm_placebo_paths, file.path(DIR_TABLES, "scm_placebos_trajetorias.csv"))

# Inferencia por randomizacao baseada na razao RMSPE pos/pre. Placebos com ajuste
# pre-tratamento muito pior que o da Ucrania nao integram a distribuicao final.
randomization_inference <- purrr::map_dfr(SCM_OUTCOMES, function(outcome) {
  observed <- scm_diagnostics |>
    dplyr::filter(.data$outcome == .env$outcome) |>
    dplyr::slice(1)
  if (nrow(observed) == 0L || nrow(scm_placebo_diagnostics) == 0L) {
    return(tibble::tibble())
  }
  placebo_all <- scm_placebo_diagnostics |>
    dplyr::filter(.data$outcome == .env$outcome, is.finite(rmspe_ratio))
  placebo_eligible <- placebo_all |>
    dplyr::filter(
      pre_rmspe <= MAX_PRE_RMSPE_MULTIPLIER * observed$pre_rmspe[[1L]]
    )
  if (nrow(placebo_eligible) == 0L) return(tibble::tibble())
  
  tibble::tibble(
    outcome = outcome,
    observed_rmspe_ratio = observed$rmspe_ratio[[1L]],
    observed_mean_post_gap = observed$mean_post_gap[[1L]],
    n_placebos_total = nrow(placebo_all),
    n_placebos_eligible = nrow(placebo_eligible),
    randomization_p_rmspe = (
      1 + sum(placebo_eligible$rmspe_ratio >= observed$rmspe_ratio[[1L]])
    ) / (1 + nrow(placebo_eligible)),
    randomization_p_gap = (
      1 + sum(abs(placebo_eligible$mean_post_gap) >= abs(observed$mean_post_gap[[1L]]))
    ) / (1 + nrow(placebo_eligible))
  )
})
write_if_nonempty(randomization_inference, file.path(DIR_TABLES, "scm_inferencia_randomizacao.csv"))

# 11.3 Sensibilidade a retirada do doador de maior peso em cada desfecho.
scm_leave_largest_weight <- purrr::imap_dfr(scm_results, function(result, outcome) {
  if (inherits(result, "error")) return(tibble::tibble())
  largest_donor <- result$weights |>
    dplyr::slice_max(weight, n = 1, with_ties = FALSE) |>
    dplyr::pull(reporter_iso)
  refit <- fit_scm_safe(
    outcome = outcome,
    donors = setdiff(SELECTED_CONTROLS, largest_donor)
  )
  if (inherits(refit, "error")) {
    return(tibble::tibble(
      outcome = outcome,
      excluded_donor = largest_donor,
      status = "failed",
      message = conditionMessage(refit)
    ))
  }
  refit$diagnostics |>
    dplyr::mutate(
      excluded_donor = largest_donor,
      status = "ok",
      message = NA_character_,
      .before = 1
    )
})
write_if_nonempty(
  scm_leave_largest_weight,
  file.path(DIR_TABLES, "scm_robustez_sem_maior_doador.csv")
)

# 11.4 Exclusao da China.
transactions_without_china <- panel_harmonized_transactions |>
  dplyr::filter(partner_iso != "CHN")
panel_without_china <- build_annual_outcomes(transactions_without_china)$metrics |>
  dplyr::filter(reporter_iso %in% ANALYSIS_REPORTERS, ref_year %in% common_years)

scm_without_china <- stats::setNames(lapply(SCM_OUTCOMES, function(outcome) {
  fit_scm_safe(outcome = outcome, data = panel_without_china)
}), SCM_OUTCOMES)
china_exclusion <- purrr::imap_dfr(scm_without_china, function(result, outcome) {
  if (inherits(result, "error")) {
    return(tibble::tibble(
      outcome = outcome, status = "failed", message = conditionMessage(result)
    ))
  }
  result$diagnostics |>
    dplyr::mutate(status = "ok", message = NA_character_, .before = 1)
})
china_exclusion_paths <- purrr::imap_dfr(scm_without_china, function(result, outcome) {
  if (inherits(result, "error")) return(tibble::tibble())
  result$path |>
    dplyr::mutate(outcome = outcome, .before = 1)
})
readr::write_csv(china_exclusion, file.path(DIR_TABLES, "scm_robustez_sem_china.csv"))
write_if_nonempty(
  china_exclusion_paths,
  file.path(DIR_TABLES, "scm_robustez_sem_china_trajetorias.csv")
)
saveRDS(scm_without_china, file.path(DIR_MODELS, "modelos_scm_sem_china.rds"))

# 11.5 Limiares alternativos para fornecedor ativo.
threshold_results <- purrr::map_dfr(SUPPLIER_THRESHOLDS, function(threshold) {
  d <- build_annual_outcomes(panel_harmonized_transactions, threshold = threshold)$metrics |>
    dplyr::filter(reporter_iso %in% ANALYSIS_REPORTERS, ref_year %in% common_years)
  purrr::map_dfr(SCM_OUTCOMES, function(outcome) {
    result <- fit_scm_safe(outcome = outcome, data = d)
    if (inherits(result, "error")) {
      return(tibble::tibble(
        supplier_threshold = threshold,
        outcome = outcome,
        status = "failed",
        message = conditionMessage(result)
      ))
    }
    result$diagnostics |>
      dplyr::mutate(
        supplier_threshold = threshold,
        status = "ok",
        message = NA_character_,
        .before = 1
      )
  })
})
readr::write_csv(threshold_results, file.path(DIR_TABLES, "scm_robustez_limites_fornecedor.csv"))

# 11.6 Wild-cluster bootstrap, quando o pacote estiver disponivel.
wild_bootstrap_results <- tibble::tibble()
if (requireNamespace("fwildclusterboot", quietly = TRUE) && exists("did_models")) {
  wild_bootstrap_results <- purrr::imap_dfr(did_models[primary_outcomes], function(model, outcome) {
    if (inherits(model, "error")) return(tibble::tibble())
    param <- names(stats::coef(model))[stringr::str_detect(names(stats::coef(model)), "treated:post|post:treated")]
    if (length(param) != 1L) return(tibble::tibble())
    ans <- tryCatch(
      fwildclusterboot::boottest(
        model,
        clustid = "reporter_iso",
        param = param,
        B = N_WILD_BOOT,
        type = "rademacher",
        impose_null = TRUE,
        seed = SEED
      ),
      error = function(e) NULL
    )
    if (is.null(ans)) return(tibble::tibble())
    p_candidate <- ans$p_val
    if (is.null(p_candidate)) p_candidate <- ans$p.value
    if (is.null(p_candidate)) p_candidate <- ans$pvalue
    if (is.null(p_candidate) || length(p_candidate) == 0L) return(tibble::tibble())
    tibble::tibble(
      outcome = outcome,
      parameter = param,
      bootstrap_p = as.numeric(p_candidate)[[1L]],
      B = N_WILD_BOOT
    )
  })
  write_if_nonempty(wild_bootstrap_results, file.path(DIR_TABLES, "wild_cluster_bootstrap.csv"))
} else {
  mensagem("Pacote fwildclusterboot indisponivel: wild bootstrap nao executado.")
}

# ------------------------------------------------------------------------------
# 12. SERIE TEMPORAL INTERROMPIDA MENSAL - ANALISE COMPLEMENTAR
# ------------------------------------------------------------------------------
monthly_metrics <- NULL
its_newey_west <- NULL

if (RUN_MONTHLY_ITS) {
  monthly_raw_path <- file.path(
    DIR_RAW,
    paste0("comtrade_ucrania_mensal_harmonizado_", QUERY_SCHEMA_VERSION, "_raw.rds")
  )
  
  if (RUN_DOWNLOAD_MONTHLY) {
    monthly_pre_raw <- download_comtrade_grid(
      reporters = REPORTER_TRATADO,
      years = ANO_INICIO_INFERENCIAL:(ANO_TRATAMENTO - 1L),
      codes = CODIGOS_CONSULTA_PRE2022,
      frequency = "M"
    )
    monthly_post_raw <- download_comtrade_grid(
      reporters = REPORTER_TRATADO,
      years = ANO_TRATAMENTO:ANO_FIM_MENSAL,
      codes = CODIGOS_CONSULTA_POS2022_HARMONIZADO,
      frequency = "M"
    )
    monthly_raw <- dplyr::bind_rows(monthly_pre_raw, monthly_post_raw)
    saveRDS(monthly_raw, monthly_raw_path)
  } else {
    if (!file.exists(monthly_raw_path)) stop("Arquivo mensal bruto nao encontrado.")
    monthly_raw <- readRDS(monthly_raw_path)
  }
  
  monthly_clean <- standardize_comtrade(monthly_raw) |>
    dplyr::mutate(month_date = parse_month_date(period, ref_year, ref_month)) |>
    dplyr::filter(!is.na(month_date), month_date <= DATA_FIM_MENSAL)
  
  coverage_monthly <- monthly_clean |>
    dplyr::distinct(month_date, cmd_code) |>
    dplyr::count(month_date, name = "n_codes") |>
    tidyr::complete(
      month_date = seq.Date(
        as.Date(sprintf("%s-01-01", ANO_INICIO_INFERENCIAL)),
        DATA_FIM_MENSAL,
        by = "month"
      ),
      fill = list(n_codes = 0L)
    )
  readr::write_csv(coverage_monthly, file.path(DIR_TABLES, "cobertura_mensal_ucrania.csv"))
  
  monthly_structure <- calculate_structure_metrics(
    monthly_clean,
    time_vars = c("reporter_iso", "month_date")
  )$metrics
  
  full_month_grid <- tibble::tibble(
    month_date = seq.Date(
      as.Date(sprintf("%s-01-01", ANO_INICIO_INFERENCIAL)),
      DATA_FIM_MENSAL,
      by = "month"
    )
  )
  
  monthly_metrics <- full_month_grid |>
    dplyr::left_join(monthly_structure, by = "month_date") |>
    dplyr::mutate(reporter_iso = dplyr::coalesce(reporter_iso, REPORTER_TRATADO)) |>
    dplyr::arrange(month_date) |>
    dplyr::mutate(
      month_index = dplyr::row_number(),
      post = as.integer(month_date >= DATA_CHOQUE_MENSAL),
      shock_index = min(month_index[post == 1L]),
      time_after = ifelse(post == 1L, month_index - shock_index + 1L, 0L),
      month_factor = factor(format(month_date, "%m")),
      log_import_value = safe_log(reported_import_value)
    ) |>
    dplyr::select(-shock_index)
  
  readr::write_csv(monthly_metrics, file.path(DIR_PROCESSED, "serie_mensal_ucrania.csv"))
  
  monthly_coverage_rate <- mean(!is.na(monthly_metrics$reported_import_value))
  if (monthly_coverage_rate < MIN_MONTHLY_COVERAGE_FOR_ITS) {
    warning(sprintf(
      paste0(
        "Cobertura mensal de %.1f%%, abaixo do minimo de %.1f%%. ",
        "A serie interrompida foi omitida; as demais analises continuam."
      ),
      100 * monthly_coverage_rate,
      100 * MIN_MONTHLY_COVERAGE_FOR_ITS
    ))
  } else {
    its_outcomes <- c("log_import_value", "hhi", "n_suppliers", "top1_share")
    its_models <- stats::setNames(lapply(its_outcomes, function(outcome) {
      stats::lm(
        stats::as.formula(sprintf(
          "%s ~ month_index + post + time_after + month_factor", outcome
        )),
        data = monthly_metrics
      )
    }), its_outcomes)
    
    its_newey_west <- purrr::imap_dfr(its_models, function(model, outcome) {
      lag_nw <- floor(4 * (stats::nobs(model) / 100)^(2 / 9))
      test <- lmtest::coeftest(
        model,
        vcov. = sandwich::NeweyWest(
          model, lag = lag_nw, prewhite = FALSE, adjust = TRUE
        )
      )
      broom::tidy(test) |>
        dplyr::mutate(outcome = outcome, newey_west_lag = lag_nw, .before = 1)
    })
    
    saveRDS(its_models, file.path(DIR_MODELS, "modelos_serie_interrompida.rds"))
    readr::write_csv(its_newey_west, file.path(DIR_TABLES, "serie_interrompida_newey_west.csv"))
  }
}

# ------------------------------------------------------------------------------
# 13. ANALISE DESCRITIVA COMPLETA E REDES BIMODAIS
# ------------------------------------------------------------------------------
# Decomposicao da dependencia da China. Essa tabela evita interpretar todo
# aumento do HHI como reconcentracao difusa quando ele pode refletir um unico hub.
china_dependence <- ukraine_full_transactions |>
  dplyr::group_by(ref_year) |>
  dplyr::summarise(
    total_value = sum(primary_value),
    china_value = sum(primary_value[partner_iso == "CHN"]),
    china_share = dplyr::if_else(total_value > 0, china_value / total_value, NA_real_),
    .groups = "drop"
  )
readr::write_csv(china_dependence, file.path(DIR_TABLES, "dependencia_china_ucrania.csv"))

network_edges <- ukraine_full_transactions |>
  dplyr::group_by(ref_year, partner_iso, partner_desc, macrobloco_id, macrobloco) |>
  dplyr::summarise(weight = sum(primary_value), .groups = "drop")
readr::write_csv(network_edges, file.path(DIR_NETWORKS, "arestas_pais_macrobloco.csv"))

for (year_i in sort(unique(network_edges$ref_year))) {
  edges_year <- network_edges |>
    dplyr::filter(ref_year == year_i) |>
    dplyr::transmute(
      source = partner_desc,
      source_iso = partner_iso,
      target = macrobloco,
      target_id = macrobloco_id,
      weight = weight
    )
  
  nodes_countries <- edges_year |>
    dplyr::group_by(name = source, iso = source_iso) |>
    dplyr::summarise(total_value = sum(weight), .groups = "drop") |>
    dplyr::mutate(type = "country")
  nodes_blocks <- edges_year |>
    dplyr::group_by(name = target) |>
    dplyr::summarise(total_value = sum(weight), .groups = "drop") |>
    dplyr::mutate(iso = NA_character_, type = "macroblock")
  nodes_year <- dplyr::bind_rows(nodes_countries, nodes_blocks)
  
  readr::write_csv(edges_year, file.path(DIR_NETWORKS, paste0("arestas_", year_i, ".csv")))
  readr::write_csv(nodes_year, file.path(DIR_NETWORKS, paste0("nos_", year_i, ".csv")))
}

# Figuras descritivas principais.
p_imports <- ggplot2::ggplot(
  ukraine_full_block_metrics,
  ggplot2::aes(ref_year, reported_import_value, color = macrobloco_id, group = macrobloco_id)
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::scale_y_continuous(labels = scales::label_dollar(scale = 1e-9, suffix = " bi")) +
  ggplot2::scale_x_continuous(breaks = sort(unique(ukraine_full_block_metrics$ref_year))) +
  ggplot2::labs(
    x = NULL, y = "Valor importado (US$)", color = "Macrobloco",
    title = "Importacoes ucranianas por macrobloco tecnologico"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  file.path(DIR_FIGURES, "descritivo_importacoes_macroblocos.png"),
  p_imports, width = 10, height = 6, dpi = 300
)

p_structure <- ukraine_full$metrics |>
  dplyr::select(ref_year, hhi, top1_share, top5_share, core_share, peripheral_share) |>
  tidyr::pivot_longer(-ref_year, names_to = "indicator", values_to = "value") |>
  ggplot2::ggplot(ggplot2::aes(ref_year, value, color = indicator, group = indicator)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  ggplot2::scale_x_continuous(breaks = sort(unique(ukraine_full$metrics$ref_year))) +
  ggplot2::labs(
    x = NULL, y = "Indicador", color = NULL,
    title = "Concentracao observada e distribuicao nucleo-periferia"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  file.path(DIR_FIGURES, "descritivo_concentracao_nucleo_periferia.png"),
  p_structure, width = 10, height = 6, dpi = 300
)

# ------------------------------------------------------------------------------
# 14. EXPORTACAO CONSOLIDADA
# ------------------------------------------------------------------------------
excel_outputs <- list(
  hs_crosswalk = codigo_harmonizacao_hs,
  cobertura_anual = coverage_annual,
  presenca_codigos = code_presence,
  elegibilidade_doadores = control_selection$eligibility,
  equilibrio_pre = balance_table,
  covariaveis_pre = pre_summary,
  painel_A_total = analysis_panel_common,
  painel_A_blocos = analysis_block_panel_common,
  painel_B_ucrania = ukraine_full$metrics,
  painel_B_blocos = ukraine_full_block_metrics,
  composicao = composition_full,
  china_dependencia = china_dependence,
  scm_status = scm_status,
  scm_trajetorias = scm_paths,
  scm_pesos = scm_weights,
  scm_diagnosticos = scm_diagnostics,
  scm_equilibrio = synthetic_balance,
  scm_placebo_status = scm_placebo_status,
  scm_placebo_diag = scm_placebo_diagnostics,
  scm_randomizacao = randomization_inference,
  scm_sem_maior_doador = scm_leave_largest_weight,
  scm_blocos_status = scm_block_status,
  scm_blocos_traj = scm_block_paths,
  scm_blocos_diag = scm_block_diagnostics,
  event_study = if (exists("event_coefficients")) event_coefficients else tibble::tibble(),
  did = if (exists("did_coefficients")) did_coefficients else tibble::tibble(),
  pretrend_tests = if (exists("pretrend_tests")) pretrend_tests else tibble::tibble(),
  block_event = block_coefficients,
  block_contrasts = block_contrasts,
  placebo_time = time_placebos,
  without_china = china_exclusion,
  without_china_paths = china_exclusion_paths,
  thresholds = threshold_results
)

if (!is.null(monthly_metrics)) excel_outputs$monthly_series <- monthly_metrics
if (!is.null(its_newey_west)) excel_outputs$monthly_its <- its_newey_west
if (nrow(wild_bootstrap_results) > 0L) excel_outputs$wild_boot <- wild_bootstrap_results

# Remove abas vazias que podem ser produzidas quando um teste nao e estimavel.
excel_outputs <- excel_outputs[vapply(
  excel_outputs,
  function(x) is.data.frame(x) && ncol(x) > 0L,
  logical(1)
)]

# O Excel limita nomes de abas a 31 caracteres.
names(excel_outputs) <- substr(names(excel_outputs), 1L, 31L)
writexl::write_xlsx(
  excel_outputs,
  file.path(DIR_TABLES, "resultados_ijopm_v4.xlsx")
)

# ------------------------------------------------------------------------------
# 15. CHECAGENS FINAIS E LOG DE REPRODUTIBILIDADE
# ------------------------------------------------------------------------------
capture.output(sessionInfo(), file = file.path(DIR_LOGS, "sessionInfo.txt"))

run_manifest <- tibble::tibble(
  parameter = c(
    "analysis_start", "descriptive_start", "treatment_year", "analysis_end",
    "monthly_analysis_end", "treated_reporter", "donor_pool", "scm_method",
    "harmonized_codes", "complete_codes",
    "query_codes_pre2022", "query_codes_post2022_harmonized",
    "query_codes_post2022_complete", "hs_mapping_version", "comtradr_version",
    "synth_version", "partner_strategy", "supplier_threshold",
    "core_share_threshold", "core_cumulative_threshold",
    "max_pre_rmspe_multiplier", "excluded_inference_years", "seed"
  ),
  value = c(
    ANO_INICIO_INFERENCIAL, ANO_INICIO_DESCRITIVO, ANO_TRATAMENTO, ANO_FIM,
    as.character(DATA_FIM_MENSAL), REPORTER_TRATADO,
    paste(SELECTED_CONTROLS, collapse = ";"), "Synth_classic_separate_by_outcome",
    paste(CODIGOS_HARMONIZADOS, collapse = ";"),
    paste(CODIGOS_COMPLETOS, collapse = ";"),
    paste(CODIGOS_CONSULTA_PRE2022, collapse = ";"),
    paste(CODIGOS_CONSULTA_POS2022_HARMONIZADO, collapse = ";"),
    paste(CODIGOS_CONSULTA_POS2022_COMPLETO, collapse = ";"),
    QUERY_SCHEMA_VERSION,
    as.character(utils::packageVersion("comtradr")),
    as.character(utils::packageVersion("Synth")),
    PARTNER_STRATEGY_ACTIVE,
    MIN_VALUE_SUPPLIER, CORE_SHARE_THRESHOLD, CORE_CUMULATIVE_THRESHOLD,
    MAX_PRE_RMSPE_MULTIPLIER,
    paste(YEARS_EXCLUDED_INFERENCE, collapse = ";"), SEED
  )
)
readr::write_csv(run_manifest, file.path(DIR_LOGS, "manifesto_execucao.csv"))

if (length(SELECTED_CONTROLS) < MIN_DONOR_POOL) {
  warning(
    paste0(
      "Donor pool pequeno. Priorize trajetorias, RMSPE e placebos; ",
      "nao dependa apenas de p-valores convencionais."
    )
  )
}

mensagem("Pipeline V4 concluido.")
mensagem("Tratado: %s", REPORTER_TRATADO)
mensagem("Donor pool: %s", paste(SELECTED_CONTROLS, collapse = ", "))
mensagem("Anos comuns: %s", paste(common_years, collapse = ", "))
mensagem("Resultados salvos em: %s", normalizePath(OUTPUT_ROOT))
 

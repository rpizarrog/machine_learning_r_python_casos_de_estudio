# Autor: Rubén Pizarro Gurrola, soporte IAG
# 2025
# Funciones para el caso LDA

# Función para cargar datos de una url local o internet
f_cargar_datos <- function (url){
  datos <- read.csv(url)
  return (datos)
}

# Función para presentar datos en formato de tablas
# Se unen los datos head y tail
# Función que une y visualiza los primeros y últimos registros
f_visualizar_tabla <- function(datos, n_registros = 5) {
  # Validar que los datos tengan suficientes registros
  if (nrow(datos) < (n_registros * 2)) {
    stop("El data.frame es muy pequeño para mostrar head y tail.")
  }
  
  # Obtener los primeros y últimos registros
  head_df <- head(datos, n_registros)
  tail_df <- tail(datos, n_registros)
  
  # Dos posiciones decimales
  head_df <- head_df %>%
    mutate(across(where(is.numeric), ~ round(., 2)))
  
  tail_df <- tail_df %>%
    mutate(across(where(is.numeric), ~ round(., 2)))
  
  # Crear una fila de separación visual
  separador <- tibble(!!!setNames(rep("...", ncol(datos)), names(datos)))
  
  # Unir los data.frames
  tabla_final <- rbind(head_df, separador, tail_df)
  
  
  
  # Crear y formatear la flextable
  flextable(tabla_final) %>%
    set_caption("Primeros y últimos registros del conjunto de datos") %>%
    colformat_num(digits = 2) %>%   # redondea todas las columnas numéricas a 2 decimales
    autofit() %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    bold(part = "header") %>%
    set_table_properties(layout = "autofit") %>%
    theme_box()
}



# Función para constuir modelo LDA
# rercibe datos limpios
# depura el atributo clase
# Utiliza enm el modelo solo las variables numéricas
# Devuelkve conjunto de datos redimensionados
f_construir_LDA <- function(datos) {
  if (!("clase" %in% names(datos))) {
    stop("El data.frame debe contener la columna 'clase'.")
  }
  
  clase <- as.factor(datos$clase)
  
  # Seleccionar SOLO numéricas como predictoras
  es_num <- vapply(datos, is.numeric, logical(1))
  X <- as.matrix(datos[, es_num, drop = FALSE])
  # excluir la columna 'clase' si quedó marcada como numérica por error
  if ("clase" %in% colnames(X)) X <- X[, setdiff(colnames(X), "clase"), drop = FALSE]
  if (ncol(X) == 0) stop("No hay columnas numéricas para entrenar LDA.")
  
  # Validaciones básicas
  if (any(!is.finite(X))) stop("Hay valores no finitos en X. Limpia o imputa antes.")
  if (nlevels(clase) < 2) stop("Se requieren al menos 2 clases para LDA.")
  
  # Escalar (centrar y escalar)
  Xs <- scale(X, center = TRUE, scale = TRUE)
  
  # --- LDA ---
  suppressPackageStartupMessages(require(MASS))
  fit <- lda(x = Xs, grouping = clase)  # K clases → hasta K-1 dims
  
  # Scores LDA (proyecciones). Para 2 clases → 1 columna; K>2 → varias.
  Z <- predict(fit, Xs)$x
  if (is.null(dim(Z))) Z <- matrix(Z, ncol = 1)
  colnames(Z) <- paste0("LD", seq_len(ncol(Z)))
  
  # D1 siempre existe
  D1 <- as.numeric(Z[, 1])
  
  # D2:
  if (ncol(Z) >= 2) {
    # Si hay ≥2 discriminantes (K>2), usamos el segundo eje real de LDA
    D2 <- as.numeric(Z[, 2])
  } else {
    # K = 2 → fabricamos eje auxiliar: PC1 ortogonal a w (solo visual)
    pca <- prcomp(Xs, center = FALSE, scale. = FALSE)
    pc1 <- pca$rotation[, 1]           # vector (d)
    
    w <- fit$scaling[, 1]              # dirección LDA (d)
    w_norm <- as.vector(w / sqrt(sum(w^2)))
    
    # Gram–Schmidt: quitar componente alineada con w
    v2_star <- pc1 - sum(pc1 * w_norm) * w_norm
    v2 <- as.vector(v2_star / sqrt(sum(v2_star^2)))   # dirección unitaria
    
    D2 <- as.numeric(Xs %*% v2)
  }
  
  resultado <- data.frame(
    id    = seq_len(nrow(datos)),
    clase = clase,
    D1    = round(D1, 4),
    D2    = round(D2, 4)
  )
  return(resultado)
}



# función para visualizar tabla de dos dimensiones
# Visualzia la dispersi´kn de los datos dos dimensiones en relación a la clase
f_dispersion_dimensiones_LDA <- function(dimensiones, etiquetar_id = FALSE) {
  req <- c("D1", "D2", "clase")
  if (!all(req %in% names(dimensiones))) {
    stop("El data.frame debe contener 'D1', 'D2' y 'clase'.")
  }
  
  suppressPackageStartupMessages(require(ggplot2))
  
  # paleta
  pal <- c("Sano" = "green", "Enfermo" = "red", "Riesgo" = "yellow")
  # fallback para clases no previstas
  clases <- unique(dimensiones$clase)
  missing_cols <- setdiff(clases, names(pal))
  if (length(missing_cols) > 0) {
    extra <- scales::hue_pal()(length(missing_cols))
    names(extra) <- missing_cols
    pal <- c(pal, extra)
  }
  
  p <- ggplot(dimensiones, aes(x = D1, y = D2, color = clase)) +
    geom_point(size = 2.5, alpha = 0.85, stroke = 0.3) +
    scale_color_manual(values = pal) +
    labs(
      title = "Proyección en dos dimensiones (LDA + eje ortogonal)",
      x = "Dimensión 1 (LDA)",
      y = "Dimensión 2 (PCA ortogonal a w)",
      color = "Clase"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
  
  if (etiquetar_id && "id" %in% names(dimensiones)) {
    if (!requireNamespace("ggrepel", quietly = TRUE)) {
      warning("Para etiquetas usa ggrepel::geom_text_repel; instala 'ggrepel'.")
    } else {
      p <- p + ggrepel::geom_text_repel(aes(label = id), size = 3, max.overlaps = 50)
    }
  }
  
  print(p)
  invisible(p)
}


# ---------------------------------------------
# f_fisher_razon(datos)
# Calcula la razón de Fisher J = (w' S_B w) / (w' S_W w)
# - datos: data.frame con columnas numéricas predictoras y una columna 'clase'
# - Escala solo las variables numéricas
# - Usa el primer discriminante lineal (LD1) como w
# Devuelve: lista con numerador, denominador y razon_fisher
# ---------------------------------------------
f_fisher_razon <- function(datos) {
  if (!("clase" %in% names(datos))) {
    stop("El data.frame debe contener la columna 'clase'.")
  }
  # Seleccionar solo numéricas
  num_mask <- sapply(datos, is.numeric)
  num_mask["clase"] <- FALSE
  if (!any(num_mask)) stop("No hay variables numéricas en 'datos' además de 'clase'.")
  
  X <- as.matrix(datos[, num_mask, drop = FALSE])
  y <- as.factor(datos$clase)
  clases <- levels(y)
  p <- ncol(X)
  
  # Escalar (media 0, sd 1)
  Xs <- scale(X)
  
  # Medias
  mu_global <- colMeans(Xs)
  medias <- lapply(clases, function(cl) colMeans(Xs[y == cl, , drop = FALSE]))
  names(medias) <- clases
  
  # SW: dispersión dentro de clases
  SW <- matrix(0, nrow = p, ncol = p)
  for (cl in clases) {
    Xc <- Xs[y == cl, , drop = FALSE]
    Mc <- sweep(Xc, 2, medias[[cl]], FUN = "-")
    SW <- SW + t(Mc) %*% Mc
  }
  
  # SB: dispersión entre clases
  SB <- matrix(0, nrow = p, ncol = p)
  for (cl in clases) {
    nk <- sum(y == cl)
    diff <- (medias[[cl]] - mu_global)
    SB <- SB + nk * (matrix(diff, ncol = 1) %*% matrix(diff, nrow = 1))
  }
  
  # LDA para obtener w (primer discriminante)
  fit <- lda(x = Xs, grouping = y)
  # 'scaling' es matriz p x k; tomamos la 1a columna (LD1)
  w <- matrix(fit$scaling[, 1], ncol = 1)
  
  numerador   <- as.numeric(t(w) %*% SB %*% w)
  denominador <- as.numeric(t(w) %*% SW %*% w)
  J <- numerador / denominador
  
  list(
    numerador = numerador,
    denominador = denominador,
    razon_fisher = J
  )
}

# ---------------------------------------------
# f_matriz_confusion_LDA(datos)
# Ajusta LDA (con variables numéricas escaladas), predice y construye:
# - Matriz de confusión
# - Métricas básicas por clase: precision, recall, F1
# - Accuracy global
# Devuelve una lista con la matriz de confusión y las métricas
# ---------------------------------------------
f_matriz_confusion_LDA <- function(datos) {
  if (!("clase" %in% names(datos))) {
    stop("El data.frame debe contener la columna 'clase'.")
  }
  # Seleccionar solo numéricas
  num_mask <- sapply(datos, is.numeric)
  num_mask["clase"] <- FALSE
  if (!any(num_mask)) stop("No hay variables numéricas en 'datos' además de 'clase'.")
  
  X <- as.matrix(datos[, num_mask, drop = FALSE])
  y <- as.factor(datos$clase)
  niveles <- levels(y)
  
  # Escalar
  Xs <- scale(X)
  
  # Modelo LDA
  fit <- lda(x = Xs, grouping = y)
  pred <- predict(fit, Xs)$class
  
  # Matriz de confusión (orden consistente con niveles)
  cm <- table(Real = y, Pred = factor(pred, levels = niveles))
  
  # Accuracy
  acc <- sum(diag(cm)) / sum(cm)
  
  # Métricas por clase (precision, recall, F1)
  # precision_c = TP / (TP + FP), recall_c = TP / (TP + FN)
  métricas <- lapply(niveles, function(cl) {
    TP <- cm[cl, cl]
    FP <- sum(cm[, cl]) - TP
    FN <- sum(cm[cl, ]) - TP
    precision <- if ((TP + FP) == 0) NA else TP / (TP + FP)
    recall    <- if ((TP + FN) == 0) NA else TP / (TP + FN)
    F1 <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA else 2 * precision * recall / (precision + recall)
    c(precision = precision, recall = recall, F1 = F1)
  })
  métricas <- do.call(rbind, métricas)
  rownames(métricas) <- niveles
  
  cat("\nMatriz de Confusión:\n")
  print(cm)
  cat(sprintf("\nExactitud global (Accuracy): %.4f\n", acc))
  cat("\nMétricas por clase (precision, recall, F1):\n")
  print(round(métricas, 4))
  
  invisible(list(
    matriz_confusion = cm,
    accuracy = acc,
    metricas = métricas,
    niveles = niveles
  ))
}


# ---------------------------------------------
# f_construir_LDA_de_mas_valores_en_clase(datos, devolver_modelos=FALSE)
# - datos: data.frame con variables numéricas y una columna 'clase'
# - Escala X (media 0, sd 1)
# - Si K >= 3: devuelve LD1 y LD2 reales de LDA
# - Si K = 2: devuelve LD1 de LDA y un D2 artificial ortogonal a w (basado en PC1)
# - Si devolver_modelos=TRUE, retorna también objetos del modelo:
#   lista(list(dimensiones, lda_fit, scaler, w_vec))
#   * scaler: list(center, scale)
#   * w_vec: vector normalizado usado solo en K=2 (si K>=3 será NULL)
# ---------------------------------------------
f_construir_LDA_de_mas_valores_en_clase <- function(datos, devolver_modelos = FALSE) {
  # Validaciones
  if (!("clase" %in% names(datos))) {
    stop("El data.frame debe contener la columna 'clase'.")
  }
  # Solo numéricas como X
  num_mask <- sapply(datos, is.numeric)
  num_mask["clase"] <- FALSE
  if (!any(num_mask)) stop("No hay columnas numéricas para entrenar LDA.")
  
  X <- as.matrix(datos[, num_mask, drop = FALSE])
  if (!all(is.finite(X))) stop("Hay valores no finitos en las columnas numéricas. Limpia/imputa antes.")
  
  # Objetivo
  y <- as.factor(datos$clase)
  clases <- levels(y)
  K <- length(clases)
  if (K < 2) stop("Se requieren al menos 2 clases para LDA.")
  
  # Escalado
  Xs <- scale(X)
  scaler <- list(center = attr(Xs, "scaled:center"),
                 scale  = attr(Xs, "scaled:scale"))
  
  p <- ncol(Xs)
  # Máximo de componentes: min(p, K - 1)
  n_comp <- min(p, K - 1, 2)
  
  # Ajuste LDA
  fit <- MASS::lda(x = Xs, grouping = y)
  pred <- predict(fit, Xs)
  
  # D1 siempre
  if (is.null(pred$x)) {
    stop("No se pudieron obtener las puntuaciones de LDA (pred$x es NULL).")
  }
  D1 <- pred$x[, 1]
  
  w_vec <- NULL
  # D2: si hay >= 2 discriminantes disponibles, usar LD2 real
  if (n_comp >= 2 && ncol(pred$x) >= 2) {
    D2 <- pred$x[, 2]
  } else {
    # K=2 → fabricar D2 ortogonal a w (solo visual)
    # w en el espacio de variables (cargas de LD1)
    w <- as.numeric(fit$scaling[, 1])
    w_norm <- w / sqrt(sum(w^2))
    
    # PC1 en el espacio de variables
    pc <- prcomp(Xs, center = FALSE, scale. = FALSE)
    pc1 <- as.numeric(pc$rotation[, 1])
    
    # Ortogonalizar PC1 respecto a w: D2_dir = pc1 - (pc1·w_norm) * w_norm
    proj <- sum(pc1 * w_norm)
    orto <- pc1 - proj * w_norm
    # Si la norma es ~0 (numéricamente inestable), usar la siguiente PC disponible
    if (sqrt(sum(orto^2)) < .Machine$double.eps^0.5) {
      # intentar con PC2 (si existe)
      if (ncol(pc$rotation) >= 2) {
        pc2 <- as.numeric(pc$rotation[, 2])
        proj2 <- sum(pc2 * w_norm)
        orto <- pc2 - proj2 * w_norm
      }
    }
    D2_dir <- orto / sqrt(sum(orto^2))
    D2 <- as.vector(Xs %*% D2_dir)
    w_vec <- w_norm
  }
  
  # Salida como data.frame
  dimensiones <- data.frame(
    id    = seq_len(nrow(datos)),
    clase = y,
    D1    = round(as.numeric(D1), 4),
    D2    = round(as.numeric(D2), 4)
  )
  
  if (devolver_modelos) {
    return(list(
      dimensiones  = dimensiones,
      lda_fit      = fit,
      scaler       = scaler,
      w_vec        = w_vec   # solo no-NULL cuando K=2
    ))
  }
  return(dimensiones)
}
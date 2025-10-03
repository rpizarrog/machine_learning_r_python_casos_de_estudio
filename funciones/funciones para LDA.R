# Autor: Rubén Pizarro Gurrola, soporte IAG
# 2025
# Funciones para el caso LDA
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

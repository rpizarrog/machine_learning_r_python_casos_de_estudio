# Funciones para PCA
# Agosto 2025
# Rubén Pizarro Gurrola
# Función para cargar datos de una url local o internet
f_cargar_datos <- function (url){
  datos <- read.csv(url)
  return (datos)
}

# Función para presentar datos a manera de tablas
# Recibe los datos y el tipo de registros head o tail default son 10
f_datos_tablas <- function(datos, tipo = 'head', decimales = 2) {
  # Crear tabla con los primeros 10 registros 
  if (tipo == 'head') {
    tabla <- as.data.frame(head(datos, 10,) %>%
                             round(decimales))
    # Crear y formatear la flextable
    flextable(tabla) %>%
      autofit() %>% # Ajusta automáticamente el ancho de columnas
      align(align = "center", part = "all") %>%  # Centra el contenido
      fontsize(size = 4, part = "all") %>%      # Tamaño de fuente
      bold(part = "header") %>%        # Negrita en el encabezado
      set_table_properties(layout = "autofit") %>%  # Ajuste automático para Word
      theme_box() %>%                  # Tema con bordes
      set_caption("Primeros registros de ventas")  # Título de la tabla
    
  } else {
    tabla <- as.data.frame(tail(datos, 10) %>%
                             round(decimales))
    # Crear y formatear la flextable
    flextable(tabla) %>%
      autofit() %>% # Ajusta automáticamente el ancho de columnas
      align(align = "center", part = "all") %>%  # Centra el contenido
      fontsize(size = 4, part = "all") %>%      # Tamaño de fuente
      bold(part = "header") %>%        # Negrita en el encabezado
      set_table_properties(layout = "autofit") %>%  # Ajuste automático para Word
      theme_box() %>%                  # Tema con bordes
      set_caption("Últimos registros de ventas")  # Título de la tabla
  }
  
}


# Función para analizar cada componente
f_analizar_componente <- function(cargas, componente_num, n_vars = 5) {
  componente <- paste0("PC", componente_num)
  
  # Ordenar variables por importancia absoluta
  vars_importantes <- cargas %>%
    select(Variable, all_of(componente)) %>%
    mutate(Magnitud = abs(.[[2]])) %>%
    arrange(desc(Magnitud)) %>%
    head(n_vars)
  rownames(vars_importantes) <- NULL
  return(vars_importantes)
}

# Función para gráficos de variables importantes de un PCA
# Recibe las cargas del pca con su varianza explicada de cada # variable de cada componente de las cuales representa las 
# principales variables de cada componente
# Por default 5 componentes 5 variables 
# aunque puede ser variar
f_graf_cargas_importantes <- function(cargas, n_componentes = 5, n_vars = 5) {
  # Seleccionar dinámicamente los componentes (PC1, PC2, ..., PCn)
  componentes <- paste0("PC", 1:n_componentes)
  
  cargas_abs <- cargas %>%
    select(Variable, all_of(componentes)) %>%
    pivot_longer(
      cols = -Variable,
      names_to = "Componente",
      values_to = "Loading"
    ) %>%
    group_by(Componente) %>%
    mutate(Magnitud = abs(Loading)) %>%
    slice_max(order_by = Magnitud, n = n_vars, with_ties = FALSE) %>%
    ungroup()
  
  # Gráfico
  ggplot(cargas_abs, aes(x = reorder(Variable, Loading), y = Loading, fill = Loading)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "blue",
      midpoint = 0, name = "Contribución"
    ) +
    facet_wrap(~Componente, scales = "free_y") +
    coord_flip() +
    labs(
      title = "Variables más importantes por Componente Principal",
      subtitle = paste("Top", n_vars, "variables por cada componente"),
      x = "Variables",
      y = "Loading (Contribución)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

# Función para presentar datos en foramto de tablas
# Se unen los datos hedad y tail
# Función que une y visualiza los primeros y últimos registros
f_visualizar_tabla <- function(datos, n_registros = 5) {
  # Validar que los datos tengan suficientes registros
  if (nrow(datos) < (n_registros * 2)) {
    stop("El data.frame es muy pequeño para mostrar head y tail.")
  }
  
  # Obtener los primeros y últimos registros
  head_df <- head(datos, n_registros)
  tail_df <- tail(datos, n_registros)
  
  # Crear una fila de separación visual
  separador <- tibble(!!!setNames(rep("...", ncol(datos)), names(datos)))
  
  # Unir los data.frames
  tabla_final <- rbind(head_df, separador, tail_df)
  
  # Crear y formatear la flextable
  flextable(tabla_final) %>%
    set_caption("Primeros y últimos registros del conjunto de datos") %>%
    autofit() %>%
    align(align = "center", part = "all") %>%
    fontsize(size = 8, part = "all") %>%
    bold(part = "header") %>%
    set_table_properties(layout = "autofit") %>%
    theme_box()
}
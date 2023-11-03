# Visualización Interactiva de Datos Financieros en 3D con Predicciones de Series Temporales y Optimización de Cartera basada en el S&P 500

## Fase 1: Recopilación de Datos Financieros:

### 1.1.	Precios Históricos de Acciones del S&P 500:

#### Insertar el archivo con el listado de acciones del S&P 500
  # Fuente: https://es.wikipedia.org/wiki/Anexo%3ACompa%C3%B1%C3%ADas_del_S%26P_500 
library(readxl)
gspc_stocks <- read_excel("C:\\Users\\Leidy Hernandez\\Documents\\GSPC project\\GSPC stocks.xlsx")

#### Descargar los datos desde Yahoo Finance
library(quantmod) 

if (!dir.exists("files_csv")) {
  dir.create("files_csv")
}

for (i in 1:nrow(gspc_stocks)) {
  symbol <- gspc_stocks$Símbolo[i]
  data_yf <- getSymbols(symbol, src = "yahoo", from = "2013-11-01", to = "2023-11-02")
  data_df <- data.frame(Fecha = index(data_yf), coredata(data_yf))
  name_file <- paste0("files_csv/", symbol, ".csv")
  write.csv(data_df, file = name_file, row.names = FALSE)
  cat('File', symbol,'.csv was created\n')
}

#### De ser necesario cargar los datos de nuevo
library(readr)
data_stocks <- list()

for (i in 1:nrow(gspc_stocks)) {
  symbol <- gspc_stocks$Símbolo[i]
  data <- read_csv(paste0("C:\\Users\\Leidy Hernandez\\Documents\\GSPC project\\files_csv\\", symbol, ".csv"))
  data_stocks[[symbol]] <- data
}

### 1.2 Datos del Índice Bursátil:
getSymbols("^GSPC", src = "yahoo", from = "2013-11-01", to = "2023-11-01")

### 1.3.	Datos de Effective Federal Funds Rate de EE.UU:
  # Fuente: https://fred.stlouisfed.org/series/DFF
funds_rate <- read.csv("C:\\Users\\Leidy Hernandez\\Documents\\GSPC project\\Effective Federal Funds Rate.csv")

### 1.4.	Datos de Tasas de Inflación:
  # Fuente: https://www.bls.gov/regions/mid-atlantic/data/consumerpriceindexhistorical_us_table.htm
cpi <- read_excel("C:\\Users\\Leidy Hernandez\\Documents\\GSPC project\\Consumer Price Index Historical.xlsx")


## Fase 2: Preprocesamiento y Transformación:

### 2.1.	Limpieza de Datos:

#### 2.1.1 Eliminar datos nulos o faltantes

symbols_with_missing_values <- c()

#####  Iterar sobre los símbolos y almacenar aquellos con valores faltantes
for (i in 1:nrow(gspc_stocks)) {
  symbol <- gspc_stocks$Símbolo[i]
  missing_val <- sum(is.na(data_stocks[[symbol]]))
  
  if (missing_val > 0) {
    symbols_with_missing_values <- c(symbols_with_missing_values, symbol)
    print(paste("For stock", symbol, "there are", missing_val, "missing values, so it was marked for deletion"))
  }
}

#####  Eliminar las columnas con valores faltantes en data_stocks
for (symbol in symbols_with_missing_values) {
  data_stocks[[symbol]] <- NULL
}

#####  Eliminar las entradas correspondientes en gspc_stocks
gspc_stocks <- gspc_stocks[!gspc_stocks$Símbolo %in% symbols_with_missing_values, ]



##### 2.1.2 Eliminar valores atípicos
for (i in 1:nrow(gspc_stocks)) {
  symbol <- gspc_stocks$Símbolo[i]
  if (grepl("-", symbol)) {
    new_symbol <- gsub("-", ".", symbol)
    column <- paste0(new_symbol, ".Close")
  } else{
    column <- paste0(symbol, ".Close")  
  }
  numeric_vector <- data_stocks[[symbol]][[column]]
  
  # Calcula el rango intercuartílico (IQR)
  Q1 <- quantile(numeric_vector, 0.25)
  Q3 <- quantile(numeric_vector, 0.75)
  IQR <- Q3 - Q1
  
  # Calcula los límites
  inf_limit <- Q1 - 1.5 * IQR
  sup_limit <- Q3 + 1.5 * IQR
  
  # Elimina filas con valores atípicos basándose en el precio de cierre
  data_stocks[[symbol]] <- data_stocks[[symbol]][numeric_vector >= inf_limit & numeric_vector <= sup_limit, ]
  
  # Verifica el número de filas después de eliminar valores atípicos
  num_rows <- nrow(data_stocks[[symbol]])
}


###  2.2. Cálculo de Métricas Financieras Clave:
#### 2.2.1. Rendimientos Diarios:

returns <- list()

for (i in 1:nrow(gspc_stocks)) {
  symbol <- gspc_stocks$Símbolo[i]
  
  if (grepl("-", symbol)) {
    new_symbol <- gsub("-", ".", symbol)
  } else {
    new_symbol <- symbol
  }
  
  data <- data_stocks[[symbol]]  

  if ("Fecha" %in% colnames(data) && paste0(new_symbol, ".Close") %in% colnames(data)) {
    dates <- data$Fecha
    column <- paste0(new_symbol, ".Close")
    prices <- data[[column]]
    
    data_xts <- xts(prices, order.by = as.Date(dates))
    
    returns[[symbol]] <- dailyReturn(data_xts, type = "log")
  }
}


####  2.2.2. Volatilidad (Desviación Estándar):
volatility <- list()

for (i in 1:nrow(gspc_stocks)) {
  symbol <- gspc_stocks$Símbolo[i]
  volatility[[symbol]] <- sd(returns[[symbol]], na.rm = TRUE)
}


####  2.2.3. Ratios Financieros:

##### Cargar el PE Ratio
pe_ratios <- read_excel("C:\\Users\\Leidy Hernandez\\Documents\\GSPC project\\PE Ratios.xlsx")

for (i in 1:nrow(gspc_stocks)) {
  if (pe_ratios$Stock[i] == gspc_stocks$Símbolo[i]){
  } else {
    cat("The stock",pe_ratios$Stock[i], "is worng, it must be",gspc_stocks$Símbolo[i],"\n")
  }
}

gspc_stocks

# tpcol.zoom.us/my/brandonmendoza - 8 AM

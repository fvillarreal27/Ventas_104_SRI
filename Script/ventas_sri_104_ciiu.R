# ==============================================================================
# Informacion de ventas locales SRI (Estadisticas Multidimensionales)
# DNPRMF
# Fabian Villarreal
#
# SRI Formulario 104 por CIIU
# https://srienlinea.sri.gob.ec/saiku-ui/
# ==============================================================================

# Librerias
pacman::p_load(tidyverse, readxl, openxlsx, dplyr, ggplot2, ggfortify, foreign, 
               timeDate, forecast, xts, urca, tseries, lubridate, stringi, 
               stringr, reshape2, expsmooth, seasonal, Metrics, highcharter)

# Colores
my_pal <- c('#1E4976', '#5FA3BF', '#BBBE64', '#EFCB68', '#E5825E', '#9D3C11')

# Directorio
setwd('D:/fvillarreal/Programacion Sector Real/Otros indicadores/PruebaSRICIIU')


### Datos: Estadisticas Multidimensionales ----------

# Columnas: 
# VENTAS LOCALES 12% (411), VENTAS LOCALES 0% (413), VENTAS 0% CON CRED TRIB (415)
# Filas: 
# FAMILIA, ANIO FISCAL, MES FISCAL

df <- read_excel('Datos/saiku-export.xlsx',
                 sheet = 'Sheet 1')

df <- data.frame(df)

# Tratamiento

colnames(df) <- c('ciiu', 'year', 'month',
                  'vt_locales_12', 'vt_locales_0', 'vt_0_cred_trib')

df <- df[grepl('^[A-Za-z]+$', df$ciiu),] # Filtrar industrias CIIU

periodo <- as.yearmon(as.Date(paste(df$year, df$month, 01), '%Y %m %d'))
df <- cbind(df, 'periodo' = periodo)

df <- df[df$periodo <= 'jun.2022', ] # Eliminar periodos sin datos


labels_ciiu <- data.frame(
  'cod_ciiu' = c('A', 'B', 'C', 'D','E', 'F', 'G', 'H', 'I', 'J', 
                 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U'),
  'descripcion' = c(
  'Agricultura, ganaderia, silvicultura y pesca', 
  'Explotacion de minas y canteras',
  'Industrias manufactureras',
  'Suministro de electricidad, gas, vapor y aire acondicionado',
  'Suministro de agua; evacuacion de aguas residuales, gestion de desechos y descontaminacion', 
  'Construccion',
  'Comercio al por mayor y al por menor; reparacion de vehiculos automotores y motocicletas',
  'Transporte y almacenamiento',
  'Actividades de alojamiento y de servicio de comidas', 
  'Informacion y comunicaciones', 
  'Actividades financieras y de seguros',
  'Actividades inmobiliarias',
  'Actividades profesionales, cientificas y tecnicas',
  'Actividades de servicios administrativos y de apoyo',
  'Administracion publica y', 
  'Ensenanza',
  'Actividades de atencion de la salud humana y de asistencia social',
  'Actividades artisticas, de entretenimiento y recreativas',
  'Otras actividades de servicios',
  'Actividades de los hogares como empleadores; actividades no diferenciadas de los hogares como productores de bienes y servicios para uso propio',
  'Actividades de organizaciones y organos extraterritoriales'))

labels_ciiu$descripcion <- paste0('CIIU ', labels_ciiu$cod_ciiu, ': ', labels_ciiu$descripcion)

### Procesamiento ----------

# Ventas locales totales
ventas <- c('vt_locales_12', 'vt_locales_0', 'vt_0_cred_trib')
df$vt_locales <- rowSums(df[, ventas])

df <- df %>% 
  group_by(ciiu, periodo) %>% 
  summarise('vt_locales' = sum(vt_locales))

df$anio <- year(df$periodo)
df$mes <- month.abb[month(df$periodo)]

df <- df %>% 
  group_by(ciiu) %>% 
  mutate('var_tt12' = (vt_locales/lag(vt_locales, n = 12) - 1) * 100) # Variacion interanual  

df <- df[df$anio >= 2018,] # Datos historicos


### Figuras ----------

# Opciones para hchart
hcoptslang <- getOption('highcharter.lang')
hcoptslang$decimalPoint <- ','
hcoptslang$thousandsSep <- '.'
options(highcharter.lang = hcoptslang)

# Figuras: nivel USD
hchart_level_ciiu <- list()

for (ind in 1:nrow(labels_ciiu)) {
  hchart_level_ciiu[[labels_ciiu[ind, 1]]] <- 
    hchart(df[df$ciiu == labels_ciiu[ind, 1],], 
           'line',
           hcaes(x = mes, y = vt_locales, group = anio),
           color = my_pal[1:5]) %>% 
    hc_title(text = labels_ciiu[ind, 2],
             style = list(fontSize = '14px', 
                          fontWeight = 'bold')) %>% 
    hc_subtitle(text = 'Ventas locales: Formulario 104 SRI') %>% 
    hc_yAxis(title = list(text = 'USD',
                          style = list(fontWeight = 'bold'))) %>% 
    hc_xAxis(title = list(text = '')) %>% 
    hc_tooltip(formatter = JS(
      "function() {
        var ret = '',
        multi,
        axis = this.series.yAxis,
        numericSymbols = ['k', 'M', 'G', 'T', 'P', 'E'],
        i = numericSymbols.length;
      while (i-- && ret === '') {
        multi = Math.pow(1000, i + 1);
        if (axis.tickInterval >= multi && numericSymbols[i] !== null) {
          ret = Highcharts.numberFormat(this.y / multi, 1) + numericSymbols[i];
        }
      }
      return ret;
    }"
    )) %>% 
    hc_plotOptions(
      line = list(
        marker = list(
          fillColor = "white",
          lineWidth = 2,
          lineColor = NULL,
          symbol = 'circle', 
          radius = 3
        ))) %>%
    hc_legend(itemStyle = list(fontSize = '11px'))
}

# Figuras: variacion interanual
hchart_vartt12_ciiu <- list()

for (ind in 1:nrow(labels_ciiu)) {
  hchart_vartt12_ciiu[[labels_ciiu[ind, 1]]] <- 
    hchart(df[df$ciiu == labels_ciiu[ind, 1],], 
           'column',
           hcaes(x = mes, y = var_tt12, group = anio),
           color = my_pal[1:5]) %>% 
    hc_title(text = labels_ciiu[ind, 2],
             style = list(fontSize = '14px', 
                          fontWeight = 'bold')) %>% 
    hc_subtitle(text = '\u0394% (t/t-12) Ventas locales: Formulario 104 SRI') %>% 
    hc_yAxis(title = list(text = 'Porcentaje', 
                          style = list(fontWeight = 'bold'))) %>% 
    hc_xAxis(title = list(text = '')) %>% 
    hc_tooltip(pointFormat = 'Porcentaje: <b>{point.y:.2f}</b><br/>') %>%
    hc_legend(itemStyle = list(fontSize = '11px'))
}


# Resultados
save(df, hchart_level_ciiu, hchart_vartt12_ciiu, labels_ciiu,
     file = 'Resultados/lst_df_charts.RData')

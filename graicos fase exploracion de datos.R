setwd("D:/PROMIDAT2/practicas proyecto")
source('funciones_base_de_datos.R')
source('funciones_verificacion_datos_ventas.R')

con <- conectar_bd_sqlserver("BD_ZEUS_PLUS", "localhost", "sa", "m0t1t4s@2009")
resultado <- consulta_ventas_por_mes_cliente( con, 1620877 )
desconectar_bd( con )

cliente.ventas <- fomatea_datos_ventas_cliente( resultado )

# Ventas de la regional por mes
consulta1<- function(conDB ) {
  rs <- dbSendQuery(
    conDB,
    "select  CAST( YEAR(s.FECHA_VENTA) as VARCHAR(4)) + '/' + CAST(MONTH(s.FECHA_VENTA) AS VARCHAR(2)) + '/01' FECHA, sum( s.MONTO_VENTA - s.DESCUENTO_FIDELIDAD2) MONTO_VENTA
from SERIES_VENTAS_HECHOS s
where s.COD_AREA_EMPRESA = 46
--and s.COD_LINEAVENTA = 1
group by CAST( YEAR(s.FECHA_VENTA) as VARCHAR(4)) + '/' + CAST(MONTH(s.FECHA_VENTA) AS VARCHAR(2))
ORDER BY FECHA
    ;"
  )
  return( rs )
}

con <- conectar_bd_sqlserver("BD_ZEUS_PLUS", "localhost", "sa", "m0t1t4s@2009")
resultado <- consulta1( con )
desconectar_bd( con )
datos <- fomatea_datos_ventas_cliente( resultado )
str(datos)

library(dplyr)
library(apexcharter)
library(ggplot2)

apex(data = datos,
     type = "line",
     mapping = aes(x = FECHA,
                   y = MONTO_VENTA))%>% 
  ax_markers(size = 4) %>% 
  ax_stroke(width = 2) %>% 
  ax_labs(
    x = "Fecha",
    y = "Monto de venta Bs."
  ) 


# Ventas de la regional de un mes
consulta2<- function(conDB ) {
  rs <- dbSendQuery(
    conDB,
    "set dateformat ymd;
select  s.FECHA_VENTA FECHA, sum( s.MONTO_VENTA - s.DESCUENTO_FIDELIDAD2) MONTO_VENTA
from SERIES_VENTAS_HECHOS s
where s.COD_AREA_EMPRESA = 46
and s.FECHA_VENTA BETWEEN '2020-04-01' and '2020-04-30 23:59:59'
group by s.FECHA_VENTA
ORDER BY FECHA
    ;"
  )
  return( rs )
}

con <- conectar_bd_sqlserver("BD_ZEUS_PLUS", "localhost", "sa", "m0t1t4s@2009")
resultado <- consulta2( con )
desconectar_bd( con )
datos <- fomatea_datos_ventas_cliente( resultado )

apex(data = datos,
     type = "line",
     mapping = aes(x = FECHA,
                   y = MONTO_VENTA))%>% 
  ax_markers(size = 4) %>% 
  ax_stroke(width = 2) %>% 
  ax_labs(
    x = "Fecha",
    y = "Monto de venta Bs."
  ) 


# Ventas por funcionario
consulta3<- function(conDB ) {
  rs <- dbSendQuery(
    conDB,
    "set dateformat ymd;
select s.FECHA_VENTA FECHA, p.AP_PATERNO_PERSONAL + ' ' +p.AP_MATERNO_PERSONAL PROMOTOR, sum( s.MONTO_VENTA - s.DESCUENTO_FIDELIDAD2) MONTO_VENTA
from SERIES_VENTAS_HECHOS s inner join personal p
on s.COD_PERSONAL = p.COD_PERSONAL 
where s.COD_AREA_EMPRESA = 46
and s.FECHA_VENTA BETWEEN '2020-04-01' and '2020-04-30 23:59:59'
and p.COD_PERSONAL in (1818,2658,2755)
group by s.FECHA_VENTA, p.AP_PATERNO_PERSONAL + ' ' +p.AP_MATERNO_PERSONAL
ORDER BY FECHA
    ;"
  )
  return( rs )
}

con <- conectar_bd_sqlserver("BD_ZEUS_PLUS", "localhost", "sa", "m0t1t4s@2009")
resultado <- consulta3( con )
desconectar_bd( con )
datos <- fomatea_datos_ventas_cliente( resultado )

apex(data = datos,
     type = "line",
     mapping = aes(x = FECHA,
                   y = MONTO_VENTA, group=PROMOTOR, col=PROMOTOR))%>% 
  ax_markers(size = 4) %>% 
  ax_stroke(width = 2) %>% 
  ax_labs(
    x = "Fecha",
    y = "Monto de venta Bs."
  ) 


# Ventas de un cliente
consulta4<- function(conDB ) {
  rs <- dbSendQuery(
    conDB,
    "select  CAST( YEAR(s.FECHA_VENTA) as VARCHAR(4)) + '/' + CAST(MONTH(s.FECHA_VENTA) AS VARCHAR(2)) + '/01' FECHA, sum( s.MONTO_VENTA - s.DESCUENTO_FIDELIDAD2) MONTO_VENTA
    from SERIES_VENTAS_HECHOS s
    where s.COD_CLIENTE = 141667
    --and s.COD_LINEAVENTA = 1
    group by CAST( YEAR(s.FECHA_VENTA) as VARCHAR(4)) + '/' + CAST(MONTH(s.FECHA_VENTA) AS VARCHAR(2))
    ORDER BY FECHA
    ;"
  )
  return( rs )
}

con <- conectar_bd_sqlserver("BD_ZEUS_PLUS", "localhost", "sa", "m0t1t4s@2009")
resultado <- consulta4( con )
desconectar_bd( con )
datos <- fomatea_datos_ventas_cliente( resultado )

apex(data = datos,
     type = "line",
     mapping = aes(x = FECHA,
                   y = MONTO_VENTA))%>% 
  ax_markers(size = 4) %>% 
  ax_stroke(width = 2) %>% 
  ax_labs(
    x = "Fecha",
    y = "Monto de venta Bs."
  ) 

/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP 10 ["fecha"]
      ,["mes"]
      ,["anio"]
      ,["semana"]
      ,["nombre_mes"]
      ,["tipo_fecha"]
      ,["cod_cuando"]
  FROM [covid19and].[dbo].[dimension_cuando]
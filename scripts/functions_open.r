##
get_df_sheck <- function(start_date = '2022-05-01',end_date = '2022-06-01',
                         check_sales_start = 0, check_sales_end = 1000000,
                         check_margin_start = 0, check_margin_end = 1000000,
                         con_dalion_en) {
  
  request_code <- paste0("SELECT [code_1c_shop],
[department_name],
[ds],
ROUND(SUM([sales_amount_fact]),2) AS sales,
ROUND(SUM([prime_cost]),2) AS prime,
COUNT(distinct [guid_check]) AS n_checks,
SUM([check_time_sec]) AS check_time

FROM(
SELECT [code_1c_shop],
[department_name],
[date] AS ds,
[sales_amount_fact],
[prime_cost],
[guid_check],
DATEDIFF(SECOND, [check_time_opening], [check_time_closing]) AS check_time_sec

FROM [dalion_en].[dbo].[sales]
WHERE ([date] BETWEEN 'Repl_start_date' AND 'Repl_end_date') 
AND ([sales_amount_fact] BETWEEN Repl_chsl0 AND Repl_chsl1)
AND (([sales_amount_fact]-[prime_cost]) BETWEEN Repl_chmrg0 AND Repl_chmrg1)) AS df_sales_time

GROUP BY [ds], [code_1c_shop], [department_name]
ORDER BY [code_1c_shop], [department_name], [ds]")
  
  # date
  request_code <- gsub("Repl_start_date", start_date, request_code)
  request_code <- gsub("Repl_end_date",   end_date,   request_code)
  # check sale
  request_code <- gsub("Repl_chsl0",   check_sales_start,   request_code)
  request_code <- gsub("Repl_chsl1",   check_sales_end,   request_code)
  # check margin
  request_code <- gsub("Repl_chmrg0",   check_margin_start,   request_code)
  request_code <- gsub("Repl_chmrg1",   check_margin_end,   request_code)
  
  df_sheck <- dbGetQuery(con_dalion_en,request_code)
  df_sheck$ds <- as.Date(df_sheck$ds)
  
  df_sheck <- df_sheck %>%
    mutate(dep_id = case_when(department_name == 'Мясной' ~ 'СМ',
                              department_name == 'Колбасный' ~ 'БЛ',
                              department_name == 'Кондитерский' ~ 'НК',
                              department_name == 'Молочный' ~ 'ЛМ',
                              department_name == 'Рыбный' ~ 'БР',
                              department_name == 'Кулинария' ~ 'ЛК'), .after = code_1c_shop) %>%
    
    mutate(dep_code = case_when(department_name == 'Мясной' ~ 'МЯСО',
                                department_name == 'Колбасный' ~ 'КОЛБАСА',
                                department_name == 'Кондитерский' ~ 'КОНДИТ',
                                department_name == 'Молочный' ~ 'МОЛОЧКА',
                                department_name == 'Рыбный' ~ 'РЫБА',
                                department_name == 'Кулинария' ~ 'КУЛИНАРИЯ'), .after = code_1c_shop) %>%
    
    mutate(dep_name = department_name) %>%
    dplyr::select(-department_name) %>%
    
    mutate(adr = case_when(code_1c_shop == '00001' ~ 'Альпийский25',
                           code_1c_shop == '00021' ~ 'БелыКуна16',
                           code_1c_shop == '00005' ~ 'Будапештская71',
                           code_1c_shop == '00004' ~ 'Будапештская85',
                           code_1c_shop == '00019' ~ 'Европейский8',
                           code_1c_shop == '00016' ~ 'Индустриальный34',
                           code_1c_shop == '00013' ~ 'Комендантский17',
                           code_1c_shop == '00015' ~ 'Комендантский26',
                           code_1c_shop == '00026' ~ 'Ленинградская5',
                           code_1c_shop == '00025' ~ 'Ленинский82',
                           code_1c_shop == '00022' ~ 'Ленинский90',
                           code_1c_shop == '00014' ~ 'Парнас',
                           code_1c_shop == '00008' ~ 'Пражская17',
                           code_1c_shop == '00002' ~ 'Просвещения69',
                           code_1c_shop == '00023' ~ 'Савушкина128',
                           code_1c_shop == '00006' ~ 'Софийская29',
                           code_1c_shop == '00024' ~ 'Художников26',
                           code_1c_shop == '00017' ~ 'Энгельса128',
                           code_1c_shop == '00020' ~ 'Энгельса134',
                           code_1c_shop == '00027' ~ 'Шувалова11',
                           code_1c_shop == '00028' ~ 'Менделеева11',
                           code_1c_shop == '00029' ~ 'Гражданский107',
                           code_1c_shop == '00030' ~ 'ФедораАбрамова8',
                           code_1c_shop == '00033' ~ 'Петровский2',
                           code_1c_shop == '00034' ~ 'Толубеевский14',
                           code_1c_shop == '00035' ~ 'Парашютная63',
                           code_1c_shop == '00040' ~ 'Графская15'), .after = code_1c_shop) %>%
    arrange(code_1c_shop,dep_name,ds) %>%
    mutate(#n_month = month(ds),
           #n_year  = year(ds),
           margine = sales - prime,
           margine_pers = round(100*margine/prime,2))
  
  return(df_sheck)
}

get_ipc_sql <- function(con_analytics, ipc_type = 'food', prediciton = FALSE) {
  
  request_code <- paste0("SELECT [date]
,[ipc]
FROM [analytics].[dbo].[ipc]
WHERE ([type] = 'Repl_ipc_type') AND ([prediction] = Repl_prediction)")
  
  # date
  request_code <- gsub("Repl_ipc_type", ipc_type, request_code)
  request_code <- gsub("Repl_prediction", as.integer(prediciton), request_code)
  
  
  df_ipc <- dbGetQuery(con_analytics, request_code)
  df_ipc$date <- as.Date(df_ipc$date)
  df_ipc <- df_ipc %>%
    mutate(n_year  = year(date),
           n_month = month(date)) %>%
    arrange(desc(date)) %>%
    dplyr::select(-date)
  
  df_ipc$ipc_today <- 1
  for(i in 2:nrow(df_ipc)) {
    df_ipc[i,'ipc_today'] <- df_ipc[i-1,'ipc_today']*df_ipc[i-1,'ipc']
  }
  
  colnames(df_ipc) <- c('k_ipc','n_year','n_month','ipc_today')
  
  return(df_ipc)
}

get_weather_sql <- function(con_analytics) {
  
  request_code <- paste0("SELECT [date]
      ,[temp]
      ,[prediction]
      ,[pred_date]
      ,[pred_name]
  FROM [analytics].[dbo].[weather]")
  
  
  df_weather <- dbGetQuery(con_analytics, request_code)
  df_weather$date <- as.Date(df_weather$date)
  df_weather <- df_weather %>%
    mutate(n_year  = year(date),
           n_month = month(date)) %>%
    arrange(desc(date))
  
  df_weather_fact <- df_weather %>%
    filter(prediction == 0)
  max_date_fact <- max(df_weather_fact$date)
  
  df_weather_pred <- df_weather %>%
    filter(prediction == 1) %>%
    filter(date > max_date_fact)
  
  max_pred_date <- max(df_weather_pred$pred_date)
  df_weather_pred <- df_weather_pred %>%
    filter(pred_date == max_pred_date)
  
  df_weather_final <- rbind(df_weather_fact,df_weather_pred)
  
  return(df_weather_final %>%
           dplyr::select(date, temp))
}

get_n_weeks_sql <- function(con_analytics) {
  
  request_code <- paste0("SELECT [date]
      ,[n_week]
      ,[n_month]
      ,[n_year]
  FROM [analytics].[dbo].[n_weeks]")
  
  
  df_n_weeks_sgl      <- dbGetQuery(con_analytics, request_code)
  df_n_weeks_sgl$date <- as.Date(df_n_weeks_sgl$date)
  
  return(df_n_weeks_sgl)
}

get_disasters_sql <- function(con_analytics) {
  
  request_code <- paste0("SELECT [date]
      ,[impact_value]
      ,[disaster_name]
  FROM [analytics].[dbo].[disasters]")
  
  
  df_disasters_sql      <- dbGetQuery(con_analytics, request_code)
  df_disasters_sql$date <- as.Date(df_disasters_sql$date)
  
  return(df_disasters_sql)
}

get_hdays_sql <- function(con_analytics) {
  
  request_code <- paste0("SELECT [holiday]
	  ,[date] as ds
      ,[lower_window]
      ,[upper_window]
  FROM [analytics].[dbo].[hdays]")
  
  
  df_hdays_sql    <- dbGetQuery(con_analytics, request_code)
  df_hdays_sql$ds <- as.Date(df_hdays_sql$ds)
  
  return(df_hdays_sql)
}

get_plans_sql <- function(con_analytics,
                          i_pred_date = '2022-06-24',
                          i_pred_name = 'prophet') {
  
  request_code <- paste0("SELECT [date]
      ,[shop_code]
      ,[dept_code]
      ,[pred_date]
      ,[pred_name]
      ,[pred_value]
      ,[pred_value_low]
      ,[pred_value_high]
  FROM [analytics].[dbo].[pred_dept]
  WHERE [pred_date] = 'Repl_pred_date' AND [pred_name] = 'Repl_pred_name'")
  
  request_code <- gsub("Repl_pred_date", i_pred_date, request_code)
  request_code <- gsub("Repl_pred_name", i_pred_name, request_code)
  
  df_pred           <- dbGetQuery(con_analytics,request_code)
  df_pred$date      <- as.Date(df_pred$date)
  df_pred$pred_date <- as.Date(df_pred$pred_date)
  
  df_pred <- df_pred %>%
    mutate(version = paste0(pred_date,'_',pred_name))
  
  return(df_pred)
}

get_unique_plans_list_sql <- function(con_analytics) {
  
  request_code <- paste0("SELECT [pred_name]
,[pred_date]
FROM [analytics].[dbo].[pred_dept]
GROUP BY [pred_name],[pred_date]")
  
  df_pred_list <- dbGetQuery(con_analytics,request_code)
  df_pred_list$pred_date <- as.character(df_pred_list$pred_date)
  
  return(df_pred_list)
}


get_df_check_structure <- function(start_date = '2022-06-01',
                                   end_date   = '2022-06-01',
                                   dept_name  = 'Мясной',
                                   con_dalion_en) {
  
  request_code <- paste0("SELECT 

[df_sales].[code_1c_shop],
[df_sales].[code_1c_nomenclature],
[df_nomenclature].[nomenclature_name],
[df_sales].[department_name],
[df_sales].[date],
[df_sales].[sales],
[df_sales].[prime],
[df_sales].[number],
[df_sales].[n_sales],
[df_sales].[sales_dept],
[df_sales].[n_checks],
[df_sales].[value_1000_checks],
[df_sales].[n_sales_1000_checks],
[df_sales].[value_100_tr],
[df_sales].[n_sales_100_tr],
[df_sales].[marg],
[df_nomenclature].[measurement_unit],
[df_nomenclature].[category_1],
[df_nomenclature].[category_2],
[df_nomenclature].[category_3],
[df_nomenclature].[category_4],
[df_nomenclature].[category_5],
[df_nomenclature].[supplier_name]

FROM(
SELECT 
[df_sales_nom].[code_1c_shop],
[df_sales_nom].[code_1c_nomenclature],
[df_sales_nom].[department_name],
[df_sales_nom].[date],
[df_sales_nom].[sales],
[df_sales_nom].[prime],
[df_sales_nom].[number],
[df_sales_nom].[n_sales],
[df_sales_dept].[sales_dept],
[df_sales_dept].[n_checks],
(CASE
     WHEN [df_sales_dept].[n_checks] <> 0 THEN ([df_sales_nom].[number]  / [df_sales_dept].[n_checks] * 1000)
	 ELSE NULL
	 END) AS value_1000_checks,
(CASE
     WHEN [df_sales_dept].[n_checks] <> 0 THEN (([df_sales_nom].[n_sales] + 0.0) / [df_sales_dept].[n_checks] * 1000)
	 ELSE NULL
	 END) AS n_sales_1000_checks,
(CASE
     WHEN [df_sales_dept].[sales_dept] <> 0 THEN ([df_sales_nom].[number]  / [df_sales_dept].[sales_dept] * 100000)
	 ELSE NULL
	 END) AS value_100_tr,
(CASE
     WHEN [df_sales_dept].[sales_dept] <> 0 THEN (([df_sales_nom].[n_sales] + 0.0) / [df_sales_dept].[sales_dept] * 100000)
	 ELSE NULL
	 END) AS n_sales_100_tr,
(CASE
     WHEN [df_sales_nom].[prime] <> 0 THEN ([df_sales_nom].[sales]    / [df_sales_nom].[prime])
	 ELSE NULL
	 END) AS marg

FROM(
SELECT [code_1c_shop],
[code_1c_nomenclature],
[department_name],
[date],
ROUND(SUM([sales_amount_fact]),2) AS sales,
ROUND(SUM([prime_cost]),2) AS prime,
ROUND(SUM([number_sales_fact]),2) AS number,
COUNT(distinct [guid_check]) AS n_sales

FROM [dalion_en].[dbo].[sales]

WHERE (([date] BETWEEN 'Repl_start_date' AND 'Repl_end_date') AND ([department_name] = 'Repl_dept_name'))

GROUP BY [date], [code_1c_shop], [department_name], [code_1c_nomenclature]

) AS df_sales_nom

LEFT JOIN (
SELECT 
[code_1c_shop],
[department_name],
[date],
ROUND(SUM([sales_amount_fact]),2) AS sales_dept,
COUNT(distinct [guid_check]) AS n_checks

FROM [dalion_en].[dbo].[sales]

GROUP BY [date], [code_1c_shop], [department_name]
) AS df_sales_dept

ON (
[df_sales_nom].[date]                 = [df_sales_dept].[date] AND
[df_sales_nom].[code_1c_shop]         = [df_sales_dept].[code_1c_shop] AND
[df_sales_nom].[department_name]      = [df_sales_dept].[department_name]
)
) AS df_sales

LEFT JOIN (
SELECT *
FROM [dalion_en].[dbo].[nomenclature]
) AS df_nomenclature

ON [df_sales].[code_1c_nomenclature] = [df_nomenclature].[code_1c_nomenclature]


ORDER BY [code_1c_shop], [department_name], [sales] DESC, [date]")
  
  # date
  request_code <- gsub("Repl_start_date", start_date, request_code)
  request_code <- gsub("Repl_end_date",   end_date,   request_code)
  request_code <- gsub("Repl_dept_name",  dept_name,   request_code)

  df_sheck <- dbGetQuery(con_dalion_en,request_code)
  
  return(df_sheck)
}

get_dalion_plans_sql <- function(con_analytics,
                                 start_date = '2022-08-29',
                                 end_date   = '2022-09-04') {
  
  request_code <- paste0("SELECT [date]
      ,[code_1c_shop]
      ,[department_name]
      ,[sales_plan]
  FROM [dalion_en].[dbo].[sales_planned]
  WHERE [date] BETWEEN 'Repl_pred_date' AND 'Repl_pred_name'")
  
  request_code <- gsub("Repl_pred_date", start_date, request_code)
  request_code <- gsub("Repl_pred_name", end_date,   request_code)
  
  df_pred           <- dbGetQuery(con_analytics,request_code)
  df_pred$date      <- as.Date(df_pred$date)
  
  return(df_pred)
}

get_df_car_sales_sql <- function(con_car) {
  
  request_code <- paste0("SELECT [df_sales].[check_guid]
,[df_sales].[dt_date]
,[df_sales].[dt_year]
,[df_sales].[dt_month]
,[df_sales].[week_day]
,[df_sales].[start_hour]
,[df_sales].[end_hour]
,[df_sales].[order_time_min]
,[df_sales].[client_guid]
,[df_sales].[client_name]
,[df_sales].[kkm]
,[ks]
,[nomenclature_type]
,[nomenclature_name]
,[number_sales_fact]
,[sales_amount_fact]
,[car_name]
,[car_model]
,[car_category_name]
,[n_days]
,[client_period]

FROM(

SELECT [check_guid]
	  , CAST(date_time as DATE)      as dt_date
	  , DATEPART(YEAR, [date_time])  as dt_year
	  , DATEPART(MONTH, [date_time]) as dt_month
	  , DATEPART(WEEKDAY, [date_time])   as week_day
	  , DATEPART(HOUR, [time_start]) as start_hour
	  , DATEPART(HOUR, [time_end])   as end_hour
	  , DATEDIFF(MINUTE, [time_start], [time_end]) as order_time_min
	  	  
      ,[client_guid]
      ,[client_name]
      ,[seller_name]
      ,[kkm]
	  ,[ks]
  FROM [car_wash].[dbo].[sales]) AS df_sales

  LEFT JOIN (SELECT [check_guid]
                               ,(CASE
							   WHEN [nomenclature_type] = 'Услуга' THEN 'Moika'
							   ELSE 'Kafe'
							   END) AS nomenclature_type
      ,[nomenclature_name]
      ,[number_sales_fact]
      ,[price_fact]
      ,[sales_amount_fact]
  FROM [car_wash].[dbo].[sales_goods]) AS df_goods
 
ON (
[df_sales].[check_guid] = [df_goods].[check_guid]
)

LEFT JOIN (SELECT [client_guid]
      ,[client_name]
      ,[car_name]
      ,[car_model]
      ,[car_category_name]
  FROM [car_wash].[dbo].[clients]) AS df_client
ON (
[df_sales].[client_guid] = [df_client].[client_guid]
)

LEFT JOIN (SELECT  [check_guid], [n_days]

,(case WHEN ([n_days] = -1) then 'kafe'
       WHEN ([n_days] = -2) then 'new'
	   WHEN ([n_days] < 15 ) then 'less 14'
	   WHEN ([n_days] < 61 ) then 'less 60'
	   ELSE 'more 60'
	   END) as client_period

FROM 
(SELECT  [check_guid]

,(case WHEN ([client_guid] = '3aa8ee80-dbc6-11ea-9bc1-005056a86321') then -1
	  WHEN ([prev_date_time] IS NULL) then -2
	  ELSE [n_days]
	  END) as [n_days]

FROM 
(SELECT [check_guid]
,[client_guid]
,[date_time]
,[prev_date_time]
,DATEDIFF(DAY, [prev_date_time], [date_time]) as n_days

FROM
(SELECT [check_guid]
,[client_guid]
,[date_time]
,LAG([date_time], 1) OVER (
						PARTITION BY [client_guid]
						ORDER BY [date_time]
						) prev_date_time
  FROM [car_wash].[dbo].[sales]) as df_clients1) as df_clients2) as df_clients3) AS df_client_period
ON (
[df_sales].[check_guid] = [df_client_period].[check_guid]
)")

df_car_sales_sql <- dbGetQuery(con_car,request_code)
df_car_sales_sql$dt_date <- as.Date(df_car_sales_sql$dt_date)
return(df_car_sales_sql)
}

get_position_distribution_sql <- function(con_dalion,
                                 start_date = '2022-09-01',
                                 end_date   = '2022-10-01') {
  
  request_code <- paste0("SELECT 
[date],
[code_1c_shop],
[n_positions],
ROUND(SUM([sales]),2) AS sales,
ROUND(SUM([prime]),2) AS prime,
ROUND(SUM([n_sales]),2) AS n_sales
FROM(
SELECT 
[date],
[guid_check],
[code_1c_shop],
ROUND(SUM([sales_amount_fact]),2) AS sales,
ROUND(SUM([prime_cost]),2) AS prime,
ROUND(SUM([number_sales_fact]),2) AS number,
COUNT(distinct [guid_check]) AS n_sales,
COUNT([guid_check]) AS n_positions
FROM(
SELECT [date],
[code_1c_shop],
[guid_check],
[df_sales].[code_1c_nomenclature],
[sales_amount_fact],
[prime_cost],
[number_sales_fact]
FROM(SELECT [code_1c_nomenclature]
  FROM [dalion_en].[dbo].[nomenclature]
  WHERE [category_4] in ('Готовая еда от поставщика','Готовая еда СП')) AS df_food
LEFT JOIN (SELECT 
[date],
[guid_check],
[code_1c_nomenclature],
[code_1c_shop],
[sales_amount_fact],
[prime_cost],
[number_sales_fact]
  FROM [dalion_en].[dbo].[sales]
  WHERE [date] BETWEEN 'Repl_start_date' AND 'Repl_end_date') AS df_sales
  ON [df_food].[code_1c_nomenclature] = [df_sales].[code_1c_nomenclature]) AS df_checks
  GROUP BY [date],[guid_check],[code_1c_shop]) as df_checks_0
  GROUP BY [date],[code_1c_shop],[n_positions]
  ORDER BY [date],[code_1c_shop],[n_positions]")
  
  request_code <- gsub("Repl_start_date", start_date, request_code)
  request_code <- gsub("Repl_end_date", end_date,   request_code)
  
  df_pred           <- dbGetQuery(con_dalion,request_code)
  df_pred$date      <- as.Date(df_pred$date)
  
  return(df_pred)
}

get_position_distribution_by_dept_sql <- function(con_dalion,
                                          start_date = '2022-09-01',
                                          end_date   = '2022-10-01',
                                          i_dept_name  = 'Кулинария') {
  
  request_code <- paste0("SELECT 
[date],
[code_1c_shop],
[n_positions],
ROUND(SUM([sales]),2) AS sales,
ROUND(SUM([prime]),2) AS prime,
ROUND(SUM([n_sales]),2) AS n_sales
FROM(
SELECT 
[date],
[guid_check],
[code_1c_shop],
ROUND(SUM([sales_amount_fact]),2) AS sales,
ROUND(SUM([prime_cost]),2) AS prime,
ROUND(SUM([number_sales_fact]),2) AS number,
COUNT(distinct [guid_check]) AS n_sales,
COUNT([guid_check]) AS n_positions
FROM(
SELECT [date],
[code_1c_shop],
df_food.[guid_check],
[df_sales].[code_1c_nomenclature],
[sales_amount_fact],
[prime_cost],
[number_sales_fact]
FROM(SELECT [guid_check]
  FROM [dalion_en].[dbo].[sales]
  WHERE [code_1c_nomenclature] = '31385' AND [date] BETWEEN 'Repl_start_date' AND 'Repl_end_date' AND [department_name] = 'Repl_dept') AS df_food
LEFT JOIN (SELECT 
[date],
[guid_check],
[code_1c_nomenclature],
[code_1c_shop],
[sales_amount_fact],
[prime_cost],
[number_sales_fact]
  FROM [dalion_en].[dbo].[sales]
  ) AS df_sales
  ON [df_food].[guid_check] = [df_sales].[guid_check]) AS df_checks
  GROUP BY [date],[guid_check],[code_1c_shop]) as df_checks_0
  GROUP BY [date],[code_1c_shop],[n_positions]
  ORDER BY [date],[code_1c_shop],[n_positions]")
  
  request_code <- gsub("Repl_start_date", start_date, request_code)
  request_code <- gsub("Repl_end_date",   end_date,   request_code)
  request_code <- gsub("Repl_dept",       i_dept_name,   request_code)
  
  df_pred <- dbGetQuery(con_dalion,request_code)
  df_pred <- df_pred %>%
    filter(!is.na(n_positions)) %>%
    filter(n_positions > 0)
  df_pred$date <- as.Date(df_pred$date)
  
  return(df_pred)
}

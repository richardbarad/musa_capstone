CREATE OR REPLACE TABLE results_data.results_geom
CLUSTER BY (geography)
AS
SELECT  
  GEOID,
  NAME AS name,
  clusters AS cluster_num,
  CASE clusters
    WHEN 1 THEN 'A'
    WHEN 2 THEN 'B'
    WHEN 3 THEN 'C'
    WHEN 4 THEN 'D'
    WHEN 5 THEN 'E'
    WHEN 6 THEN 'F'
    WHEN 7 THEN 'G'
    WHEN 8 THEN 'H'
    WHEN 9 THEN 'I'
    ELSE NULL
  END AS cluster_letter,
  --CAST(clusters AS STRING) AS assigned_cluster,
  ROUND(flood_pct,2) AS flooding,
  ROUND(pct_minority,2) AS minority_pop,
  ROUND(pct_non_english,2) AS non_english_pop,
  ROUND(pct_poverty,2) AS poverty,
  ROUND(pct_no_university,2) AS no_uni,
  ROUND(health_hazard_index,2) AS health_hazard_index,
  ROUND(air_index,2) AS air_index,
  ROUND(fire,2) AS fire,
  ROUND(waste_proximity,2) AS water_proximity,
  ROUND(Wastewater,2) AS wastewater,
  CASE
    WHEN NTILE(4) OVER (ORDER BY flood_pct) = 1 THEN 'Lowest 25%'
    WHEN NTILE(4) OVER (ORDER BY flood_pct) = 2 THEN '25 – 50th percentile'
    WHEN NTILE(4) OVER (ORDER BY flood_pct) = 3 THEN '50th - 75th percentile'
    WHEN NTILE(4) OVER (ORDER BY flood_pct) = 4 THEN 'Highest 25%'
  END AS flood_quartile,
  CASE
    WHEN NTILE(4) OVER (ORDER BY pct_minority) = 1 THEN 'Lowest 25%'
    WHEN NTILE(4) OVER (ORDER BY pct_minority) = 2 THEN '25 – 50th percentile'
    WHEN NTILE(4) OVER (ORDER BY pct_minority) = 3 THEN '50th - 75th percentile'
    WHEN NTILE(4) OVER (ORDER BY pct_minority) = 4 THEN 'Highest 25%'
  END AS minority_quartile,
    CASE
    WHEN NTILE(4) OVER (ORDER BY pct_non_english) = 1 THEN 'Lowest 25%'
    WHEN NTILE(4) OVER (ORDER BY pct_non_english) = 2 THEN '25 – 50th percentile'
    WHEN NTILE(4) OVER (ORDER BY pct_non_english) = 3 THEN '50th - 75th percentile'
    WHEN NTILE(4) OVER (ORDER BY pct_non_english) = 4 THEN 'Highest 25%'
  END AS non_english_quartile,
    CASE
    WHEN NTILE(4) OVER (ORDER BY pct_poverty) = 1 THEN 'Lowest 25%'
    WHEN NTILE(4) OVER (ORDER BY pct_poverty) = 2 THEN '25 – 50th percentile'
    WHEN NTILE(4) OVER (ORDER BY pct_poverty) = 3 THEN '50th - 75th percentile'
    WHEN NTILE(4) OVER (ORDER BY pct_poverty) = 4 THEN 'Highest 25%'
  END AS poverty_quartile,
    CASE
    WHEN NTILE(4) OVER (ORDER BY pct_no_university) = 1 THEN 'Lowest 25%'
    WHEN NTILE(4) OVER (ORDER BY pct_no_university) = 2 THEN '25 – 50th percentile'
    WHEN NTILE(4) OVER (ORDER BY pct_no_university) = 3 THEN '50th - 75th percentile'
    WHEN NTILE(4) OVER (ORDER BY pct_no_university) = 4 THEN 'Highest 25%'
  END AS no_uni_quartile,
    CASE
    WHEN NTILE(4) OVER (ORDER BY air_index) = 1 THEN 'Lowest 25%'
    WHEN NTILE(4) OVER (ORDER BY air_index) = 2 THEN '25 – 50th percentile'
    WHEN NTILE(4) OVER (ORDER BY air_index) = 3 THEN '50th - 75th percentile'
    WHEN NTILE(4) OVER (ORDER BY air_index) = 4 THEN 'Highest 25%'
  END AS air_index_quartile,
    CASE
    WHEN NTILE(4) OVER (ORDER BY health_hazard_index) = 1 THEN 'Lowest 25%'
    WHEN NTILE(4) OVER (ORDER BY health_hazard_index) = 2 THEN '25 – 50th percentile'
    WHEN NTILE(4) OVER (ORDER BY health_hazard_index) = 3 THEN '50th - 75th percentile'
    WHEN NTILE(4) OVER (ORDER BY health_hazard_index) = 4 THEN 'Highest 25%'
  END AS health_hazard_index_quartile,
    CASE
    WHEN NTILE(4) OVER (ORDER BY fire) = 1 THEN 'Lowest 25%'
    WHEN NTILE(4) OVER (ORDER BY fire) = 2 THEN '25 – 50th percentile'
    WHEN NTILE(4) OVER (ORDER BY fire) = 3 THEN '50th - 75th percentile'
    WHEN NTILE(4) OVER (ORDER BY fire) = 4 THEN 'Highest 25%'
  END AS fire_quartile,
    CASE
    WHEN NTILE(4) OVER (ORDER BY waste_proximity) = 1 THEN 'Lowest 25%'
    WHEN NTILE(4) OVER (ORDER BY waste_proximity) = 2 THEN '25 – 50th percentile'
    WHEN NTILE(4) OVER (ORDER BY waste_proximity) = 3 THEN '50th - 75th percentile'
    WHEN NTILE(4) OVER (ORDER BY waste_proximity) = 4 THEN 'Highest 25%'
  END AS waste_quartile,
    CASE
    WHEN NTILE(4) OVER (ORDER BY Wastewater) = 1 THEN 'Lowest 25%'
    WHEN NTILE(4) OVER (ORDER BY Wastewater) = 2 THEN '25 – 50th percentile'
    WHEN NTILE(4) OVER (ORDER BY Wastewater) = 3 THEN '50th - 75th percentile'
    WHEN NTILE(4) OVER (ORDER BY Wastewater) = 4 THEN 'Highest 25%'
  END AS wastewater_quartile,
  st_geogfromtext(geometry, make_valid => TRUE) AS geography
FROM results_data.results
--Raster were imported in postgresql using following types of command in a windows command line
--raster2pgsql -I -C -s 21781 -t auto tmind_totalc.tif | psql -U postgres -d climgen_ch -p 5433

--Example in postgresql to check if the raster was correctly imported (get the raster value of the first 20 farms). The x/y coordinates of farm are given in km
--The farm in given in the old Swiss projection (21781), while the new is a WGS based system (4030)
SELECT ST_Value(rast, 1, ST_Transform(st_setsrid(st_makepoint(x*1000, y*1000),21781),4030)) 
FROM tmind_2010, b01
where ST_Intersects(rast,ST_Transform(st_setsrid(st_makepoint(x*1000, y*1000),21781),4030))
limit 20;


--Example to select the minimal daily temperature of at the date and place of milk record (the first lines in k33 are from year 2001 but needs to change manusally the raster layer)
select dt_probe, extract(year from dt_probe) as year, extract(doy from dt_probe) as dayOfYear, x, y,
ST_Value(rast, 1, ST_Transform(st_setsrid(st_makepoint(x*1000, y*1000),21781),4030)) 
from (select * from k33 limit 10) k33 left join b01 on k33.betriebid=b01.betriebid 
join TminD_2001 on ST_Intersects(rast,ST_Transform(st_setsrid(st_makepoint(x*1000, y*1000),21781),4030))
where x is not null

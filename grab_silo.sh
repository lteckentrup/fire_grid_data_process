### SILO data has been updated - grab new data. SILO variable names
### are not consistent with Jim's naming so need to grab each variable
### separately

### Define URL
URL="https://s3-ap-southeast-2.amazonaws.com/silo-open-data/Official/annual"

### Get precip
var="precip"
mkdir ${var}_silo
for year in {1994..2018..1}; do
    wget -O ${var}_silo/${var}.${year}.nc \
            ${URL}/monthly_rain/${year}.monthly_rain.nc
done

### Relative humidity
var="rh"
mkdir ${var}_silo
for year in {1994..2018..1}; do
    wget -O ${var}_silo/${var}.${year}.nc \
            ${URL}/rh_tmax/${year}.rh_tmax.nc
done

### Maximum temperature
var="tmax"
mkdir ${var}_silo
for year in {1994..2018..1}; do
    wget -O ${var}_silo/${var}.${year}.nc \
            ${URL}/max_temp/${year}.max_temp.nc
done

### VPD
var="vpd"
mkdir ${var}_silo
for year in {1994..2018..1}; do
    wget -O ${var}_silo/${var}.${year}.nc \
            ${URL}/vp_deficit/${year}.vp_deficit.nc
done

### VP
var="vp"
mkdir ${var}_silo
for year in {1994..2018..1}; do
    wget -O ${var}_silo/${var}.${year}.nc \
            ${URL}/${var}/${year}.${var}.nc
done

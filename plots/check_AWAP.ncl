;*************************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;*************************************************************

begin

fltname = "/short/w35/mm3972/data/AWAP_data/awap_tmax_C_day_2000-2017/2002/bom-tmax-day-20020201-20020201.flt"
tmp = fbindirread(fltname,0,(/681,841/),"float")
tmax = tmp(299,399)
print("tmax "+tmax)

fltname = "/short/w35/mm3972/data/AWAP_data/awap_tmax_C_day_2000-2017/2002/bom-tmax-day-20020202-20020202.flt"
tmp = fbindirread(fltname,0,(/681,841/),"float")
tmax = tmp(299,399)
print("tmax "+tmax)

fltname = "/short/w35/mm3972/data/AWAP_data/awap_tmin_C_day_2000-2017/2002/bom-tmin-day-20020202-20020202.flt"
tmp = fbindirread(fltname,0,(/681,841/),"float")
tmin = tmp(299,399)
print("tmin "+tmin)

fltname = "/short/w35/mm3972/data/AWAP_data/awap_tmin_C_day_2000-2017/2002/bom-tmin-day-20020203-20020203.flt"
tmp = fbindirread(fltname,0,(/681,841/),"float")
tmin = tmp(299,399)
print("tmin "+tmin)


ncname = "/short/w35/mm3972/data/AWAP_to_netcdf/Tair/AWAP.Tair.3hr.2002.nc"
f = addfile (ncname,"r")
var_nc = f->Tair(256:263,381,399) -273.16
lat = f->lat(381)
lon = f->lon(399)
print(var_nc)
print(lat)
print(lon)

end

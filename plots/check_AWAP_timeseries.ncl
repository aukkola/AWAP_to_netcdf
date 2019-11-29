;*************************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;*************************************************************

begin

flt_path = "/short/w35/mm3972/data/AWAP_data/"

flt1      = (/\
           "awap_rain_mm_day_2000-2017/",\
           "awap_rad_MJ_day_2000-2017/",\
           "awap_tmax_C_day_2000-2017/",\
           "awap_tmin_C_day_2000-2017/",\
           "mcvicar_windspeed_ms_day_2000-2017/",\
           "awap_vph09_hpa_day_2000-2017/",\
           "awap_vph15_hpa_day_2000-2017/"\
           /)
flt2      = (/\
            "/bom-rain_recal-day-",\
            "/bom-rad-day-",\
            "/bom-tmax-day-",\
            "/bom-tmin-day-",\
            "/mcv_windspeed_ms-day-",\
            "/bom-vph09-day-",\
            "/bom-vph15-day-"\
            /)


nc_path = "/short/w35/mm3972/data/AWAP_to_netcdf/"

var_name = (/\
              "Rainf","Snowf",\
              "SWdown","LWdown",\
              "Tair","Wind",\
              "Qair","PSurf"\
            /)

vname = (/\
          "(a) Precipitation",\
          "(b) Shortwave Radiation",\
          "(c) Temperture",\
          "(d) Wind Speed",\
          "(e) Water Vapour Pressure"\
          /)
vunit = (/\
          "mm/day",\
          "MJ/m~S~2~N~/day",\
          "~S~o~N~C",\
          "m/s",\
          "hPa"\
          /)

;***************** �� �� *******************
year_s = 2003;2000
year_e = 2003;2005
year_sum = year_e-year_s+1

do year = year_s,year_e

    print("year = " +year)
    if ( mod(year,4) .eq. 0) then
       dom = (/31,29,31,30,31,30,31,31,30,31,30,31/)
    else
       dom = (/31,28,31,30,31,30,31,31,30,31,30,31/)
    end if

;___________________________ AWAP flt input ___________________________
day_sum = sum(dom)
tot = day_sum*8

var_flt = new((/7,day_sum/),float)
var_flt = 0.0

var_day = new((/5,2,day_sum/),float)
var_day = 0.0

var_flt@_FillValue = -999.
var_day@_FillValue = -999.

do var_num = 0,6

   print("flt data var_name = " +var_num)

   counter = 0

   do month = 1,12
      do day = 1,dom(month-1)

         fltname = flt_path+flt1(var_num)+year+flt2(var_num)+year+sprinti("%0.2i",month)+sprinti("%0.2i",day)+\
                   "-"+year+sprinti("%0.2i",month)+sprinti("%0.2i",day)+".flt"

         tmp = fbindirread(fltname,0,(/681,841/),"float")
         ;printVarSummary(tmp)
         var_flt(var_num,counter) = tmp(299,399)

         counter = counter + 1

       end do ; day
   end do ; month
end do ; var_num

var_day(0,0,:) = var_flt(0,:) ; rainfall+snowfall
var_day(1,0,:) = var_flt(1,:) ; rad MJ
var_day(2,0,:) = (var_flt(2,:)+var_flt(3,:))/2. ; temp
var_day(3,0,:) = var_flt(4,:) ; wind
var_day(4,0,:) = (var_flt(5,:)+var_flt(6,:))/2. ; qair

delete(tmp)
delete(var_flt)

;___________________________ AWAP nc input ____________________________
var_nc = new((/8,tot/),float)
var_nc = 0.0
var_nc@_FillValue = -999.

do var_num = 0,7
   print("nc data var_name = " +var_num)
   ncname = nc_path+var_name(var_num)+"/AWAP."+var_name(var_num)+".3hr."+year+".nc"
   f = addfile (ncname,"r")
   var_nc@_FillValue = f->$var_name(var_num)$@_fillvalue
   ;print(var_nc@_FillValue)
   var_nc(var_num,:) = f->$var_name(var_num)$(:,381,399)
end do
   print(f->lat(381))
   print(f->lon(399))
counter = 0
do day = 0, tot-1, 8

; rainfall+snowfall
   var_day(0,1,counter) = (sum(var_nc(0,day:day+7))+sum(var_nc(1,day:day+7)))*3600.*3
; rad
   var_day(1,1,counter) = avg(var_nc(2,day:day+7)) * 0.0864 ;W m-2, 1 W m-2 = 0.0864 MJ m-2 day-1
   ; WG%PhiSd    = WG%PhiSd*1e6/SecDay  [MJ/m2/day]->[W/m2], SecDay = 86400.0, Seconds/day
; temp
   var_day(2,1,counter) = avg(var_nc(4,day:day+7)) - 273.16
; wind
   var_day(3,1,counter) = avg(var_nc(5,day:day+7))
; qair
   var_day(4,1,counter) = avg(var_nc(6,day:day+7))*1000/0.62188471 ; KG/KG->hpa
   ; WG%QV     = WG%VapPPa/WG%PPa*RMWbyRMA, RMWbyRMA   = 0.62188471
   counter = counter + 1
end do

delete(var_nc)

;_______________________________ PLOT _______________________________
x = ispan(1,day_sum,1)
plots = new(5,graphic)

pic = "check_AWAP_timeseries_"+year
wks = gsn_open_wks("pdf",pic) ;png jpeg tiff psd pdf
gsn_define_colormap(wks,"rainbow")

res                    = True
res@gsnMaximize        = False
res@vpHeightF          = 0.6
res@vpWidthF           = 0.6
res@vpXF               = 0.3
res@vpYF               = 0.85
res@pmLegendWidthF     = 0.14                     ; Resize legend width
res@pmLegendHeightF    = 0.11                     ; and height
;res@xyLineLabelFontHeightF = 0.01
res1                   = True
res1                   = res

res@xyLineThicknesses  = (/  1.0,   1.0/)          ; make second line thicker
res@xyLineColors       = (/"blue","red"/)          ; change line color
res@pmLegendDisplayMode = "Never"

pres                    = True                                      ; ���� panel resources.
pres@gsnMaximize        = True

do var_num = 0,4
res@tiMainString   = vname(var_num)
res@tiYAxisString	 = vunit(var_num)
res@tiXAxisString  = "Day of Year"
if var_num .eq. 4 then
res@pmLegendDisplayMode = "Always"
res@xyExplicitLegendLabels = (/"Daily","Subdiurnal"/)
res@pmLegendSide        = "Right"
end if
plots(var_num) = gsn_csm_xy(wks,x,var_day(var_num,:,:),res)
end do
gsn_panel(wks,(/plots/),(/2,3/),pres)

res@pmLegendDisplayMode = "Never"
do var_num = 0,4
res@tiMainString   = vname(var_num)
res@tiYAxisString	 = vunit(var_num)
res1@tiMainString    = "Difference of "+vname(var_num)
res1@tiXAxisString  = "Day of Year"
plots(var_num) = gsn_csm_xy(wks,x,var_day(var_num,0,:)-var_day(var_num,1,:),res1)
end do

gsn_panel(wks,(/plots/),(/2,3/),pres)

delete(var_day)
delete(res)
delete(x)
end do; year

end

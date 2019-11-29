;*************************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;*************************************************************

;______ From Mark Decker's code ______
setvalues NhlGetWorkspaceObjectId()
  "wsMaximumSize" : 5000000000 ;
end setvalues
;_____________________________________

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
          "Precipitation (mm/day)",\
          "Shortwave Radiation (MJ/m2/day)",\
          "Temperture (C)",\
          "Wind Speed (m/s)",\
          "Water Vapour Pressure (hPa)"\
          /)

;***************** �� �� *******************
year_s = 2000
year_e = 2005
year_sum = year_e-year_s+1

;_____________________________ PLOT ___________________________________
pic = "check_AWAP_spitial"

wks = gsn_open_wks("pdf",pic)
gsn_define_colormap(wks,"WhiteBlueGreenYellowRed")

res                    = True
res@cnFillMode         = "RasterFill"            ; Raster Mode
res@cnFillOn           = True                            ;��ɫ
res@tmBorderThicknessF = 3.0

res@gsnDraw            = False  ; Don't draw plots
res@gsnFrame           = False  ; ����ҳ
res@lbLabelBarOn       = True

;************** ����labelbar ***************
res@lbBoxLinesOn       = True                       ;�ر�lbar box ����
res@lbTitleFont        = 25
res@lbLabelFont        = 25
res@lbTitleFontHeightF = 0.013
res@lbLabelFontHeightF = 0.013
res@txString           = ""
res@tmXBLabelFont      = 25 ;Sets the font index for the bottom X-Axis labels.
res@tmYLLabelFont      = 25

;*************** ���õ�ֵ�� *****************
res@cnLinesOn          = False                       ; �رյ�ֵ������
res@cnLineColor        = "black"
res@cnLineThicknessF   = 1.5
res@cnLineLabelsOn     = False
res@gsnMaximize        = True
res@cnExplicitLabelBarLabelsOn = True   ;?
res@cnLevelSelectionMode = "ExplicitLevels"

;************ ����������ֵ��Χ **************
res@tmXBLabelFontThicknessF = 0.015
res@tmYLLabelFontThicknessF = 0.015
res@tmXBLabelFontHeightF = 0.015
res@tmYLLabelFontHeightF = 0.015

res@gsnAddCyclic       = False
res@trYMinF            = -44
res@trYMaxF            = -10
res@mpMinLatF          = -44
res@mpMaxLatF          = -10
res@trXMinF            = 112
res@trXMaxF            = 154
res@mpMinLonF          = 112
res@mpMaxLonF          = 154

res@tmYLMode  = "Explicit"             ;��Y����������������,��ʾ�Լ���Ҫ��ֵ
res@tmYLValues=(/-10,-20,-30,-40/)
res@tmYLLabels=(/"10~S~o~N~S","20~S~o~N~S","30~S~o~N~S","40~S~o~N~S"/)
res@tmXBMode  = "Explicit"
res@tmXBValues=(/120,130,140,150/)
res@tmXBLabels=(/"120~S~o~N~E","130~S~o~N~E","140~S~o~N~E","150~S~o~N~E"/)


;**************** ͼ�������� *****************
pres                    = True                                      ; ���� panel resources.
pres@gsnMaximize        = True
pres@gsnPanelLabelBar   = False                    ; ���� panel labelbar.
pres@cnLevelSelectionMode = "ExplicitLevels" ;"ManualLevels"
pres@lbBoxLinesOn       = True                       ;�ر�lbar box ����
pres@lbTitleFont        = 25
pres@lbLabelFont        = 25
pres@lbLabelBarOn       = False ; True                    ; �رո���plot��labelbar
pres@lbTitleFontHeightF = 0.013
pres@lbLabelFontHeightF = 0.013
pres@lbAutoManage       = False
pres@lbTitleDirection   = "Across"             ; title direction
pres@lbTitlePosition    = "Bottom";Right"
;pres@pmLabelBarSide     = "Bottom";"Right"
;pres@pmLabelBarOrthogonalPosF = -0.021
;pres@pmLabelBarParallelPosF = 0.

plots = new(15,graphic)

;___________________________ AWAP flt input ___________________________
var_day = new((/5,681,841/),float)
var_day = 0.0
var_day@_FillValue = -999.

do year = year_s,year_e

    var_flt = new((/7,681,841/),float)
    var_flt = 0.0
    var_flt@_FillValue = -999.

    print("year = " +year)
    if ( mod(year,4) .eq. 0) then
       dom = (/31,29,31,30,31,30,31,31,30,31,30,31/)
    else
       dom = (/31,28,31,30,31,30,31,31,30,31,30,31/)
    end if

    day_sum = sum(dom)
    tot = day_sum*8

    do var_num = 0,6

      print("flt data var_name = " +var_num)
      do month = 1,12
        do day = 1,dom(month-1)
          fltname = flt_path+flt1(var_num)+year+flt2(var_num)+year+sprinti("%0.2i",month)+sprinti("%0.2i",day)+\
                    "-"+year+sprinti("%0.2i",month)+sprinti("%0.2i",day)+".flt"
          tmp = fbindirread(fltname,0,(/681,841/),"float")
         ;printVarSummary(tmp)
          var_flt(var_num,:,:) = var_flt(var_num,:,:) + tmp
          delete(tmp)
          delete(fltname)
        end do ; day
      end do ; month
    end do ; var_num

    var_day(0,:,:) = var_day(0,:,:) + var_flt(0,:,:)/day_sum ; rainfall+snowfall
    var_day(1,:,:) = var_day(1,:,:) + var_flt(1,:,:)/day_sum ; rad MJ
    var_day(2,:,:) = var_day(2,:,:) + (var_flt(2,:,:)+var_flt(3,:,:))/2./day_sum ; temp
    var_day(3,:,:) = var_day(3,:,:) + var_flt(4,:,:)/day_sum ; wind
    var_day(4,:,:) = var_day(4,:,:) + (var_flt(5,:,:)+var_flt(6,:,:))/2./day_sum ; qair

    delete(var_flt)
end do ; year

var_day(0,:,:) = var_day(0,680:0,:)/year_sum
var_day(1,:,:) = var_day(1,680:0,:)/year_sum
var_day(2,:,:) = var_day(2,680:0,:)/year_sum
var_day(3,:,:) = var_day(3,680:0,:)/year_sum
var_day(4,:,:) = var_day(4,680:0,:)/year_sum

ncname = "/short/w35/mm3972/data/AWAP_to_netcdf/Rainf/AWAP.Rainf.3hr.2000.nc"
f = addfile (ncname,"r")
var_day!0 ="var"
var_day!1 ="lat"
var_day!2 ="lon"
var_day&lat = f->lat
var_day&lon = f->lon

; rainfall+snowfall
res@tiMainString  = vname(0)
levels = (/0.,2.,4.,6.,8.,10.,12.,14.,16.,18.,20./)
res@cnLevels = levels
plots(0) = gsn_csm_contour_map(wks,var_day(0,:,:),res)
delete(levels)
delete(res@cnLevels)

; rad MJ
res@tiMainString  = vname(1)
levels = (/0.,2.,4.,6.,8.,10.,12.,14.,16.,18.,20.,22.,24.,26./)
res@cnLevels = levels
plots(3) = gsn_csm_contour_map(wks,var_day(1,:,:),res)
delete(levels)
delete(res@cnLevels)

; temp
res@tiMainString  = vname(2)
levels = (/0.,2.,4.,6.,8.,10.,12.,14.,16.,18.,20.,22.,24.,26.,28.,30./)
res@cnLevels = levels
plots(6) = gsn_csm_contour_map(wks,var_day(2,:,:),res)
delete(levels)
delete(res@cnLevels)

; wind
res@tiMainString  = vname(3)
levels = (/0.,0.5,1,1.5,2.,2.5,3.0,3.5,4.,4.5,5.,5.5,6.,6.5,7.,7.5,8.,8.5/)
res@cnLevels = levels
plots(9) = gsn_csm_contour_map(wks,var_day(3,:,:),res)
delete(levels)
delete(res@cnLevels)

; qair
res@tiMainString  = vname(4)
levels = (/0.,5.,10.,15.,20.,25.,30.,35.,40./)
res@cnLevels = levels
plots(12) = gsn_csm_contour_map(wks,var_day(4,:,:),res)
delete(levels)
delete(res@cnLevels)

delete(var_day)
delete(ncname)
delete(f)

;___________________________ AWAP nc input ____________________________
var_nc = new((/7,681,841/),float)
var_nc = 0.0
var_nc@_FillValue = -999.

do year =year_s, year_e

  if ( mod(year,4) .eq. 0) then
     dom = (/31,29,31,30,31,30,31,31,30,31,30,31/)
  else
     dom = (/31,28,31,30,31,30,31,31,30,31,30,31/)
  end if
  day_sum = sum(dom)

  do var_num = 0,6
    print("nc data var_name = " +var_num)
    ncname = nc_path+var_name(var_num)+"/AWAP."+var_name(var_num)+".3hr."+year+".nc"
    f = addfile (ncname,"r")

    tmp = new((/681,841/),float)
    tmp = 0.0
    tmp@_FillValue = -999.
    do day = 0,day_sum-1
       tmp = tmp + dim_avg_n_Wrap(f->$var_name(var_num)$(day*8:(day*8+7),:,:),0)
    end do
    var_nc(var_num,:,:) = var_nc(var_num,:,:)+tmp/day_sum
    print("read nc file")
    delete(ncname)
    delete(tmp)
  end do
end do; year

var_nc = var_nc/year_sum
print("setting var_day")

var_day = new((/5,681,841/),float)
var_day = 0.0
var_day@_FillValue = -999.
; rainfall+snowfall
var_day(0,:,:) = (var_nc(0,:,:)+var_nc(1,:,:))*3600.*3*8
; rad
var_day(1,:,:) = var_nc(2,:,:) * 0.0864 ;W m-2, 1 W m-2 = 0.0864 MJ m-2 day-1
   ; WG%PhiSd    = WG%PhiSd*1e6/SecDay  [MJ/m2/day]->[W/m2], SecDay = 86400.0, Seconds/day
; temp
var_day(2,:,:) = var_nc(4,:,:) - 273.16
; wind
var_day(3,:,:) = var_nc(5,:,:)
; qair
var_day(4,:,:) = var_nc(6,:,:)*1000/0.62188471 ; KG/KG->hpa
   ; WG%QV     = WG%VapPPa/WG%PPa*RMWbyRMA, RMWbyRMA   = 0.62188471

delete(var_nc)

var_day!0 ="var"
var_day!1 ="lat"
var_day!2 ="lon"
var_day&lat = f->lat
var_day&lon = f->lon

; rainfall+snowfall
res@tiMainString  = vname(0)
levels = (/0.,2.,4.,6.,8.,10.,12.,14.,16.,18.,20./)
res@cnLevels = levels
plots(1) = gsn_csm_contour_map(wks,var_day(0,:,:),res)
delete(levels)
delete(res@cnLevels)

; rad MJ
res@tiMainString  = vname(1)
levels = (/0.,2.,4.,6.,8.,10.,12.,14.,16.,18.,20.,22.,24.,26./)
res@cnLevels = levels
plots(4) = gsn_csm_contour_map(wks,var_day(1,:,:),res)
delete(levels)
delete(res@cnLevels)

; temp
res@tiMainString  = vname(2)
levels = (/0.,2.,4.,6.,8.,10.,12.,14.,16.,18.,20.,22.,24.,26.,28.,30./)
res@cnLevels = levels
plots(7) = gsn_csm_contour_map(wks,var_day(2,:,:),res)
delete(levels)
delete(res@cnLevels)

; wind
res@tiMainString  = vname(3)
levels = (/0.,0.5,1,1.5,2.,2.5,3.0,3.5,4.,4.5,5.,5.5,6.,6.5,7.,7.5,8.,8.5/)
res@cnLevels = levels
plots(10) = gsn_csm_contour_map(wks,var_day(3,:,:),res)
delete(levels)
delete(res@cnLevels)

; qair
res@tiMainString  = vname(4)
levels = (/0.,5.,10.,15.,20.,25.,30.,35.,40./)
res@cnLevels = levels
plots(13) = gsn_csm_contour_map(wks,var_day(4,:,:),res)
delete(levels)
delete(res@cnLevels)
delete(var_day)

;___________________________ GSWP nc input ____________________________
var_gswp = new((/7,360,720/),float)
var_gswp = 0.0
var_gswp@_FillValue = 1.e+20

do year =year_s, year_e
  if ( mod(year,4) .eq. 0) then
     dom = (/31,29,31,30,31,30,31,31,30,31,30,31/)
  else
     dom = (/31,28,31,30,31,30,31,31,30,31,30,31/)
  end if
  day_sum = sum(dom)
  do var_num = 0,6
    print("gswp data var_name = " +var_num)
    ncname = "/g/data1a/wd9/MetForcing/Global/GSWP3_2017/"+var_name(var_num)+"/GSWP3.BC."+var_name(var_num)+".3hrMap."+year+".nc"
    f = addfile (ncname,"r")

    tmp = new((/360,720/),float)
    tmp = 0.0
    do day = 0,day_sum-1
       tmp = tmp + dim_avg_n_Wrap(f->$var_name(var_num)$(day*8:(day*8+7),:,:),0)
    end do
    var_gswp(var_num,:,:) = var_gswp(var_num,:,:) + tmp/day_sum
   ; ncl cannot read all timestep (e.g. 2920 ), thus there needs a loop
   delete(ncname)
   delete(tmp)
;   delete(f)
end do
end do; year

var_gswp = var_gswp/year_sum

var_G = new((/5,360,720/),float)
var_G = 0.0
var_G@_FillValue = var_gswp@_Fillvalue

; rainfall+snowfall
var_G(0,:,:) = (var_gswp(0,:,:)+var_gswp(1,:,:))*3600.*3*8
; rad
var_G(1,:,:) = var_gswp(2,:,:) * 0.0864 ;W m-2, 1 W m-2 = 0.0864 MJ m-2 day-1
   ; WG%PhiSd    = WG%PhiSd*1e6/SecDay  [MJ/m2/day]->[W/m2], SecDay = 86400.0, Seconds/day
; temp
var_G(2,:,:) = var_gswp(4,:,:) - 273.16
; wind
var_G(3,:,:) = var_gswp(5,:,:)
; qair
var_G(4,:,:) = var_gswp(6,:,:)*1000/0.62188471 ; KG/KG->hpa
   ; WG%QV     = WG%VapPPa/WG%PPa*RMWbyRMA, RMWbyRMA   = 0.62188471

delete(var_gswp)
var_G!0 ="var"
var_G!1 ="lat"
var_G!2 ="lon"
var_G&lat = f->lat
var_G&lon = f->lon

; rainfall+snowfall
res@tiMainString  = vname(0)
levels = (/0.,2.,4.,6.,8.,10.,12.,14.,16.,18.,20./)
res@cnLevels = levels
plots(2) = gsn_csm_contour_map(wks,var_G(0,{-44:-10},{112:154}),res)
delete(levels)
delete(res@cnLevels)

; rad MJ
res@tiMainString  = vname(1)
levels = (/0.,2.,4.,6.,8.,10.,12.,14.,16.,18.,20.,22.,24.,26./)
res@cnLevels = levels
plots(5) = gsn_csm_contour_map(wks,var_G(1,{-44:-10},{112:154}),res)
delete(levels)
delete(res@cnLevels)

; temp
res@tiMainString  = vname(2)
levels = (/0.,2.,4.,6.,8.,10.,12.,14.,16.,18.,20.,22.,24.,26.,28.,30./)
res@cnLevels = levels
plots(8) = gsn_csm_contour_map(wks,var_G(2,{-44:-10},{112:154}),res)
delete(levels)
delete(res@cnLevels)

; wind
res@tiMainString  = vname(3)
levels = (/0.,0.5,1,1.5,2.,2.5,3.0,3.5,4.,4.5,5.,5.5,6.,6.5,7.,7.5,8.,8.5/)
res@cnLevels = levels
plots(11) = gsn_csm_contour_map(wks,var_G(3,{-44:-10},{112:154}),res)
delete(levels)
delete(res@cnLevels)

; qair
res@tiMainString  = vname(4)
levels = (/0.,5.,10.,15.,20.,25.,30.,35.,40./)
res@cnLevels = levels
plots(14) = gsn_csm_contour_map(wks,var_G(4,{-44:-10},{112:154}),res)
delete(levels)
delete(res@cnLevels)
delete(var_G)

gsn_panel(wks,(/plots/),(/5,3/),pres)


end

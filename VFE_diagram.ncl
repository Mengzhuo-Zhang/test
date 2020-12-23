;*******************************************************************
; Plot VFE diagram
;*******************************************************************
function VFE_diagram (wks:graphic ,RATIO[*]:numeric, Rv[*]:numeric \
                          ,nmodel[1]:integer,opt_VFE:logical)
;*******************************************************************	
; The script is adapted from taylor_diagram.ncl
; It requires NCL version 4.2.0.a034 	
;*******************************************************************
; Arguments:
;  wks     : graphic workstation
;  RATIO   : ratio of amplitude of model/observation/reanalysis
;            to that of reference
;  Rv      : similarity cofficient between reference and
;            model/observation/reanalysis
;  nmodel  : number of model
;  opt_VFE : plot attribution
;*******************************************************************
; plot attribution in opt_VFE and defalut:
; opt_VFE@xyMax                     = 1.65             ;# max value of axis (array of numeric values) 
; opt_VFE@xyFontHeightF             = 0.0175           ;# font height for axis (numeric value) 
; opt_VFE@OneX                      = "REF"            ;# X-axis label at one point (str) 
; opt_VFE@tiYAxisString             = "RMSL"           ;# Y-axis string (str)  
; opt_VFE@CorLabel                  = "Similarity"     ;# Arc label (str)
; opt_VFE@stnRad                    = (not draw)       ;# standard radius for RMSL (array of numeric values)
; opt_VFE@ccRays                    = (not draw)       ;# radial lines for VSC (array of numeric values)
; opt_VFE@centerDiffRMS             = False            ;# whether to draw radii from REF (logical) 
; opt_VFE@Markers                   = (/4,8/)          ;# marker types for models and observations/reanalysis (array of 2 values)
; opt_VFE@gsMarkerThicknessF        = 1.0              ;# marker thinckness (numeric value)
; opt_VFE@gsMarkerSizeF             = 0.0085           ;# marker size (numeric value) 
; opt_VFE@Colors                    =                  ;# colors of points (array of str)
;                                                      ;## [1] array of named colors (str) [2] N x 3 RGB array with NhlTColorIndex (int)
; opt_VFE@markerTxOn                = True             ;# whether to add text for count of points (logical)
; opt_VFE@ptxFontHeightF            = 0.0085           ;# textfont height for count of points (numeric value)
; opt_VFE@markerTxXOffset           = 0.015            ;# X distance between point markers and point text (numeric value)
; opt_VFE@markerTxYOffset           = 0.015            ;# Y distance between point markers and point text
; opt_VFE@DatatypeLabelsOn          = False            ;# whether to draw data type labels (logical)
; opt_VFE@DatatypeLabels            = (/"Models","Renalysis"/)
;                                                      ;# labels of data type (array of 2 str) 
; opt_VFE@DatatypeLabelsFontHeightF = 0.05             ;# data type labels font height (numeric value)  
; opt_VFE@DatatypelegendWidth       = 0.15             ;# width of data type labels (numeric value)
; opt_VFE@DatatypelegendHeight      = (have default)   ;# height of data type labels (numeric value)
; opt_VFE@caseLabels                = (not draw)                 ;# case names for labels   
; opt_VFE@caseLabelsFontHeightF     = 0.013            ;# font height of caselabels (numeric value)
; opt_VFE@caseLabelsYinterval       = 0.06             ;# leading of caselabels (numeric value)
; opt_VFE@caseLabelsXloc            = 0.05             ;# X strat location of caselabels (numeric value)
; opt_VFE@caseLabelsYloc            = 0.3              ;# Y strat location of caselabels (numeric value)
;
; opt_VFE@stdline                   = (not draw)       ;# plot rms_std/SD_std line
;*******************************************************************
begin
;*******************************************************************
   nCase = dimsizes(RATIO)  

   if ((dimsizes(dimsizes(RATIO)).ne.1) .or. (dimsizes(dimsizes(Rv)).ne.1) \
         .or. (dimsizes(RATIO).ne.dimsizes(Rv))) then
      print("VFE_diagram error: dimsizes of RATIO and Rv should match !")
      exit
   end if
      
   ;compute location for points  
   X = RATIO * cos(acos(Rv))  
   Y = RATIO * sin(acos(Rv))  
                                              
   ;compute location for std line  
   if (opt_VFE .and. isatt(opt_VFE,"stdline")) then 
      STD  = opt_VFE@stdline

      if (typeof(STD) .ne. "float") then   
         STD  := tofloat(STD)
      end if

      if ((dimsizes(dimsizes(STD)).ne.1) .or. (dimsizes(STD).ne.dimsizes(Rv))) then
         print("VFE_diagram error: dimsizes of opt_VFE@stdline and Rv should match !")
         exit
      end if 
 
      X1   = (RATIO - STD) * cos(acos(Rv)) 
      Y1   = (RATIO - STD) * sin(acos(Rv)) 
      X2   = (RATIO + STD) * cos(acos(Rv)) 
      Y2   = (RATIO + STD) * sin(acos(Rv)) 
   end if
  
   ;set value in axis
   xyMin           = 0.  
   xyOne           = 1.00

   ;set max of coordinate
   if (opt_VFE .and. isatt(opt_VFE,"xyMax")) then            
      xyMax = opt_VFE@xyMax

      if ((.not.isnumeric(xyMax)) .or. (.not.isscalar(xyMax))) then
         print("VFE diagram error: opt_VFE@xyMax should be a numeric value !")
         exit
      end if

      if (xyMax .le. 1.65) then
         xyMax = 1.65
      else
         if (xyMax .le. 2.0) then
	         xyMax = 2.0
	      end if
      end if  
   else
      if (max(RATIO) .le. 1.65) then                                           
         xyMax = 1.65
      else
         if (max(RATIO) .le. 2.0) then
            xyMax = 2.0
         else
            xyMax = ceil(max(RATIO)*10)/10.
         end if
      end if
   end if
 
   ;set font heightF in value in axis
   if (opt_VFE .and. isatt(opt_VFE, "xyFontHeightF"))  then 
      FontHeightF  = opt_VFE@xyFontHeightF                 

      if (.not.isnumeric(FontHeightF) .or. (.not.isscalar(FontHeightF))) then
         print("VFE_diagram error: opt_VFE@xyFontHeightF should be a numeric value !")
         exit
      end if
   else
      FontHeightF  = 0.0175
   end if
;-------------------------------------------------------------------
; Part 1:
; base plot: basic x-y and draw the 1.0 observed
;------------------------------------------------------------------- 
   ;set attr of radii (x/y)
   rxy                   = True       
   rxy@gsnDraw           = False
   rxy@gsnFrame          = False         
  
   rxy@vpHeightF         = 0.65
   rxy@vpWidthF          = 0.65

   rxy@tmYLBorderOn      = False
   rxy@tmXBBorderOn      = False
   rxy@tmYRBorderOn      = False
   rxy@tmYROn            = False         
   rxy@tmXTBorderOn      = False
   rxy@tmXTOn            = False         
   rxy@tmXBMode          = "Explicit"
  
   XB_V                  = ispan(0,toint(xyMax*100),25)/100.
   rxy@tmXBValues        = XB_V                                    

   XB_L                  = tostring(sprintf("%3.2f",XB_V))         
   XB_L(0)               = "   "
   XB_L(4)               = "REF"

   if (opt_VFE .and. isatt(opt_VFE,"OneX")) then
      if ((.not.isstring(opt_VFE@OneX)) .or. (.not.isscalar(opt_VFE@OneX))) then
         print("VFE_diagram error: opt_VFE@OneX should be a string !") 
         exit
      end if  

      XB_L(4) = opt_VFE@OneX
   end if
  
   rxy@tmXBLabels           = XB_L
   rxy@tmXBMajorLengthF     = 0.015                                   
   rxy@tmXBLabelFontHeightF = FontHeightF
   rxy@tmXBMinorOn          = False
   rxy@tmYLMode             = "Manual"
   rxy@tmYLMinorOn          = False
   rxy@tmYLMajorLengthF     = rxy@tmXBMajorLengthF
   rxy@tmYLLabelFontHeightF = FontHeightF
   rxy@tmYLMode             = "Explicit" 
   rxy@tmYLValues           = XB_V 
   rxy@tmYLLabels           = tostring(sprintf("%3.2f",XB_V))

   if (opt_VFE .and. isatt(opt_VFE,"tiYAxisString") )  then  
      if ((.not.isstring(opt_VFE@tiYAxisString)) .or. \
         (.not.isscalar(opt_VFE@tiYAxisString)))  then
         print("VFE_diagram error: opt_VFE@tiYAxisString should be a string !")
         exit
      end if

      rxy@tiYAxisString     = opt_VFE@tiYAxisString
   else
      rxy@tiYAxisString     = "RMSL"
   end if
  
   xyMax_Panel              = xyMax+ 0.10
   rxy@tiYAxisFontHeightF   = FontHeightF                                           
   rxy@trXMaxF              = xyMax_Panel
   rxy@trYMaxF              = xyMax_Panel
   rxy@xyDashPatterns       = (/0./)              
   rxy@xyLineThicknesses    = (/2./)             

   if (opt_VFE .and. isatt(opt_VFE, "tiMainString")) then
      if ((.not.isstring(opt_VFE@tiMainString)) .or. \
         (.not.isscalar(opt_VFE@tiMainString))) then
         print("VFE_diagram error: opt_VFE@tiMainString should be a string !")
         exit
      end if
      rxy@tiMainString      = opt_VFE@tiMainString
    
      if (isatt(opt_VFE, "tiMainFontHeightF")) then
         if ((.not.isnumeric(opt_VFE@tiMainFontHeightF)) .or. \ 
            (.not.isscalar(opt_VFE@tiMainFontHeightF))) then
            print("VFE_diagram error: opt_VFE@tiMainFontHeightF should be a numeric value !")
            exit
         end if
         rxy@tiMainFontHeightF = opt_VFE@tiMainFontHeightF
      else
         rxy@tiMainFontHeightF = 0.0225                    
      end if    
   end if     

   ;create outer 'correlation axis'
   npts    = 200                                            
   xx      = fspan(xyMin,xyMax,npts) 
   yy      = sqrt(xyMax^2 - xx^2) 

   ;create and draw XY plot.
   VFE  = gsn_xy(wks,xx,yy,rxy)
    
   ;draw x and y to xyMax
   rsrRes                   = True 
   rsrRes@gsLineThicknessF  = rxy@xyLineThicknesses(0)      
   rsrRes@gsLineDashPattern = 0                            
  
   dum0 = gsn_add_polyline(wks, VFE, (/0.,0./), (/0.,xyMax/), rsrRes)
   dum1 = gsn_add_polyline(wks, VFE, (/0.,xyMax/), (/0.,0./), rsrRes)

   VFE@$unique_string("dum")$ = dum0
   VFE@$unique_string("dum")$ = dum1

   ;draw stn Rad
   xx = fspan(xyMin, xyOne ,npts)                         
   yy = sqrt(xyOne - xx^2)
  
   rsrRes@gsLineDashPattern = 1                           
   rsrRes@gsLineThicknessF  = rxy@xyLineThicknesses(0)
   rsrRes@gsLineColor       =  "gray20"     
  
   dum2 = gsn_add_polyline(wks, VFE, xx, yy, rsrRes)

   VFE@$unique_string("dum")$ = dum2
  
   delete(xx)
   delete(yy)

   rsrRes@gsLineThicknessF  = 1
                                                  
   if (opt_VFE .and. isatt(opt_VFE, "stnRad") ) then
      if ((.not.isnumeric(opt_VFE@stnRad)) .or. \
         (dimsizes(dimsizes(opt_VFE@stnRad)).ne.1))  then
         print("VFE_diagram error: opt_VFE@stnRad should be 1D array of numeric values !")
         exit
      end if                    
      nStnRad = dimsizes(opt_VFE@stnRad)

      dum3  = new(nStnRad, graphic)
      
      do n=0,nStnRad-1
         rr = opt_VFE@stnRad(n)
         xx = fspan(xyMin, rr, npts) 
         yy = sqrt(rr^2 - xx^2)
	 
         dum3(n) = gsn_add_polyline(wks, VFE, xx, yy, rsrRes)
      end do
      
      VFE@$unique_string("dum")$ = dum3
      delete(xx)
      delete(yy)
   end if

   getvalues VFE                                   
      "tmYLLabelFont"        : tmYLLabelFont        
      "tmYLLabelFontHeightF" : tmYLLabelFontHeightF
   end getvalues
;-------------------------------------------------------------------
; Part 2: Correlation labels
;-------------------------------------------------------------------
   ;plot Rv Arc label
   txRes               = True                      
   txRes@txFontHeightF = FontHeightF               
   txRes@tmYLLabelFont = tmYLLabelFont             
   txRes@txAngleF      = -45.

   if (opt_VFE .and. isatt(opt_VFE,"CorLabel")) then 
      if ((.not.isstring(opt_VFE@CorLabel))  \
         .or. (.not.isscalar(opt_VFE@CorLabel))) then
         print("VFE_diagram error: opt_VFE@CorLabel should be a string !")
         exit
      end if

      dum4 = gsn_add_text(wks,VFE,opt_VFE@CorLabel,xyMax_Panel/sqrt(2.)+0.1,xyMax_Panel/sqrt(2.)+0.1,txRes)
   else
      dum4 = gsn_add_text(wks,VFE,"Similarity",xyMax_Panel/sqrt(2.)+0.1,xyMax_Panel/sqrt(2.)+0.1,txRes)
   end if

   VFE@$unique_string("dum")$ = dum4

   ;plot correlation labels
   sLabels = (/"0.0","0.1","0.2","0.3","0.4","0.5","0.6" \  
              ,"0.7","0.8","0.9","0.95","0.99","1.0"     /)    
   cLabels = stringtofloat(sLabels)
   rad     = get_pi("float")/180.
   radC    = acos(cLabels)
   angC    = radC/rad

   xC      = xyMax * cos(radC)
   yC      = xyMax * sin(radC)
   xC      = xC + 0.020 * cos(radC)
   yC      = yC + 0.060 * sin(radC)

   txRes@txAngleF      = 0.0 
   txRes@txFontHeightF = FontHeightF*0.50          
   txRes@txJust        = "CenterLeft"              
   txRes@txFontHeightF = FontHeightF              

   plRes               = True
   plRes@gsLineThicknessF = 2.
 
   tmEnd = 0.975
   radTM = xyMax*tmEnd                             

   dum5 = new(dimsizes(sLabels),graphic)
   dum6 = dum5
  
   do i = 0,dimsizes(sLabels)-1                   
      txRes@txAngleF = angC(i)
    
      dum5(i) = gsn_add_text(wks, VFE, sLabels(i), xC(i), yC(i), txRes) 
    
      xTM   = (/xyMax * cos(radC(i)), radTM * cos(radC(i))/)         
      yTM   = (/xyMax * sin(radC(i)), radTM * sin(radC(i))/)       
    
      dum6(i) = gsn_add_polyline(wks, VFE, xTM, yTM, plRes)
   end do

   VFE@$unique_string("dum")$ = dum5
   VFE@$unique_string("dum")$ = dum6
                                                 
   mTM     = (/0.05,0.15,0.25,0.35,0.45,0.55,0.65 \ 
             ,0.75,0.85,0.91,0.92,0.93,0.94,0.96,0.97,0.98  /)
   angmTM  = acos(mTM) / rad                       
   radmTM  = xyMax * (1. - (1. - tmEnd) * 0.5)     

   dum7 = new(dimsizes(mTM), graphic)
  
   do i = 0,(dimsizes(mTM)-1)                     
      xTM    = (/xyMax * cos(angmTM(i) * rad), radmTM * cos(angmTM(i) * rad)/)    
      yTM    = (/xyMax * sin(angmTM(i) * rad), radmTM * sin(angmTM(i) * rad)/)
    
      dum7(i)  = gsn_add_polyline(wks, VFE, xTM, yTM, plRes)
   end do

   VFE@$unique_string("dum")$ = dum7
                                                  
   if (opt_VFE .and. isatt(opt_VFE, "ccRays") ) then
      if ((.not.isnumeric(opt_VFE@ccRays)) .or. \
         (dimsizes(dimsizes(opt_VFE@ccRays)).ne.1)) then
         print("VFE_diagram error: opt_VFE@ccRays should be 1D array of numeric values !")
         exit
      end if
      angRL = acos(opt_VFE@ccRays) / rad           

      rlRes = True
      rlRes@gsLineDashPattern = 2                  
      rlRes@gsLineThicknessF  = 1                    
      rlRes@gsLineColor       =  "gray38"

      dum8 = new(dimsizes(angRL), graphic)
      
      do i = 0,(dimsizes(angRL)-1)
         xRL     = xyMax * cos(angRL(i) * rad)
         yRL     = xyMax * sin(angRL(i) * rad)
	 
         dum8(i) = gsn_add_polyline(wks, VFE, (/0,xRL/), (/0,yRL/), rlRes)
      end do
      
      VFE@$unique_string("dum")$ = dum8
   end if 
;-------------------------------------------------------------------
; Part 3: Concentric about 1.0 on XB axis
;-------------------------------------------------------------------
   ;draw radii from 1.0 in x-axis
   if (opt_VFE .and. isatt(opt_VFE, "centerDiffRMS")) then 
      if ((.not.islogical(opt_VFE@centerDiffRMS)) .or. \
         (.not.isscalar(opt_VFE@centerDiffRMS))) then
         print("VFE_diagram error: opt_VFE@centerDiffRMS should be a logical values !")
         exit
      end if
	    
      respl                    = True                
      respl@xyLineThicknessF   = 1.0                 
      respl@xyLineDashPattern  = 2                   
      respl@gsLineColor        = "Black"                

      dx   = 0.25                                     
      npts = 100                                     
      ang  = fspan(180,360,npts)*rad

      if (xyMax .eq. 1.65) then                      
         ncon = 4
	      dum9 = new(ncon,graphic)
	 
         do n = 1,ncon 
            rr  = n*dx            
            xx  = 1. + rr*cos(ang)
            yy  = fabs( rr*sin(ang) )

            if (n.le.2) then
               dum9(n-1) = gsn_add_polyline(wks,VFE,xx,yy,respl)
            end if
	    
            if (n.eq.3) then
               n3 = floattointeger( 0.77*npts ) 
               dum9(n-1) = gsn_add_polyline(wks,VFE,xx(0:n3),yy(0:n3),respl)
            end if
	    
	         if (n.eq.4) then
               n4 = floattointeger( 0.61*npts ) 
               dum9(n-1) = gsn_add_polyline(wks,VFE,xx(0:n4),yy(0:n4),respl)
            end if
         end do
      end if

      if ((xyMax .ge. 2.0) .and. (xyMax .lt. 2.25)) then
         ncon = 4
	      dum9 = new(ncon,graphic)
	 
	      do n=1,ncon 
            rr  = n*dx            
            xx  = 1. + rr*cos(ang)
            yy  = fabs( rr*sin(ang) )
        
            dum9(n-1) = gsn_add_polyline(wks,VFE,xx,yy,respl)
	      end do
      end if

      if (xyMax .ge. 2.25) then
         ncon = toint((max(XB_V) - 1.)/0.25)
	      dum9 = new(ncon,graphic)
	 
         do n=1,ncon 
            rr  = n*dx            
            xx  = 1. + rr*cos(ang)
            yy  = fabs( rr*sin(ang) )

            if (n.le.4) then
               dum9(n-1) = gsn_add_polyline(wks,VFE,xx,yy,respl)
            else
               n_s = floattointeger(acos(1./rr)*npts/rad/180.)
               dum9(n-1) = gsn_add_polyline(wks,VFE,xx((n_s-1):99),yy((n_s-1):99),respl)
            end if	   
         end do
      end if
      
      delete(ang)
      delete(xx)
      delete(yy)
      
      VFE@$unique_string("dum")$ = dum9
  end if
;------------------------------------------------------------------
; Part 4: generic resources that will be applied to all users data 
;         points of course, these can be changed 
;------------------------------------------------------------------
   ;set markers for points of models/obs
   if (opt_VFE .and. isatt(opt_VFE, "Markers")) then
      if ((.not.isint(opt_VFE@Markers)) .or. (dimsizes(dimsizes(opt_VFE@Markers)).ne.1)) then
         print("VFE_diagram error: opt_VFE@Markers should be an array of 2 integer !")
         exit

         if (nCase.gt.nmodel) then
            if (dimsizes(opt_VFE@Markers).ne.2) then
               print("VFE_diagram error: opt_VFE@Markers should be an array of 2 integer !")
               exit
            end if
         end if   
      end if

      Markers = opt_VFE@Markers
   else
      Markers = (/4, 8/) 
   end if

   if (opt_VFE .and. isatt(opt_VFE, "gsMarkerThicknessF")) then
      if ((.not.isnumeric(opt_VFE@gsMarkerThicknessF)) .or. \
         (.not.isscalar(opt_VFE@gsMarkerThicknessF)))then
         print("VFE_diagram error: opt_VFE@gsMarkerThicknessF should be a numeric value !")
         exit
      end if
      gsMarkerThicknessF = opt_VFE@gsMarkerThicknessF
   else
      gsMarkerThicknessF = 1.0
   end if

   if (opt_VFE .and. isatt(opt_VFE, "gsMarkerSizeF")) then
      if ((.not.isnumeric(opt_VFE@gsMarkerSizeF)) .or. \
         (.not.isscalar(opt_VFE@gsMarkerSizeF))) then
         print("VFE_diagram error: opt_VFE@gsMarkerSizeF should be a numeric value !")
         exit
      end if
      gsMarkerSizeF      = opt_VFE@gsMarkerSizeF
   else
      gsMarkerSizeF      = 0.0085                  
   end if
 
   ;set colors for points of models/obs
   if (opt_VFE .and. isatt(opt_VFE, "Colors")) then
      if (isstring(opt_VFE@Colors)) then
         if (dimsizes(dimsizes(opt_VFE@Colors)).ne.1) then
            print("VFE_diagram error: if setting opt_VFE@Colors with named colors, "+ \
                  " it should be 1D array of string!")
            exit
         end if     
      else
         if (isint(opt_VFE@Colors)) then
            dim_color = dimsizes(opt_VFE@Colors)
            if ((dimsizes(dim_color).ne.2).or.(dim_color(1).ne.3)) then
               print("VFE_diagram error: if setting opt_VFE@Colors with NhlTColorIndex, "+ \
                     "it should be N x 3 interger array !")
               exit
            end if
         else
            print("VFE_diagram error: opt_VFE@Colors should be 1D array of string with named color," \
                  +" or N x 3 interger array of NhlTColorIndex !")
            exit
         end if
      end if
      Colors0  = opt_VFE@Colors
   else
      Colors0  = (/"aquamarine3" ,"brown", "blue", "cadetblue", "chocolate", \
                  "cornflowerblue", "darkolivegreen", "darkorange", "darkkhaki", "darksalmon", \
                  "darkslategray", "darkslateblue", "dimgrey" , "chartreuse4", "indianred", \
                  "khaki4", "maroon", "mediumpurple", "deeppink", "mediumorchid4", \
                  "navajowhite4", "navy", "yellow3", "orange", "orangered", \
                  "palegreen2", "palevioletred", "turquoise4", "pink3", "plum4", \
                  "rosybrown1",  "seagreen", "sienna", "skyblue3", "tomato",\
                  "slategray", "tan", "steelblue"/)
   end if

   if (isstring(Colors0)) then
      if (dimsizes(Colors0) .lt. nCase) then
         Colors = new(nCase, string)
         Colors(0:(dimsizes(Colors0)-1)) = Colors0

         do ic = dimsizes(Colors0),(nCase-1)
            ind_color := generate_sample_indices(dimsizes(Colors0), 1)
            Colors(ic) = Colors0(ind_color(0))
         end do
      else
         Colors = Colors0
      end if
   else
      if (dim_color(0) .lt. nCase) then
         Colors = new((/nCase,3/), integer)
         Colors(0:(dim_color(0)-1),:) = Colors0

         do ic = dim_color(0),(nCase-1)
            ind_color := generate_sample_indices(dim_color(0), 1)
            Colors(ic,:) = Colors0(ind_color(0),:)
         end do
      else
         Colors = Colors0
      end if    
   end if

   ;set count for points of models/obs
   if (opt_VFE .and. isatt(opt_VFE, "markerTxOn")) then
      if ((.not.islogical(opt_VFE@markerTxOn)) .or. \
         (.not.isscalar(opt_VFE@markerTxOn))) then
         print("VFE_diagram error: opt_VFE@markerTxOn should be a logical value !")
         exit
      end if

      MarkerTxOn = opt_VFE@markerTxOn
   else
      MarkerTxOn = True
   end if

   if (MarkerTxOn) then   
      if (opt_VFE .and. isatt(opt_VFE, "ptxFontHeightF")) then
         if ((.not.isnumeric(opt_VFE@ptxFontHeightF)) .or. \
            (.not.isscalar(opt_VFE@ptxFontHeightF))) then
            print("VFE_diagram error: opt_VFE@ptxFontHeightF should be a numeric value !")
            exit
         end if

         ptxFontHeightF     = opt_VFE@ptxFontHeightF  
      else
         ptxFontHeightF     = 0.0085          
      end if

      if (opt_VFE .and. isatt(opt_VFE, "markerTxYOffset")) then
         if ((.not.isnumeric(opt_VFE@markerTxYOffset)) .or. \
            (.not.isscalar(opt_VFE@markerTxYOffset))) then
            print("VFE_diagram error: opt_VFE@markerTxYOffset should be a numeric value !")
            exit
         end if
         markerTxYOffset    = opt_VFE@markerTxYOffset             
      else
         markerTxYOffset    = 0.015                    
      end if

      if (opt_VFE .and. isatt(opt_VFE, "markerTxXOffset")) then
         if ((.not.isnumeric(opt_VFE@markerTxXOffset)) .or. \
            (.not.isscalar(opt_VFE@markerTxXOffset))) then
            print("VFE_diagram error: opt_VFE@markerTxXOffset should be a numeric value !")
            exit
         end if
         markerTxXOffset    = opt_VFE@markerTxXOffset             
      else
         markerTxXOffset    = 0.015                    
      end if

      ptRes = True                                       
      ptRes@txJust             = "BottomCenter"          
      ptRes@txFontThicknessF   = 1.0                  
      ptRes@txFontHeightF      = ptxFontHeightF 
   end if
  
   gsRes = True 
   gsRes@gsMarkerThicknessF = gsMarkerThicknessF      
   gsRes@gsMarkerSizeF      = gsMarkerSizeF           
        
   dum10 = new(nCase,graphic)
   dum11 = dum10
   dum12 = dum10
  
   ;plot model and observation/reanalysis points
   do n = 0,(nCase-1)
      if (n.lt.nmodel) then                         
         gsRes@gsMarkerIndex = Markers(0) 
      else
         gsRes@gsMarkerIndex = Markers(1) 
      end if
      if (isstring(Colors)) then    
         gsRes@gsMarkerColor := Colors(n) 
      else
         gsRes@gsMarkerColor := Colors(n,:)
      end if 
     
      dum10(n) = gsn_add_polymarker(wks, VFE, X(n), Y(n), gsRes) 

      if (MarkerTxOn) then
         ptRes@txFontColor := gsRes@gsMarkerColor 

         dum11(n) = gsn_add_text(wks, VFE, (n+1), X(n)+markerTxXOffset, Y(n)+markerTxYOffset, ptRes)
      end if

      if (opt_VFE .and. isatt(opt_VFE, "stdline")) then     
         xLAstd = (/X1(n), X2(n)/)
         yLAstd = (/Y1(n), Y2(n)/)
	
         gsRes@gsLineColor       := gsRes@gsMarkerColor                 
         gsRes@gsLineDashPattern = 0                       
         gsRes@gsLineThicknessF  = 2.                  
	
         dum12(n) = gsn_add_polyline(wks, VFE, xLAstd, yLAstd, gsRes)  
      end if
   end do

   VFE@$unique_string("dum")$ = dum10 

   if (MarkerTxOn) then
      VFE@$unique_string("dum")$ = dum11 
   end if 

   if (opt_VFE .and. isatt(opt_VFE, "stdline")) then 
      VFE@$unique_string("dum")$ = dum12                     
   end if
;------------------------------------------------------------------
; Part 5: ; add case legend and variable labels
;------------------------------------------------------------------
   ;plot labels of type of data
   if (opt_VFE .and. isatt(opt_VFE, "DatatypeLabelsOn")) then
      if ((.not.islogical(opt_VFE@DatatypeLabelsOn)) .or. \
         (.not.isscalar(opt_VFE@DatatypeLabelsOn))) then
         print("VFE_diagram error: opt_VFE@DatatypeLabelsOn should be a logical value !")
         exit
      end if

      if (isatt(opt_VFE, "DatatypeLabels")) then
         if ((.not.isstring(opt_VFE@DatatypeLabels)) .or. \
            (dimsizes(dimsizes(opt_VFE@DatatypeLabels)).ne.1)) then
            print("VFE_diagram error: opt_VFE@DatatypeLabels should be 1D array of string !")
            exit
         end if
         DatatypeLabels = opt_VFE@DatatypeLabels

         if ((nCase.gt.nmodel).and.(dimsizes(DatatypeLabels).lt.2)) then
            print("VFE_diagram error: opt_VFE@DatatypeLabels should be 1D array of 2 string !")
            exit
         end if 
      else
         DatatypeLabels = (/"Models","Reanalysis"/)
      end if

      if (nCase.eq.nmodel) then
         DatatypeLabels := DatatypeLabels(0)
      else
         DatatypeLabels := DatatypeLabels(0:1)
      end if

      if (isatt(opt_VFE, "DatatypeLabelsFontHeightF")) then
         if ((.not.isnumeric(opt_VFE@DatatypeLabelsFontHeightF)) \
            .or. (.not.isscalar(opt_VFE@DatatypeLabelsFontHeightF))) then
            print("VFE_diagram error: opt_VFE@DatatypeLabelsFontHeightF should be a numeric value !")
            exit
         end if
         DatatypeLabelsFontHeightF = opt_VFE@DatatypeLabelsFontHeightF
      else
         DatatypeLabelsFontHeightF = 0.05  
      end if
      
      lgres                    = True
     
      if (nCase.eq.nmodel) then 
         if (isstring(Colors)) then
            lgres@lgMarkerColors  = Colors(0)
         else
            lgres@lgMarkerColors  = Colors(0,:)
         end if
	      lgres@lgMarkerIndexes    = Markers(0)
      else
         if (isstring(Colors)) then
            lgres@lgMarkerColors  = (/Colors(0),Colors(nmodel)/)
         else
            lgres@lgMarkerColors  = (/Colors(0,:),Colors(nmodel,:)/)
         end if
	      lgres@lgMarkerIndexes    = Markers
      end if
     
      lgres@lgMarkerSizeF      = gsMarkerSizeF*1.25                                 
      lgres@lgMarkerThicknessF = gsMarkerThicknessF
      lgres@lgItemType         = "Markers"                         
      lgres@lgLabelFontHeightF = DatatypeLabelsFontHeightF         

      if (isatt(opt_VFE, "DatatypelegendWidth")) then
         if ((.not.isnumeric(opt_VFE@DatatypelegendWidth)) \
            .or. (.not.isscalar(opt_VFE@DatatypelegendWidth))) then
            print("VFE_diagram error: opt_VFE@DatatypelegendWidth should be a numeric value !")
            exit
         end if
         lgres@vpWidthF        = opt_VFE@DatatypelegendWidth
      else
         lgres@vpWidthF        = 0.15                             
      end if

      if (isatt(opt_VFE, "DatatypelegendHeight")) then
         if ((.not.isnumeric(opt_VFE@DatatypelegendHeight)) \
            .or. (.not.isscalar(opt_VFE@DatatypelegendHeight))) then
            print("VFE_diagram error: opt_VFE@DatatypelegendHeight should be a numeric value !")
            exit
         end if
         lgres@vpHeightF       = opt_VFE@DatatypelegendHeight
      else   
         lgres@vpHeightF       = where(nCase.eq.nmodel,0.03,0.03*2)
      end if

      lgres@lgPerimOn          = False                           
     
      nDatatype                = dimsizes(DatatypeLabels)
      
      lbid = gsn_create_legend(wks, nDatatype, DatatypeLabels, lgres)
	 
      amres = True 
      amres@amParallelPosF     =  0.35           
      amres@amOrthogonalPosF   = -0.35
     
      annoid1 = gsn_add_annotation(VFE, lbid, amres) 	            
   end if

   ;plot case labels
   if (opt_VFE .and. isatt(opt_VFE, "caseLabels")) then 
      caselabels = opt_VFE@caseLabels
      if ((.not.isstring(caselabels)) .or. \
         (dimsizes(dimsizes(caselabels)).ne.1)) then
         print("VFE_diagram error: opt_VFE@caseLabels should be 1D array of string !")
         exit
      end if
      Ncaselabels  = dimsizes(caselabels)

      if (isatt(opt_VFE, "caseLabelsFontHeightF")) then
         if ((.not.isnumeric(opt_VFE@caseLabelsFontHeightF)) \
            .or. (.not.isscalar(opt_VFE@caseLabelsFontHeightF))) then
            print("VFE_diagram error: opt_VFE@caseLabelsFontHeightF should be a numeric value !")
            exit
         end if
         caseLabelsFontHeightF = opt_VFE@caseLabelsFontHeightF
      else
         caseLabelsFontHeightF = 0.013
      end if

      txres               = True 
      txres@txFontHeightF = caseLabelsFontHeightF
      txres@txJust        = "CenterLeft"                                 

      if (opt_VFE .and. isatt(opt_VFE, "caseLabelsYinterval")) then
         if ((.not.isnumeric(opt_VFE@caseLabelsYinterval)) .or. \
            (.not.isscalar(opt_VFE@caseLabelsYinterval))) then
            print("VFE_diagram error: opt_VFE@caseLabelsYinterval should be a numeric value !")
            exit
         end if
         delta_y = opt_VFE@caseLabelsYinterval
      else
         delta_y = 0.06
      end if
      
      if (opt_VFE .and. isatt(opt_VFE, "caseLabelsYloc")) then
         if ((.not.isnumeric(opt_VFE@caseLabelsYloc)) .or. \
            (.not.isscalar(opt_VFE@caseLabelsYloc))) then
            print("VFE_diagram error: opt_VFE@caseLabelsYloc should be a numeric value !")
            exit
         end if
         ys  = opt_VFE@caseLabelsYloc                       
      else
         ys  = max( (/nCase*delta_y ,0.30/) )
      end if

      if (opt_VFE .and. isatt(opt_VFE, "caseLabelsXloc")) then
         if ((.not.isnumeric(opt_VFE@caseLabelsXloc)) .or. \
            (.not.isscalar(opt_VFE@caseLabelsXloc))) then
            print("VFE_diagram error: opt_VFE@caseLabelsXloc should be a numeric value !")
            exit
         end if
         xs  = opt_VFE@caseLabelsXloc                       
      else
         xs  = 0.125
      end if
      
      dum13 = new(Ncaselabels, graphic)

      do i = 1,Ncaselabels  
        dum13(i-1) = gsn_add_text(wks, VFE, i+" - "+caselabels(i-1), xs, ys, txres)

        ys = ys - delta_y
      end do
 
      VFE@$unique_string("dum")$ = dum13
   end if
   
   draw(VFE)
   frame(wks)
   
   return(VFE)
;*******************************************************************
end


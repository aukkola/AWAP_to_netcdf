  g9  {   k820309    `          17.0        Tê[                                                                                                           
       cable_bios_met_obs_params.F90 CABLE_BIOS_MET_OBS_PARAMS                                                     
                                                           
       GET_UNIT                      @                              
       WEATHER_GENERATOR_TYPE WGEN_INIT WGEN_DAILY_CONSTANTS WGEN_SUBDIURNAL_MET                   @               @                '             +      #NP    #NDTIME    #DELT    #LATDEG    #WINDDAY 	   #TEMPMINDAY 
   #TEMPMAXDAY    #TEMPMINDAYNEXT    #TEMPMAXDAYPREV    #SOLARMJDAY    #PRECIPDAY    #SNOWDAY    #PPADAY    #VAPPPA0900    #VAPPPA1500    #VAPPPA0900NEXT    #VAPPPA1500PREV    #DECRAD    #WINDDARK    #WINDLITE    #SOLARNORM    #LATRAD    #DAYLENGTH    #TIMESUNSETPREV    #TIMESUNRISE    #TIMEMAXTEMP    #TIMESUNSET    #TEMPSUNSETPREV     #TEMPSUNSET !   #TEMPNIGHTRATE "   #TEMPNIGHTRATEPREV #   #TEMPRANGEDAY $   #TEMPRANGEAFT %   #PHISD &   #PHILD '   #PRECIP (   #SNOW )   #WIND *   #TEMP +   #VAPPPA ,   #PPA -   #QV .   #COSZEN /                                                                                                                                                                                           	                                                                         
            &                                                                                    	            X                 
            &                                                                                    
                              
            &                                                                                                è                 
            &                                                                                                0                
            &                                                                                                x             	   
            &                                                                                                À             
   
            &                                                                                                                
            &                                                                                                P                
            &                                                                                                                
            &                                                                                                à                
            &                                                                                                (                
            &                                                                                                p                
            &                                                                                                ¸                
            &                                                                                                     
                                                                        
            &                                                                                                P                
            &                                                                                                                
            &                                                                                                à                
            &                                                                                                (                
            &                                                                                                p                
            &                                                                                                ¸                
            &                                                                                                                 
            &                                                                                                H                
            &                                                                                                                 
            &                                                                                    !            Ø                
            &                                                                                    "                             
            &                                                                                    #            h                
            &                                                                                    $            °                 
            &                                                                                    %            ø             !   
            &                                                                                    &            @             "   
            &                                                                                    '                         #   
            &                                                                                    (            Ð             $   
            &                                                                                    )            	             %   
            &                                                                                    *            `	             &   
            &                                                                                    +            ¨	             '   
            &                                                                                    ,            ð	             (   
            &                                                                                    -            8
             )   
            &                                                                                    .            
             *   
            &                                                                                    /            È
             +   
            &                                                             @               @           0     '             	      #PATH_IN 1   #PATH_OUT 2   #RAIN_FILE 3   #SWDOWN_FILE 4   #WIND_FILE 5   #TAIRMAX_FILE 6   #TAIRMIN_FILE 7   #VPH09_FILE 8   #VPH15_FILE 9                                              1     È                                                                 2     È       È              .                                         3                  È                      &                                                   .                                         4            Ø      È                      &                                                   .                                         5                   È                      &                                                   .                                         6            h      È                      &                                                   .                                         7            °      È                      &                                                   .                                         8            ø      È                      &                                                   .                                         9            @      È       	               &                                                   #         @                                  :                    #IUNIT ;                                              ;            #         @                                  <                    #WG =   #NP >   #LATITUDE ?   #DELS @                                              =                   #WEATHER_GENERATOR_TYPE              
                                  >                    
                                  ?                    	 (   p          5 O p            5 O p                                    
                                  @     	      #         @                                  A                    #WG B   #NP C   #YEARDAY D                                              B                   #WEATHER_GENERATOR_TYPE              
                                  C                     
                                  D           #         @                                  E                    #WG F   #NP G   #ITIME H                                              F                   #WEATHER_GENERATOR_TYPE              
                                  G                     
                                  H                                                        I                                                                                                      J                                                                    @@                               K                                                         L                                                                  @ @                               M                   	                &                                                      @ @                              N                       @ @                              O                       @                                P     	                  @                                Q     	                  @ @                              R     	                  @ @                              S     	                @                                T                   	                &                                                    @                                U                   	                &                                                    @                                V                   	                &                                                    @                                W                   	                &                                                    @                                X                   	                &                                                    @                                Y                   	                &                                                    @                                Z                   	                &                                                    @                                [                   	                &                                                    @                                \                   	                &                                                    @                                ]                   	                &                                                    @                                ^                   	                &                                                      @ @                              _                       @ @                              `                       @ @                              a                       @ @                              b                       @ @                              c                       @ @                              d                       @ @                              e                       @ @                              f                       @ @                              g                                                        h     	                 	                  À¨G            86400.#         @                                   i                    #WG j   #DELS k   #CURYEAR l   #KEND m   #KTAUDAY n   #FILE_PATH o   #FILENAME p             D @                               j                   #WEATHER_GENERATOR_TYPE              
 @                               k     	                 
                                 l                      
                                 m                      
                                  n                      @                              o     È                                 D                                 p                   #FILE_NAME 0   #         @                                   q                 	   #WG r   #FILENAME s   #COUNTER t   #CURYEAR u   #YEARSTART v   #YEAREND w   #KTAU x   #KEND y   #DELS z             D @                               r                   #WEATHER_GENERATOR_TYPE                                               s                   #FILE_NAME 0             
D                                 t                      
                                  u                     
                                  v                     
                                  w                     
                                  x                     
                                  y                     
  @                               z     	             @      fn#fn    à   @   J   TYPE_DEF_MOD       I   J  BIOS_IO_MOD '   i     J  CABLE_WEATHERGENERATOR >   ó  Â      WEATHER_GENERATOR_TYPE+CABLE_WEATHERGENERATOR A   µ  H   a   WEATHER_GENERATOR_TYPE%NP+CABLE_WEATHERGENERATOR E   ý  H   a   WEATHER_GENERATOR_TYPE%NDTIME+CABLE_WEATHERGENERATOR C   E  H   a   WEATHER_GENERATOR_TYPE%DELT+CABLE_WEATHERGENERATOR E        a   WEATHER_GENERATOR_TYPE%LATDEG+CABLE_WEATHERGENERATOR F   !     a   WEATHER_GENERATOR_TYPE%WINDDAY+CABLE_WEATHERGENERATOR I   µ     a   WEATHER_GENERATOR_TYPE%TEMPMINDAY+CABLE_WEATHERGENERATOR I   I     a   WEATHER_GENERATOR_TYPE%TEMPMAXDAY+CABLE_WEATHERGENERATOR M   Ý     a   WEATHER_GENERATOR_TYPE%TEMPMINDAYNEXT+CABLE_WEATHERGENERATOR M   q     a   WEATHER_GENERATOR_TYPE%TEMPMAXDAYPREV+CABLE_WEATHERGENERATOR I   	     a   WEATHER_GENERATOR_TYPE%SOLARMJDAY+CABLE_WEATHERGENERATOR H   	     a   WEATHER_GENERATOR_TYPE%PRECIPDAY+CABLE_WEATHERGENERATOR F   -
     a   WEATHER_GENERATOR_TYPE%SNOWDAY+CABLE_WEATHERGENERATOR E   Á
     a   WEATHER_GENERATOR_TYPE%PPADAY+CABLE_WEATHERGENERATOR I   U     a   WEATHER_GENERATOR_TYPE%VAPPPA0900+CABLE_WEATHERGENERATOR I   é     a   WEATHER_GENERATOR_TYPE%VAPPPA1500+CABLE_WEATHERGENERATOR M   }     a   WEATHER_GENERATOR_TYPE%VAPPPA0900NEXT+CABLE_WEATHERGENERATOR M        a   WEATHER_GENERATOR_TYPE%VAPPPA1500PREV+CABLE_WEATHERGENERATOR E   ¥  H   a   WEATHER_GENERATOR_TYPE%DECRAD+CABLE_WEATHERGENERATOR G   í     a   WEATHER_GENERATOR_TYPE%WINDDARK+CABLE_WEATHERGENERATOR G        a   WEATHER_GENERATOR_TYPE%WINDLITE+CABLE_WEATHERGENERATOR H        a   WEATHER_GENERATOR_TYPE%SOLARNORM+CABLE_WEATHERGENERATOR E   ©     a   WEATHER_GENERATOR_TYPE%LATRAD+CABLE_WEATHERGENERATOR H   =     a   WEATHER_GENERATOR_TYPE%DAYLENGTH+CABLE_WEATHERGENERATOR M   Ñ     a   WEATHER_GENERATOR_TYPE%TIMESUNSETPREV+CABLE_WEATHERGENERATOR J   e     a   WEATHER_GENERATOR_TYPE%TIMESUNRISE+CABLE_WEATHERGENERATOR J   ù     a   WEATHER_GENERATOR_TYPE%TIMEMAXTEMP+CABLE_WEATHERGENERATOR I        a   WEATHER_GENERATOR_TYPE%TIMESUNSET+CABLE_WEATHERGENERATOR M   !     a   WEATHER_GENERATOR_TYPE%TEMPSUNSETPREV+CABLE_WEATHERGENERATOR I   µ     a   WEATHER_GENERATOR_TYPE%TEMPSUNSET+CABLE_WEATHERGENERATOR L   I     a   WEATHER_GENERATOR_TYPE%TEMPNIGHTRATE+CABLE_WEATHERGENERATOR P   Ý     a   WEATHER_GENERATOR_TYPE%TEMPNIGHTRATEPREV+CABLE_WEATHERGENERATOR K   q     a   WEATHER_GENERATOR_TYPE%TEMPRANGEDAY+CABLE_WEATHERGENERATOR K        a   WEATHER_GENERATOR_TYPE%TEMPRANGEAFT+CABLE_WEATHERGENERATOR D        a   WEATHER_GENERATOR_TYPE%PHISD+CABLE_WEATHERGENERATOR D   -     a   WEATHER_GENERATOR_TYPE%PHILD+CABLE_WEATHERGENERATOR E   Á     a   WEATHER_GENERATOR_TYPE%PRECIP+CABLE_WEATHERGENERATOR C   U     a   WEATHER_GENERATOR_TYPE%SNOW+CABLE_WEATHERGENERATOR C   é     a   WEATHER_GENERATOR_TYPE%WIND+CABLE_WEATHERGENERATOR C   }     a   WEATHER_GENERATOR_TYPE%TEMP+CABLE_WEATHERGENERATOR E        a   WEATHER_GENERATOR_TYPE%VAPPPA+CABLE_WEATHERGENERATOR B   ¥     a   WEATHER_GENERATOR_TYPE%PPA+CABLE_WEATHERGENERATOR A   9     a   WEATHER_GENERATOR_TYPE%QV+CABLE_WEATHERGENERATOR E   Í     a   WEATHER_GENERATOR_TYPE%COSZEN+CABLE_WEATHERGENERATOR '   a  Þ       FILE_NAME+TYPE_DEF_MOD /   ?  P   a   FILE_NAME%PATH_IN+TYPE_DEF_MOD 0     P   a   FILE_NAME%PATH_OUT+TYPE_DEF_MOD 1   ß     a   FILE_NAME%RAIN_FILE+TYPE_DEF_MOD 3   {     a   FILE_NAME%SWDOWN_FILE+TYPE_DEF_MOD 1        a   FILE_NAME%WIND_FILE+TYPE_DEF_MOD 4   ³     a   FILE_NAME%TAIRMAX_FILE+TYPE_DEF_MOD 4   O      a   FILE_NAME%TAIRMIN_FILE+TYPE_DEF_MOD 2   ë      a   FILE_NAME%VPH09_FILE+TYPE_DEF_MOD 2   !     a   FILE_NAME%VPH15_FILE+TYPE_DEF_MOD %   #"  S       GET_UNIT+BIOS_IO_MOD +   v"  @   a   GET_UNIT%IUNIT+BIOS_IO_MOD 1   ¶"  p       WGEN_INIT+CABLE_WEATHERGENERATOR 4   &#  d   a   WGEN_INIT%WG+CABLE_WEATHERGENERATOR 4   #  @   a   WGEN_INIT%NP+CABLE_WEATHERGENERATOR :   Ê#  ¤   a   WGEN_INIT%LATITUDE+CABLE_WEATHERGENERATOR 6   n$  @   a   WGEN_INIT%DELS+CABLE_WEATHERGENERATOR <   ®$  e       WGEN_DAILY_CONSTANTS+CABLE_WEATHERGENERATOR ?   %  d   a   WGEN_DAILY_CONSTANTS%WG+CABLE_WEATHERGENERATOR ?   w%  @   a   WGEN_DAILY_CONSTANTS%NP+CABLE_WEATHERGENERATOR D   ·%  @   a   WGEN_DAILY_CONSTANTS%YEARDAY+CABLE_WEATHERGENERATOR ;   ÷%  c       WGEN_SUBDIURNAL_MET+CABLE_WEATHERGENERATOR >   Z&  d   a   WGEN_SUBDIURNAL_MET%WG+CABLE_WEATHERGENERATOR >   ¾&  @   a   WGEN_SUBDIURNAL_MET%NP+CABLE_WEATHERGENERATOR A   þ&  @   a   WGEN_SUBDIURNAL_MET%ITIME+CABLE_WEATHERGENERATOR !   >'  p       I4B+TYPE_DEF_MOD     ®'  p       SP+TYPE_DEF_MOD #   (  @       MLAND+TYPE_DEF_MOD !   ^(  p       LGT+TYPE_DEF_MOD    Î(         LATITUDE    Z)  @       MASKCOLS    )  @       MASKROWS    Ú)  @       MASKCTRW    *  @       MASKCTRS    Z*  @       MASKRES    *  @       NODATAVAL    Ú*         RAIN_DAY    f+         SWDOWN_DAY    ò+         WIND_DAY    ~,         TAIRMAX_DAY    
-         TAIRMIN_DAY    -         VPH_0900    ".         VPH_1500 !   ®.         PREV_TAIRMAX_DAY !   :/         NEXT_TAIRMIN_DAY    Æ/         PREV_VPH_1500    R0         NEXT_VPH_0900    Þ0  @       RAIN_UNIT    1  @       SWDOWN_UNIT    ^1  @       WIND_UNIT    1  @       TAIRMAX_UNIT    Þ1  @       TAIRMIN_UNIT    2  @       VPH09_UNIT    ^2  @       VPH15_UNIT !   2  @       TAIRMINNEXT_UNIT    Þ2  @       VPH09NEXT_UNIT    3  v       SECDAY     3         CABLE_BIOS_INIT #   /4  d   a   CABLE_BIOS_INIT%WG %   4  @   a   CABLE_BIOS_INIT%DELS (   Ó4  @   a   CABLE_BIOS_INIT%CURYEAR %   5  @   a   CABLE_BIOS_INIT%KEND (   S5  @   a   CABLE_BIOS_INIT%KTAUDAY *   5  P   a   CABLE_BIOS_INIT%FILE_PATH )   ã5  W   a   CABLE_BIOS_INIT%FILENAME $   :6  ²       CABLE_BIOS_READ_MET '   ì6  d   a   CABLE_BIOS_READ_MET%WG -   P7  W   a   CABLE_BIOS_READ_MET%FILENAME ,   §7  @   a   CABLE_BIOS_READ_MET%COUNTER ,   ç7  @   a   CABLE_BIOS_READ_MET%CURYEAR .   '8  @   a   CABLE_BIOS_READ_MET%YEARSTART ,   g8  @   a   CABLE_BIOS_READ_MET%YEAREND )   §8  @   a   CABLE_BIOS_READ_MET%KTAU )   ç8  @   a   CABLE_BIOS_READ_MET%KEND )   '9  @   a   CABLE_BIOS_READ_MET%DELS 
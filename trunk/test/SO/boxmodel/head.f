c$openad XXX Template ad_template.f
c-----------------------------------------------------------------------
      subroutine box_forward ( ilev1 )
c-----------------------------------------------------------------------

      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop


      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c -- routine arguments:
      
c-- local variables:

      integer ilev1


c-- routine body

            ikey = ilev1
CADJ STORE tnow = comlev1, key = ikey, byte = isbyte
CADJ STORE snow = comlev1, key = ikey, byte = isbyte

c-- calculate densities:
            call box_density( tNow, sNow, rho )

c-- calculate transport:
            call box_transport( rho, uVel )

CADJ STORE uvel = comlev1, key = ikey, byte = isbyte

c-- leap frog time stepping:
            call box_timestep(
     &           gamma_t, tStar, nullforce, 
     &           uVel, tNow, tOld, tNew )
            call box_timestep (
     &           gamma_s, sStar,        FW, 
     &           uVel, sNow, sOld, sNew )

c-- Robert filter:
            call box_robert_filter( tNow, tOld, tNew )
            call box_robert_filter( sNow, sOld, sNew )

c-- cycle fields
            call box_cycle_fields

CADJ STORE tNow = comlev1, key = ikey, byte = isbyte

            do l=1, ndim
               if ( tNow(l) .LT. -2. )  tNow(l) = 2.
            end do

      end

c$openad XXX Template ad_template.f
c-----------------------------------------------------------------------
      subroutine box_final_state
c-----------------------------------------------------------------------

      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c -- routine arguments:
      
c-- local variables:

c-- routine body

c-- re-initialise
      do l = 1, 2*ndim
         tsvec(l) = 0.
      end do

c-- move T,S into T-S-space

      do l = 1, ndim
         tsvec(l) = tnow(l)
         tsvec(l+ndim) = snow(l)
      end do

c-- check values once again
cph      do l = 1, 2*ndim
cph         print *, 'ph-check tsvec, r, r_t, r_s ', l,
cph     &        tsvec(l), r(l), r_t(l), r_s(l)
cph      end do
cph      print *, 'ph-check uvel ', uvel

      end

c$openad XXX Template ad_template.f
c-----------------------------------------------------------------------
      subroutine box_ini_fields
c-----------------------------------------------------------------------

      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c -- routine arguments:
      
c-- local variables:

c-- routine body

c-- initialize THC and metric
      thc_tot = 0.D0
      thc_t   = 0.D0
      thc_s   = 0.D0

      metric1 = 0.D0
      metric2 = 0.D0
      metric  = 0.D0

c-- forcing initial conditions

      nullforce(1) = 0.D0
      nullforce(2) = 0.D0

      FW(1)=(hundred/year)*35.D0*Area(1)
      FW(2)=-FW(1)

      tStar(1) = 22.D0
      tStar(2) =  0.D0
      sStar(1) = 36.D0
      sStar(2) = 34.D0

c-- steady state initial conditions:
      ubar= 20.D0*Sv

      T(1)= 20.D0
      T(2)= 1.D0
      T(3)= 1.D0
      S(1)= 35.5D0
      S(2)= 34.5D0
      S(3)= 34.5D0

c-- initialise final state
      do l = 1, 2*ndim
         tsvec(l) = 0.
      end do

c-- apply perturbations
      do l = 1, ndim
         t(l) = t(l) + xx(l)
         s(l) = s(l) + xx(l+ndim)
      end do

c-- map onto model state
      do l = 1, ndim
         tNew(l) = t(l)
         sNew(l) = s(l)
         tOld(l) = t(l)
         sOld(l) = s(l)
         tNow(l) = t(l)
         sNow(l) = s(l)
      end do
      uvel = ubar

      end

c$openad XXX Template ad_template.f
c-----------------------------------------------------------------------
      subroutine box_ini_params
c-----------------------------------------------------------------------

      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c--
c-- units are in CGS!
c--

c-- set switches
      verbmode = .FALSE.

c-- box geometry:

c-- north-south size:
      blength(1)=5000.0D5
      blength(2)=1000.0D5
      blength(3)=blength(1)
c-- depth:
      bheight(1)=1.0D5
      bheight(3)=4.0D5
      bheight(2)=bheight(1)+bheight(3)
      delta= bheight(1)/(bheight(1)+bheight(3))
c-- bwidth:
      bwidth=4000.0D5
c-- areas:
      area(1)=blength(1)*bwidth
      area(2)=blength(2)*bwidth
      area(3)=blength(3)*bwidth
c-- volumes:
      vol(1)=area(1)*bheight(1)
      vol(2)=area(2)*(bheight(1)+bheight(3))
      vol(3)=area(3)*bheight(3)

c-- Robert filter
      robert_filter_coeff=0.25D0

c-- parameters for linearized model matrix:

      hundred  = 1.0D2
      thousand = 1.0D3
      day      = 3600.D0*24.D0
      year     = day*365.D0
      Sv       = 1.D12
      u0       = 16.D0*Sv/0.0004D0
      alpha    = 1668.D-7
      beta     = 0.781D-3

c-- set SST restoring time in days per 50 meters upper layer thickness:

      days_per_50m_mixed_layer = 50.D0
      gamma_T = 1./(300.D0*day)
      gamma_S = 0./((days_per_50m_mixed_layer*day)*
     &               (bheight(1)/50.D2))

c-- epsilon_ic multiplies the eigenvector of fastest growing mode,
c-- before using it as initial conditions in the short mode runs
      epsilon_ic = -1.D-4

c-- parameters for numerically solving the steady state problem:

      noise_correlation_time = day*15.D0
c-- time step:
      delta_t=5.D0*day
c-- integration time:
cph time to stable solution:
cph      integration_time=5000.D0*year
cph time for the optimization
      integration_time=50.D0*year
c-- number of time steps:
      n_max = INT((integration_time/delta_t))

      fdeps = 1.D-6

c-- a norm kernel that is just an energy norm (sum of squares)
c-- weighted by the respective variance during a model run under
c-- stochastic forcing.  each element here is 1/variance:
      Y(1) = thousand*1./0.03268D0
      Y(2) = thousand*1./0.00794D0
      Y(3) = thousand*1./0.00140D0
      Y(4) = thousand*1./0.1417D0
      Y(5) = thousand*1./0.1286D0
      Y(6) = thousand*1./0.0878D0

c-- A vector used to form a norm Kernel that measures amplitude of THC
c-- (the transport in the model, in Sv):
      R(1) =       delta*alpha
      R(2) =            -alpha
      R(3) =  (1.-delta)*alpha
      R(4) =       -delta*beta
      R(5) =              beta 
      R(6) =  -(1.-delta)*beta

      do l = 1, 2*ndim
         R(l) = R(l)*u0/Sv
      end do

c-- The thermohaline circulation due to the Salinity/ temperature
c-- effects alone is given by R_S*P, R_T*P:
      do l = 1, 2*ndim
         if ( l. LE. ndim ) then
            proj_t(l) = 1.
            proj_s(l) = 0.
         else
            proj_t(l) = 0.
            proj_s(l) = 1.
         endif
      enddo

      do  l = 1, 2*ndim
         R_T(l) = proj_t(l)*R(l)
         R_S(l) = proj_s(l)*R(l)
      enddo

c-- A vector that measures the temperature gradient (T_1-T_2).
c-- in conjunction with R above, this is used to form a Kernel that
c-- measures the meridional heat transport = U*(T_1-T_2):
      R1(1) =  1.D0
      R1(2) = -1.D0
      R1(3) =  0.D0
      R1(4) =  0.D0
      R1(5) =  0.D0
      R1(6) =  0.D0

      do j = 1, 2*ndim
         do i = 1, 2*ndim
            X(i,j) = R(i)*R(j)
c--            X1(i,j) = R(i)*R1(j)
         enddo
      enddo

c-- regularize the norm kernel using Petros' suggestion:
      epsilon_regularize=1.0D-8

      do j = 1, 2*ndim
         do i = 1, 2*ndim
            X(i,j) = X(i,j) + epsilon_regularize
         enddo
      enddo

c-- **************
c-- TO BE FINISHED
c-- (see script .m)
c-- **************


      end

c$openad XXX Template ad_template.f
c-----------------------------------------------------------------------
      subroutine box_model_body
c-----------------------------------------------------------------------

      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c -- routine arguments:
      
c-- local variables:

      integer ilev1
      integer ilev2
      integer maxlev2

c-- routine body

c$openad INDEPENDENT(xx)

c-- inititialise this before loop to avoid additional forward run!
CADJ INIT tapelev2    = USER
CADJ INIT comlevfinal = COMMON, 1

      call box_ini_fields

      maxlev2 = n_max/nlev1 + 1
      if ( nlev1*nlev2 .LT. n_max ) then 
         print *, 'NEED TO SET nlev1*nlev2 >= n_max '
      else

      do ilev2 = 1, nlev2
       if ( ilev2 .LE. maxlev2 ) then

CADJ STORE tnow = tapelev2, key = ilev2
CADJ STORE told = tapelev2, key = ilev2
CADJ STORE snow = tapelev2, key = ilev2
CADJ STORE sold = tapelev2, key = ilev2

CADJ INIT comlev1 = COMMON, nlev1
        do ilev1 = 1, nlev1
         iloop = ( ilev2 - 1 )*nlev1 + ilev1
         if ( iloop .LE. n_max ) then

            call box_forward ( ilev1 )

c-- end of if ( iloop .LE. n_max ) then
         endif
c-- end of ilev1 loop
        enddo
c-- end of if ( ilev2 .LE. maxlev2 ) then
       endif
c-- end of ilev2 loop
      enddo

cph(
cph      print *, 'box_metric START'
cph      call box_metric
cph)
      call box_final_state

cph(
c-- taf seems to be confused by this
c-- It should recognize that there is no more dependency of
c-- tsvec on xx beyond box_final_state, shouldn't it?
c-- So, when tsvec rather than metric is the dependent variable,
c-- move this call to box_main, in order to hide it from TAF.
c-- Alternatively, I move box_metric before cost_finale_state:
c-- Now, taf correctly recognizes that metric isn't needed
c-- and kicks it out. That's good, but of course now it's missing.
cph      print *, 'box_metric START'
cph      call box_metric
cph)

      endif 

c$openad DEPENDENT(tnew)
c$openad DEPENDENT(snew)

      end

c$openad XXX Template ad_template.f
c-----------------------------------------------------------------------
      subroutine box_density ( tLoc, sLoc, rhoLoc )
c-----------------------------------------------------------------------

      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c -- routine arguments:
      double precision tLoc(ndim)
      double precision sLoc(ndim)
      double precision rhoLoc(ndim)

c-- local variables:

c-- linear equation of state

      do l = 1, ndim
         rhoLoc(l) = -alpha*tLoc(l) + beta*sLoc(l)
      end do

      end

c$openad XXX Template ad_template.f
c-----------------------------------------------------------------------
      subroutine box_transport ( rhoLoc, uVelLoc )
c-----------------------------------------------------------------------

      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c -- routine arguments:
      double precision rhoLoc(ndim)
      double precision uVelLoc

c-- local variables:

      uVelLoc = -u0*
     &     ( rhoLoc(1)*delta + rhoLoc(3)*(1.D0-delta) - rhoLoc(2) )

      end

c$openad XXX Template ad_template.f
c-----------------------------------------------------------------------
      subroutine box_robert_filter ( 
     &     fldNow, fldOld, fldNew )
c-----------------------------------------------------------------------
      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c -- routine arguments:
      double precision fldNow(ndim)
      double precision fldNew(ndim)
      double precision fldOld(ndim)
      
c-- local variables:

c-- routine body

      do l = 1, ndim
         fldNow(l) = fldNow(l) + robert_filter_coeff*
     &               ( fldNew(l) - 2.D0*fldNow(l) + fldOld(l) )
      end do

      end

c$openad XXX Template ad_template.f
c-----------------------------------------------------------------------
      subroutine box_cycle_fields
c-----------------------------------------------------------------------
      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c -- routine arguments:
      
c-- local variables:

c-- routine body

      do l = 1, ndim
         tOld(l) = tNow(l)
         tNow(l) = tNew(l)
         sOld(l) = sNow(l)
         sNow(l) = sNew(l)
      enddo

      end

c$openad XXX Template ad_template.f
c-----------------------------------------------------------------------
      subroutine box_update ( fldNew, fldOld, dFldDt )
c-----------------------------------------------------------------------
      use all_globals_mod


      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c -- routine arguments:
      
      double precision fldNew(ndim)
      double precision fldOld(ndim)
      double precision dFldDt(ndim)

c-- local variables:

c-- routine body

      do l = 1, ndim
         fldNew(l) = fldOld(l) + 2.D0*delta_t*dFldDt(l)
      enddo

      end

c$openad XXX Template ad_template.f
c-----------------------------------------------------------------------
      subroutine box_timestep ( 
     &     gammaLoc, fldStar, extForLoc, 
     &     uVelLoc, fldNow, fldOld, fldNew )
c-----------------------------------------------------------------------
      use all_globals_mod

      implicit none


c-- declaring parameter and constants

      integer l, i, j
      integer iloop

      integer nlev1
      integer nlev2
      integer isbyte
      parameter ( nlev1  =  73 )
      parameter ( nlev2  =  50 )
      parameter ( isbyte =   8 )

      integer ikey

c -- routine arguments:
      double precision uVelLoc
      double precision gammaLoc
      double precision fldStar(ndim-1)
      double precision extForLoc(ndim-1)
      double precision fldNow(ndim)
      double precision fldNew(ndim)
      double precision fldOld(ndim)
      character        ytstype*1
      
c-- local variables:
      double precision dFldDt(ndim)
cph      double precision velsign

c-- routine body

cph the following block simulates the bug in the matlab code
cph      if ( ytstype .EQ. 'T' ) then
cph         velsign = 1.D0
cph      else
cph         velsign = -1.D0
cph      endif

      if ( uVelLoc .GE. 0. ) then
         dFldDt(1) = 
     &        ( extForLoc(1)
     &        + gammaLoc*( fldStar(1) - fldNow(1) )*vol(1)
     &        + uVelLoc* ( fldNow(3)  - fldNow(1) ) ) / vol(1)
         dFldDt(2) = 
     &        ( extForLoc(2)
     &        + gammaLoc*( fldStar(2) - fldNow(2) )*vol(2)
     &        + uVelLoc* ( fldNow(1)  - fldNow(2) ) ) / vol(2)
         dFldDt(3) = 
     &        uVelLoc*( fldNow(2) - fldNow(3) ) / vol(3)
      else
         dFldDt(1) = 
     &        ( extForLoc(1)
     &        + gammaLoc*( fldStar(1) - fldNow(1) )*vol(1)
     &        - uVelLoc* ( fldNow(2) -  fldNow(1) ) ) / vol(1)
         dFldDt(2) = 
     &        ( extForLoc(2)
     &        + gammaLoc*( fldStar(2) - fldNow(2) )*vol(2)
     &        - uVelLoc* ( fldNow(3) -  fldNow(2) ) ) / vol(2)
         dFldDt(3) = 
     &        -uVelLoc*( fldNow(1) - fldNow(3) ) / vol(3)
      end if

      call box_update ( fldNew, fldOld, dFldDt )

      end

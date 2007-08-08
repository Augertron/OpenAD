      module all_globals_mod
        integer ndim
        parameter ( ndim = 3 )
        integer kdim
        parameter ( kdim = 3 )

        integer n_max

        double precision blength(ndim)
        double precision bheight(ndim)
        double precision bwidth
        double precision area(ndim)
        double precision vol(ndim)

        double precision y(2*ndim) 
        double precision r(2*ndim) 
        double precision r1(2*ndim) 
        double precision r_t(2*ndim) 
        double precision r_s(2*ndim) 
        double precision proj_t(2*ndim) 
        double precision proj_s(2*ndim) 
        double precision x(2*ndim,2*ndim)

        double precision alpha
        double precision beta

        double precision u0
        double precision delta

        double precision robert_filter_coeff

        double precision delta_t

        double precision hundred
        double precision thousand
        double precision day
        double precision year
        double precision Sv
        double precision days_per_50m_mixed_layer
        double precision gamma_T
        double precision gamma_S
        double precision epsilon_ic
        double precision noise_correlation_time
        double precision integration_time
        double precision epsilon_regularize
        double precision fdeps

        logical verbmode

        double precision thc_tot, thc_t, thc_s

        double precision 
     &                 told(ndim)
     &               , tnow(ndim)
     &               , tnew(ndim)
     &               , sold(ndim)
     &               , snow(ndim)
     &               , snew(ndim)

        double precision uvel

        double precision rho(ndim)

        double precision nullForce(ndim-1)
        double precision fw(ndim-1)
        double precision tStar(ndim-1)
        double precision sStar(ndim-1)

        double precision ubar, t(ndim), s(ndim)

c-- dependent and independent variables

        double precision metric1, metric2

        double precision metric

        double precision xx(2*ndim)

        double precision tsvec(2*ndim)

      end module


!-----------------------------------------------------------------------
!     Function:      out_beams3d_nag
!     Authors:       S. Lazerson (lazerson@pppl.gov), M. McMillan (matthew.mcmillan@my.wheaton.edu)
!     Date:          06/20/2012
!     Description:   Save output from field line following while running
!                    and updates progress bar.
!-----------------------------------------------------------------------
SUBROUTINE out_beams3d_nag(t, q)
    !-----------------------------------------------------------------------
    !     Libraries
    !-----------------------------------------------------------------------
    USE stel_kinds, ONLY: rprec
    USE beams3d_runtime, ONLY: dt, lverb, pi2, lneut, t_end, lvessel, &
                               lhitonly, npoinc, lcollision, ldepo, &
                               weight
    USE beams3d_lines, ONLY: R_lines, Z_lines, PHI_lines, myline, moment, &
                             nsteps, nparticles, moment_lines, myend, &
                             vll_lines, neut_lines, mytdex, next_t,&
                             lost_lines, dt_out, xlast, ylast, zlast,&
                             ltherm, S_lines, U_lines, B_lines, &
                             dist_prof, ns_prof, j_prof, ndot_prof, &
                             partvmax, mymass, mycharge, mybeam, end_state
    USE beams3d_grid
    USE beams3d_physics_mod, ONLY: beams3d_physics
    USE wall_mod, ONLY: collide, get_wall_ik, get_wall_area
    USE EZspline_obj
    USE EZspline
    USE mpi_params
    !-----------------------------------------------------------------------
    !     Input Parameters
    !          t          Location along fieldline in t
    !          q            (q(1),q(2),q(3),q(4)) = (R,phi,Z,vll)
    !-----------------------------------------------------------------------
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(inout) :: t
    DOUBLE PRECISION, INTENT(inout) :: q(4)
    !-----------------------------------------------------------------------
    !     Local Variables
    !     jint      Index along phi
    !-----------------------------------------------------------------------
    INTEGER             :: ier
    DOUBLE PRECISION         :: x0,y0,z0,x1,y1,z1,xw,yw,zw
    DOUBLE PRECISION    :: q2(4),qdot(4)
    LOGICAL             :: lhit
    ! For splines
    INTEGER :: i,j,k,l,m
    REAL*8 :: xparam, yparam, zparam, hx, hy, hz, hxi, hyi, hzi
    REAL*8 :: fval(1)
    INTEGER, parameter :: ict(8)=(/1,0,0,0,0,0,0,0/)
    REAL*8, PARAMETER :: one = 1
    !-----------------------------------------------------------------------
    !     Begin Function
    !-----------------------------------------------------------------------
    R_lines(mytdex, myline)      = q(1)
    PHI_lines(mytdex, myline)    = q(2)
    Z_lines(mytdex, myline)      = q(3)
    vll_lines(mytdex, myline)    = q(4)
    moment_lines(mytdex, myline) = moment
    neut_lines(mytdex,myline)     = lneut
    x0 = MOD(q(2), phimax)
    IF (x0 < 0) x0 = x0 + phimax
    !CALL EZspline_isInDomain(S_spl,q(1),x0,q(3),ier)
    y0 = 0  ! If we're out of domain then don't worry about collisions
    !IF (ier==0) THEN
    IF ((q(1) >= rmin-eps1) .and. (q(1) <= rmax+eps1) .and. &
        (x0 >= phimin-eps2) .and. (x0 <= phimax+eps2) .and. &
        (q(3) >= zmin-eps3) .and. (q(3) <= zmax+eps3)) THEN
       i = MIN(MAX(COUNT(raxis < q(1)),1),nr-1)
       j = MIN(MAX(COUNT(phiaxis < x0),1),nphi-1)
       k = MIN(MAX(COUNT(zaxis < q(3)),1),nz-1)
       hx     = raxis(i+1) - raxis(i)
       hy     = phiaxis(j+1) - phiaxis(j)
       hz     = zaxis(k+1) - zaxis(k)
       hxi    = one / hx
       hyi    = one / hy
       hzi    = one / hz
       xparam = (q(1) - raxis(i)) * hxi
       yparam = (x0 - phiaxis(j)) * hyi
       zparam = (q(3) - zaxis(k)) * hzi
       CALL R8HERM3FCN(ict,1,1,fval,i,j,k,xparam,yparam,zparam,&
                       hx,hxi,hy,hyi,hz,hzi,&
                       S4D(1,1,1,1),nr,nphi,nz)
       y0 = fval(1)
       S_lines(mytdex, myline) = y0 
       CALL R8HERM3FCN(ict,1,1,fval,i,j,k,xparam,yparam,zparam,&
                       hx,hxi,hy,hyi,hz,hzi,&
                       U4D(1,1,1,1),nr,nphi,nz)
       U_lines(mytdex, myline) = fval(1)
       CALL R8HERM3FCN(ict,1,1,fval,i,j,k,xparam,yparam,zparam,&
                       hx,hxi,hy,hyi,hz,hzi,&
                       MODB4D(1,1,1,1),nr,nphi,nz)
       B_lines(mytdex, myline) = fval(1)
       xw = SQRT(2*moment*fval(1)/mymass)
       l = MAX(MIN(1+ns_prof/2+FLOOR(0.5*ns_prof*q(4)/partvmax),ns_prof),1)
       m = MAX(MIN(CEILING(ns_prof*xw/partvmax),ns_prof),1)
       dist_prof(mybeam,l,m) = dist_prof(mybeam,l,m) + weight(myline)
       !CALL EZspline_interp(S_spl,q(1),x0,q(3),y0,ier)
       !CALL EZspline_interp(U_spl,q(1),x0,q(3),z0,ier)
       !CALL EZspline_interp(MODB_spl,q(1),x0,q(3),x1,ier)
       !IF (myworkid == 0) PRINT *,'--',y0,z0,x1
       l = MAX(MIN(CEILING(SQRT(y0)*ns_prof),ns_prof),1)
       j_prof(mybeam,l)      =      j_prof(mybeam,l) + mycharge*q(4)*weight(myline)
    END IF
    IF (lcollision) CALL beams3d_physics(t,q)
    IF (ltherm) THEN
      ndot_prof(mybeam,l)   =   ndot_prof(mybeam,l) + weight(myline)
      end_state(myline) = 1
      t = t_end(myline)
    END IF
    IF (lvessel .and. mytdex > 0 .and. y0 > 0.5) THEN
       lhit = .false.
       x0    = xlast
       y0    = ylast
       z0    = zlast
       x1    = q(1)*cos(q(2))
       y1    = q(1)*sin(q(2))
       z1    = q(3)
       CALL collide(x0,y0,z0,x1,y1,z1,xw,yw,zw,lhit)
       IF (lhit) THEN
          q2(1) = SQRT(xw*xw+yw*yw)
          q2(2) = atan2(yw,xw)
          q2(3) = zw
          R_lines(mytdex,myline)       = q2(1)
          PHI_lines(mytdex,myline)     = q2(2)
          Z_lines(mytdex,myline)       = zw
          lost_lines(myline) = .TRUE.
          t = t_end(myline)+dt
          end_state(myline) = 2
          l = get_wall_ik()
          CALL fpart_nag(t,q2,qdot)
          qdot(4)=0
          wall_load(mybeam,l) = wall_load(mybeam,l) + weight(myline)*0.5*mymass*SUM(qdot*qdot)/get_wall_area(l)
          IF (lneut) end_state(myline) = 3
          IF (lhitonly) THEN
             R_lines(0,myline)      = SQRT(xlast*xlast+ylast*ylast)
             PHI_lines(0,myline)    = ATAN2(ylast,xlast)
             Z_lines(0,myline)      = zlast
             vll_lines(0,myline)    = q(4)
             moment_lines(0,myline) = moment
             neut_lines(0,myline)   = lneut
             R_lines(2,myline)      = q(1)
             PHI_lines(2,myline)    = q(2)
             Z_lines(2,myline)      = q(3)
             vll_lines(2,myline)    = q(4)
             moment_lines(2,myline) = moment
             neut_lines(2,myline)   = lneut
          END IF
       ELSE
          xlast = x1
          ylast = y1
          zlast = z1
       END IF
    ELSE
       xlast = q(1)*cos(q(2))
       ylast = q(1)*sin(q(2))
       zlast = q(3)
    END IF
    IF (lhitonly) mytdex = 0
    IF (ABS((t+dt)) .ge. ABS(mytdex*dt_out)) mytdex = mytdex + 1
    t = t + dt

    RETURN
    !-----------------------------------------------------------------------
    !     End Function
    !-----------------------------------------------------------------------
END SUBROUTINE out_beams3d_nag


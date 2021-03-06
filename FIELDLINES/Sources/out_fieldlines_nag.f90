!-----------------------------------------------------------------------
!     Function:      out_fieldlines
!     Authors:       S. Lazerson (lazerson@pppl.gov)
!     Date:          02/21/2012
!     Description:   Save output from field line following while running
!                    and updates progress bar.
!-----------------------------------------------------------------------
      SUBROUTINE out_fieldlines_nag(phi,q)
!-----------------------------------------------------------------------
!     Libraries
!-----------------------------------------------------------------------
      USE stel_kinds, ONLY: rprec
      USE fieldlines_runtime, ONLY: dphi, lverb, pi2, lvessel, phi_end,&
                                    lhitonly, lmu, mu, lwall_trans, lmodb
      USE fieldlines_lines, ONLY: R_lines, Z_lines, PHI_lines, myline,&
                                  nsteps,nlines,myldex, B_lines, xlast,&
                                  ylast, zlast, myend, L_lines
      USE fieldlines_grid
      USE wall_mod, ONLY: collide
      USE random, ONLY: random_normal
      USE mpi_params                                                    ! MPI
      USE EZspline_obj
      USE EZspline
!-----------------------------------------------------------------------
!     Input Parameters
!          phi          Location along fieldline in phi
!          q            (q(1),q(2)) = (R,Z)
!-----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION, INTENT(inout) :: phi
      DOUBLE PRECISION, INTENT(inout) :: q(2)
!-----------------------------------------------------------------------
!     Local Variables
!     jint      Index along phi
!-----------------------------------------------------------------------
      INTEGER             :: ier
      REAL(rprec)         :: x0,y0,z0,x1,y1,z1,xw,yw,zw,phi_save
      LOGICAL             :: lhit
      ! For splines
      INTEGER :: i,j,k
      REAL*8 :: xparam, yparam, zparam, hx, hy, hz, hxi, hyi, hzi
      REAL*8 :: fval(1)
      INTEGER, parameter :: ict(8)=(/1,0,0,0,0,0,0,0/)
      REAL*8, PARAMETER :: one = 1

!-----------------------------------------------------------------------
!     Begin Function
!-----------------------------------------------------------------------
      ! Save position
      phi_save = phi
      R_lines(myline,myldex)  = q(1)
      Z_lines(myline,myldex)  = q(2)
      PHI_lines(myline,myldex) = phi

      ! Handle Collisions
      lhit = .false.
      IF (lvessel .and. myldex>0) THEN
         x0 = xlast ! NEW
         y0 = ylast ! NEW
         z0 = zlast ! NEW
         x1 = q(1)*cos(phi) !NEW
         y1 = q(1)*sin(phi) !NEW
         z1 = q(2)    !NEW
         CALL collide(x0,y0,z0,x1,y1,z1,xw,yw,zw,lhit)
         IF (lhit) THEN
            R_lines(myline,myldex) = SQRT(xw*xw+yw*yw)
            PHI_lines(myline,myldex) = ATAN2(yw,xw)
            Z_lines(myline,myldex) = zw
            L_lines(myline) = L_lines(myline) + &
                              sqrt((xw-x0)*(xw-x0) + &
                                   (yw-y0)*(yw-y0) + &
                                   (zw-z0)*(zw-z0))
            IF (lhitonly) THEN
               R_lines(myline,0) = SQRT(xlast*xlast+ylast*ylast)
               PHI_lines(myline,0) = ATAN2(ylast,xlast)
               Z_lines(myline,0) = zlast
               R_lines(myline,2) = q(1)
               PHI_lines(myline,2) = phi
               Z_lines(myline,2) = q(2)
            END IF
            IF (.not. lwall_trans) phi = phi_end(myline)+dphi ! End the line
         ELSE 
            L_lines(myline) = L_lines(myline) + &
                              sqrt((x1-x0)*(x1-x0) + &
                                   (y1-y0)*(y1-y0) + &
                                   (z1-z0)*(z1-z0))
            xlast = x1
            ylast = y1
            zlast = z1
         END IF
      ELSE
         xlast = q(1)*cos(phi)
         ylast = q(1)*sin(phi)
         zlast = q(2)
      END IF

      ! Handle Diffusion
      IF (lmu .and. .not.lhit .and. myldex>0) THEN
         x0 = q(1)
         y0 = MOD(phi_save,delta_phi)
         IF (y0 < 0) y0 = delta_phi + y0
         z0 = q(2)
         zw = 0; ier=0
         IF ((x0 >= rmin-eps1) .and. (x0 <= rmax+eps1) .and. &
            (y0 >= phimin-eps2) .and. (y0 <= phimax+eps2) .and. &
            (z0 >= zmin-eps3) .and. (z0 <= zmax+eps3)) THEN
            !CALL EZspline_interp(MU_spl,x0,y0,z0,zw,ier)
            i = MIN(MAX(COUNT(raxis < x0),1),nr-1)
            j = MIN(MAX(COUNT(phiaxis < y0),1),nphi-1)
            k = MIN(MAX(COUNT(zaxis < z0),1),nz-1)
            hx     = raxis(i+1) - raxis(i)
            hy     = phiaxis(j+1) - phiaxis(j)
            hz     = zaxis(k+1) - zaxis(k)
            hxi    = one / hx
            hyi    = one / hy
            hzi    = one / hz
            xparam = (x0 - raxis(i)) * hxi
            yparam = (y0 - phiaxis(j)) * hyi
            zparam = (z0 - zaxis(k)) * hzi
            CALL R8HERM3FCN(ict,1,1,fval,i,j,k,xparam,yparam,zparam,&
                       hx,hxi,hy,hyi,hz,hzi,&
                       MU4D(1,1,1,1),nr,nphi,nz)
            q(1) = q(1) + random_normal() * sqrt(fval(1)*ABS(dphi)) !EDIT
            q(2) = q(2) + random_normal() * sqrt(fval(1)*ABS(dphi)) !EDIT
         END IF
      END IF

      ! Save MODB
      IF (lmodb) THEN
         x0 = R_lines(myline,myldex)
         y0 = MOD(phi_save,delta_phi)
         IF (y0 < 0) y0 = delta_phi + y0
         z0 = Z_lines(myline,myldex)
         zw = 0
         IF ((x0 >= rmin-eps1) .and. (x0 <= rmax+eps1) .and. &
            (y0 >= phimin-eps2) .and. (y0 <= phimax+eps2) .and. &
            (z0 >= zmin-eps3) .and. (z0 <= zmax+eps3)) THEN
            i = MIN(MAX(COUNT(raxis < x0),1),nr-1)
            j = MIN(MAX(COUNT(phiaxis < y0),1),nphi-1)
            k = MIN(MAX(COUNT(zaxis < z0),1),nz-1)
            hx     = raxis(i+1) - raxis(i)
            hy     = phiaxis(j+1) - phiaxis(j)
            hz     = zaxis(k+1) - zaxis(k)
            hxi    = one / hx
            hyi    = one / hy
            hzi    = one / hz
            xparam = (x0 - raxis(i)) * hxi
            yparam = (y0 - phiaxis(j)) * hyi
            zparam = (z0 - zaxis(k)) * hzi
            CALL R8HERM3FCN(ict,1,1,fval,i,j,k,xparam,yparam,zparam,&
                       hx,hxi,hy,hyi,hz,hzi,&
                       MODB4D(1,1,1,1),nr,nphi,nz)
            B_lines(myline,myldex) = fval(1)
         END IF
      END IF

      ! Update position
      phi    = phi + dphi
      myldex = myldex + 1
      IF (lwall_trans .and. .not. lhit) myldex = myldex - 1
      IF (lhitonly) myldex = 1
      RETURN
!-----------------------------------------------------------------------
!     End Function
!-----------------------------------------------------------------------
      END SUBROUTINE out_fieldlines_nag

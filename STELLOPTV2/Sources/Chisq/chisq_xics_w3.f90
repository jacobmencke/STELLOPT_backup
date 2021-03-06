!-----------------------------------------------------------------------
!     Subroutine:    chisq_xics_w3
!     Authors:       S. Lazerson (lazerson@pppl.gov)
!     Date:          01/22/2019
!     Description:   Calculate difference measured and equilibrium
!                    XICS W3 Factor Signals.
!                    The W3 Factor is actually the ratio between
!                    the main W line and n=3 satellite line intensities.
!                    The ratio of the W3 emissivity and the n=3
!                    emissivities is related to the electron
!                    temperature through a formula which is hardcoded
!                    into fcn_xics_w3 (basically linear in log/log).
!                    So we line integrate over the n=3 emissivity,
!                    which is just the W emissivity times this formula.
!-----------------------------------------------------------------------
      SUBROUTINE chisq_xics_w3(target,sigma,niter,iflag)
!-----------------------------------------------------------------------
!     Libraries
!-----------------------------------------------------------------------
      USE stellopt_runtime
      USE stellopt_targets
      USE equil_utils
      
!-----------------------------------------------------------------------
!     Input/Output Variables
!
!-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL(rprec), INTENT(in)    ::  target(nprof)
      REAL(rprec), INTENT(in)    ::  sigma(nprof)
      INTEGER,     INTENT(in)    ::  niter
      INTEGER,     INTENT(inout) ::  iflag
      
!-----------------------------------------------------------------------
!     Local Variables
!        lreset_s    Gets set to true if using R,PHI,Z Specification
!        ik          Dummy index
!        ti_val      Holds profile evaulation
!-----------------------------------------------------------------------
      INTEGER ::  ik
      REAL(rprec) :: xics_val, xics_length
      REAL(rprec) :: x0(3), x1(3)
!----------------------------------------------------------------------
!     BEGIN SUBROUTINE
!----------------------------------------------------------------------
      IF (iflag < 0 ) RETURN
      ik = COUNT(sigma < bigno)
      IF (iflag == 1) WRITE(iunit_out,'(A,2(2X,I3.3))') 'XICS_W3',ik,9
      IF (iflag == 1) WRITE(iunit_out,'(A)') 'TARGET  SIGMA  EQUIL  R0  PHI0  Z0  R1  PHI1  Z1'
      IF (niter >= 0) THEN
         DO ik = 1, nprof
            IF (sigma(ik) >= bigno) CYCLE
            x0(1)=r0_xics(ik); x1(1)=r1_xics(ik)
            x0(2)=phi0_xics(ik); x1(2)=phi1_xics(ik)
            x0(3)=z0_xics(ik); x1(3)=z1_xics(ik)
            xics_val = 0.0
            CALL line_int(fcn_xics_w3,x0,x1,xics_val,LENGTH=xics_length)
            mtargets = mtargets + 1
            targets(mtargets) = target(ik)
            sigmas(mtargets)  = sigma(ik)
            vals(mtargets)    = xics_val
            IF (iflag == 1) WRITE(iunit_out,'(9ES22.12E3)') target(ik),sigma(ik),xics_val,x0(1),x0(2),x0(3),x1(1),x1(2),x1(3)
         END DO
      ELSE
         DO ik = 1, nprof
            IF (sigma(ik) < bigno) THEN
               mtargets = mtargets + 1
               IF (niter == -2) target_dex(mtargets) = jtarget_xics_w3
            END IF
         END DO
      END IF
      RETURN
!----------------------------------------------------------------------
!     END SUBROUTINE
!----------------------------------------------------------------------
      END SUBROUTINE chisq_xics_w3

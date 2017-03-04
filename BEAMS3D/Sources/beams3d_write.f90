!-----------------------------------------------------------------------
!     Module:        beams3d_write
!     Authors:       S. Lazerson (lazerson@pppl.gov) M. McMillan (matthew.mcmillan@my.wheaton.edu)
!     Date:          06/21/2012
!     Description:   This subroutine outputs the particle trajectory data to an
!                    HDF5 file or binary file.
!-----------------------------------------------------------------------
      SUBROUTINE beams3d_write(write_type)
!-----------------------------------------------------------------------
!     Libraries
!-----------------------------------------------------------------------
      USE stel_kinds, ONLY: rprec
!DEC$ IF DEFINED (LHDF5)
      USE ez_hdf5
!DEC$ ENDIF  
      USE beams3d_lines
      USE beams3d_grid, ONLY: nr, nphi, nz, B_R, B_PHI, B_Z, raxis, &
                                 zaxis, phiaxis, S_ARR, U_ARR, POT_ARR
      USE beams3d_runtime, ONLY: id_string, npoinc, nbeams, beam, t_end, lverb, lflux, &
                                    lvmec, lpies, lspec, lcoil, lmgrid, lmu, lbeam, &
                                    lvessel, lvac, lbeam_simple, handle_err, nparticles_start, &
                                    HDF5_OPEN_ERR,HDF5_WRITE_ERR,&
                                    HDF5_CLOSE_ERR, BEAMS3D_VERSION, weight, e_beams, p_beams,&
                                    charge, Zatom, mass, ldepo, v_neut
      USE safe_open_mod, ONLY: safe_open
      USE wall_mod, ONLY: nface,nvertex,face,vertex,ihit_array
      USE mpi_params
!-----------------------------------------------------------------------
!     Input Variables
!          write_type  Type of write to preform
!-----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER(*), INTENT(IN):: write_type
!-----------------------------------------------------------------------
!     Local Variables
!          ier          Error Flag
!          iunit        File ID
!-----------------------------------------------------------------------
      INTEGER :: ier, iunit
!-----------------------------------------------------------------------
!     Begin Subroutine
!-----------------------------------------------------------------------
      IF (myworkid == master) THEN
         SELECT CASE (TRIM(write_type))
            CASE('GRID_INIT')
!DEC$ IF DEFINED (LHDF5)
               CALL open_hdf5('beams3d_'//TRIM(id_string)//'.h5',fid,ier,LCREATE=.true.)
               IF (ier /= 0) CALL handle_err(HDF5_OPEN_ERR,'beams3d_'//TRIM(id_string)//'.h5',ier)
               CALL write_scalar_hdf5(fid,'VERSION',ier,DBLVAR=BEAMS3D_VERSION,ATT='Version Number',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'VERSION',ier)
               CALL write_scalar_hdf5(fid,'lvmec',ier,BOOVAR=lvmec,ATT='VMEC input',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'lvmec',ier)
               CALL write_scalar_hdf5(fid,'lpies',ier,BOOVAR=lpies,ATT='PIES input',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'lpies',ier)
               CALL write_scalar_hdf5(fid,'lspec',ier,BOOVAR=lspec,ATT='SPEC input',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'lspec',ier)
               CALL write_scalar_hdf5(fid,'lcoil',ier,BOOVAR=lcoil,ATT='Coil input',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'lcoil',ier)
               CALL write_scalar_hdf5(fid,'lmgrid',ier,BOOVAR=lmgrid,ATT='MGRID input',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'lmgrid',ier)
               CALL write_scalar_hdf5(fid,'lvessel',ier,BOOVAR=lvessel,ATT='Vessel input',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'lvessel',ier)
               CALL write_scalar_hdf5(fid,'lvac',ier,BOOVAR=lvac,ATT='Vacuum calc',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'lvac',ier)
               CALL write_scalar_hdf5(fid,'lbeam',ier,BOOVAR=lbeam,ATT='Neutral Beam Calc',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'lbeam',ier)
               CALL write_scalar_hdf5(fid,'lbeam_simple',ier,BOOVAR=lbeam_simple,ATT='Simple Beam Energy',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'lbeam_simple',ier)
               CALL write_scalar_hdf5(fid,'lflux',ier,BOOVAR=lflux,ATT='Flux Calculation',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'lflux',ier)
               CALL write_scalar_hdf5(fid,'ldepo',ier,BOOVAR=ldepo,ATT='Only Deposition Flag',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'ldepo',ier)
               CALL write_scalar_hdf5(fid,'nr',ier,INTVAR=nr,ATT='Number of Radial Gridpoints',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'nr',ier)
               CALL write_scalar_hdf5(fid,'nphi',ier,INTVAR=nphi,ATT='Number of Toroidal Gridpoints',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'nphi',ier)
               CALL write_scalar_hdf5(fid,'nz',ier,INTVAR=nz,ATT='Number of Vertical Gridpoints',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'nz',ier)
               CALL write_var_hdf5(fid,'raxis',nr,ier,DBLVAR=raxis,ATT='Radial Axis [m]',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'raxis',ier)
               CALL write_var_hdf5(fid,'phiaxis',nphi,ier,DBLVAR=phiaxis,ATT='Toroidal Axis [rad]',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'phiaxis',ier)
               CALL write_var_hdf5(fid,'zaxis',nz,ier,DBLVAR=zaxis,ATT='Vertical Axis [rad]',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'zaxis',ier)
               CALL write_var_hdf5(fid,'B_R',nr,nphi,nz,ier,DBLVAR=B_R,ATT='Radial Trajectory Eq. (BR)',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'B_R',ier)
               CALL write_var_hdf5(fid,'B_Z',nr,nphi,nz,ier,DBLVAR=B_Z,ATT='Vertical Trajectory Eq. (BZ)',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'B_Z',ier)
               CALL write_var_hdf5(fid,'B_PHI',nr,nphi,nz,ier,DBLVAR=B_PHI,ATT='Toroidal Trajectory (BPHI)',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'B_PHI',ier)
               CALL write_var_hdf5(fid,'S_ARR',nr,nphi,nz,ier,DBLVAR=S_ARR,ATT='Normalized Toroidal Flux',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'S_ARR',ier)
               CALL write_var_hdf5(fid,'U_ARR',nr,nphi,nz,ier,DBLVAR=U_ARR,ATT='Equilibrium Poloidal Angle [rad]',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'U_ARR',ier)
               CALL write_var_hdf5(fid,'POT_ARR',nr,nphi,nz,ier,DBLVAR=POT_ARR,ATT='Electrostatic Potential [V]',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'POT_ARR',ier)
               IF (lbeam) THEN
!                  CALL open_hdf5('beams3d_'//TRIM(id_string)//'.h5',fid,ier,LCREATE=.false.)
!                  IF (ier /= 0) CALL handle_err(HDF5_OPEN_ERR,'beams3d_'//TRIM(id_string)//'.h5',ier)
                  CALL write_var_hdf5(fid,'Weight',nparticles_start, nbeams,ier,DBLVAR=weight,ATT='Weight',&
                                      ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'Weight',ier)
                  CALL write_var_hdf5(fid,'Beam',nparticles,ier,INTVAR=beam,ATT='Beam Number',&
                                      ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'Beam',ier)
                  CALL write_var_hdf5(fid,'V_NEUT',3,nparticles,ier,DBLVAR=V_NEUT,ATT='Neutral Velocity [m/s]',&
                                      ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'V_NEUT',ier)
                  CALL write_var_hdf5(fid,'Energy',nbeams,ier,DBLVAR=e_beams,ATT='Beam Energy [J]',ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'E_BEAMS',ier)
               END IF
               IF (ALLOCATED(vertex)) THEN
                  CALL write_scalar_hdf5(fid,'nvertex',ier,INTVAR=nvertex,ATT='Number of Wall Vertices',ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'nvertex',ier)
                  CALL write_var_hdf5(fid,'wall_vertex',nvertex,3,ier,DBLVAR=vertex,ATT='Wall Verticies (x,y,z) [m]',ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'wall_vertex',ier)
                  DEALLOCATE(vertex)
               END IF
               IF (ALLOCATED(face)) THEN
                  CALL write_scalar_hdf5(fid,'nface',ier,INTVAR=nface,ATT='Number of Wall Faces',ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'nface',ier)
                  CALL write_var_hdf5(fid,'wall_faces',nface,3,ier,INTVAR=face,ATT='Wall Faces',ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'wall_faces',ier)
                  DEALLOCATE(face)
               END IF
            CASE('TRAJECTORY')
               CALL open_hdf5('beams3d_'//TRIM(id_string)//'.h5',fid,ier,LCREATE=.false.)
               IF (ier /= 0) CALL handle_err(HDF5_OPEN_ERR,'beams3d_'//TRIM(id_string)//'.h5',ier)
               CALL write_scalar_hdf5(fid,'nparticles',ier,INTVAR=nparticles,ATT='Number of Trajectories',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'nparticles',ier)
               CALL write_scalar_hdf5(fid,'nbeams',ier,INTVAR=nbeams,ATT='Number of Beams',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'nbeams',ier)
               CALL write_scalar_hdf5(fid,'nsteps',ier,INTVAR=nsteps+1,ATT='Number of Steps Along Trajectory',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'nsteps',ier)
               CALL write_scalar_hdf5(fid,'npoinc',ier,INTVAR=npoinc,ATT='Number of steps per trajectory period',&
                                      ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'npoinc',ier)
               CALL write_var_hdf5(fid,'t_end',nparticles,ier,DBLVAR=t_end,ATT='Time at End of Trajectory [s]',&
                                   ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'t_end',ier)
               CALL write_var_hdf5(fid,'mass',nparticles,ier,DBLVAR=mass,ATT='Particle Mass [kg]',&
                                   ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'mass',ier)
               CALL write_var_hdf5(fid,'charge',nparticles,ier,DBLVAR=charge,ATT='Particle Charge [C]',&
                                   ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'charge',ier)
               CALL write_var_hdf5(fid,'Zatom',nparticles,ier,DBLVAR=Zatom,ATT='Particle Charge Number',&
                                   ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'Zatom',ier)
               CALL write_var_hdf5(fid,'R_lines',npoinc+1,nparticles,ier,DBLVAR=R_lines,ATT='Cylindrical R of Trajectory [m]',&
                                   ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'R_lines',ier)
               CALL write_var_hdf5(fid,'Z_lines',npoinc+1,nparticles,ier,DBLVAR=Z_lines,ATT='Cylindrical Z of Trajectory [m]',&
                                   ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'Z_lines',ier)
               CALL write_var_hdf5(fid,'PHI_lines',npoinc+1,nparticles,ier,DBLVAR=PHI_lines,ATT='Cylindrical Phi of Trajectory [rad]',&
                                   ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'PHI_lines',ier)
               CALL write_var_hdf5(fid,'vll_lines',npoinc+1,nparticles,ier,DBLVAR=vll_lines,ATT='Parallel Particle Velocity [m/s]',&
                                   ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'vll_lines',ier)
               CALL write_var_hdf5(fid,'neut_lines',npoinc+1,nparticles,ier,BOOVAR=neut_lines,ATT='Neutral Indicator [1=Neut.]',&
                                   ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'neut_lines',ier)
               CALL write_var_hdf5(fid,'moment_lines',npoinc+1,nparticles,ier,DBLVAR=moment_lines,&
                                   ATT='Magnetic Moment [kg m^2 /s^2 T ]',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'moment_lines',ier)
               CALL write_var_hdf5(fid,'S_lines',npoinc+1,nparticles,ier,DBLVAR=S_lines,ATT='Toroidal Flux Coordinate',&
                                   ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'S_lines',ier)
               CALL write_var_hdf5(fid,'U_lines',npoinc+1,nparticles,ier,DBLVAR=U_lines,ATT='U Flux Coordinate',&
                                   ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'U_lines',ier)
               CALL write_var_hdf5(fid,'B_lines',npoinc+1,nparticles,ier,DBLVAR=B_lines,ATT='|B| along Fieldline',&
                                   ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'B_lines',ier)
               IF (ALLOCATED(ihit_array)) THEN
                  CALL write_var_hdf5(fid,'wall_strikes',nface,ier,INTVAR=ihit_array,&
                                   ATT='Wall Strikes',ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'moment_lines',ier)
                  DEALLOCATE(ihit_array)
               END IF
            CASE('DIAG')
               CALL open_hdf5('beams3d_'//TRIM(id_string)//'.h5',fid,ier,LCREATE=.false.)
               IF (ier /= 0) CALL handle_err(HDF5_OPEN_ERR,'beams3d_'//TRIM(id_string)//'.h5',ier)
               CALL write_var_hdf5(fid,'Shinethrough',nbeams,ier,DBLVAR=shine_through,&
                                   ATT='Total Beam Shine Through [%]',ATT_NAME='description')
               IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'Shinethrough',ier)
               IF (lflux .and. lbeam .and. .not. ldepo) THEN
                  CALL write_scalar_hdf5(fid,'ns_prof',ier,INTVAR=ns_prof,&
                                      ATT='Flux Grid Points',ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'ns_prof',ier)
                  CALL write_var_hdf5(fid,'ndot_prof',nbeams,ns_prof,ier,DBLVAR=ndot_prof,&
                                      ATT='Fast Ion Source [m^-3/s]',ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'ndot_prof',ier)
                  CALL write_var_hdf5(fid,'epower_prof',nbeams,ns_prof,ier,DBLVAR=epower_prof,&
                                      ATT='Electron Power Deposition [W*m^-3]',ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'epower_prof',ier)
                  CALL write_var_hdf5(fid,'ipower_prof',nbeams,ns_prof,ier,DBLVAR=ipower_prof,&
                                      ATT='Ion Power Deposition [W*m^-3]',ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'ipower_prof',ier)
                  CALL write_var_hdf5(fid,'J_prof',nbeams,ns_prof,ier,DBLVAR=j_prof,&
                                      ATT='Total Beam Current Density [P/(e*E)]',ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'J_prof',ier)
                  CALL write_var_hdf5(fid,'PE_lines',npoinc+1,nparticles,ier,DBLVAR=PE_lines,ATT='Deposited Power to Electrons [W]',&
                                      ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'PE_lines',ier)
                  CALL write_var_hdf5(fid,'PI_lines',npoinc+1,nparticles,ier,DBLVAR=PI_lines,ATT='Deposited Power to Ions [W]',&
                                      ATT_NAME='description')
                  IF (ier /= 0) CALL handle_err(HDF5_WRITE_ERR,'PI_lines',ier)
               END IF
         END SELECT
         CALL close_hdf5(fid,ier)
         IF (ier /= 0) CALL handle_err(HDF5_CLOSE_ERR,'beams3d_'//TRIM(id_string)//'.h5',ier)
      END IF
!DEC$ ELSE
      WRITE(6,'(A)')  '   FILE: '//'beams3d_'//TRIM(id_string)//'.bin'
      CALL safe_open(iunit,ier,'beams3d_'//TRIM(id_string)//'.bin','replace','unformatted')
      WRITE(iunit,*) BEAMS3D_VERSION
      WRITE(iunit,*) lvmec,lpies,lspec,lcoil,lmgrid,lmu,lvessel,lvac,lbeam_simple,lflux
      WRITE(iunit,*) nparticles,nsteps,npoinc,nbeams
      WRITE(iunit,*) weight
      WRITE(iunit,*) beam
      WRITE(iunit,*) t_end
      WRITE(iunit,*) R_lines
      WRITE(iunit,*) Z_lines
      WRITE(iunit,*) PHI_lines
      WRITE(iunit,*) vll_lines
      IF (lflux) THEN
         WRITE(iunit,*) S_lines
         WRITE(iunit,*) U_lines
         WRITE(iunit,*) V_lines
      END IF
      IF (lbeam) THEN
         WRITE(iunit,*) weight
         WRITE(iunit,*) beam
         WRITE(iunit,*) e_beams
         !WRITE(iunit,*) p_beams
         IF (.not.ldepo) THEN
            WRITE(iunit,*) shine_through
            WRITE(iunit,*) ndot_prof
            WRITE(iunit,*) epower_prof
            WRITE(iunit,*) ipower_prof
            WRITE(iunit,*) j_prof
         END IF
      END IF
      WRITE(iunit,*) nr,nphi,nz
      WRITE(iunit,*) raxis
      WRITE(iunit,*) phiaxis
      WRITE(iunit,*) zaxis
      WRITE(iunit,*) B_R
      WRITE(iunit,*) B_Z
      WRITE(iunit,*) B_PHI
      CLOSE(iunit)
!DEC$ ENDIF  

      RETURN

!-----------------------------------------------------------------------
!     End Subroutine
!-----------------------------------------------------------------------    
      END SUBROUTINE beams3d_write
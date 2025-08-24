program main_heat_sim
  use cylindrical_solver_module
  use data_storing_module
  use materials_module
  implicit none

  !----------------------------------------------------------
  ! Declarations
  !----------------------------------------------------------
  integer, parameter :: n_materials = 31
  integer, parameter :: n_new_materials = 18
  integer, parameter :: new_material = n_materials


  integer :: n_steps, step_number
  integer :: i_material, i_w, i_P, i_sigma, i_lambda, i_type, unit_no,i_type2
  integer :: nw, nP, nsigma, nlambda
  integer :: istat

  character(len=18) :: material_name
  character(len=18), dimension(n_new_materials) :: new_material_names
  character(len=20), dimension(n_new_materials) :: new_material_type
  character(len=20), dimension(n_materials) :: material_names

  character(len=20), dimension(new_material) :: material_type
  character(len=20), dimension(6) :: types
  character(len=100) :: filename

  real :: w, P, sigma, wavelength, max_temp
  real, allocatable :: w_subset(:), P_subset(:), sigma_subset(:), wavelength_subset(:)

  type(Material) :: mat
  logical :: mat_found
  type(solver_results) :: result_data_cylindrical

  !----------------------------------------------------------


  !----------------------------------------------------------
  ! Material names
  !----------------------------------------------------------
  material_names = [ &
      "Aluminum            ", "Copper              ", "Gold                ", "Iron                ", &
      "Lead                ", "Nickel              ", "Silver              ", "Steel               ", &
      "Titanium            ", "Tungsten            ", "Zinc                ", "Magnesium           ", &
      "Platinum            ", "Chromium            ", "Brass               ", "Bronze              ", &
      "StainlessSteel      ", "Cobalt              ", "Molybdenum          ", "Beryllium           ", &
      "Lithium             ", "Sodium              ", "Potassium           ", "Uranium             ", &
      "Mercury             ", "Cadmium             ", "Palladium           ", "Rhodium             ", &
      "Iridium             ", "Osmium              ", "Bismuth             " &
  ]

  !----------------------------------------------------------
  ! Material types (31 entries, one per material)
  !----------------------------------------------------------
  material_type = [ &
      "transition          ", "transition          ", "transition          ", "transition          ", &
      "poor_metal          ", "transition          ", "poor_metal          ", "alloy               ", &
      "transition          ", "transition          ", "poor_metal          ", "poor_metal          ", &
      "transition          ", "transition          ", "alloy               ", "alloy               ", &
      "transition          ", "transition          ", "transition          ", "poor_metal          ", &
      "alkali              ", "alkali              ", "alkali              ", "actinide            ", &
      "poor_metal          ", "poor_metal          ", "transition          ", "transition          ", &
      "transition          ", "transition          ", "poor_metal          " &
  ]

  !----------------------------------------------------------
  ! List of unique types for file generation
  !----------------------------------------------------------
  types = [ &
      "transition          ", &
      "alkali              ", &
      "alkaline_earth      ", &
      "poor_metal          ", &
      "actinide            ", &
      "alloy               " &
  ]

  !new materials added:
    !----------------------------------------------------------
  ! New materials only (15 entries)
  !----------------------------------------------------------
  new_material_names = [ &
      "Vanadium          ", "Zirconium         ", "Hafnium           ", &
      "Gallium           ", "Indium            ", "Thallium          ", &
      "Inconel           ", "Duralumin         ", "Constantan        ", &
      "Rubidium          ", "Cesium            ", "Francium          ", &
      "Calcium           ", "Strontium         ", "Barium            ", &
      "Thorium           ", "Plutonium         ", "Neptunium         " &
  ]

  new_material_type = [ &
      "transition          ", "transition          ", "transition          ", &
      "poor_metal          ", "poor_metal          ", "poor_metal          ", &
      "alloy               ", "alloy               ", "alloy               ", &
      "alkali              ", "alkali              ", "alkali              ", &
      "alkaline_earth      ", "alkaline_earth      ", "alkaline_earth      ", &
      "actinide            ", "actinide            ", "actinide            " &
  ]



  !----------------------------------------------------------
  ! Reduced subset for faster runs
  !----------------------------------------------------------

  !----------------------------------------------------------
! Allocate first

! Log-spaced w and sigma
!w_values = [1e-6, 1e-5, 1e-4, 1e-3, 1e-2]
!sigma_values = [1e-6, 1e-5, 1e-4, 1e-3, 1e-2]

! Visible spectrum wavelengths (meters)
!wavelength_values = [450e-9, 500e-9, 600e-9, 700e-9]

! Power values
!P_values = [0.05, 0.1, 0.2]

  nw = 1; nP = 1; nlambda = 1; nsigma = 1
  allocate(w_subset(nw), P_subset(nP), sigma_subset(nsigma), wavelength_subset(nlambda))
  !w_subset = [1e-6,1e-5,1e-4,1e-3,1e-2]
  !sigma_subset = [1e-6, 1e-5, 1e-4,1e-3,1e-2]  ! in seconds
  !P_subset = [0.1,0.3,0.5]
  !wavelength_subset = [400e-9,500e-9,600e-9,700e-9]

  !----------------------------------------------------------
  ! LOOP 1: Generate data by choosing lists to go through
  !----------------------------------------------------------
!----------------------------------------------------------
! New Sweep Grid Setup
!----------------------------------------------------------
! Allocate first

! Log-spaced w and sigma
!w_values = [1e-6, 1e-5, 1e-4, 1e-3, 1e-2]
!sigma_values = [1e-6, 1e-5, 1e-4, 1e-3, 1e-2]

! Visible spectrum wavelengths (meters)
!wavelength_values = [450e-9, 500e-9, 600e-9, 700e-9]

! Power values
!P_values = [0.05, 0.1, 0.2]

!----------------------------------------------------------
! Open CSV file for writing results
!----------------------------------------------------------
!open(unit=30, file="all_materials.csv", status="replace", action="write")

! Loop over all materials
!do i_material = 1, new_material
    !material_name = trim(new_material_names(i_material))
    !mat = get_material(material_name)

    ! Loop over sweep grid
    !do i_w = 1, nw
        !w = w_subset(i_w)
        !do i_lambda = 1, nlambda
            !wavelength = wavelength_subset(i_lambda)
            !do i_sigma = 1, nsigma
                !sigma = sigma_subset(i_sigma)
                !do i_P = 1, nP
                    !P = P_subset(i_P)

                    ! Solve heat equation
                    !result_data_cylindrical = heat_equation_solver_cylindrical(w, P, sigma, wavelength, material_name)

                    ! Check allocation and write to CSV
                    !if (allocated(result_data_cylindrical%temperature)) then
                        !write(30, fmt=*) log10(w), wavelength, log10(sigma), P, &
                              !         mat%k, mat%rho, mat%c, mat%epsilon, &
                             !          log10(result_data_cylindrical%final_source_sum+1e-12), &
                            !           log10(result_data_cylindrical%final_source_max+1e-12), &
                           !            log10(result_data_cylindrical%final_source_norm+1e-12), &
                          !             log10(result_data_cylindrical%I0+1e-12), &
                         !              result_data_cylindrical%max_temperature
                        !               print *, "Processed material:", trim(material_name), &
                       !                  "Max Temp:", result_data_cylindrical%max_temperature
                    !else
                        !print *, "ERROR: temperature array not allocated for ", material_name
                    !end if

                !end do
            !end do
        !end do
    !end do
!end do

!close(30)
!print *, "New sweep grid data generated successfully."



  !----------------------------------------------------------
  ! LOOP 2: Generate data for all new materials separated by type
  !----------------------------------------------------------
  ! Set a constant P value for all materials
  ! Log-spaced w and sigma
  !----------------------------------------------------------
  ! LOOP 2: Generate data for all new materials separated by type
  !----------------------------------------------------------
  ! Fixed single parameter set for generalization testing
  w_subset = [1e-4]
  sigma_subset = [1e-3]
  wavelength_subset = [600e-9]
  P_subset = [0.1]

  do i_type = 1, size(types)
      filename = trim(types(i_type)) // "_new.csv"
      unit_no = 40 + i_type   ! avoid clashing with LOOP 1 unit numbers
      open(unit=unit_no, file=filename, status="replace", action="write", iostat=istat)

      ! Loop over all new materials
      do i_material = 1, n_new_materials
          if (trim(new_material_type(i_material)) == trim(types(i_type))) then
              material_name = trim(new_material_names(i_material))
              mat = get_material(material_name)

              ! Fixed parameter sweep
              do i_w = 1, size(w_subset)
                  w = w_subset(i_w)
                  do i_lambda = 1, size(wavelength_subset)
                      wavelength = wavelength_subset(i_lambda)
                      do i_sigma = 1, size(sigma_subset)
                          sigma = sigma_subset(i_sigma)
                          do i_P = 1, size(P_subset)
                                P = P_subset(i_P)  

                              ! Call solver
                              result_data_cylindrical = heat_equation_solver_cylindrical(w, P, sigma, &
                                                            wavelength, material_name)

                              if (.not. allocated(result_data_cylindrical%temperature)) then
                                  print *, "ERROR: temperature array not allocated for", material_name
                              else
                                  write(unit_no, fmt=*) log10(w), wavelength, log10(sigma), P,&
                                      mat%k, mat%rho, mat%c, mat%epsilon, &
                                      log10(result_data_cylindrical%final_source_sum+1e-12), &
                                      log10(result_data_cylindrical%final_source_max+1e-12), &
                                      log10(result_data_cylindrical%final_source_norm+1e-12), &
                                      log10(result_data_cylindrical%I0+1e-12), &
                                      maxval(result_data_cylindrical%temperature)
                                  print *, "Processed NEW material:", trim(material_name), &
                                            maxval(result_data_cylindrical%temperature)
                              end if
                          end do
                      end do
                  end do
              end do
          end if
      end do

      close(unit_no)
  end do

  print *, "Data for all NEW materials generated successfully."

end program main_heat_sim

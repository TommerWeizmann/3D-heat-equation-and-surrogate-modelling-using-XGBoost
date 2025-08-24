module cylindrical_solver_module
  implicit none
contains
  !The following subroutine generates the boundry conditions for the heat equation in 3D in cylindrical
  !coordinates
  subroutine boundary_conditions_cylindrical(T_new, nr, nz)
  implicit none

  ! Arguments
  integer, intent(in) :: nr, nz

  real, dimension(nr, nz), intent(inout) :: T_new

  ! Local variables
  integer :: i
!  real :: q_conv, dTemp

  ! Neumann BC at r = 0 (symmetry)
  T_new(1,:) = T_new(2,:)

  ! Neumann BC at r = R (insulated outer edge)
  T_new(nr,:) = T_new(nr-1,:)

  ! Neumann BC at z = H (bottom of domain)
  do i = 1, nr
    T_new(i,nz) = T_new(i,nz-1)
  end do

  ! Convective boundary at z = 0 (surface)
 ! do i = 1, nr
  !  q_conv = h * (T_new(i,1) - T_ambient)
   ! dTemp = q_conv * dt / (rho * dz)
    !T_new(i,1) = T_new(i,1) - dTemp
  !end do
end subroutine boundary_conditions_cylindrical

  
!  ------------------------------------------------------------------------
! Heat equation solver:
! This function generates a 3D heat equation solution for the maximum temperature
! due to a laser applied on the surface (round surface, assume slab shaped object).
! Also there is a subroutine for the boundary conditions, where we assume material is insulated
! but there is heat flux to air from the surface at which the laser was applied on.
! ------------------------------------------------------------------------
  
  function heat_equation_solver_cylindrical(w, P, sigma, wavelength, material_name) result(res)
    use data_storing_module
    use materials_module
    implicit none
    type(solver_results) :: res
    logical :: mat_found
    ! Input parameters
    real, intent(in) :: w, P, sigma, wavelength
    character(len=*), intent(in) :: material_name

    ! Output
    real :: center_T,t_max_index ! Maximum temperature reached during simulation

    ! Local constants and parameters:
    real, parameter :: T0 = 293.15               ! Initial temperature (K)
    real, parameter :: pi = 4.0 * atan(1.0)      ! Pi constant
    real, parameter :: step_number = 100         ! Grid resolution: 10 
    real, parameter :: total_time = 1e-2 * 4     ! Total simulation time (s)
    real, parameter :: length = 0.01             ! Cube side length (m)

    ! Local variables:
    type(Material) :: mat
    real :: k_th, rho, c, epsilon                ! Material properties
    integer :: nr, nz, i, kk, time, n_steps
    real :: dr, dz, dt, alpha, mu_a, delta, I0, current_time
    real :: h, T_ambient
    
    real, allocatable :: Temp_field(:,:), T_new(:,:), source_term(:,:), laplacian(:,:)
    real, allocatable :: R(:,:), Z(:,:)
    !arrays to store the results I want
    real, allocatable :: times(:)
    real, allocatable :: center_temps(:)
    ! Get material propertiesm
    mat = get_material(material_name)
    k_th = mat%k
    rho  = mat%rho
    c    = mat%c
    epsilon = mat%epsilon
    alpha = k_th / (rho * c)

    ! Grid setup
    nr = step_number
    nz = step_number
    dz = length / real(step_number - 1)
    dr = dz !for simplicity, but this can be varried as needed

    alpha = k_th / (rho * c)
    dt = (dz**2 / (6.0 * alpha))  ! FTCS stability condition

    n_steps = max(1, int(total_time / dt))
    allocate(times(n_steps))
    allocate(center_temps(n_steps))

    !allocating the arrays
    allocate(res%times(n_steps))
    allocate(res%temperature(n_steps))
    !allocate(res%temperature_field(nr,nz,n_steps))
    allocate(res%source_sum(n_steps))
    allocate(res%source_max(n_steps))
    allocate(res%source_norm(n_steps))
    allocate(res%I0_values(n_steps))
    allocate(res%delta_values(n_steps))
    
    
    

    mu_a = (4.0 * pi * epsilon) / wavelength  ! Absorption coefficient
    delta = 1.0 / mu_a                        ! Penetration depth
    I0 = P / (pi * w**2)                      ! Laser intensity
    T_ambient = T0
    h = 10.0  ! Convective heat transfer coefficient

    ! Allocate and initialize arrays
    allocate(Temp_field(nr,nz));     Temp_field = T0
    allocate(T_new(nr,nz));           T_new = T0
    allocate(source_term(nr,nz))
    allocate(laplacian(nr,nz))
    allocate(R(nr,nz))
    allocate(Z(nr,nz))

    ! Coordinate system (centered in x and y, depth from surface)
    do i = 1, nr
      do kk = 1, nz
        R(i,kk) = (i - 1) * dr
        Z(i,kk) = (kk - 1) * dz
      end do
    end do
    center_T = T0
    ! Loop over time
    do time = 1, n_steps
      current_time = time * dt
    
  ! Compute source term
    do i = 1, nr
      do kk = 1, nz
        source_term(i,kk) = ((I0 * mu_a) / (rho * c)) * &
        exp(-(R(i,kk)**2)/w**2) * &
        exp(-Z(i,kk)/delta) * &
        exp(-(current_time**2)/(2.0 * sigma**2))
      end do
    end do
    !computing and storing source_term related values to output lists
    res%source_sum(time)  = sum(source_term)
    res%source_max(time)  = maxval(source_term)
    res%source_norm(time) = sqrt(sum(source_term**2))
    !computing and storing other variable values to output lists
    res%I0_values(time) = I0
    res%delta_values(time) = delta
    !-------------------------------------------------------------------------------------------
    !Calculate laplacian using finite difference:
    !laplacian formula  = (T(i-1,k) + T(i+1,k) - 2T(i,j))/dr**2 + 1/r*((T(i-1)+T(i+1))/2dr) &
    !(T(i,k+1) + T(i,k-1) - T(i,k))/dz**2
    ! Special case: r = 0 (i = 1)
    !------------------------------------------------------------------------------------------
    do kk = 2, nz-1
      laplacian(1,kk) = (2.0 * Temp_field(2,kk) - 2.0 * Temp_field(1,kk)) / dr**2 + &
                  (Temp_field(1,kk-1) - 2.0 * Temp_field(1,kk) + Temp_field(1,kk+1)) / dz**2
    end do

    ! General case: 2 < i < nr - 1
    do i = 2, nr-1
      do kk = 2, nz-1
        laplacian(i,kk) = (Temp_field(i+1,kk) - 2.0 * Temp_field(i,kk) + Temp_field(i-1,kk))&
        / dr**2 + (1.0 / R(i,kk)) * (Temp_field(i+1,kk) - Temp_field(i-1,kk)) / (2.0 * dr) + &
        (Temp_field(i,kk+1) - 2.0 * Temp_field(i,kk) + Temp_field(i,kk-1)) / dz**2
      end do
    end do
  
  !Updating temperature
    do i = 2, nr - 1
      do kk = 2, nz - 1
        T_new(i,kk) = Temp_field(i,kk) + dt * (alpha * laplacian(i,kk) + source_term(i,kk))
      end do
    end do
  !calling on boundry conditions 
  call boundary_conditions_cylindrical(T_new, nr, nz)
  !-----------------------------------------------------------------------------------------
  !Apply convective loss at z = 0
    !do i = 1, nr
      !q_conv = h * (T_new(i,1) - T_ambient)
      !dTemp = q_conv * dt / (rho * dz)
      !T_new(i,1) = T_new(i,1) - dTemp
    !end do
      ! Track maximum center temperature over time and update temperature
  !----------------------------------------------------------------------------------------
    if (T_new(1,1) > center_T) then
      center_T = T_new(1, 1)
    end if
    ! Store results for this timestep
    if (time <= n_steps) then
      times(time) = current_time
      center_temps(time) = T_new(1,1)
    end if
    ! Update Temp_field for next step
    ! Manually applying the convective heat loss function here
    do i = 1, nr
      T_new(i,1) = Temp_field(i,1) + dt * (alpha * laplacian(i,1) + source_term(i,1)) &
                   - h * dt / (rho * c* dz) * (Temp_field(i,1) - T_ambient)
    end do

  end do
! Assign to output result and dealloacting the arrays
  
  res%times = times
  res%temperature = center_temps
  res%max_temperature = maxval(res%temperature) 
  res%final_source_sum = res%source_sum(n_steps)
  res%final_source_max = res%source_max(n_steps)
  res%final_source_norm = res%source_norm(n_steps)
  res%I0 = I0
  res%delta = delta

  
  
  !res%source_term_values = source_term

  if (allocated(Temp_field)) deallocate(Temp_field)
  if (allocated(T_new))      deallocate(T_new)
  if (allocated(source_term))deallocate(source_term)
  if (allocated(laplacian))  deallocate(laplacian)
  if (allocated(R))          deallocate(R)
  if (allocated(Z))          deallocate(Z)


end function
end module cylindrical_solver_module

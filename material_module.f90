module materials_module
  implicit none
  public :: Material, get_material

  type :: Material
    real :: k        ! thermal conductivity (W/m·K)
    real :: rho      ! density (kg/m^3)
    real :: c        ! specific heat capacity (J/kg·K)
    real :: epsilon  ! extinction coefficient (dimensionless placeholder)
    character(len=30) :: material_type ! type of material
  end type Material

contains

  function get_material(name) result(mat)
    character(len=*), intent(in) :: name
    type(Material) :: mat

    select case (trim(adjustl(name)))
    case ("Aluminum")
      mat = Material(k=237.0, rho=2700.0, c=897.0, epsilon=8.4, material_type="Post-Transition Metal")
    case ("Copper")
      mat = Material(k=401.0, rho=8960.0, c=385.0, epsilon=2.6, material_type="Transition Metal")
    case ("Gold")
      mat = Material(k=318.0, rho=19300.0, c=129.0, epsilon=5.4, material_type="Transition Metal")
    case ("Iron")
      mat = Material(k=80.0, rho=7874.0, c=449.0, epsilon=3.1, material_type="Transition Metal")
    case ("Lead")
      mat = Material(k=35.0, rho=11340.0, c=128.0, epsilon=1.6, material_type="Post-Transition Metal")
    case ("Nickel")
      mat = Material(k=90.9, rho=8908.0, c=444.0, epsilon=3.3, material_type="Transition Metal")
    case ("Silver")
      mat = Material(k=429.0, rho=10490.0, c=235.0, epsilon=4.0, material_type="Transition Metal")
    case ("Steel")
      mat = Material(k=50.0, rho=7850.0, c=486.0, epsilon=2.5, material_type="Alloy")
    case ("Titanium")
      mat = Material(k=21.9, rho=4507.0, c=522.0, epsilon=3.5, material_type="Transition Metal")
    case ("Tungsten")
      mat = Material(k=173.0, rho=19300.0, c=134.0, epsilon=3.4, material_type="Transition Metal")
    case ("Zinc")
      mat = Material(k=116.0, rho=7140.0, c=388.0, epsilon=1.0, material_type="Post-Transition Metal")
    case ("Magnesium")
      mat = Material(k=156.0, rho=1740.0, c=1023.0, epsilon=1.2, material_type="Alkaline Earth Metal")
    case ("Platinum")
      mat = Material(k=71.6, rho=21450.0, c=133.0, epsilon=4.2, material_type="Transition Metal")
    case ("Chromium")
      mat = Material(k=93.9, rho=7190.0, c=448.0, epsilon=3.3, material_type="Transition Metal")
    case ("Brass")
      mat = Material(k=109.0, rho=8530.0, c=380.0, epsilon=2.0, material_type="Alloy")
    case ("Bronze")
      mat = Material(k=60.0, rho=8800.0, c=380.0, epsilon=2.0, material_type="Alloy")
    case ("StainlessSteel")
      mat = Material(k=16.0, rho=8000.0, c=500.0, epsilon=2.5, material_type="Alloy")
    case("Molybdenum")
      mat = Material(k=138.0, rho=10220.0, c=251.0, epsilon=3.0, material_type="Transition Metal")
    case("Beryllium")
      mat = Material(k=200.0, rho=1850.0, c=1825.0, epsilon=2.0, material_type="Alkaline Earth Metal")
    case("Lithium")
      mat = Material(k=85.0, rho=534.0, c=3582.0, epsilon=1.5, material_type="Alkali Metal")
    case("Sodium")
      mat = Material(k=142.0, rho=968.0, c=1230.0, epsilon=1.5, material_type="Alkali Metal")
    case("Potassium")
      mat = Material(k=102.0, rho=856.0, c=757.0, epsilon=1.5, material_type="Alkali Metal")
    case("Uranium")
      mat = Material(k=27.0, rho=19050.0, c=116.0, epsilon=2.5, material_type="Actinide")
    case("Mercury")
      mat = Material(k=8.3, rho=13534.0, c=140.0, epsilon=1.0, material_type="Post-Transition Metal")
    case("Cadmium")
      mat = Material(k=96.0, rho=8650.0, c=230.0, epsilon=2.0, material_type="Post-Transition Metal")
    case("Palladium")
      mat = Material(k=72.0, rho=12023.0, c=240.0, epsilon=3.5, material_type="Transition Metal")
    case("Rhodium")
      mat = Material(k=150.0, rho=12450.0, c=240.0, epsilon=3.5, material_type="Transition Metal")
    case("Iridium")
      mat = Material(k=147.0, rho=22560.0, c=131.0, epsilon=3.5, material_type="Transition Metal")
    case("Osmium")
      mat = Material(k=87.0, rho=22590.0, c=130.0, epsilon=3.5, material_type="Transition Metal")
    case("Bismuth")
      mat = Material(k=7.9, rho=9780.0, c=122.0, epsilon=1.5, material_type="Post-Transition Metal")
    case("Cobalt")
      mat = Material(k=100.0, rho=8900.0, c=420.0, epsilon=3.0, material_type="Transition Metal")
    !additional metals:
        ! ===== Transition Metals =====
    case("Vanadium")
      mat = Material(k=30.7, rho=6110.0, c=489.0, epsilon=2.5, material_type="Transition Metal")
    case("Zirconium")
      mat = Material(k=22.6, rho=6500.0, c=278.0, epsilon=2.5, material_type="Transition Metal")
    case("Hafnium")
      mat = Material(k=23.0, rho=13310.0, c=144.0, epsilon=2.5, material_type="Transition Metal")

    ! ===== Post-Transition Metals =====
    case("Gallium")
      mat = Material(k=29.0, rho=5907.0, c=371.0, epsilon=1.5, material_type="Post-Transition Metal")
    case("Indium")
      mat = Material(k=81.6, rho=7310.0, c=233.0, epsilon=1.5, material_type="Post-Transition Metal")
    case("Thallium")
      mat = Material(k=46.0, rho=11850.0, c=129.0, epsilon=1.5, material_type="Post-Transition Metal")

    ! ===== Alloys =====
    case("Inconel")
      mat = Material(k=11.4, rho=8470.0, c=435.0, epsilon=2.0, material_type="Alloy")
    case("Duralumin")
      mat = Material(k=130.0, rho=2800.0, c=900.0, epsilon=2.0, material_type="Alloy")
    case("Constantan")
      mat = Material(k=22.0, rho=8900.0, c=410.0, epsilon=2.0, material_type="Alloy")

    ! ===== Alkali Metals =====
    case("Rubidium")
      mat = Material(k=58.0, rho=1532.0, c=364.0, epsilon=1.5, material_type="Alkali Metal")
    case("Cesium")
      mat = Material(k=36.0, rho=1879.0, c=242.0, epsilon=1.5, material_type="Alkali Metal")
    case("Francium")
      mat = Material(k=15.0, rho=2460.0, c=200.0, epsilon=1.5, material_type="Alkali Metal") ! Hypothetical placeholder

    ! ===== Alkaline Earth Metals =====
    case("Calcium")
      mat = Material(k=201.0, rho=1550.0, c=650.0, epsilon=1.5, material_type="Alkaline Earth Metal")
    case("Strontium")
      mat = Material(k=35.0, rho=2640.0, c=300.0, epsilon=1.5, material_type="Alkaline Earth Metal")
    case("Barium")
      mat = Material(k=18.4, rho=3590.0, c=204.0, epsilon=1.5, material_type="Alkaline Earth Metal")

    ! ===== Actinides =====
    case("Thorium")
      mat = Material(k=54.0, rho=11720.0, c=117.0, epsilon=2.5, material_type="Actinide")
    case("Plutonium")
      mat = Material(k=6.7, rho=19816.0, c=35.0, epsilon=2.5, material_type="Actinide")
    case("Neptunium")
      mat = Material(k=6.3, rho=20450.0, c=29.0, epsilon=2.5, material_type="Actinide")


    case default
      print *, "Unknown material: ", name
      stop
    end select

  end function get_material

end module materials_module

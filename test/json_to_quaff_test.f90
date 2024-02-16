module json_to_quaff_test
  use veggies, only: assert_equals, assert_that, result_t, test_item_t, describe, it, succeed, fail
  use json_to_quaff, only : &
      fallible_area_t, &
      fallible_mass_t, &
      fallible_length_t, &
      fallible_temperature_t, &
      fallible_time_t, &
      fallible_volume_t, &
      fallible_integer_t, &
      fallible_bool_t, &
      fallible_real_t, &
      fallible_pressure_t, &
      fallible_mass_rate_t, &
      fallible_power_t, &
      fallible_speed_t, &
      fallible_molar_mass_t, &
      fallible_density_t, &
      fallible_energy_t, &
      fallible_inverse_molar_mass_t

  use quaff, only: &
      operator(.unit.), &
      SECONDS, &
      CUBIC_MILLIMETERS, &
      KILOGRAMS, &
      KELVIN, &
      METERS, &
      PASCALS, &
      KILOGRAMS_PER_SECOND, &
      WATTS, &
      METERS_PER_SECOND, &
      GRAMS_PER_MOL, &
      GRAMS_PER_CUBIC_METER, &
      MOLS_PER_GRAM, &
      SQUARE_CENTIMETERS, &
      JOULES
  use rojff, only: fallible_json_value_t, parse_json_from_string
  use erloff, only: error_list_t
  use quaff_asserts_m, only: assert_equals



  implicit none
  private
  public :: test_json_to_quaff
contains
  function test_json_to_quaff() result(tests)
    type(test_item_t) :: tests

    tests = describe(&
    "That a fallible_json_value_t can successfully construct ", &
    [ describe( &
        "a fallible_time_t", &
        [ it( &
            "with no errors", &
            check_time_valid) &
        , it( &
            "with errors", &
            check_time_with_errors) &
        ]) &
      , describe( &
          "a fallible_volume_t", &
          [ it( &
              "with no errors", &
              check_volume_valid) &
          , it( &
              "with errors", &
              check_volume_with_errors) &
          ]) &
      , describe( &
          "a fallible_temperature_t", &
          [ it( &
              "with no errors", &
              check_temperature_valid) &
          , it( &
              "with errors", &
              check_temperature_with_errors) &
          ]) &
      , describe( &
          "a fallible_mass_t", &
          [ it( &
              "with no errors", &
              check_mass_valid) &
          , it( &
              "with errors", &
              check_mass_with_errors) &
          ]) &
      , describe( &
          "a fallible_molar_mass_t", &
          [ it( &
              "with no errors", &
              check_molar_mass_valid) &
          , it( &
              "with errors", &
              check_molar_mass_with_errors) &
          ]) &
      , describe( &
          "a fallible_inverse_molar_mass_t", &
          [ it( &
              "with no errors", &
              check_inverse_molar_mass_valid) &
          , it( &
              "with errors", &
              check_inverse_molar_mass_with_errors) &
          ]) &
      , describe( &
          "a fallible_length_t", &
          [ it( &
              "with no errors", &
              check_length_valid) &
          , it( &
              "with errors", &
              check_length_with_errors) &
          ]) &
      , describe( &
          "a fallible_speed_t", &
          [ it( &
              "with no errors", &
              check_speed_valid) &
          , it( &
              "with errors", &
              check_speed_with_errors) &
          ]) &
      , describe( &
          "a fallible_integer_t", &
          [ it( &
              "with no errors", &
              check_integer_valid) &
          , it( &
              "with errors", &
              check_integer_with_errors) &
          ]) &
      , describe( &
          "a fallible_real_t", &
          [ it( &
              "with no errors", &
              check_real_valid) &
          , it( &
              "with errors", &
              check_real_with_errors) &
          ]) &
      , describe( &
          "a fallible_bool_t", &
          [ it( &
              "with no errors", &
              check_bool_valid) &
          , it( &
              "with errors", &
              check_bool_with_errors) &
          ]) &
      , describe( &
          "a fallible_pressure_t", &
          [ it( &
              "with no errors", &
              check_pressure_valid) &
          , it( &
              "with errors", &
              check_pressure_with_errors) &
          ]) &
      , describe( &
          "a fallible_mass_rate_t", &
          [ it( &
              "with no errors", &
              check_mass_rate_valid) &
          , it( &
              "with errors", &
              check_mass_rate_with_errors) &
          ]) &
      , describe( &
          "a fallible_power_t", &
          [ it( &
              "with no errors", &
              check_power_valid) &
          , it( &
              "with errors", &
              check_power_with_errors) &
          ]) &
      , describe( &
          "a fallible_density_t", &
          [ it( &
              "with no errors", &
              check_density_valid) &
          , it( &
              "with errors", &
              check_density_with_errors) &
          ]) &
      , describe( &
          "a fallible_area_t", &
          [ it( &
              "with no errors", &
              check_area_valid) &
          , it( &
              "with errors", &
              check_area_with_errors) &
          ]) &
      , describe( &
          "a fallible_energy_t", &
          [ it( &
              "with no errors", &
              check_energy_valid) &
          , it( &
              "with errors", &
              check_energy_with_errors) &
          ]) &
    ])
  end function

  function check_time_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_time
    type(fallible_time_t) :: fallible_quaff_time
    type(error_list_t) :: errors
    character(len=*), parameter :: time_c ='"1.0 s"'
    double precision, parameter :: time_r = 1.0d0


    fallible_json_time = parse_json_from_string(time_c)

    fallible_quaff_time = fallible_time_t(fallible_json_time)

    if (fallible_quaff_time%failed()) then
      errors = fallible_quaff_time%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_time%time(), time_r.unit.SECONDS)
    end if
  end function

  function check_time_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_time
    type(fallible_time_t) :: fallible_quaff_time
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 s'


    fallible_json_time = parse_json_from_string(not_a_json_c)

    fallible_quaff_time = fallible_time_t(fallible_json_time)

    if (fallible_quaff_time%failed()) then
      errors_quaff = fallible_quaff_time%errors()
      errors_rojff = fallible_json_time%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function

  function check_volume_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_volume
    type(fallible_volume_t) :: fallible_quaff_volume
    type(error_list_t) :: errors
    character(len=*), parameter :: volume_c ='"1.0 mm^3"'
    double precision, parameter :: volume_r = 1.0d0


    fallible_json_volume = parse_json_from_string(volume_c)

    fallible_quaff_volume = fallible_volume_t(fallible_json_volume)

    if (fallible_quaff_volume%failed()) then
      errors = fallible_quaff_volume%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_volume%volume(), volume_r.unit.CUBIC_MILLIMETERS)
    end if
  end function

  function check_volume_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_volume
    type(fallible_volume_t) :: fallible_quaff_volume
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 mm^3'


    fallible_json_volume = parse_json_from_string(not_a_json_c)

    fallible_quaff_volume = fallible_volume_t(fallible_json_volume)

    if (fallible_quaff_volume%failed()) then
      errors_quaff = fallible_quaff_volume%errors()
      errors_rojff = fallible_json_volume%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function

  function check_mass_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_mass
    type(fallible_mass_t) :: fallible_quaff_mass
    type(error_list_t) :: errors
    character(len=*), parameter :: mass_c ='"1.0 kg"'
    double precision, parameter :: mass_r = 1.0d0


    fallible_json_mass = parse_json_from_string(mass_c)

    fallible_quaff_mass = fallible_mass_t(fallible_json_mass)

    if (fallible_quaff_mass%failed()) then
      errors = fallible_quaff_mass%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_mass%mass(), mass_r.unit.KILOGRAMS)
    end if
  end function

  function check_mass_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_mass
    type(fallible_mass_t) :: fallible_quaff_mass
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 mm^3'


    fallible_json_mass = parse_json_from_string(not_a_json_c)

    fallible_quaff_mass = fallible_mass_t(fallible_json_mass)

    if (fallible_quaff_mass%failed()) then
      errors_quaff = fallible_quaff_mass%errors()
      errors_rojff = fallible_json_mass%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function
  function check_molar_mass_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_molar_mass
    type(fallible_molar_mass_t) :: fallible_quaff_molar_mass
    type(error_list_t) :: errors
    character(len=*), parameter :: molar_mass_c ='"1.0 g/mol"'
    double precision, parameter :: molar_mass_r = 1.0d0


    fallible_json_molar_mass = parse_json_from_string(molar_mass_c)

    fallible_quaff_molar_mass = fallible_molar_mass_t(fallible_json_molar_mass)

    if (fallible_quaff_molar_mass%failed()) then
      errors = fallible_quaff_molar_mass%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_molar_mass%molar_mass(), molar_mass_r.unit.GRAMS_PER_MOL)
    end if
  end function

  function check_molar_mass_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_molar_mass
    type(fallible_molar_mass_t) :: fallible_quaff_molar_mass
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 mm^3'


    fallible_json_molar_mass = parse_json_from_string(not_a_json_c)

    fallible_quaff_molar_mass = fallible_molar_mass_t(fallible_json_molar_mass)

    if (fallible_quaff_molar_mass%failed()) then
      errors_quaff = fallible_quaff_molar_mass%errors()
      errors_rojff = fallible_json_molar_mass%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function
  function check_inverse_molar_mass_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_inverse_molar_mass
    type(fallible_inverse_molar_mass_t) :: fallible_quaff_inverse_molar_mass
    type(error_list_t) :: errors
    character(len=*), parameter :: inverse_molar_mass_c ='"1.0 mol/g"'
    double precision, parameter :: inverse_molar_mass_r = 1.0d0


    fallible_json_inverse_molar_mass = parse_json_from_string(inverse_molar_mass_c)

    fallible_quaff_inverse_molar_mass = fallible_inverse_molar_mass_t(fallible_json_inverse_molar_mass)

    if (fallible_quaff_inverse_molar_mass%failed()) then
      errors = fallible_quaff_inverse_molar_mass%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_inverse_molar_mass%inverse_molar_mass(), inverse_molar_mass_r.unit.MOLS_PER_GRAM)
    end if
  end function

  function check_inverse_molar_mass_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_inverse_molar_mass
    type(fallible_inverse_molar_mass_t) :: fallible_quaff_inverse_molar_mass
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 mm^3'


    fallible_json_inverse_molar_mass = parse_json_from_string(not_a_json_c)

    fallible_quaff_inverse_molar_mass = fallible_inverse_molar_mass_t(fallible_json_inverse_molar_mass)

    if (fallible_quaff_inverse_molar_mass%failed()) then
      errors_quaff = fallible_quaff_inverse_molar_mass%errors()
      errors_rojff = fallible_json_inverse_molar_mass%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function

  function check_length_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_length
    type(fallible_length_t) :: fallible_quaff_length
    type(error_list_t) :: errors
    character(len=*), parameter :: length_c ='"1.0 m"'
    double precision, parameter :: length_r = 1.0d0


    fallible_json_length = parse_json_from_string(length_c)

    fallible_quaff_length = fallible_length_t(fallible_json_length)

    if (fallible_quaff_length%failed()) then
      errors = fallible_quaff_length%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_length%length(), length_r.unit.METERS)
    end if
  end function

  function check_length_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_length
    type(fallible_length_t) :: fallible_quaff_length
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 m'


    fallible_json_length = parse_json_from_string(not_a_json_c)

    fallible_quaff_length = fallible_length_t(fallible_json_length)

    if (fallible_quaff_length%failed()) then
      errors_quaff = fallible_quaff_length%errors()
      errors_rojff = fallible_json_length%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function
  function check_speed_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_speed
    type(fallible_speed_t) :: fallible_quaff_speed
    type(error_list_t) :: errors
    character(len=*), parameter :: speed_c ='"1.0 m/s"'
    double precision, parameter :: speed_r = 1.0d0


    fallible_json_speed = parse_json_from_string(speed_c)

    fallible_quaff_speed = fallible_speed_t(fallible_json_speed)

    if (fallible_quaff_speed%failed()) then
      errors = fallible_quaff_speed%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_speed%speed(), speed_r.unit.METERS_PER_SECOND)
    end if
  end function

  function check_speed_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_speed
    type(fallible_speed_t) :: fallible_quaff_speed
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 m'


    fallible_json_speed = parse_json_from_string(not_a_json_c)

    fallible_quaff_speed = fallible_speed_t(fallible_json_speed)

    if (fallible_quaff_speed%failed()) then
      errors_quaff = fallible_quaff_speed%errors()
      errors_rojff = fallible_json_speed%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function

  function check_temperature_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_temperature
    type(fallible_temperature_t) :: fallible_quaff_temperature
    type(error_list_t) :: errors
    character(len=*), parameter :: temperature_c ='"1.0 K"'
    double precision, parameter :: temperature_r = 1.0d0


    fallible_json_temperature = parse_json_from_string(temperature_c)

    fallible_quaff_temperature = fallible_temperature_t(fallible_json_temperature)

    if (fallible_quaff_temperature%failed()) then
      errors = fallible_quaff_temperature%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_temperature%temperature(), temperature_r.unit.KELVIN)
    end if
  end function

  function check_temperature_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_temperature
    type(fallible_temperature_t) :: fallible_quaff_temperature
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 mm^3'


    fallible_json_temperature = parse_json_from_string(not_a_json_c)

    fallible_quaff_temperature = fallible_temperature_t(fallible_json_temperature)

    if (fallible_quaff_temperature%failed()) then
      errors_quaff = fallible_quaff_temperature%errors()
      errors_rojff = fallible_json_temperature%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function

  function check_integer_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_integer
    type(fallible_integer_t) :: fallible_quaff_integer
    type(error_list_t) :: errors
    character(len=*), parameter :: integer_c = '1'
    integer, parameter :: integer_r = 1


    fallible_json_integer = parse_json_from_string(integer_c)

    fallible_quaff_integer = fallible_integer_t(fallible_json_integer)

    if (fallible_quaff_integer%failed()) then
      errors = fallible_quaff_integer%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_integer%integer_(), integer_r)
    end if
  end function

  function check_integer_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_integer
    type(fallible_integer_t) :: fallible_quaff_integer
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 mm^3'


    fallible_json_integer = parse_json_from_string(not_a_json_c)

    fallible_quaff_integer = fallible_integer_t(fallible_json_integer)

    if (fallible_quaff_integer%failed()) then
      errors_quaff = fallible_quaff_integer%errors()
      errors_rojff = fallible_json_integer%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function

  function check_real_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_real
    type(fallible_real_t) :: fallible_quaff_real
    type(error_list_t) :: errors
    character(len=*), parameter :: real_c = '1.0'
    double precision, parameter :: real_r = 1.0


    fallible_json_real = parse_json_from_string(real_c)

    fallible_quaff_real = fallible_real_t(fallible_json_real)

    if (fallible_quaff_real%failed()) then
      errors = fallible_quaff_real%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_real%real_(), real_r)
    end if
  end function

  function check_real_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_real
    type(fallible_real_t) :: fallible_quaff_real
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 mm^3'


    fallible_json_real = parse_json_from_string(not_a_json_c)

    fallible_quaff_real = fallible_real_t(fallible_json_real)

    if (fallible_quaff_real%failed()) then
      errors_quaff = fallible_quaff_real%errors()
      errors_rojff = fallible_json_real%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function


  function check_bool_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_bool
    type(fallible_bool_t) :: fallible_quaff_bool
    type(error_list_t) :: errors
    character(len=*), parameter :: bool_c = 'true'


    fallible_json_bool = parse_json_from_string(bool_c)

    fallible_quaff_bool = fallible_bool_t(fallible_json_bool)

    if (fallible_quaff_bool%failed()) then
      errors = fallible_quaff_bool%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_that(fallible_quaff_bool%bool())
    end if
  end function

  function check_bool_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_bool
    type(fallible_bool_t) :: fallible_quaff_bool
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 mm^3'


    fallible_json_bool = parse_json_from_string(not_a_json_c)

    fallible_quaff_bool = fallible_bool_t(fallible_json_bool)

    if (fallible_quaff_bool%failed()) then
      errors_quaff = fallible_quaff_bool%errors()
      errors_rojff = fallible_json_bool%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function

  function check_pressure_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_pressure
    type(fallible_pressure_t) :: fallible_quaff_pressure
    type(error_list_t) :: errors
    character(len=*), parameter :: pressure_c ='"1.0 Pa"'
    double precision, parameter :: pressure_r = 1.0d0


    fallible_json_pressure = parse_json_from_string(pressure_c)

    fallible_quaff_pressure = fallible_pressure_t(fallible_json_pressure)

    if (fallible_quaff_pressure%failed()) then
      errors = fallible_quaff_pressure%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_pressure%pressure(), pressure_r.unit.PASCALS)
    end if
  end function

  function check_pressure_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_pressure
    type(fallible_pressure_t) :: fallible_quaff_pressure
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 Pa'


    fallible_json_pressure = parse_json_from_string(not_a_json_c)

    fallible_quaff_pressure = fallible_pressure_t(fallible_json_pressure)

    if (fallible_quaff_pressure%failed()) then
      errors_quaff = fallible_quaff_pressure%errors()
      errors_rojff = fallible_json_pressure%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function

  function check_mass_rate_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_mass_rate
    type(fallible_mass_rate_t) :: fallible_quaff_mass_rate
    type(error_list_t) :: errors
    character(len=*), parameter :: mass_rate_c ='"1.0 kg/s"'
    double precision, parameter :: mass_rate_r = 1.0d0


    fallible_json_mass_rate = parse_json_from_string(mass_rate_c)

    fallible_quaff_mass_rate = fallible_mass_rate_t(fallible_json_mass_rate)

    if (fallible_quaff_mass_rate%failed()) then
      errors = fallible_quaff_mass_rate%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_mass_rate%mass_rate(), mass_rate_r.unit.KILOGRAMS_PER_SECOND)
    end if
  end function

  function check_mass_rate_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_mass_rate
    type(fallible_mass_rate_t) :: fallible_quaff_mass_rate
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 kg/s'


    fallible_json_mass_rate = parse_json_from_string(not_a_json_c)

    fallible_quaff_mass_rate = fallible_mass_rate_t(fallible_json_mass_rate)

    if (fallible_quaff_mass_rate%failed()) then
      errors_quaff = fallible_quaff_mass_rate%errors()
      errors_rojff = fallible_json_mass_rate%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function

  function check_power_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_power
    type(fallible_power_t) :: fallible_quaff_power
    type(error_list_t) :: errors
    character(len=*), parameter :: power_c ='"1.0 W"'
    double precision, parameter :: power_r = 1.0d0


    fallible_json_power = parse_json_from_string(power_c)

    fallible_quaff_power = fallible_power_t(fallible_json_power)

    if (fallible_quaff_power%failed()) then
      errors = fallible_quaff_power%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_power%power(), power_r.unit.WATTS)
    end if
  end function

  function check_power_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_power
    type(fallible_power_t) :: fallible_quaff_power
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 W'


    fallible_json_power = parse_json_from_string(not_a_json_c)

    fallible_quaff_power = fallible_power_t(fallible_json_power)

    if (fallible_quaff_power%failed()) then
      errors_quaff = fallible_quaff_power%errors()
      errors_rojff = fallible_json_power%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function
  function check_density_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_density
    type(fallible_density_t) :: fallible_quaff_density
    type(error_list_t) :: errors
    character(len=*), parameter :: density_c ='"1.0 g/m^3"'
    double precision, parameter :: density_r = 1.0d0


    fallible_json_density = parse_json_from_string(density_c)

    fallible_quaff_density = fallible_density_t(fallible_json_density)

    if (fallible_quaff_density%failed()) then
      errors = fallible_quaff_density%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_density%density(), density_r.unit.GRAMS_PER_CUBIC_METER)
    end if
  end function

  function check_density_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_density
    type(fallible_density_t) :: fallible_quaff_density
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 W'


    fallible_json_density = parse_json_from_string(not_a_json_c)

    fallible_quaff_density = fallible_density_t(fallible_json_density)

    if (fallible_quaff_density%failed()) then
      errors_quaff = fallible_quaff_density%errors()
      errors_rojff = fallible_json_density%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function
  function check_area_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_area
    type(fallible_area_t) :: fallible_quaff_area
    type(error_list_t) :: errors
    character(len=*), parameter :: area_c ='"1.0 cm^2"'
    double precision, parameter :: area_r = 1.0d0


    fallible_json_area = parse_json_from_string(area_c)

    fallible_quaff_area = fallible_area_t(fallible_json_area)

    if (fallible_quaff_area%failed()) then
      errors = fallible_quaff_area%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_area%area(), area_r.unit.SQUARE_CENTIMETERS)
    end if
  end function

  function check_area_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_area
    type(fallible_area_t) :: fallible_quaff_area
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 W'


    fallible_json_area = parse_json_from_string(not_a_json_c)

    fallible_quaff_area = fallible_area_t(fallible_json_area)

    if (fallible_quaff_area%failed()) then
      errors_quaff = fallible_quaff_area%errors()
      errors_rojff = fallible_json_area%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function
  function check_energy_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_energy
    type(fallible_energy_t) :: fallible_quaff_energy
    type(error_list_t) :: errors
    character(len=*), parameter :: energy_c ='"1.0 J"'
    double precision, parameter :: energy_r = 1.0d0


    fallible_json_energy = parse_json_from_string(energy_c)

    fallible_quaff_energy = fallible_energy_t(fallible_json_energy)

    if (fallible_quaff_energy%failed()) then
      errors = fallible_quaff_energy%errors()
      result_ = fail(errors%to_string())
    else
      result_ = assert_equals(fallible_quaff_energy%energy(), energy_r.unit.JOULES)
    end if
  end function

  function check_energy_with_errors() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_energy
    type(fallible_energy_t) :: fallible_quaff_energy
    type(error_list_t) :: errors_quaff, errors_rojff
    character(len=*), parameter :: not_a_json_c ='"1.0 W'


    fallible_json_energy = parse_json_from_string(not_a_json_c)

    fallible_quaff_energy = fallible_energy_t(fallible_json_energy)

    if (fallible_quaff_energy%failed()) then
      errors_quaff = fallible_quaff_energy%errors()
      errors_rojff = fallible_json_energy%errors
      result_ = assert_equals(errors_quaff%to_string(), errors_rojff%to_string())
    else
      result_ = fail("fallible_quaff did not succesffuly retain errors from a failed fallible_json")
    end if
  end function
end module
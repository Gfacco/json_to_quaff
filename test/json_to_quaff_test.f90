module json_to_quaff_test
  use veggies, only: assert_equals, assert_that, result_t, test_item_t, describe, it, succeed, fail
  use json_to_quaff, only : &
      fallible_mass_t, &
      fallible_length_t, &
      fallible_temperature_t, &
      fallible_time_t, &
      fallible_volume_t, &
      fallible_integer_t, &
      fallible_bool_t, &
      fallible_pressure_t
  use quaff, only: &
      operator(.unit.), &
      SECONDS, &
      CUBIC_MILLIMETERS, &
      KILOGRAMS, &
      KELVIN, &
      METERS, &
      PASCALS
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
          "a fallible_length_t", &
          [ it( &
              "with no errors", &
              check_length_valid) &
          , it( &
              "with errors", &
              check_length_with_errors) &
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
end module
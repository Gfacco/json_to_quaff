module json_to_quaff_test
  use veggies, only: assert_equals, result_t, test_item_t, describe, it, succeed, fail
  use json_to_quaff_m, only : fallible_time_t
  use quaff, only: operator(.unit.), SECONDS
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
    "That a fallible_json_value_t can successfully contruct ", &
    [ describe( &
        "a fallible_time_t", &
        [ it( &
            "with no errors", &
            check_time_valid) &
        , it( &
            "with errors", &
            check_time_with_errors) &
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
    double precision, parameter :: time_r = 1.0d0


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
end module
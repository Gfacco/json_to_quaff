module json_to_quaff_test
  use veggies, only: result_t, test_item_t, describe, it, succeed, fail
  ! use json_to_quaff_m, only : fallible_time_t
  use quaff, only: fallible_time_t
  use rojff, only: fallible_json_value_t, parse_json_from_string
  use erloff, only: error_list_t


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
        ! , it( &
        !     "with errors", &
        !     check_time_time_with_errors) &
        ]) &
    ])
  end function

  function check_time_valid() result(result_)
    type(result_t) :: result_
    type(fallible_json_value_t) :: fallible_json_time
    type(fallible_time_t) :: fallible_quaff_time
    type(error_list_t) :: errors
    character(len=*), parameter :: time_c ='"1.0 s"'

    fallible_json_time = parse_json_from_string(time_c)

    if (fallible_json_time%failed()) then
      errors = fallible_json_time%errors
      result_ = fail(errors%to_string())
    else
      result_ =succeed(time_c // ", was a valid JSON string")
    end if

  end function
end module
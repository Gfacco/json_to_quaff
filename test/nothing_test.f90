module nothing_test
  use veggies, only: result_t, test_item_t, describe, it, succeed

  implicit none
  private
  public :: test_nothing
contains
  function test_nothing() result(tests)
    type(test_item_t) :: tests

    tests = describe("Nothing", [it("works", check_nothing)])
  end function

  function check_nothing() result(result_)
    type(result_t) :: result_

    result_ = succeed("successfully")
  end function
end module
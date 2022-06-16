! Generated by cart. DO NOT EDIT
program main
    implicit none

    if (.not.run()) stop 1
contains
    function run() result(passed)
        use json_to_quaff_test, only: &
                json_to_quaff_json_to_quaff => &
                    test_json_to_quaff
        use nothing_test, only: &
                nothing_nothing => &
                    test_nothing
        use veggies, only: test_item_t, test_that, run_tests



        logical :: passed

        type(test_item_t) :: tests
        type(test_item_t) :: individual_tests(2)

        individual_tests(1) = json_to_quaff_json_to_quaff()
        individual_tests(2) = nothing_nothing()
        tests = test_that(individual_tests)


        passed = run_tests(tests)

    end function
end program

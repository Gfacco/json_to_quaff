module json_to_quaff_enthalpy_m
    use quaff, only: &
      fallible_enthalpy_t, &
      fallible_enthalpy_unit_t, &
      parse_enthalpy, &
      parse_enthalpy_unit
    use rojff, only: &
      fallible_json_value_t, &
      json_value_t, &
      json_string_t
    use erloff, only: &
      error_list_t, &
      module_t, &
      procedure_t, &
      fatal_t
  
  implicit none
  
  interface fallible_enthalpy_t
      module procedure fallible_enthalpy_from_fallible_json_value
  end interface
  interface fallible_enthalpy_unit_t
    module procedure fallible_enthalpy_unit_from_fallible_json_value
  end interface
  
  character(len=*), parameter :: MODULE_NAME = "json_to_quaff_enthalpy_m"
  
  contains
  
  function fallible_enthalpy_from_fallible_json_value( &
    fallible_json_value) result(fallible_enthalpy)
    type(fallible_json_value_t), intent(in) :: fallible_json_value
    type(fallible_enthalpy_t) :: fallible_enthalpy
  
    character(len=*), parameter :: PROCEDURE_NAME = "fallible_enthalpy_from_fallible_json_value"
  
    if (fallible_json_value%failed()) then
        fallible_enthalpy = fallible_enthalpy_t(fallible_json_value%errors)
    else
        select type (json_value => fallible_json_value%json)
            type is (json_string_t)
                fallible_enthalpy = fallible_enthalpy_t( &
                    parse_enthalpy(json_value%string), &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME))
            class default
                fallible_enthalpy = fallible_enthalpy_t(error_list_t(fatal_t( &
                    module_t(MODULE_NAME), &
                    procedure_t(PROCEDURE_NAME), &
                    json_value%to_expanded_string() // " is not json_string_t type")))
        end select
    end if
  end function
  
  function fallible_enthalpy_unit_from_fallible_json_value( &
    fallible_json_value) result(fallible_enthalpy_unit)
    type(fallible_json_value_t), intent(in) :: fallible_json_value
    type(fallible_enthalpy_unit_t) :: fallible_enthalpy_unit
  
    character(len=*), parameter :: PROCEDURE_NAME = "fallible_enthalpy_unit_from_fallible_json_value"
  
    if (fallible_json_value%failed()) then
      fallible_enthalpy_unit = fallible_enthalpy_unit_t(fallible_json_value%errors)
    else
      select type (json_value => fallible_json_value%json)
        type is (json_string_t)
          fallible_enthalpy_unit = fallible_enthalpy_unit_t( &
              parse_enthalpy_unit(json_value%string), &
                module_t(MODULE_NAME), &
                procedure_t(PROCEDURE_NAME))
        class default
          fallible_enthalpy_unit = fallible_enthalpy_unit_t(error_list_t(fatal_t( &
                module_t(MODULE_NAME), &
                procedure_t(PROCEDURE_NAME), &
                json_value%to_expanded_string() // " is not json_string_t type")))
      end select
    end if
  end function
  
  end module

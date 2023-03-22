module json_to_quaff
  use json_to_quaff_mass_m, only : fallible_mass_t, fallible_mass_unit_t
  use json_to_quaff_temperature_m, only : fallible_temperature_t, fallible_temperature_unit_t
  use json_to_quaff_time_m, only : fallible_time_t, fallible_time_unit_t
  use json_to_quaff_volume_m, only : fallible_volume_t, fallible_volume_unit_t
  use json_to_quaff_length_m, only : fallible_length_t, fallible_length_unit_t
  use json_to_quaff_speed_m, only : fallible_speed_t, fallible_speed_unit_t
  use json_to_fallible_integer_m, only : fallible_integer_t
  use json_to_fallible_real_m, only : fallible_real_t
  use json_to_fallible_bool_m, only : fallible_bool_t
  use json_to_quaff_pressure_m, only: fallible_pressure_t, fallible_pressure_unit_t
  use json_to_quaff_mass_rate_m, only: fallible_mass_rate_t, fallible_mass_rate_unit_t
  use json_to_quaff_molar_mass_m, only: fallible_molar_mass_t, fallible_molar_mass_unit_t
  use json_to_quaff_power_m, only: fallible_power_t, fallible_power_unit_t
  use json_to_quaff_density_m, only: fallible_density_t, fallible_density_unit_t
  use json_to_quaff_enthalpy_m, only: fallible_enthalpy_t, fallible_enthalpy_unit_t
  use json_to_quaff_molar_enthalpy_m, only: fallible_molar_enthalpy_t, fallible_molar_enthalpy_unit_t
  use json_to_quaff_inverse_molar_mass_m, only: fallible_inverse_molar_mass_t, fallible_inverse_molar_mass_unit_t
end module
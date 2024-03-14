use std::{collections::HashMap, fs::File, io::Write};

use serde::Deserialize;

use common_data_types::{
    BaseUnits, ConversionFactor, ConversionFactorDatabase, Dimension, DimensionNameDatabase,
    RatioTypeHint, UnitDescription, UnitList,
};

#[derive(Debug, Deserialize)]
pub struct Row {
    pub dimension_name: String,
    pub length: i8,
    pub mass: i8,
    pub time: i8,
    pub electric_current: i8,
    pub thermodynamic_temperature: i8,
    pub amount_of_substance: i8,
    pub luminous_intensity: i8,
    pub angle_kind: bool,
    pub constituent_concentration_kind: bool,
    pub information_kind: bool,
    pub solid_angle_kind: bool,
    pub temperature_kind: bool,
    pub singular: String,
    pub plural: String,
    pub abbreviation: String,
    pub keyboard_friendly_abbreviation: String,
    pub conversion_coefficient: f64,
    pub conversion_constant: f64,
}

macro_rules! get_base_unit {
    ($unit_names:ident, $unit_info:ident, $name:literal) => {{
        let dimension = $unit_info.get($name).expect("Did not find unit info");
        let name = $unit_names
            .get($name)
            .expect("Did not find unit name")
            .clone();
        (*dimension, name.into())
    }};
}

fn main() {
    let mut source_data = csv::ReaderBuilder::new()
        .flexible(false)
        .has_headers(true)
        .from_path("src/script/execution/types/measurement/units.csv")
        .unwrap();

    let mut conversion_factors = ConversionFactorDatabase::new();
    let mut dimension_names = DimensionNameDatabase::new();
    let mut unit_list: HashMap<String, Vec<UnitDescription>> = HashMap::new();

    let mut dimension_base_units: HashMap<String, Dimension> = HashMap::new();
    let mut unit_names_to_abbreviations: HashMap<String, String> = HashMap::new();

    for result in source_data.deserialize() {
        let row: Row = result.unwrap();

        // Enforce keyboard friendly abbreviations only using ascii characters.
        if !row.keyboard_friendly_abbreviation.is_ascii() {
            panic!(
                "Abbreviation `{}` contains non-ascii characters",
                row.keyboard_friendly_abbreviation
            );
        }

        unit_names_to_abbreviations.insert(
            row.singular.clone(),
            row.keyboard_friendly_abbreviation.clone(),
        );

        let mut ratio_type_hint = RatioTypeHint::default();

        ratio_type_hint.set_is_angle(row.angle_kind);
        ratio_type_hint.set_is_constituent_concentration(row.constituent_concentration_kind);
        ratio_type_hint.set_is_information(row.information_kind);
        ratio_type_hint.set_is_solid_angle(row.solid_angle_kind);
        ratio_type_hint.set_is_temperature(row.temperature_kind);

        let dimension = Dimension {
            length: row.length,
            mass: row.mass,
            time: row.time,
            electric_current: row.electric_current,
            thermodynamic_temprature: row.thermodynamic_temperature,
            amount_of_substance: row.amount_of_substance,
            luminous_intensity: row.luminous_intensity,
            ratio_type_hint,
        };

        dimension_base_units.insert(row.singular.clone(), dimension);

        // Record conversion factor.
        let already_exists = conversion_factors
            .insert(
                row.keyboard_friendly_abbreviation.clone().into(),
                ConversionFactor {
                    constant: row.conversion_constant,
                    coefficient: row.conversion_coefficient,
                    dimension,
                },
            )
            .is_some();

        if already_exists {
            panic!(
                "Multiple units use the abbreviation `{}`",
                row.keyboard_friendly_abbreviation
            );
        }

        // Self-deduplicating  list of names for the dimensions.
        dimension_names.insert(dimension, row.dimension_name.clone().into());

        unit_list
            .entry(row.dimension_name)
            .or_default()
            .push(UnitDescription {
                abbreviation: row.abbreviation,
                keyboard_friendly_abbreviation: row.keyboard_friendly_abbreviation,
                name: row.singular,
                plural_name: row.plural,
            });
    }

    let base_units: BaseUnits = BaseUnits::from([
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "meter second"
        ), // abasement
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "meter per second squared"
        ), // acceleration
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "joule second"
        ), // action
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "mole"), // amount_of_substance
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "radian"), // angle
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "radian second"
        ), // angular_abasement
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "radian per second squared"
        ), // angular_acceleration
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "radian per second cubed"
        ), // angular_jerk
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "radian per second"
        ), // angular_velocity
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "square meter"
        ), // area
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "state per square meter joule"
        ), // areal_density_of_states
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "kilogram per square meter"
        ), // areal_mass_density
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "per square meter"
        ), // areal_number_density
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "per square meter second"
        ), // areal_number_rate
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "joule per kilogram"
        ), // available_energy
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "farad"), // capacitance
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "katal"), // catalytic_activity
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "katal per cubic meter"
        ), // catalytic_activity_concentration
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "radian per meter"
        ), // curvature
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "square meter per second"
        ), // diffusion_coefficient
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "pascal second"
        ), // dynamic_viscosity
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "coulomb"), // electric_charge
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "coulomb per square meter"
        ), // electric_charge_areal_density
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "coulomb per meter"
        ), // electric_charge_linear_density
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "coulomb per cubic meter"
        ), // electric_charge_volumetric_density
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "ampere"), // electric_current
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "ampere per square meter"
        ), // electric_current_density
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "coulomb meter"
        ), // electric_dipole_moment
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "coulomb per square meter"
        ), // electric_displacement_field
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "volt per meter"
        ), // electric_field
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "volt meter"
        ), // electric_flux
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "farad per meter"
        ), // electric_permittivity
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "volt"), // electric_potential
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "coulomb square meter"
        ), // electric_quadrupole_moment
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "siemens"), // electrical_conductance
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "siemens per meter"
        ), // electrical_conductivity
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "square meter per volt second"
        ), // electrical_mobility
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "ohm"), // electrical_resistance
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "ohm meter"
        ), // electrical_resistivity
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "joule"), // energy
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "newton"), // force
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "hertz"), // frequency
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "hertz per second"
        ), // frequency_drift
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "joule per kelvin"
        ), // heat_capacity
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "watt per square meter"
        ), // heat_flux_density
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "watt per square meter kelvin"
        ), // heat_transfer
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "henry"), // inductance
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "byte"), // information
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "byte per second"
        ), // information_rate
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "second per meter"
        ), // inverse_velocity
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "meter per second cubed"
        ), // jerk
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "meter"), // length
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "state per meter joule"
        ), // linear_density_of_states
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "kilogram per meter"
        ), // linear_mass_density
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "per meter"
        ), // linear_number_density
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "per meter second"
        ), // linear_number_rate
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "watt per meter"
        ), // linear_power_density
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "candela per square meter"
        ), // luminance
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "candela"), // luminous_intensity
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "ampere per meter"
        ), // magnetic_field_strength
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "weber"), // magnetic_flux
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "tesla"), // magnetic_flux_density
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "ampere square meter"
        ), // magnetic_moment
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "henry per meter"
        ), // magnetic_permeability
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "kilogram"
        ), // mass
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "kilogram per cubic meter"
        ), // mass_concentration
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "kilogram per cubic meter"
        ), // mass_density
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "kilogram per square meter second"
        ), // mass_flux
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "kilogram per joule"
        ), // mass_per_energy
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "kilogram per second"
        ), // mass_rate
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "mole per kilogram"
        ), // molality
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "mole per cubic meter"
        ), // molar_concentration
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "joule per mole"
        ), // molar_energy
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "mole per square meter second"
        ), // molar_flux
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "joule per kelvin mole"
        ), // molar_heat_capacity
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "kilogram per mole"
        ), // molar_mass
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "becquerel per mole"
        ), // molar_radioactivity
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "cubic meter per mole"
        ), // molar_volume
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "kilogram square meter"
        ), // moment_of_inertia
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "kilogram meter per second"
        ), // momentum
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "watt"),  // power
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "watt per second"
        ), // power_rate
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "pascal"), // pressure
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "joule per square meter"
        ), // radiant_exposure
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "becquerel"
        ), // radioactivity
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "number"), // ratio
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "reciprocal meter"
        ), // reciprocal_length
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "steradian"
        ), // solid_angle
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "square meter per kilogram"
        ), // specific_area
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "joule per kilogram kelvin"
        ), // specific_heat_capacity
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "watt per kilogram"
        ), // specific_power
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "becquerel per kilogram"
        ), // specific_radioactivity
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "cubic meter per kilogram"
        ), // specific_volume
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "ampere per meter"
        ), // surface_electric_current_density
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "per kelvin"
        ), // temperature_coefficient
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "kelvin per meter"
        ), // temperature_gradient
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "kelvin"), // temperature_interval
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "watt per kelvin"
        ), // thermal_conductance
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "watt per meter kelvin"
        ), // thermal_conductivity
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "kelvin"), // thermodynamic_temperature
        get_base_unit!(unit_names_to_abbreviations, dimension_base_units, "second"), // time
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "newton meter"
        ), // torque
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "meter per second"
        ), // velocity
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "cubic meter"
        ), // volume
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "cubic meter per second"
        ), // volume_rate
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "state per cubic meter joule"
        ), // volumetric_density_of_states
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "joule per cubic meter kelvin"
        ), // volumetric_heat_capacity
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "per cubic meter"
        ), // volumetric_number_density
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "per cubic meter second"
        ), // volumetric_number_rate
        get_base_unit!(
            unit_names_to_abbreviations,
            dimension_base_units,
            "watt per cubic meter"
        ), // volumetric_power_density
    ]);

    let mut unit_list: UnitList = unit_list.into_iter().collect();
    unit_list.sort_by(|(key_a, _list_a), (key_b, _list_b)| key_a.cmp(key_b));

    // Conversion factors has some constants in it, but there's no way to represent those constants in a CSV file, so we'll just have to
    // insert this warning suppression at the start of the generated file.
    let conversion_factor_file_path: std::path::PathBuf = [
        std::env::var("OUT_DIR").unwrap(),
        "conversion_factors.rs".into(),
    ]
    .iter()
    .collect();
    let mut conversion_factor_file = File::create(conversion_factor_file_path).unwrap();
    writeln!(
        &mut conversion_factor_file,
        "#[allow(clippy::approx_constant)]"
    )
    .unwrap();
    uneval::write(conversion_factors, conversion_factor_file).unwrap();

    uneval::to_out_dir(dimension_names, "dimension_names.rs").unwrap();
    uneval::to_out_dir(unit_list, "unit_list.rs").unwrap();
    uneval::to_out_dir(base_units, "base_units.rs").unwrap();

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/script/execution/types/measurement/units.csv");
    println!("cargo:rerun-if-changed=crates/common_data_types/src/lib.rs");
}

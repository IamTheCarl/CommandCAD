use serde::Deserialize;

use common_data_types::{
    ConversionFactor, ConversionFactorDatabase, Dimension, DimensionNameDatabase, RatioTypeHint,
};

#[derive(Deserialize)]
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

fn main() {
    let mut source_data =
        csv::Reader::from_path("src/script/execution/types/measurement/units.csv").unwrap();

    let mut conversion_factors = ConversionFactorDatabase::new();
    let mut dimension_names = DimensionNameDatabase::new();

    for result in source_data.deserialize() {
        let row: Row = result.unwrap();

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

        conversion_factors.insert(
            row.keyboard_friendly_abbreviation,
            ConversionFactor {
                constant: row.conversion_constant,
                coefficient: row.conversion_coefficient,
                dimension,
            },
        );

        dimension_names.insert(dimension, row.dimension_name.into());
    }

    uneval::to_out_dir(conversion_factors, "conversion_factors.rs").unwrap();
    uneval::to_out_dir(dimension_names, "dimension_names.rs").unwrap();

    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/script/execution/types/measurement/units.csv");
    println!("cargo:rerun-if-changed=crates/command_cad_types/src/lib.rs");
}

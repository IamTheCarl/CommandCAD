use std::{collections::HashMap, fs::File};

use serde::Deserialize;

use common_data_types::{
    ConversionFactor, ConversionFactorDatabase, Dimension, DimensionNameDatabase, RatioTypeHint,
    UnitDescription, UnitList,
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
    pub pixel_kind: bool,
    pub singular: String,
    pub plural: String,
    pub abbreviation: String,
    pub keyboard_friendly_abbreviation: String,
    pub conversion_coefficient: f64,
    pub conversion_constant: f64,
}

fn main() {
    println!("cargo:rerun-if-changed=src/units.csv");

    let mut source_data = csv::ReaderBuilder::new()
        .flexible(false)
        .has_headers(true)
        .from_path("src/units.csv")
        .unwrap();

    let mut conversion_factors = ConversionFactorDatabase::new();
    let mut dimension_names = DimensionNameDatabase::new();
    let mut unit_list: HashMap<String, Vec<UnitDescription>> = HashMap::new();

    let mut unit_dimensions: HashMap<String, Dimension> = HashMap::new();
    let mut unit_names_to_abbreviations: HashMap<String, String> = HashMap::new();
    let mut base_units: HashMap<Dimension, String> = HashMap::new();

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
        ratio_type_hint.set_is_pixel(row.pixel_kind);

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

        unit_dimensions.insert(row.singular.clone(), dimension);

        // Record conversion factor.
        let already_exists = conversion_factors
            .insert(
                row.keyboard_friendly_abbreviation.clone(),
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
        dimension_names.insert(dimension, row.dimension_name.clone());

        // This is a base unit.
        if row.conversion_coefficient == 1.0 {
            base_units.insert(dimension, row.abbreviation.clone());
        }

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
    let conversion_factor_file = File::create(conversion_factor_file_path).unwrap();
    uneval::write(conversion_factors, conversion_factor_file).unwrap();

    uneval::to_out_dir(dimension_names, "dimension_names.rs").unwrap();
    uneval::to_out_dir(unit_list, "unit_list.rs").unwrap();
    uneval::to_out_dir(base_units, "base_units.rs").unwrap();
}

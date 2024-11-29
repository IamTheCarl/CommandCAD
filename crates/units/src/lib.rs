use common_data_types::{
    ConversionFactor, ConversionFactorDatabase, Dimension, DimensionNameDatabase, RatioTypeHint,
    UnitDescription, UnitList, BaseUnits
};
use std::{borrow::Borrow, sync::OnceLock};

pub fn get_conversion_factor(name: &str) -> Option<&'static ConversionFactor> {
    static CONVERSION_FACTORS: OnceLock<ConversionFactorDatabase> = OnceLock::new();
    let database = CONVERSION_FACTORS
        .get_or_init(|| include!(concat!(env!("OUT_DIR"), "/conversion_factors.rs")));

    database.get(name)
}

pub fn get_dimension_name(dimension: impl AsRef<Dimension>) -> Option<&'static str> {
    static DIMENSIONS: OnceLock<DimensionNameDatabase> = OnceLock::new();
    let database =
        DIMENSIONS.get_or_init(|| include!(concat!(env!("OUT_DIR"), "/dimension_names.rs")));

    database.get(dimension.as_ref()).map(|cow| cow.borrow())
}

pub fn get_unit_list() -> &'static Vec<(String, Vec<UnitDescription>)> {
    static UNIT_LIST: OnceLock<UnitList> = OnceLock::new();
    let list = UNIT_LIST.get_or_init(|| include!(concat!(env!("OUT_DIR"), "/unit_list.rs")));

    list
}

pub fn get_base_unit_name(dimension: impl AsRef<Dimension>) -> Option<&'static str> {
    static BASE_UNITS: OnceLock<BaseUnits> = OnceLock::new();
    let database = BASE_UNITS.get_or_init(|| include!(concat!(env!("OUT_DIR"), "/base_units.rs")));

    database.get(dimension.as_ref()).map(|cow| cow.borrow())
}

use common_data_types::{
    BaseUnits, ConversionFactor, ConversionFactorDatabase, Dimension, DimensionNameDatabase,
    RatioTypeHint, UnitDescription, UnitList,
};
use std::{
    borrow::{Borrow, Cow},
    sync::OnceLock,
};

// One conversion factors approximates f64::consts::TAU.
#[allow(clippy::approx_constant)]
pub fn get_conversion_factor(name: &str) -> Option<&'static ConversionFactor> {
    static CONVERSION_FACTORS: OnceLock<ConversionFactorDatabase> = OnceLock::new();
    let database = CONVERSION_FACTORS
        .get_or_init(|| include!(concat!(env!("OUT_DIR"), "/conversion_factors.rs")));

    database.get(name)
}

fn get_named_dimensions() -> &'static DimensionNameDatabase {
    static DIMENSIONS: OnceLock<DimensionNameDatabase> = OnceLock::new();
    DIMENSIONS.get_or_init(|| include!(concat!(env!("OUT_DIR"), "/dimension_names.rs")))
}

pub fn get_dimension_name(dimension: &Dimension) -> Cow<'static, str> {
    let database = get_named_dimensions();

    if let Some(name) = database.get(dimension) {
        Cow::Borrowed(name)
    } else {
        // This is a custom unit. We will need to generate a name to represent it.
        format!(
            "<L{}, M{}, T{}, I{}, Th{}, N{}, J{}, K{}>",
            dimension.length,
            dimension.mass,
            dimension.time,
            dimension.electric_current,
            dimension.thermodynamic_temprature,
            dimension.amount_of_substance,
            dimension.luminous_intensity,
            dimension.ratio_type_hint
        )
        .into()
    }
}

pub fn list_named_dimensions() -> impl Iterator<Item = (&'static str, Dimension)> {
    let database = get_named_dimensions();

    database
        .iter()
        .map(|(dimension, name)| (name.as_str(), *dimension))
}

pub fn get_unit_list() -> &'static Vec<(String, Vec<UnitDescription>)> {
    static UNIT_LIST: OnceLock<UnitList> = OnceLock::new();
    let list = UNIT_LIST.get_or_init(|| include!(concat!(env!("OUT_DIR"), "/unit_list.rs")));

    list
}

pub fn get_base_unit_name(dimension: &Dimension) -> Option<&'static str> {
    static BASE_UNITS: OnceLock<BaseUnits> = OnceLock::new();
    let database = BASE_UNITS.get_or_init(|| include!(concat!(env!("OUT_DIR"), "/base_units.rs")));

    let name = database.get(dimension).map(|cow| cow.borrow());

    match name {
        Some(name) => Some(name),
        Option::None if *dimension != Dimension::zero() => Some("?"),
        _ => Option::None,
    }
}

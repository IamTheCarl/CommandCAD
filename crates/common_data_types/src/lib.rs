use std::{borrow::Cow, collections::HashMap};

use paste::paste;
use serde::Serialize;

pub type ConversionFactorDatabase = HashMap<String, ConversionFactor>;
pub type DimensionNameDatabase = HashMap<Dimension, Cow<'static, str>>;

#[derive(Serialize, PartialEq, Eq, Hash, Clone, Copy, Default)]
pub struct RatioTypeHint(pub u8);

macro_rules! bit_getter_setter {
    ($mask:path, $name:ident) => {
        paste! {
            pub fn [< set_is_ $name >] (&mut self, value: bool) {
                if value {
                    self.0 |= $mask;
                } else {
                    self.0 &= !$mask;
                }
            }

            pub fn [< is_ $name >] (&self) -> bool {
                self.0 & $mask != 0
            }
        }
    };
}

impl RatioTypeHint {
    const ANGLE_KIND_MASK: u8 = 0x01;
    const CONSTITUENT_CONCENTRATION_MASK: u8 = 0x02;
    const INFORMATION_MASK: u8 = 0x04;
    const SOLID_ANGLE_MASK: u8 = 0x08;
    const TEMPRATURE_MASK: u8 = 0x10;

    bit_getter_setter!(Self::ANGLE_KIND_MASK, angle);
    bit_getter_setter!(
        Self::CONSTITUENT_CONCENTRATION_MASK,
        constituent_concentration
    );
    bit_getter_setter!(Self::INFORMATION_MASK, information);
    bit_getter_setter!(Self::SOLID_ANGLE_MASK, solid_angle);
    bit_getter_setter!(Self::TEMPRATURE_MASK, temperature);
}

impl std::fmt::Debug for RatioTypeHint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RatioTypeHint")
            .field("is_angle", &self.is_angle())
            .field(
                "is_constituent_concentration",
                &self.is_constituent_concentration(),
            )
            .field("is_information", &self.is_information())
            .field("is_solid_angle", &self.is_solid_angle())
            .field("is_temperature", &self.is_temperature())
            .finish()
    }
}

#[derive(Debug, Serialize, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Dimension {
    // Meter
    pub length: i8,

    // Kilogram
    pub mass: i8,

    // Second
    pub time: i8,

    // Apere
    pub electric_current: i8,

    // Kelvin
    pub thermodynamic_temprature: i8,

    // Mole
    pub amount_of_substance: i8,

    // Candela
    pub luminous_intensity: i8,

    // Hints of type for ratios.
    pub ratio_type_hint: RatioTypeHint,
}

impl Dimension {
    pub fn is_zero_dimension(&self) -> bool {
        self.length == 0
            && self.mass == 0
            && self.time == 0
            && self.electric_current == 0
            && self.thermodynamic_temprature == 0
            && self.amount_of_substance == 0
            && self.luminous_intensity == 0
    }
}

#[derive(Debug, Serialize)]
pub struct ConversionFactor {
    pub constant: f64,
    pub coefficient: f64,
    pub dimension: Dimension,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn dimension_size() {
        assert_eq!(std::mem::size_of::<Dimension>(), 8);
    }

    #[test]
    fn ratio_type_hint_storage() {
        let mut hint = RatioTypeHint(0);

        assert!(!hint.is_angle());
        hint.set_is_angle(true);
        assert!(hint.is_angle());
        assert!(!hint.is_information());

        hint.set_is_information(true);
        assert!(hint.is_angle());
        assert!(hint.is_information());

        hint.set_is_angle(false);
        assert!(!hint.is_angle());
        assert!(hint.is_information());
    }
}

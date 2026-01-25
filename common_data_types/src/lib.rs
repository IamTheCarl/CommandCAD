use std::collections::HashMap;

use ordered_float::NotNan;
use paste::paste;
use serde::Serialize;

pub type RawFloat = f64;
pub type Float = NotNan<RawFloat>;
pub use ordered_float::{FloatIsNan, ParseNotNanError};
pub use std::f64::consts;

pub type ConversionFactorDatabase = HashMap<String, ConversionFactor>;
pub type DimensionNameDatabase = HashMap<Dimension, String>;
pub type UnitList = Vec<(String, Vec<UnitDescription>)>;
pub type BaseUnits = HashMap<Dimension, String>;

#[derive(Debug, Serialize)]
pub struct UnitDescription {
    pub abbreviation: String,
    pub keyboard_friendly_abbreviation: String,
    pub name: String,
    pub plural_name: String,
}

#[derive(Serialize, Hash, Eq, PartialEq, Clone, Copy, Default)]
pub struct DimensionBitSet(pub u8);

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

impl DimensionBitSet {
    const ANGLE_KIND_MASK: u8 = 0x01;
    const CONSTITUENT_CONCENTRATION_MASK: u8 = 0x02;
    const INFORMATION_MASK: u8 = 0x04;
    const SOLID_ANGLE_MASK: u8 = 0x08;
    const TEMPRATURE_MASK: u8 = 0x10;
    const PIXEL_MASK: u8 = 0x20;
    const INHERIT_MASK: u8 = 0x40;

    bit_getter_setter!(Self::ANGLE_KIND_MASK, angle);
    bit_getter_setter!(
        Self::CONSTITUENT_CONCENTRATION_MASK,
        constituent_concentration
    );
    bit_getter_setter!(Self::INFORMATION_MASK, information);
    bit_getter_setter!(Self::SOLID_ANGLE_MASK, solid_angle);
    bit_getter_setter!(Self::TEMPRATURE_MASK, temperature);
    bit_getter_setter!(Self::PIXEL_MASK, pixel);
    bit_getter_setter!(Self::INHERIT_MASK, inherit);
}

impl std::fmt::Display for DimensionBitSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_angle() {
            write!(f, "-ANGLE")?;
        }

        if self.is_constituent_concentration() {
            write!(f, "-CONSTITUENT_CONCENTRATION")?;
        }

        if self.is_information() {
            write!(f, "-INFORMATION")?;
        }

        if self.is_solid_angle() {
            write!(f, "-SOLID_ANGLE")?;
        }

        if self.is_temperature() {
            write!(f, "-TEMPRATURE")?;
        }

        if self.is_pixel() {
            write!(f, "-PIXEL")?;
        }

        Ok(())
    }
}

impl std::ops::BitOr for DimensionBitSet {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        let mut new = Self(self.0 | rhs.0);
        new.set_is_inherit(false);

        new
    }
}

impl std::fmt::Debug for DimensionBitSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DimensionBitSet")
            .field("is_angle", &self.is_angle())
            .field(
                "is_constituent_concentration",
                &self.is_constituent_concentration(),
            )
            .field("is_information", &self.is_information())
            .field("is_solid_angle", &self.is_solid_angle())
            .field("is_temperature", &self.is_temperature())
            .field("is_pixel", &self.is_pixel())
            .field("is_inherit", &self.is_inherit())
            .finish()
    }
}

#[derive(Debug, Serialize, Hash, Clone, Copy)]
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

    // Boolean aspects of a dimension.
    pub bitset: DimensionBitSet,
}

impl Eq for Dimension {}

impl PartialEq for Dimension {
    fn eq(&self, rhs: &Self) -> bool {
        self.bitset.is_inherit()
            || rhs.bitset.is_inherit()
            || (self.length == rhs.length
                && self.mass == rhs.mass
                && self.time == rhs.time
                && self.electric_current == rhs.electric_current
                && self.thermodynamic_temprature == rhs.thermodynamic_temprature
                && self.amount_of_substance == rhs.amount_of_substance
                && self.luminous_intensity == rhs.luminous_intensity
                && self.bitset == rhs.bitset)
    }
}

impl std::ops::Add for Dimension {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            length: self.length + rhs.length,
            mass: self.mass + rhs.mass,
            time: self.time + rhs.time,
            electric_current: self.electric_current + rhs.electric_current,
            thermodynamic_temprature: self.thermodynamic_temprature + rhs.thermodynamic_temprature,
            amount_of_substance: self.amount_of_substance + rhs.amount_of_substance,
            luminous_intensity: self.luminous_intensity + rhs.luminous_intensity,
            bitset: self.bitset | rhs.bitset,
        }
    }
}

impl std::ops::Sub for Dimension {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            length: self.length - rhs.length,
            mass: self.mass - rhs.mass,
            time: self.time - rhs.time,
            electric_current: self.electric_current - rhs.electric_current,
            thermodynamic_temprature: self.thermodynamic_temprature - rhs.thermodynamic_temprature,
            amount_of_substance: self.amount_of_substance - rhs.amount_of_substance,
            luminous_intensity: self.luminous_intensity - rhs.luminous_intensity,
            bitset: self.bitset | rhs.bitset,
        }
    }
}

impl std::ops::Neg for Dimension {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self {
            length: -self.length,
            mass: -self.mass,
            time: -self.time,
            electric_current: -self.electric_current,
            thermodynamic_temprature: -self.thermodynamic_temprature,
            amount_of_substance: -self.amount_of_substance,
            luminous_intensity: -self.luminous_intensity,
            bitset: self.bitset,
        }
    }
}

impl std::ops::BitOr for Dimension {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        match (self.bitset.is_inherit(), rhs.bitset.is_inherit()) {
            (true, false) => Self {
                length: self.length,
                mass: self.mass,
                time: self.time,
                electric_current: self.electric_current,
                thermodynamic_temprature: self.thermodynamic_temprature,
                amount_of_substance: self.amount_of_substance,
                luminous_intensity: self.luminous_intensity,
                bitset: self.bitset | rhs.bitset,
            },
            (false, true) => Self {
                length: rhs.length,
                mass: rhs.mass,
                time: rhs.time,
                electric_current: rhs.electric_current,
                thermodynamic_temprature: rhs.thermodynamic_temprature,
                amount_of_substance: rhs.amount_of_substance,
                luminous_intensity: rhs.luminous_intensity,
                bitset: self.bitset | rhs.bitset,
            },
            (_, _) => self,
        }
    }
}

impl std::ops::Mul<i8> for Dimension {
    type Output = Self;

    fn mul(self, rhs: i8) -> Self::Output {
        Self {
            length: self.length * rhs,
            mass: self.mass * rhs,
            time: self.time * rhs,
            electric_current: self.electric_current * rhs,
            thermodynamic_temprature: self.thermodynamic_temprature * rhs,
            amount_of_substance: self.amount_of_substance * rhs,
            luminous_intensity: self.luminous_intensity * rhs,
            bitset: self.bitset,
        }
    }
}

impl std::ops::Div<i8> for Dimension {
    type Output = Self;

    fn div(self, rhs: i8) -> Self::Output {
        Self {
            length: self.length / rhs,
            mass: self.mass / rhs,
            time: self.time / rhs,
            electric_current: self.electric_current / rhs,
            thermodynamic_temprature: self.thermodynamic_temprature / rhs,
            amount_of_substance: self.amount_of_substance / rhs,
            luminous_intensity: self.luminous_intensity / rhs,
            bitset: self.bitset,
        }
    }
}

impl Dimension {
    pub const fn zero() -> Self {
        Self {
            length: 0,
            mass: 0,
            time: 0,
            electric_current: 0,
            thermodynamic_temprature: 0,
            amount_of_substance: 0,
            luminous_intensity: 0,
            bitset: DimensionBitSet(0),
        }
    }

    pub const fn angle() -> Self {
        Self {
            length: 0,
            mass: 0,
            time: 0,
            electric_current: 0,
            thermodynamic_temprature: 0,
            amount_of_substance: 0,
            luminous_intensity: 0,
            bitset: DimensionBitSet(DimensionBitSet::ANGLE_KIND_MASK),
        }
    }

    pub const fn inherit() -> Self {
        Self {
            length: 0,
            mass: 0,
            time: 0,
            electric_current: 0,
            thermodynamic_temprature: 0,
            amount_of_substance: 0,
            luminous_intensity: 0,
            bitset: DimensionBitSet(DimensionBitSet::INHERIT_MASK),
        }
    }

    pub const fn length() -> Self {
        Self {
            length: 1,
            mass: 0,
            time: 0,
            electric_current: 0,
            thermodynamic_temprature: 0,
            amount_of_substance: 0,
            luminous_intensity: 0,
            bitset: DimensionBitSet(0),
        }
    }

    pub const fn area() -> Self {
        Self {
            length: 2,
            mass: 0,
            time: 0,
            electric_current: 0,
            thermodynamic_temprature: 0,
            amount_of_substance: 0,
            luminous_intensity: 0,
            bitset: DimensionBitSet(0),
        }
    }

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

impl ConversionFactor {
    pub fn convert_to_base_unit(&self, input: Float) -> Float {
        input * self.coefficient + self.constant
    }

    pub fn convert_from_base_unit(&self, input: Float) -> Float {
        (input - self.constant) / self.coefficient
    }
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
        let mut hint = DimensionBitSet(0);

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

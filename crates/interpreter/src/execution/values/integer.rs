/*
 * Copyright 2025 James Carl
 * AGPL-3.0-only or AGPL-3.0-or-later
 *
 * This file is part of Command Cad.
 *
 * Command CAD is free software: you can redistribute it and/or modify it under the terms of
 * the GNU Affero General Public License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License along with this
 * program. If not, see <https://www.gnu.org/licenses/>.
 */

use enum_downcast::AsVariant;
use num_traits::{
    pow::checked_pow, CheckedAdd, CheckedDiv, CheckedMul, CheckedSub, One, ToPrimitive,
};
use std::{
    cmp::Ordering,
    hash::Hash,
    ops::{BitAnd, BitOr, BitXor, Not, Shl, Shr},
};

use crate::{
    compile::SourceReference,
    execution::{
        errors::{ExpressionResult, GenericFailure, Raise},
        logging::RuntimeLog,
        values::{closure::BuiltinCallableDatabase, StaticType},
    },
};

use super::{value_type::ValueType, Object, StaticTypeName, Value};

#[derive(Debug, Hash, Clone, Copy, Eq, PartialEq)]
pub struct Integer<I>(pub I);

impl<I> From<I> for Integer<I> {
    fn from(value: I) -> Self {
        Self(value)
    }
}

impl<I> Object for Integer<I>
where
    I: IntOps,
    Self: StaticTypeName + Into<Value>,
    Value: AsVariant<Self>,
{
    fn get_type(&self, _callable_database: &BuiltinCallableDatabase) -> ValueType {
        I::static_type()
    }

    fn bit_and(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self(self.0 & rhs.0).into())
    }
    fn bit_or(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self(self.0 | rhs.0).into())
    }
    fn bit_xor(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self(self.0 ^ rhs.0).into())
    }

    fn cmp(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Ordering> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(self.0.cmp(&rhs.0))
    }
    fn addition(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self(self.0.checked_add(&rhs.0).ok_or_else(|| {
            GenericFailure(
                "Integer overflow: The computed value is too large to store in the integer",
            )
            .to_error(stack_trace)
        })?)
        .into())
    }
    fn subtraction(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self(self.0.checked_sub(&rhs.0).ok_or_else(|| {
            GenericFailure(
                "Integer underflow: The computed value is too small to store in the integer",
            )
            .to_error(stack_trace)
        })?)
        .into())
    }
    fn multiply(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self(self.0.checked_mul(&rhs.0).ok_or_else(|| {
            GenericFailure(
                "Integer overflow: The computed value is too large to store in the integer",
            )
            .to_error(stack_trace)
        })?)
        .into())
    }
    fn divide(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self(
            self.0
                .checked_div(&rhs.0)
                .ok_or_else(|| GenericFailure("The computed value is either too large to store in the integer or you attempted to divide by zero").to_error(stack_trace))?,
        )
        .into())
    }
    fn floor_divide(
        self,
        log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        self.divide(log, stack_trace, rhs)
    }
    fn exponent(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;

        // This failure can only happen on 32bit (or less) systems.
        let rhs = rhs.0.to_usize().ok_or_else(|| {
            GenericFailure(
                "Integer overflow: The requested exponent is larger than the host machine word size",
            )
            .to_error(stack_trace)
        })?;

        Ok(Self(checked_pow(self.0, rhs).ok_or_else(|| {
            GenericFailure(
                "Integer overflow: The computed value is too large to store in the integer",
            )
            .to_error(stack_trace)
        })?)
        .into())
    }
    fn left_shift(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self(self.0 << rhs.0).into())
    }
    fn right_shift(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self(self.0 >> rhs.0).into())
    }
    fn unary_plus(
        self,
        _log: &mut dyn RuntimeLog,
        _stack_trace: &[SourceReference],
    ) -> ExpressionResult<Value> {
        Ok(self.clone().into())
    }
    fn unary_minus(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
    ) -> ExpressionResult<Value> {
        self.0.neg(stack_trace).into()
    }
    fn unary_not(
        self,
        _log: &mut dyn RuntimeLog,
        _stack_trace: &[SourceReference],
    ) -> ExpressionResult<Value> {
        Ok(Self(!self.0).into())
    }
}

impl<I> StaticType for Integer<I>
where
    I: StaticType,
{
    fn static_type() -> ValueType {
        I::static_type()
    }
}

trait IntOps:
    CheckedAdd
    + CheckedDiv
    + CheckedMul
    + CheckedSub
    + CheckedAdd
    + Ord
    + Hash
    + Copy
    + Eq
    + BitAnd<Output = Self>
    + BitOr<Output = Self>
    + BitXor<Output = Self>
    + Shl<Output = Self>
    + Shr<Output = Self>
    + Not<Output = Self>
    + One
    + ToPrimitive
    + StaticType
{
    fn neg(&self, stack_trace: &[SourceReference]) -> ExpressionResult<Value>;

    fn count_ones(&self) -> u64;
    fn count_zeros(&self) -> u64;

    fn leading_zeros(&self) -> u64;
    fn trailing_zeros(&self) -> u64;

    fn leading_ones(&self) -> u64;
    fn trailing_ones(&self) -> u64;

    fn rotate_left(&self, by: u64) -> Self;
    fn rotate_right(&self, by: u64) -> Self;

    fn reverse_bits(&self) -> Self;

    fn abs(&self) -> Self;
    fn sqrt(&self) -> Self;

    fn abs_diff(&self, rhs: Self) -> u64;
    fn signum(&self) -> i64;
    fn is_positive(&self) -> bool;
    fn is_negative(&self) -> bool;
    fn midpoint(&self, rhs: Self) -> Self;
}

// TODO cast_signed and cast_unsigned

impl IntOps for i64 {
    fn neg(&self, _stack_trace: &[SourceReference]) -> ExpressionResult<Value> {
        Ok(SignedInteger::from(-self).into())
    }

    fn count_ones(&self) -> u64 {
        i64::count_ones(*self) as u64
    }

    fn count_zeros(&self) -> u64 {
        i64::count_zeros(*self) as u64
    }

    fn leading_zeros(&self) -> u64 {
        i64::leading_zeros(*self) as u64
    }

    fn trailing_zeros(&self) -> u64 {
        i64::trailing_zeros(*self) as u64
    }

    fn leading_ones(&self) -> u64 {
        i64::leading_ones(*self) as u64
    }

    fn trailing_ones(&self) -> u64 {
        i64::trailing_ones(*self) as u64
    }

    fn rotate_left(&self, by: u64) -> Self {
        i64::rotate_left(*self, by.min(u32::MIN as u64) as u32)
    }

    fn rotate_right(&self, by: u64) -> Self {
        i64::rotate_right(*self, by.min(u32::MIN as u64) as u32)
    }

    fn reverse_bits(&self) -> Self {
        i64::reverse_bits(*self)
    }

    fn abs(&self) -> Self {
        i64::abs(*self)
    }

    fn sqrt(&self) -> Self {
        i64::isqrt(*self)
    }

    fn abs_diff(&self, rhs: Self) -> u64 {
        i64::abs_diff(*self, rhs)
    }

    fn signum(&self) -> i64 {
        i64::signum(*self)
    }

    fn is_positive(&self) -> bool {
        i64::is_positive(*self)
    }

    fn is_negative(&self) -> bool {
        i64::is_negative(*self)
    }

    fn midpoint(&self, rhs: Self) -> Self {
        i64::midpoint(*self, rhs)
    }
}

impl StaticTypeName for Integer<i64> {
    fn static_type_name() -> &'static str {
        "Signed Integer"
    }
}

impl StaticType for i64 {
    fn static_type() -> ValueType {
        ValueType::SignedInteger
    }
}

impl IntOps for u64 {
    fn neg(&self, stack_trace: &[SourceReference]) -> ExpressionResult<Value> {
        Err(super::UnsupportedOperationError {
            type_name: UnsignedInteger::static_type_name().into(),
            operation_name: "negate",
        }
        .to_error(stack_trace))
    }

    fn count_ones(&self) -> u64 {
        u64::count_ones(*self) as u64
    }

    fn count_zeros(&self) -> u64 {
        u64::count_zeros(*self) as u64
    }

    fn leading_zeros(&self) -> u64 {
        u64::leading_zeros(*self) as u64
    }

    fn trailing_zeros(&self) -> u64 {
        u64::trailing_zeros(*self) as u64
    }

    fn leading_ones(&self) -> u64 {
        u64::leading_ones(*self) as u64
    }

    fn trailing_ones(&self) -> u64 {
        u64::trailing_ones(*self) as u64
    }

    fn rotate_left(&self, by: u64) -> Self {
        u64::rotate_left(*self, by.min(u32::MIN as u64) as u32)
    }

    fn rotate_right(&self, by: u64) -> Self {
        u64::rotate_right(*self, by.min(u32::MIN as u64) as u32)
    }

    fn reverse_bits(&self) -> Self {
        u64::reverse_bits(*self)
    }

    fn abs(&self) -> Self {
        *self
    }

    fn sqrt(&self) -> Self {
        u64::isqrt(*self)
    }

    fn abs_diff(&self, rhs: Self) -> u64 {
        u64::abs_diff(*self, rhs)
    }

    fn signum(&self) -> i64 {
        if *self > 0 {
            1
        } else {
            0
        }
    }

    fn is_positive(&self) -> bool {
        *self > 0
    }

    fn is_negative(&self) -> bool {
        false
    }

    fn midpoint(&self, rhs: Self) -> Self {
        u64::midpoint(*self, rhs)
    }
}

impl StaticTypeName for Integer<u64> {
    fn static_type_name() -> &'static str {
        "Unsigned Integer"
    }
}

impl StaticType for u64 {
    fn static_type() -> ValueType {
        ValueType::UnsignedInteger
    }
}

pub type SignedInteger = Integer<i64>;
pub type UnsignedInteger = Integer<u64>;

pub fn register_methods(database: &mut BuiltinCallableDatabase) {}

#[cfg(test)]
mod test {
    use crate::execution::{test_run, values::Boolean};

    use super::*;

    #[test]
    fn signed_bit_or() {
        let product = test_run("0xAAi | 0x55i").unwrap();
        assert_eq!(product, SignedInteger::from(0xFF).into());
    }

    #[test]
    fn signed_bit_and() {
        let product = test_run("0xFFi & 0x0Fi").unwrap();
        assert_eq!(product, SignedInteger::from(0x0F).into());
    }

    #[test]
    fn signed_bit_xor() {
        let product = test_run("0xF0i ^ 0xFFi").unwrap();
        assert_eq!(product, SignedInteger::from(0x0F).into());
    }

    #[test]
    fn signed_cmp_greater_than() {
        let product = test_run("3i > 2i").unwrap();
        assert_eq!(product, Boolean(true).into());
        let product = test_run("2i > 3i").unwrap();
        assert_eq!(product, Boolean(false).into());
    }

    #[test]
    fn signed_cmp_greater_than_eq() {
        let product = test_run("3i >= 2i").unwrap();
        assert_eq!(product, Boolean(true).into());
        let product = test_run("3i >= 3i").unwrap();
        assert_eq!(product, Boolean(true).into());
        let product = test_run("2i >= 3i").unwrap();
        assert_eq!(product, Boolean(false).into());
    }

    #[test]
    fn signed_cmp_eq() {
        let product = test_run("3i == 3i").unwrap();
        assert_eq!(product, Boolean(true).into());
        let product = test_run("3i == 2i").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("3i != 3i").unwrap();
        assert_eq!(product, Boolean(false).into());
        let product = test_run("3i != 2i").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_cmp_less_than_eq() {
        let product = test_run("3i <= 2i").unwrap();
        assert_eq!(product, Boolean(false).into());
        let product = test_run("3i <= 3i").unwrap();
        assert_eq!(product, Boolean(true).into());
        let product = test_run("2i <= 3i").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_cmp_less_than() {
        let product = test_run("3i < 2i").unwrap();
        assert_eq!(product, Boolean(false).into());
        let product = test_run("2i < 3i").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_addition() {
        let product = test_run("3i + 2i").unwrap();
        assert_eq!(product, SignedInteger::from(5).into());
    }

    #[test]
    fn signed_subtraction() {
        let product = test_run("3i - 2i").unwrap();
        assert_eq!(product, SignedInteger::from(1).into());
    }

    #[test]
    fn signed_multiply() {
        let product = test_run("3i * 2i").unwrap();
        assert_eq!(product, SignedInteger::from(6).into());
    }

    #[test]
    fn signed_divide() {
        let product = test_run("6i / 2i").unwrap();
        assert_eq!(product, SignedInteger::from(3).into());
    }

    #[test]
    fn signed_floor_divide() {
        let product = test_run("6i // 2i").unwrap();
        assert_eq!(product, SignedInteger::from(3).into());
    }

    #[test]
    fn signed_exponent() {
        let product = test_run("6i ** 3i").unwrap();
        assert_eq!(product, SignedInteger::from(216).into());
    }

    #[test]
    fn signed_shift_left() {
        let product = test_run("0x0Fi << 4i").unwrap();
        assert_eq!(product, SignedInteger::from(0xF0).into());
    }

    #[test]
    fn signed_shift_right() {
        let product = test_run("0xF0i >> 4i").unwrap();
        assert_eq!(product, SignedInteger::from(0x0F).into());
    }

    #[test]
    fn signed_unary_plus() {
        let product = test_run("+3i").unwrap();
        assert_eq!(product, SignedInteger::from(3).into());
    }

    #[test]
    fn signed_unary_minus() {
        let product = test_run("-3i").unwrap();
        assert_eq!(product, SignedInteger::from(-3).into());
    }

    #[test]
    fn signed_unary_bit_not() {
        let product = test_run("!0xAAi").unwrap();
        assert_eq!(product, SignedInteger::from(!0xAA).into());
    }

    #[test]
    fn unsigned_bit_or() {
        let product = test_run("0xAAu | 0x55u").unwrap();
        assert_eq!(product, UnsignedInteger::from(0xFF).into());
    }

    #[test]
    fn unsigned_bit_and() {
        let product = test_run("0xFFu & 0x0Fu").unwrap();
        assert_eq!(product, UnsignedInteger::from(0x0F).into());
    }

    #[test]
    fn unsigned_bit_xor() {
        let product = test_run("0xF0u ^ 0xFFu").unwrap();
        assert_eq!(product, UnsignedInteger::from(0x0F).into());
    }

    #[test]
    fn unsigned_cmp_greater_than() {
        let product = test_run("3u > 2u").unwrap();
        assert_eq!(product, Boolean(true).into());
        let product = test_run("2u > 3u").unwrap();
        assert_eq!(product, Boolean(false).into());
    }

    #[test]
    fn unsigned_cmp_greater_than_eq() {
        let product = test_run("3u >= 2u").unwrap();
        assert_eq!(product, Boolean(true).into());
        let product = test_run("3u >= 3u").unwrap();
        assert_eq!(product, Boolean(true).into());
        let product = test_run("2u >= 3u").unwrap();
        assert_eq!(product, Boolean(false).into());
    }

    #[test]
    fn unsigned_cmp_eq() {
        let product = test_run("3u == 3u").unwrap();
        assert_eq!(product, Boolean(true).into());
        let product = test_run("3u == 2u").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("3u != 3u").unwrap();
        assert_eq!(product, Boolean(false).into());
        let product = test_run("3u != 2u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_cmp_less_than_eq() {
        let product = test_run("3u <= 2u").unwrap();
        assert_eq!(product, Boolean(false).into());
        let product = test_run("3u <= 3u").unwrap();
        assert_eq!(product, Boolean(true).into());
        let product = test_run("2u <= 3u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_cmp_less_than() {
        let product = test_run("3u < 2u").unwrap();
        assert_eq!(product, Boolean(false).into());
        let product = test_run("2u < 3u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_addition() {
        let product = test_run("3u + 2u").unwrap();
        assert_eq!(product, UnsignedInteger::from(5).into());
    }

    #[test]
    fn unsigned_subtraction() {
        let product = test_run("3u - 2u").unwrap();
        assert_eq!(product, UnsignedInteger::from(1).into());
    }

    #[test]
    fn unsigned_multiply() {
        let product = test_run("3u * 2u").unwrap();
        assert_eq!(product, UnsignedInteger::from(6).into());
    }

    #[test]
    fn unsigned_divide() {
        let product = test_run("6u / 2u").unwrap();
        assert_eq!(product, UnsignedInteger::from(3).into());
    }

    #[test]
    fn unsigned_floor_divide() {
        let product = test_run("6u // 2u").unwrap();
        assert_eq!(product, UnsignedInteger::from(3).into());
    }

    #[test]
    fn unsigned_exponent() {
        let product = test_run("6u ** 3u").unwrap();
        assert_eq!(product, UnsignedInteger::from(216).into());
    }

    #[test]
    fn unsigned_shift_left() {
        let product = test_run("0x0Fu << 4u").unwrap();
        assert_eq!(product, UnsignedInteger::from(0xF0).into());
    }

    #[test]
    fn unsigned_shift_right() {
        let product = test_run("0xF0u >> 4u").unwrap();
        assert_eq!(product, UnsignedInteger::from(0x0F).into());
    }

    #[test]
    fn unsigned_unary_plus() {
        let product = test_run("+3u").unwrap();
        assert_eq!(product, UnsignedInteger::from(3).into());
    }

    #[test]
    fn unsigned_unary_minus() {
        test_run("-3u").unwrap_err();
    }

    #[test]
    fn unsigned_unary_bit_not() {
        let product = test_run("!0xAAu").unwrap();
        assert_eq!(product, UnsignedInteger::from(!0xAA).into());
    }
}

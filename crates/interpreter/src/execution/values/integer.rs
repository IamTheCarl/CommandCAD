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
    borrow::Cow,
    cmp::Ordering,
    hash::Hash,
    ops::{BitAnd, BitOr, BitXor, Not, Shl, Shr},
};

use crate::execution::{
    errors::{ExpressionResult, GenericFailure, Raise},
    logging::{LogLevel, LogMessage, StackTrace},
    values::{
        closure::BuiltinCallableDatabase, integer::methods::MethodSet, string::formatting::Style,
        BuiltinFunction, MissingAttributeError, StaticType,
    },
    ExecutionContext,
};

use super::{value_type::ValueType, Object, StaticTypeName, Value};

#[derive(Debug, Hash, Clone, Copy, Eq, PartialEq)]
pub struct Integer<I>(pub I);

impl<I> From<I> for Integer<I> {
    fn from(value: I) -> Self {
        Self(value)
    }
}

const INTEGER_FORMAT_WARNING_MESSAGE: &'static str =
    "Integer formats such as Octal and Hex ignore precision";

impl<I> Object for Integer<I>
where
    I: IntOps,
    Self: StaticTypeName + Into<Value>,
    Value: AsVariant<Self>,
{
    fn get_type(&self, _context: &ExecutionContext) -> ValueType {
        I::static_type()
    }

    fn format(
        &self,
        context: &ExecutionContext,
        f: &mut dyn std::fmt::Write,
        style: Style,
        precision: Option<u8>,
    ) -> std::fmt::Result {
        match style {
            Style::Default => write!(f, "{}", self.0),
            Style::Debug => write!(f, "{}", self.0),
            Style::Octal => {
                if precision.is_some() {
                    context.log.push_message(LogMessage {
                        origin: context.stack_trace.bottom().clone(),
                        level: LogLevel::Warning,
                        message: INTEGER_FORMAT_WARNING_MESSAGE.into(),
                    });
                }
                write!(f, "{:o}", self.0)
            }
            Style::Hex => {
                if precision.is_some() {
                    context.log.push_message(LogMessage {
                        origin: context.stack_trace.bottom().clone(),
                        level: LogLevel::Warning,
                        message: INTEGER_FORMAT_WARNING_MESSAGE.into(),
                    });
                }
                write!(f, "{:x}", self.0)
            }
            Style::CapitalizedHex => {
                if precision.is_some() {
                    context.log.push_message(LogMessage {
                        origin: context.stack_trace.bottom().clone(),
                        level: LogLevel::Warning,
                        message: INTEGER_FORMAT_WARNING_MESSAGE.into(),
                    });
                }
                write!(f, "{:X}", self.0)
            }
            Style::Exponent => {
                write!(f, "{:e}", self.0)
            }
            Style::CapitalizedExponent => {
                write!(f, "{:E}", self.0)
            }
        }
    }

    fn bit_and(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(context.stack_trace)?;
        Ok(Self(self.0 & rhs.0).into())
    }
    fn bit_or(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(context.stack_trace)?;
        Ok(Self(self.0 | rhs.0).into())
    }
    fn bit_xor(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(context.stack_trace)?;
        Ok(Self(self.0 ^ rhs.0).into())
    }

    fn cmp(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Ordering> {
        let rhs: &Self = rhs.downcast_ref(context.stack_trace)?;
        Ok(self.0.cmp(&rhs.0))
    }
    fn addition(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(context.stack_trace)?;
        Ok(Self(self.0.checked_add(&rhs.0).ok_or_else(|| {
            GenericFailure(
                "Integer overflow: The computed value is too large to store in the integer".into(),
            )
            .to_error(context.stack_trace)
        })?)
        .into())
    }
    fn subtraction(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(context.stack_trace)?;
        Ok(Self(self.0.checked_sub(&rhs.0).ok_or_else(|| {
            GenericFailure(
                "Integer underflow: The computed value is too small to store in the integer".into(),
            )
            .to_error(context.stack_trace)
        })?)
        .into())
    }
    fn multiply(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(context.stack_trace)?;
        Ok(Self(self.0.checked_mul(&rhs.0).ok_or_else(|| {
            GenericFailure(
                "Integer overflow: The computed value is too large to store in the integer".into(),
            )
            .to_error(context.stack_trace)
        })?)
        .into())
    }
    fn divide(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(context.stack_trace)?;
        Ok(Self(
            self.0
                .checked_div(&rhs.0)
                .ok_or_else(|| GenericFailure("The computed value is either too large to store in the integer or you attempted to divide by zero".into()).to_error(context.stack_trace))?,
        )
        .into())
    }
    fn exponent(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(context.stack_trace)?;

        // This failure can only happen on 32bit (or less) systems.
        let rhs = rhs.0.to_usize().ok_or_else(|| {
            GenericFailure(
                "Integer overflow: The requested exponent is larger than the host machine word size".into(),
            )
            .to_error(context.stack_trace)
        })?;

        Ok(Self(checked_pow(self.0, rhs).ok_or_else(|| {
            GenericFailure(
                "Integer overflow: The computed value is too large to store in the integer".into(),
            )
            .to_error(context.stack_trace)
        })?)
        .into())
    }
    fn left_shift(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(context.stack_trace)?;
        Ok(Self(self.0 << rhs.0).into())
    }
    fn right_shift(self, context: &ExecutionContext, rhs: Value) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(context.stack_trace)?;
        Ok(Self(self.0 >> rhs.0).into())
    }
    fn unary_plus(self, _context: &ExecutionContext) -> ExpressionResult<Value> {
        Ok(self.clone().into())
    }
    fn unary_minus(self, context: &ExecutionContext) -> ExpressionResult<Value> {
        self.0.neg(context.stack_trace).into()
    }
    fn unary_not(self, _context: &ExecutionContext) -> ExpressionResult<Value> {
        Ok(Self(!self.0).into())
    }
    fn get_attribute(
        &self,
        context: &ExecutionContext,
        attribute: &str,
    ) -> ExpressionResult<Value> {
        match attribute {
            "count_ones" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::CountOnes,
            >()
            .into()),
            "count_zeros" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::CountZeros,
            >()
            .into()),
            "leading_zeros" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::LeadingZeros,
            >()
            .into()),
            "trailing_zeros" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::TrailingZeros,
            >()
            .into()),
            "leading_ones" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::LeadingOnes,
            >()
            .into()),
            "trailing_ones" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::TrailingOnes,
            >()
            .into()),
            "rotate_left" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::RotateLeft,
            >()
            .into()),
            "rotate_right" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::RotateRight,
            >()
            .into()),
            "reverse_bits" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::ReverseBits,
            >()
            .into()),
            "abs" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::Abs,
            >()
            .into()),
            "sqrt" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::Sqrt,
            >()
            .into()),
            "abs_diff" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::AbsDiff,
            >()
            .into()),
            "signum" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::Signum,
            >()
            .into()),
            "is_positive" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::IsPositive,
            >()
            .into()),
            "is_negative" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::IsNegative,
            >()
            .into()),
            "midpoint" => Ok(BuiltinFunction::new::<
                <<I as IntOps>::MethodSet as methods::MethodSet>::Midpoint,
            >()
            .into()),
            _ => Err(MissingAttributeError {
                name: attribute.into(),
            }
            .to_error(context.stack_trace)),
        }
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
    + std::fmt::Display
    + std::fmt::Debug
    + std::fmt::Binary
    + std::fmt::LowerExp
    + std::fmt::UpperExp
    + std::fmt::LowerHex
    + std::fmt::UpperHex
    + std::fmt::Octal
{
    type MethodSet: MethodSet + 'static;

    fn neg(&self, stack_trace: &StackTrace) -> ExpressionResult<Value>;

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
    type MethodSet = methods::SignedIntegerMethodSet;

    fn neg(&self, _stack_trace: &StackTrace) -> ExpressionResult<Value> {
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
        i64::rotate_left(*self, by.min(u32::MAX as u64) as u32)
    }

    fn rotate_right(&self, by: u64) -> Self {
        i64::rotate_right(*self, by.min(u32::MAX as u64) as u32)
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
    fn static_type_name() -> Cow<'static, str> {
        "Signed Integer".into()
    }
}

impl StaticType for i64 {
    fn static_type() -> ValueType {
        ValueType::SignedInteger
    }
}

impl IntOps for u64 {
    type MethodSet = methods::UnsignedIntegerMethodSet;

    fn neg(&self, stack_trace: &StackTrace) -> ExpressionResult<Value> {
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
        u64::rotate_left(*self, by.min(u32::MAX as u64) as u32)
    }

    fn rotate_right(&self, by: u64) -> Self {
        u64::rotate_right(*self, by.min(u32::MAX as u64) as u32)
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
    fn static_type_name() -> Cow<'static, str> {
        "Unsigned Integer".into()
    }
}

impl StaticType for u64 {
    fn static_type() -> ValueType {
        ValueType::UnsignedInteger
    }
}

mod methods {
    use enum_downcast::IntoVariant;

    use super::*;
    use crate::{build_method, execution::values::Boolean};

    pub trait MethodSet {
        type CountOnes;
        type CountZeros;

        type LeadingZeros;
        type TrailingZeros;

        type LeadingOnes;
        type TrailingOnes;

        type RotateLeft;
        type RotateRight;

        type ReverseBits;

        type Abs;
        type Sqrt;

        type AbsDiff;
        type Signum;
        type IsPositive;
        type IsNegative;
        type Midpoint;
    }

    macro_rules! build_method_set {
        ($name:ident) => {
            paste::paste! {
                pub struct [<$name CountOnes>];
                pub struct [<$name CountZeros>];
                pub struct [<$name LeadingZeros>];
                pub struct [<$name TrailingZeros>];
                pub struct [<$name LeadingOnes>];
                pub struct [<$name TrailingOnes>];
                pub struct [<$name RotateLeft>];
                pub struct [<$name RotateRight>];
                pub struct [<$name ReverseBitsf>];
                pub struct [<$name Abs>];
                pub struct [<$name Sqrt>];
                pub struct [<$name AbsDiff>];
                pub struct [<$name Signum>];
                pub struct [<$name IsPositive>];
                pub struct [<$name IsNegative>];
                pub struct [<$name Midpoint>];

                pub struct [<$name MethodSet>];
                impl MethodSet for [<$name MethodSet>] {
                    type CountOnes = [<$name CountOnes>];
                    type CountZeros = [<$name CountZeros>];
                    type LeadingZeros = [<$name LeadingZeros>];
                    type TrailingZeros = [<$name TrailingZeros>];
                    type LeadingOnes = [<$name LeadingOnes>];
                    type TrailingOnes = [<$name TrailingOnes>];
                    type RotateLeft = [<$name RotateLeft>];
                    type RotateRight = [<$name RotateRight>];
                    type ReverseBits = [<$name ReverseBitsf>];
                    type Abs = [<$name Abs>];
                    type Sqrt = [<$name Sqrt>];
                    type AbsDiff = [<$name AbsDiff>];
                    type Signum = [<$name Signum>];
                    type IsPositive = [<$name IsPositive>];
                    type IsNegative = [<$name IsNegative>];
                    type Midpoint = [<$name Midpoint>];
                }
            }
        };
    }

    build_method_set!(SignedInteger);
    build_method_set!(UnsignedInteger);

    pub fn register_methods<I>(database: &mut BuiltinCallableDatabase)
    where
        I: IntOps + Send + Sync + std::fmt::Debug + std::fmt::LowerHex,
        Integer<I>: StaticTypeName + 'static + Into<Value>,
        Value: IntoVariant<Integer<I>> + AsVariant<Integer<I>>,
    {
        build_method!(
            database,
            <I::MethodSet as MethodSet>::CountOnes, format!("{}::count_ones", Integer::<I>::static_type_name()), (
                _context: &ExecutionContext,
                this: Integer<I>
            ) -> UnsignedInteger {
                Ok(UnsignedInteger::from(this.0.count_ones()))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::CountZeros, format!("{}::count_zeros", Integer::<I>::static_type_name()), (
                _context: &ExecutionContext,
                this: Integer<I>
            ) -> UnsignedInteger {
                Ok(UnsignedInteger::from(this.0.count_zeros()))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::LeadingZeros, format!("{}::leading_zeros", Integer::<I>::static_type_name()), (
                _context: &ExecutionContext,
                this: Integer<I>
            ) -> UnsignedInteger {
                Ok(UnsignedInteger::from(this.0.leading_zeros()))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::TrailingZeros, format!("{}::trailing_zeros", Integer::<I>::static_type_name()), (
                _context: &ExecutionContext,
                this: Integer<I>
            ) -> UnsignedInteger {
                Ok(UnsignedInteger::from(this.0.trailing_zeros()))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::LeadingOnes, format!("{}::leading_ones", Integer::<I>::static_type_name()), (
                _context: &ExecutionContext,
                this: Integer<I>
            ) -> UnsignedInteger {
                Ok(UnsignedInteger::from(this.0.leading_ones()))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::TrailingOnes, format!("{}::trailing_ones", Integer::<I>::static_type_name()), (
                _context: &ExecutionContext,
                this: Integer<I>
            ) -> UnsignedInteger {
                Ok(UnsignedInteger::from(this.0.trailing_ones()))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::RotateLeft, format!("{}::rotate_left", Integer::<I>::static_type_name()), (
                _context: &ExecutionContext,
                this: Integer<I>,
                n: UnsignedInteger
            ) -> Integer<I> {
                Ok(Integer::<I>::from(this.0.rotate_left(n.0)))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::RotateRight, format!("{}::rotate_right", Integer::<I>::static_type_name()), (
                _context: &ExecutionContext,
                this: Integer<I>,
                n: UnsignedInteger
            ) -> Integer<I> {
                Ok(Integer::<I>::from(this.0.rotate_right(n.0)))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::ReverseBits, format!("{}::reverse_bits", Integer::<I>::static_type_name()), (
                _context: &ExecutionContext,
                this: Integer<I>
            ) -> Integer<I> {
                Ok(Integer::<I>::from(this.0.reverse_bits()))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::Abs, format!("{}::abs", Integer::<I>::static_type_name()), (
                _context: &ExecutionContext,
                this: Integer<I>
            ) -> Integer<I> {
                Ok(Integer::<I>::from(this.0.abs()))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::Sqrt, format!("{}::sqrt", Integer::<I>::static_type_name()), (
                _context: &ExecutionContext,
                this: Integer<I>
            ) -> Integer<I> {
                Ok(Integer::<I>::from(this.0.sqrt()))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::AbsDiff, format!("{}::abs_diff", Integer::<I>::static_type_name()), (
                context: &ExecutionContext,
                this: Integer<I>,
                other: Value
            ) -> UnsignedInteger {
                let other = other.downcast::<Integer<I>>(context.stack_trace)?;
                Ok(UnsignedInteger::from(this.0.abs_diff(other.0)))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::Signum, format!("{}::signum", Integer::<I>::static_type_name()), (
                _context: &ExecutionContext,
                this: Integer<I>
            ) -> SignedInteger {
                Ok(SignedInteger::from(this.0.signum()))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::IsPositive, format!("{}::is_positive", Integer::<I>::static_type_name()), (
                _context: &ExecutionContext,
                this: Integer<I>
            ) -> Boolean {
                Ok(Boolean(this.0.is_positive()))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::IsNegative, format!("{}::is_negative", Integer::<I>::static_type_name()), (
                _context: &ExecutionContext,
                this: Integer<I>
            ) -> Boolean {
                Ok(Boolean(this.0.is_negative()))
            }
        );
        build_method!(
            database,
            <I::MethodSet as MethodSet>::Midpoint, format!("{}::midpoint", Integer::<I>::static_type_name()), (
                context: &ExecutionContext,
                this: Integer<I>,
                rhs: Value
            ) -> Integer<I> {
                let rhs = rhs.downcast::<Integer<I>>(context.stack_trace)?;
                Ok(Integer::<I>::from(this.0.midpoint(rhs.0)))
            }
        );
    }
}

pub type SignedInteger = Integer<i64>;
pub type UnsignedInteger = Integer<u64>;

pub fn register_methods(database: &mut BuiltinCallableDatabase) {
    methods::register_methods::<u64>(database);
    methods::register_methods::<i64>(database);
}

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

    #[test]
    fn unsigned_count_ones() {
        let product = test_run("0x00FF00FF00FF00FFu::count_ones() == 32u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_count_ones() {
        let product = test_run("0x00FF00FF00FF00FFi::count_ones() == 32u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_count_zeros() {
        let product = test_run("0x00FF00FF00FF00FFu::count_zeros() == 32u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_count_zeros() {
        let product = test_run("0x00FF00FF00FF00FFi::count_zeros() == 32u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_leading_zeros() {
        let product = test_run("0x00FF00FF00FF00FFu::leading_zeros() == 8u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_leading_zeros() {
        let product = test_run("0x00FF00FF00FF00FFi::leading_zeros() == 8u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_trailing_zeros() {
        let product = test_run("0xFF00FF00FF00FF00u::trailing_zeros() == 8u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_trailing_zeros() {
        let product = test_run("0x0000FF00FF00FF00i::trailing_zeros() == 8u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_leading_ones() {
        let product = test_run("0xFF00FF00FF00FF00u::leading_ones() == 8u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_leading_ones() {
        let product = test_run("(-1i)::leading_ones() == 64u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_trailing_ones() {
        let product = test_run("0x00FF00FF00FF00FFu::trailing_ones() == 8u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_trailing_ones() {
        let product = test_run("0x00FF00FF00FF00FFi::trailing_ones() == 8u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_rotate_left() {
        let product =
            test_run("0x00FF00FF00FF00FFu::rotate_left(n = 4u) == 0x0FF00FF00FF00FF0u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_rotate_left() {
        let product =
            test_run("0x00FF00FF00FF00FFi::rotate_left(n = 4u) == 0x0FF00FF00FF00FF0i").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_rotate_right() {
        let product =
            test_run("0x00FF00FF00FF00FFu::rotate_right(n = 4u) == 0xF00FF00FF00FF00Fu").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_rotate_right() {
        let product = test_run("0xAAi::rotate_right(n = 12u) == 0x0AA0000000000000i").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_reverse_bits() {
        let product =
            test_run("0x5555555555555555u::reverse_bits() == 0xAAAAAAAAAAAAAAAAu").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_reverse_bits() {
        let product =
            test_run("0x00000000FFFF0000i::reverse_bits() == 0x0000FFFF00000000i").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_abs() {
        let product = test_run("10u::abs() == 10u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_abs() {
        let product = test_run("(-10i)::abs() == 10i").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_sqrt() {
        let product = test_run("64u::sqrt() == 8u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_sqrt() {
        let product = test_run("64i::sqrt() == 8i").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_abs_diff() {
        let product = test_run("10u::abs_diff(other = 5u) == 5u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_abs_diff() {
        let product = test_run("(-10i)::abs_diff(other = 5i) == 15u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_signum() {
        let product = test_run("10u::signum() == 1i").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("0u::signum() == 0i").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_signum() {
        let product = test_run("10i::signum() == 1i").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("0i::signum() == 0i").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("(-1i)::signum() == -1i").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_is_positive() {
        let product = test_run("10u::is_positive()").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("0u::is_positive()").unwrap();
        assert_eq!(product, Boolean(false).into());
    }

    #[test]
    fn signed_is_positive() {
        let product = test_run("10i::is_positive()").unwrap();
        assert_eq!(product, Boolean(true).into());

        let product = test_run("0i::is_positive()").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("(-1i)::is_positive()").unwrap();
        assert_eq!(product, Boolean(false).into());
    }

    #[test]
    fn unsigned_is_negative() {
        let product = test_run("10u::is_negative()").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("0u::is_negative()").unwrap();
        assert_eq!(product, Boolean(false).into());
    }

    #[test]
    fn signed_is_negative() {
        let product = test_run("10i::is_negative()").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("0i::is_negative()").unwrap();
        assert_eq!(product, Boolean(false).into());

        let product = test_run("(-1i)::is_negative()").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    // type Midpoint = [<$name Midpoint>];
    #[test]
    fn unsigned_midpoint() {
        let product = test_run("10u::midpoint(rhs = 10u) == 10u").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_midpoint() {
        let product = test_run("10i::midpoint(rhs = 10i) == 10i").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn unsigned_format() {
        let product = test_run("\"{a} {b:?} {c:o} {d:x} {e:X} {f:e} {g:E}\"::format(a = 10u, b = 32u, c = 0o123u, d = 0xDEADBEEFu, e = 0xDEADBEEFu, f = 1000u, g = 1000u) == \"10 32 123 deadbeef DEADBEEF 1e3 1E3\"").unwrap();
        assert_eq!(product, Boolean(true).into());
    }

    #[test]
    fn signed_format() {
        let product = test_run("\"{a} {b:?} {c:o} {d:x} {e:X} {f:e} {g:E}\"::format(a = 10i, b = 32i, c = 0o123i, d = 0xDEADBEEFi, e = 0xDEADBEEFi, f = 1000i, g = 1000i) == \"10 32 123 deadbeef DEADBEEF 1e3 1E3\"").unwrap();
        assert_eq!(product, Boolean(true).into());
    }
}

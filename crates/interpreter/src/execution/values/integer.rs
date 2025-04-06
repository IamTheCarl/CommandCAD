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
    ops::{BitAnd, BitOr, BitXor, Not},
};

use crate::{
    compile::SourceReference,
    execution::{
        errors::{ExpressionResult, GenericFailure, Raise},
        logging::RuntimeLog,
    },
};

use super::{value_type::VariableType, Object, StaticTypeName, Value};

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
pub struct Integer<I>(pub I);

impl<I> From<I> for Integer<I> {
    fn from(value: I) -> Self {
        Self(value)
    }
}

impl<I> Object for Integer<I>
where
    I: CheckedAdd
        + CheckedDiv
        + CheckedMul
        + CheckedSub
        + CheckedAdd
        + Ord
        + GetType
        + Hash
        + Copy
        + Eq
        + BitAnd<Output = I>
        + BitOr<Output = I>
        + BitXor<Output = I>
        + Neg
        + Not<Output = I>
        + One
        + ToPrimitive,
    Self: StaticTypeName + Into<Value>,
    Value: AsVariant<Self>,
{
    fn matches_type(
        &self,
        ty: &VariableType,
        _log: &mut dyn RuntimeLog,
        _stack_trace: &[SourceReference],
    ) -> ExpressionResult<bool> {
        Ok(*ty == I::get_type())
    }

    fn bit_and(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: &Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self(self.0 & rhs.0).into())
    }
    fn bit_or(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: &Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self(self.0 | rhs.0).into())
    }
    fn bit_xor(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: &Value,
    ) -> ExpressionResult<Value> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(Self(self.0 ^ rhs.0).into())
    }

    fn cmp(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: &Value,
    ) -> ExpressionResult<Ordering> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(self.0.cmp(&rhs.0))
    }
    fn addition(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: &Value,
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
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: &Value,
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
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: &Value,
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
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: &Value,
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
        &self,
        log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: &Value,
    ) -> ExpressionResult<Value> {
        self.divide(log, stack_trace, rhs)
    }
    fn exponent(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: &Value,
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
    fn unary_plus(
        &self,
        _log: &mut dyn RuntimeLog,
        _stack_trace: &[SourceReference],
    ) -> ExpressionResult<Value> {
        Ok(self.clone().into())
    }
    fn unary_minus(
        &self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
    ) -> ExpressionResult<Value> {
        self.0.neg(stack_trace).into()
    }
    fn unary_not(
        &self,
        _log: &mut dyn RuntimeLog,
        _stack_trace: &[SourceReference],
    ) -> ExpressionResult<Value> {
        Ok(Self(!self.0).into())
    }
}

trait GetType {
    fn get_type() -> VariableType;
}

/// Custom version of the std::ops::Neg trait that can fail.
trait Neg {
    fn neg(&self, stack_trace: &[SourceReference]) -> ExpressionResult<Value>;
}

impl GetType for i64 {
    fn get_type() -> VariableType {
        VariableType::SignedInteger
    }
}

impl Neg for i64 {
    fn neg(&self, _stack_trace: &[SourceReference]) -> ExpressionResult<Value> {
        Ok(SignedInteger::from(-self).into())
    }
}

impl StaticTypeName for Integer<i64> {
    fn static_type_name() -> &'static str {
        "Signed Integer"
    }
}

impl GetType for u64 {
    fn get_type() -> VariableType {
        VariableType::UnsignedInteger
    }
}

impl Neg for u64 {
    fn neg(&self, stack_trace: &[SourceReference]) -> ExpressionResult<Value> {
        super::UnsupportedOperationError {
            type_name: UnsignedInteger::static_type_name().into(),
            operation_name: "negate",
        }
        .raise(stack_trace)
    }
}

impl StaticTypeName for Integer<u64> {
    fn static_type_name() -> &'static str {
        "Unsigned Integer"
    }
}

pub type SignedInteger = Integer<i64>;
pub type UnsignedInteger = Integer<u64>;

#[cfg(test)]
mod test {
    use crate::{compile, execution::execute_expression};

    use super::*;

    #[test]
    fn signed_bit_or() {
        let root = compile::full_compile("test_file.ccm", "0xAAi | 0x55i");

        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, SignedInteger::from(0xFF).into());
    }

    #[test]
    fn signed_bit_and() {
        let root = compile::full_compile("test_file.ccm", "0xFFi & 0x0Fi");
        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, SignedInteger::from(0x0F).into());
    }

    #[test]
    fn signed_bit_xor() {
        let root = compile::full_compile("test_file.ccm", "0xF0i ^ 0x0FF");
        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, SignedInteger::from(0x0F).into());
    }

    #[test]
    fn cmp() {
        todo!()
    }

    #[test]
    fn signed_addition() {
        let root = compile::full_compile("test_file.ccm", "3i + 2i");

        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, SignedInteger::from(5).into());
    }

    #[test]
    fn signed_subtraction() {
        let root = compile::full_compile("test_file.ccm", "3i - 2i");

        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, SignedInteger::from(1).into());
    }

    #[test]
    fn signed_multiply() {
        let root = compile::full_compile("test_file.ccm", "3i * 2i");

        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, SignedInteger::from(6).into());
    }

    #[test]
    fn signed_divide() {
        let root = compile::full_compile("test_file.ccm", "6i / 2i");

        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, SignedInteger::from(3).into());
    }

    #[test]
    fn signed_floor_divide() {
        let root = compile::full_compile("test_file.ccm", "6i // 2i");

        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, SignedInteger::from(3).into());
    }

    #[test]
    fn signed_exponent() {
        let root = compile::full_compile("test_file.ccm", "6i ** 3i");

        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, SignedInteger::from(216).into());
    }

    #[test]
    fn signed_unary_plus() {
        let root = compile::full_compile("test_file.ccm", "+3i");

        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, SignedInteger::from(3).into());
    }

    #[test]
    fn signed_unary_minus() {
        let root = compile::full_compile("test_file.ccm", "-3i");

        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, SignedInteger::from(-3).into());
    }

    #[test]
    fn signed_unary_bit_not() {
        let root = compile::full_compile("test_file.ccm", "!0xAAi");

        let product = execute_expression(&mut Vec::new(), &mut Vec::new(), &root).unwrap();
        assert_eq!(product, SignedInteger::from(!0xAA).into());
    }
}

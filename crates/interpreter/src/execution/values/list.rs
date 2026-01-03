/*
 * Copyright 2024 James Carl
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

use crate::{
    compile::{AstNode, Expression, SourceReference},
    execute_expression,
    execution::{
        logging::RuntimeLog,
        stack::Stack,
        values::{closure::BuiltinCallableDatabase, StaticType},
    },
};

use super::{value_type::ValueType, ExpressionResult, Object, StaticTypeName, Value};

use std::sync::Arc;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct List {
    // In theory, we could use a lot less memory by dynamically sizing everything to fit
    // our smallest type, but we aren't going to implement that today.

    // None means that the list is empty and can be used anywhere.
    internal_type: Option<ValueType>,
    values: Arc<Vec<Value>>,
}

impl List {
    pub fn from_ast(
        log: &mut dyn RuntimeLog,
        stack_trace: &mut Vec<SourceReference>,
        stack: &mut Stack,
        database: &BuiltinCallableDatabase,
        ast_node: &AstNode<Vec<AstNode<Expression>>>,
    ) -> ExpressionResult<Self> {
        let values: ExpressionResult<Vec<Value>> = ast_node
            .node
            .iter()
            .map(|expression| execute_expression(log, stack_trace, stack, database, expression))
            .collect();

        Ok(List::from_iter(database, values?))
    }

    pub fn from_iter<I>(database: &BuiltinCallableDatabase, iterator: I) -> Self
    where
        I: IntoIterator<Item = Value>,
    {
        let values: Vec<_> = iterator.into_iter().collect();

        let internal_type =
            if let Some(initial_type) = values.first().map(|first| first.get_type(database)) {
                Some(values.iter().fold(initial_type, |accumulated, next| {
                    accumulated.merge(next.get_type(database))
                }))
            } else {
                // This is an empty list.
                None
            };

        Self {
            internal_type,
            values: Arc::new(values),
        }
    }
}

impl Object for List {
    fn get_type(&self, _callable_database: &BuiltinCallableDatabase) -> ValueType {
        ValueType::List(self.internal_type.clone().map(Box::new))
    }

    fn eq(
        self,
        _log: &mut dyn RuntimeLog,
        stack_trace: &[SourceReference],
        rhs: Value,
    ) -> ExpressionResult<bool> {
        let rhs: &Self = rhs.downcast_ref(stack_trace)?;
        Ok(self.values == rhs.values)
    }

    // TODO set operations
    // Append operations
}

impl StaticTypeName for List {
    fn static_type_name() -> &'static str {
        "List"
    }
}

impl StaticType for List {
    fn static_type() -> ValueType {
        ValueType::List(Option::None)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::execution::{test_run, values};

    #[test]
    fn create_empty() {
        let database = BuiltinCallableDatabase::default();
        let product = test_run("[]").unwrap();
        assert_eq!(product, List::from_iter(&database, []).into());
    }

    #[test]
    fn create() {
        let database = BuiltinCallableDatabase::default();
        let product = test_run("[1u, 2u, 3u]").unwrap();
        assert_eq!(
            product,
            List::from_iter(
                &database,
                [
                    values::UnsignedInteger::from(1).into(),
                    values::UnsignedInteger::from(2).into(),
                    values::UnsignedInteger::from(3).into()
                ]
            )
            .into()
        );
    }

    #[test]
    fn create_multi_type() {
        let database = BuiltinCallableDatabase::default();
        let product = test_run("[1u, 2i, 3u]").unwrap();
        assert_eq!(
            product,
            List::from_iter(
                &database,
                [
                    values::UnsignedInteger::from(1).into(),
                    values::SignedInteger::from(2).into(),
                    values::UnsignedInteger::from(3).into()
                ]
            )
            .into()
        );
    }

    #[test]
    fn type_detection() {
        let database = BuiltinCallableDatabase::default();
        assert_eq!(
            List::from_iter(
                &database,
                [
                    values::UnsignedInteger::from(1).into(),
                    values::SignedInteger::from(2).into(),
                    values::UnsignedInteger::from(3).into()
                ]
            )
            .internal_type,
            Some(ValueType::MultiType(
                Box::new(ValueType::UnsignedInteger),
                Box::new(ValueType::SignedInteger)
            ))
        );
    }
}
